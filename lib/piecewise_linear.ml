open Std_internal

include Piecewise_linear_intf

(** The underlying implementation, using float for keys and values. *)
module Impl : sig
  type t with bin_io, compare

  (** If strict is true, the x-values must be strictly increasing. *)
  val create : (float * float) list -> strict:bool -> t Or_error.t

  val to_knots : t -> (float * float) list

  (** Return the inverse of t. Requires strict monotonicity of y-values. Does not
      check x-values. *)
  val invert : t -> t Or_error.t

  val get : t -> float -> float
end = struct
  type t = (float * float) list with bin_io, compare

  (** Like [List.is_sorted], but requires strictness if [strict] is set to true. *)
  let is_sorted l ~strict ~compare =
    let test =
      if strict
      then fun x1 x2 -> compare x1 x2 < 0
      else fun x1 x2 -> compare x1 x2 <= 0
    in
    let rec loop l =
      match l with
      | [] | [_] -> true
      | x1 :: ((x2 :: _) as rest) ->
        test x1 x2 && loop rest
    in loop l

  let create knots ~strict =
    if knots = []
    then Or_error.error_string "no knots given"
    else if List.exists knots ~f:(fun (x, y) -> Float.is_nan x || Float.is_nan y)
    then Or_error.error_string "knots contain nan"
    else
      let x_values = List.map knots ~f:fst in
      if is_sorted ~strict ~compare:Float.compare x_values
      then Ok knots
      else match strict with
      | true  -> Or_error.error_string "knots are not strictly increasing"
      | false -> Or_error.error_string "knots are unsorted"

  let to_knots t = t

  let invert t =
    let y_values = List.map t ~f:snd in
    if is_sorted ~strict:true y_values ~compare:Float.compare
    then Ok (List.map t ~f:(fun (x,y) -> (y,x)))
    else if is_sorted ~strict:true y_values ~compare:Float.descending
    then Ok (List.rev_map t ~f:(fun (x,y) -> (y,x)))
    else Or_error.error_string "y values not monotone"

  let linear x (x1, y1) (x2, y2) =
    let weight = (x -. x1) /. (x2 -. x1) in (* note: numerically unstable if x2=.x1 *)
    let weight = Float.max_inan 0. (Float.min_inan 1. weight) in
    (1. -. weight) *. y1 +. weight *. y2

  let get t x =
    if Float.is_nan x
    then invalid_arg "Piecewise_linear.get on nan";
    let rec loop = function
      | left :: ((right :: _) as rest) ->
          if x <= fst left then snd left
          else if x <= fst right then linear x left right
          else loop rest
      | last :: [] -> snd last
      | [] -> failwith "Bug in Piecewise_linear.get"
    in
    loop t

end

module Make (Key : Key) (Value : Value) = struct
  type key = Key.t
  type value = Value.t

  type t = Impl.t with bin_io, compare

  let create knots =
    let float_knots =
      List.map knots ~f:(fun (x, y) -> (Key.to_float x, Value.to_float y))
    in
    Impl.create ~strict:false float_knots

  type knots = (Key.t * Value.t) list with sexp

  let to_knots t =
    List.map (Impl.to_knots t) ~f:(fun (x, y) -> (Key.of_float x, Value.of_float y))

  let t_of_sexp sexp =
    let knots = knots_of_sexp sexp in
    match create knots with
    | Error error -> Sexplib.Conv.of_sexp_error (Error.to_string_hum error) sexp
    | Ok t -> t

  let sexp_of_t t =
    sexp_of_knots (to_knots t)

  let get t x = Value.of_float (Impl.get t (Key.to_float x))

end


module Make_invertible (Key : Key) (Value : Value) = struct
  module M = Make (Key) (Value)


  type key = Key.t
  type value = Value.t

  type t =
    { regular : Impl.t
    ; inverse : Impl.t
    }
  with bin_io, compare

  let sexp_of_t t = M.sexp_of_t t.regular

  let t_of_sexp sexp =
    let regular = M.t_of_sexp sexp in
    match Impl.invert regular with
    | Ok inverse -> { regular; inverse }
    | Error error -> Sexplib.Conv.of_sexp_error (Error.to_string_hum error) sexp

  let create knots =
    let open Result.Monad_infix in
    let float_knots =
      List.map knots ~f:(fun (x, y) -> (Key.to_float x, Value.to_float y))
    in
    Impl.create ~strict:true float_knots
    >>= fun regular ->
    Impl.invert regular
    >>| fun inverse ->
    { regular; inverse }

  let get t = M.get t.regular

  let to_knots t = M.to_knots t.regular

  let get_inverse t y = Key.of_float (Impl.get t.inverse (Value.to_float y))

end

module F = Float
module Time  = Make (Time)  (F)
module Ofday = Make (Ofday) (F)
module Span  = Make (Span)  (F)
module Float = Make (Float) (F)
module Int   = Make (Int)   (F)

TEST_MODULE = struct

  let expected name expected actual =
    if expected <> actual
    then failwithf "in %s: expected %.2f, actual %.2f" name expected actual ()

  let to_knots x_values = List.map x_values ~f:(fun x -> (x, 0.))
  let bad_knots = to_knots [1.; 0.; 2.]

  TEST_MODULE "normal" = struct
    TEST = Result.is_ok (Float.create (to_knots [1.; 1.; 2.]))
    TEST = Result.is_error (Float.create bad_knots)

    TEST_UNIT = (* Test the normal case *)
      let knots = [(1., 1.); (1.1, 1.5); (2., 2.)] in
      let t = Or_error.ok_exn (Float.create knots) in
      expected "get" 1. (Float.get t 1.);
      expected "get" 1.5 (Float.get t 1.1);
      expected "get" 1.25 (Float.get t 1.05)
  end

  TEST_MODULE "invertible" = struct
    module Float_invertible = Make_invertible (F) (F)

    let not_invertible = [(1., 1.); (1., 1.5); (2., 2.)]
    let not_invertible2 = [(1., 1.); (1.5, 1.); (2., 2.)]
    TEST = Result.is_error (Float_invertible.create not_invertible)
    TEST = Result.is_error (Float_invertible.create not_invertible2)
    TEST = Result.is_error (Float_invertible.create bad_knots)

    let check_both t x y =
      expected "get" y (Float_invertible.get t x);
      expected "get_inverse" x (Float_invertible.get_inverse t y)

    TEST_UNIT = (* Test the case where both x and y are increasing *)
      let is_invertible = [(1., 1.); (1.1, 1.5); (2., 2.)] in
      let t = Or_error.ok_exn (Float_invertible.create is_invertible) in
      check_both t 1. 1.;
      check_both t 1.1 1.5;
      check_both t 1.05 1.25

    TEST_UNIT =   (* Test the case where y is decreasing *)
      let is_invertible = [(1., 2.); (1.1, 1.5); (2., 1.)] in
      let t = Or_error.ok_exn (Float_invertible.create is_invertible) in
      check_both t 1. 2.;
      check_both t 1.1 1.5;
      check_both t 1.05 1.75

    TEST_UNIT =   (* Test the case where both y is decreasing *)
      let is_invertible = [(1., 2.); (1.1, 1.5); (2., 1.)] in
      let t = Or_error.ok_exn (Float_invertible.create is_invertible) in
      check_both t 1. 2.;
      check_both t 1.1 1.5;
      check_both t 1.05 1.75

  end
end

