open Std_internal

module type Key = sig
  type t
  include Floatable with type t := t
  include Sexpable with type t := t
  include Binable with type t := t
end

module type S = sig
  type key

  type t
  include Sexpable with type t := t
  include Binable with type t := t

  val create : (key * float) list -> (t, string) Result.t
  val get : t -> key -> float
end

module Make (Key : Key) = struct

  (* if long lists are needed, consider rewriting with binary search in array *)
  type t = (float * float) list with bin_io

  type key = Key.t

  let create knots =
    let t = List.map knots ~f:(fun (x, y) -> (Key.to_float x, y)) in
    let x_values = List.map t ~f:fst in
    if t = [] then
      Error "no knots given"
    else if not (List.is_sorted x_values ~compare:Float.compare) then
      Error "knots are unsorted"
    else if List.exists t ~f:(fun (x, y) -> Float.is_nan x || Float.is_nan y) then
      Error "knots contain nan"
    else
      Ok t

  type knots = (Key.t * float) list with sexp

  let t_of_sexp sexp =
    let knots = knots_of_sexp sexp in
    match create knots with
    | Error str -> Sexplib.Conv.of_sexp_error str sexp
    | Ok t -> t

  let sexp_of_t t =
    let knots = List.map t ~f:(fun (x, y) -> (Key.of_float x, y)) in
    sexp_of_knots knots

  let linear x (x1, y1) (x2, y2) =
    let weight = (x -. x1) /. (x2 -. x1) in (* note: numerically unstable if x2=.x1 *)
    let weight = Float.max_inan 0. (Float.min_inan 1. weight) in
    (1. -. weight) *. y1 +. weight *. y2

  let get t x =
    let x = Key.to_float x in
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

module Time = Make (Time)
module Float = Make (Float)
module Int = Make (Int)
