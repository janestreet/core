open Core_kernel.Std

open Int.Replace_polymorphic_compare

open Piecewise_linear_intf

module Common = struct
  (** Like [Array.is_sorted], but requires strictness if [strict] is true. *)
  (* Checks array is in (possibly strictly) ascending order per compare function *)
  let array_is_sorted ~strict a ~compare =
    let test =
      if strict
      then fun x1 x2 -> compare x1 x2 < 0
      else fun x1 x2 -> compare x1 x2 <= 0
    in
    let len = Array.length a in
    let rec loop i =
      if i >= len - 1
      then true
      else
        test a.(i) a.(i + 1) && loop (i + 1)
    in
    loop 0

  (** [array_is_strongly_finite a] checks that [a] is "strongly finite": in addition to
      requiring each individual element to be finite, we require that differences of
      consecutive elements are finite.

      This last condition is only relevant in corner cases like [a.(0) = -1e308, a.(1) =
      1e308], which will have [a.(1) -. a.(0) = infinity], even though the elements
      themselves are finite.

      The motivation for strong finiteness is that, if it fails, we can run into
      trouble with linear interpolation on the corresponding interval.

      Rather than [bool], the return type is [(unit, string) Result.t] for the sake of
      more informative error messages.  [Ok ()] is returned when the array is
      strongly finite, and [Error error_message] is returned if it is not strongly
      finite.
  *)
  let array_is_strongly_finite a =
    match Array.findi a ~f:(fun _ x -> not (Float.is_finite x)) with
    | Some (index, x) ->
      Error (sprintf "at index %i, had non-finite value %g" index x)
    | None ->
      let len = Array.length a in
      let rec loop i =
        if i >= len - 1
        then Ok ()
        else if Float.is_finite (a.(i+1) -. a.(i))
        then loop (i + 1)
        else Error (sprintf "at indices %i, %i, had non-finite difference \
                             %g -. %g = %g" i (i + 1) a.(i+1) a.(i) (a.(i+1) -. a.(i)))
      in
      loop 0

  TEST_MODULE = struct
      let a = [|0.; 0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8|];;

      let b = [|0.; 0.1; 0.2; 0.4; 0.4; 0.4; 0.6; 0.7|];;

      let c = [|0.; 0.5; 0.4; 0.6|] (* an array that is not sorted *)

      TEST =      array_is_sorted ~strict:false a ~compare:Float.compare
      TEST =      array_is_sorted ~strict:true  a ~compare:Float.compare
      TEST =      array_is_sorted ~strict:false b ~compare:Float.compare
      TEST = not (array_is_sorted ~strict:true  b ~compare:Float.compare)
      TEST = not (array_is_sorted ~strict:false c ~compare:Float.compare)
      TEST = not (array_is_sorted ~strict:true  c ~compare:Float.compare)

      let not_strongly_finite1 = [| (-1e308); 1e308 |]
      let not_strongly_finite2 = [| 1.; 2.; Float.nan |]
      let not_strongly_finite3 = [| Float.infinity |]

      TEST = Result.is_error (array_is_strongly_finite not_strongly_finite1)
      TEST = Result.is_error (array_is_strongly_finite not_strongly_finite2)
      TEST = Result.is_error (array_is_strongly_finite not_strongly_finite3)

  end
end

module Stable = struct
  module V1 = struct

    module Impl : sig
      type t with compare

      (** If [strict] is [true], the x-values must be strictly increasing. *)
      val create : strict:bool -> (float * float) list -> t Or_error.t

      val to_knots : t -> (float * float) list

      (** Data is not copied *)
      val to_knots' : t -> float array * float array

      (** Data is not copied or validated. *)
      val of_knots' : x:float array -> y:float array -> t

      (** Return the inverse of t. Requires strict monotonicity of y-values. Does not
          check x-values. *)
      val invert : t -> t Or_error.t

      val get : t -> float -> float

    end = struct
      (* float arrays don't have the boxing overhead that (float * float) arrays have.
         Also, when inverting, we can often swap x and y without duplicating the floats.
      *)
      type t =
        { x : float array (* the x coordinates of the knots *)
        ; y : float array (* the corresponding y coordinates of the knots *)
        } with compare

      (** [validate ~strict t] returns [Ok t] if [t] is valid, and an [Error] otherwise. *)
      let validate ~strict t =
        let { x; y } = t in
        let len_x = Array.length x in
        let len_y = Array.length y in
        if len_x <> len_y
        then Or_error.error_string (sprintf "length x = %i <> length y = %i" len_x len_y)
        else if len_x = 0
        then Or_error.error_string "no knots given"
        else match Common.array_is_strongly_finite y with
          | Error error_message ->
            error "problem with knot values" error_message <:sexp_of<string>>
          | Ok () ->
            match Common.array_is_strongly_finite x with
            | Error error_message ->
              error "problem with knot keys" error_message <:sexp_of<string>>
            | Ok () ->
              if Common.array_is_sorted ~strict ~compare:Float.compare x
              then Ok t
              else Or_error.error_string
                     (if strict
                      then "knot keys are not strictly increasing"
                      else "knot keys are unsorted")

      let create ~strict knots =
        let x = Array.of_list_map knots ~f:fst in
        let y = Array.of_list_map knots ~f:snd in
        let tentative_result = { x; y } in
        validate ~strict tentative_result

      let to_knots t =
        (* [validate] above checks that [t.x] and [t.y] have the same length. *)
        Array.map2_exn t.x t.y ~f:(fun x y -> (x,y))
        |! Array.to_list

      (* Note that while we are not copying t.x and t.y here, they are not returned
         directly to the user.  Rather, they will get passed to Array.map in
         Make(...).to_knots'. *)
      let to_knots' t = (t.x, t.y)

      let of_knots' ~x ~y = { x; y}

      let invert t =
        (* We try swapping x and y and validating *)
        match validate ~strict:true { x = t.y; y = t.x } with
        | Ok _ as result -> result
        | Error error_when_same_order ->
          (* Try reversing them; i.e. [invert t] should work if t.y is either
             strictly increasing or strictly decreasing *)
          let x = Array.copy t.y in
          Array.rev_inplace x;
          let y = Array.copy t.x in
          Array.rev_inplace y;
          match validate ~strict:true { x; y } with
          | Ok _ as result -> result
          | Error error_when_reversed ->
            Or_error.error "Swapping x and y failed, for both original and reversed order"
              (`Same_order error_when_same_order, `Reverse_order error_when_reversed)
              <:sexp_of<[`Same_order of Error.t] * [`Reverse_order of Error.t]>>

      let linear ~x ~x1 ~y1 ~x2 ~y2 =
        let weight = (x -. x1) /. (x2 -. x1) in (* note: numerically unstable if x2=.x1 *)
        (* A note about [weight]: [linear] is only called with [x1 <= x < x2], and [x2
           -. x1] finite.  Even with float issues, this should yield the following
           guarantees: 0. <= x -. x1 <= x2 -. x1 < infinity, 0. < x2 -. x1 (As annoying as
           denormals are, they do lead to the desirable property that x1 < x2 implies 0. <
           x2 -. x1.)  So, we should get 0. <= weight <= 1.  *)
        assert (Float.(<=) 0. weight && Float.(<=) weight 1.);
        y1 +. weight *. (y2 -. y1)


      let get t x =
        if Float.(<>) x x (* same as Float.is_nan but slightly faster *)
        then invalid_arg "Piecewise_linear.get on nan";
        let t_x = t.x in
        let t_y = t.y in
        let l = Array.length t_x in
        if Float.(<=) x t_x.(0)
        then t_y.(0)
        else if Float.(<=) t_x.(l - 1) x
        then t_y.(l - 1)
        else
          (* loop invariant: t_x.(m) <= x < t_x.(n) *)
          let rec loop m n =
            if n - m <= 1
            then m
            else
              let mid = (m + n) / 2 in
              if Float.(<=) t_x.(mid) x
              then loop mid n
              else loop m mid
          in
          let i = loop 0 (l - 1) in
          linear ~x ~x1:t_x.(i) ~y1:t_y.(i) ~x2:t_x.(i + 1) ~y2:t_y.(i + 1)

    end

    type ('key, 'value) t_ = Impl.t

    module type S = S with type ('k, 'v) t_ := ('k, 'v) t_

    module Make (Key : Float_like) (Value : Float_like) = struct
      type key = Key.t
      type value = Value.t

      module T = struct
        type t = Impl.t with compare

        let create knots =
          let float_knots =
            List.map knots ~f:(fun (x, y) -> (Key.to_float x, Value.to_float y))
          in
          Impl.create ~strict:false float_knots

        type knots = (Key.t * Value.t) list with sexp

        let to_knots t =
          List.map (Impl.to_knots t)
            ~f:(fun (x, y) -> (Key.of_float x, Value.of_float y))

        let to_knots' t =
          let (x, y) = Impl.to_knots' t in
          (Array.map x ~f:Key.of_float, Array.map y ~f:Value.of_float)

        let t_of_sexp sexp =
          let knots = knots_of_sexp sexp in
          match create knots with
          | Error error -> Sexplib.Conv.of_sexp_error (Error.to_string_hum error) sexp
          | Ok t -> t

        let sexp_of_t t =
          sexp_of_knots (to_knots t)

        (* We convert back to Key.t and Value.t for bin_io, because, for example,
           Time.Stable.V1 has bin_io, but there is no guarantee that Time.to_float and
           Time.of_float will not have their semantics change.  So, the user who goes to
           the trouble of creating inputs K and V to Make with stable bin_io will be
           assured that Make(K)(V).Stable.V1.t has stable bin_io.  *)
        module Binable = struct
          type t =
            { x : Key.t array
            ; y : Value.t array
            } with bin_io
        end

        let to_binable t =
          let (x, y) = to_knots' t in
          { Binable. x; y }

        let of_binable bt =
          let x = Array.map bt.Binable.x ~f:Key.to_float in
          let y = Array.map bt.Binable.y ~f:Value.to_float in
          Impl.of_knots' ~x ~y

      end

      include T
      include Bin_prot.Utils.Make_binable(T)

      let get t x = Value.of_float (Impl.get t (Key.to_float x))

    end

    type invertible =
      { regular : Impl.t
      ; inverse : Impl.t
      }
    with compare

    type ('key, 'value) t_invertible = invertible

    module type S_invertible = S_invertible with type ('k, 'v) t_ := ('k, 'v) t_invertible

    module Make_invertible (Key : Float_like) (Value : Float_like) = struct
      module M = Make (Key) (Value)

      type key = Key.t
      type value = Value.t

      module T = struct
        type t = invertible with compare

        let sexp_of_t t = M.sexp_of_t t.regular

        let t_of_sexp sexp =
          let regular = M.t_of_sexp sexp in
          match Impl.invert regular with
          | Ok inverse -> { regular; inverse }
          | Error error -> Sexplib.Conv.of_sexp_error (Error.to_string_hum error) sexp

        (* Our bin_io only stores [regular], and reconstructs [inverse] when loading. *)
        module Binable = struct
          type t = M.t with bin_io
        end

        let to_binable t = t.regular

        let of_binable regular =
          let inverse  =
            Or_error.tag (Impl.invert regular)
              "Got non-invertible set of knots when deserializing?"
            |> Or_error.ok_exn
          in
          { regular; inverse }

      end

      include T
      include Bin_prot.Utils.Make_binable(T)

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
      let to_knots' t = M.to_knots' t.regular

      let get_inverse t y = Key.of_float (Impl.get t.inverse (Value.to_float y))

    end
  end
end
include Stable.V1

module F = Float
module Time_ = Time (* so we can refer to it later *)
module Time  = Make (Time)  (F)
module Ofday = Make (Ofday) (F)
module Span  = Make (Span)  (F)
module Float = Make (Float) (F)
module Int   = Make (Int)   (F)

TEST_MODULE = struct

  let expected name expected actual =
    if F.(<>) expected actual
    then failwithf "in %s: expected %.2f, actual %.2f" name expected actual ()

  let expected_tol ~tol name expected actual =
    if F.(>) (F.abs (expected -. actual)) tol
    then failwithf "in %s: expected %.15g, actual %.15g" name expected actual ()

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
      expected "get" 1.25 (Float.get t 1.05);
      expected "get" 1. (Float.get t 0.9);
      expected "get" 2. (Float.get t 2.5);
      expected "get" 2. (Float.get t 2.)

    TEST_UNIT = (* Test the normal case with repeated x-values in knots *)
      let knots = [(0.1, 2.); (0.5, 4.); (0.5, 5.); (0.9, 1.); (2.2, 2.2)] in
      let t = Or_error.ok_exn (Float.create knots) in
      expected "get" 5. (Float.get t 0.5); (* right continuity *)
      expected "get" 4. (Float.get t 0.6);
      expected "get" 3.5 (Float.get t 0.4);
      expected_tol ~tol:1e-12 "get" (4. -. 5e-7) (Float.get t (0.5 -. 1e-7))

    TEST_UNIT = (* Test a degenerate case *)
      let knots = [(1., 2.)] in
      let t = Or_error.ok_exn (Float.create knots) in
      expected "get" 2. (Float.get t 0.5);
      expected "get" 2. (Float.get t 1.);
      expected "get" 2. (Float.get t 1.5)

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
      check_both t 1.05 1.25;
      (* Some values before first knot, after last knot *)
      expected "get" 1. (Float_invertible.get t 0.5);
      expected "get" 2. (Float_invertible.get t 2.5);
      expected "get_inverse" 1. (Float_invertible.get_inverse t 0.5);
      expected "get_inverse" 2. (Float_invertible.get_inverse t 2.5)

    TEST_UNIT =   (* Test the case where y is decreasing *)
      let is_invertible = [(1., 2.); (1.1, 1.5); (2., 1.)] in
      let t = Or_error.ok_exn (Float_invertible.create is_invertible) in
      check_both t 1. 2.;
      check_both t 1.1 1.5;
      check_both t 1.05 1.75;
      (* Some values before first knot, after last knot *)
      expected "get" 2. (Float_invertible.get t 0.5);
      expected "get" 1. (Float_invertible.get t 2.5);
      expected "get_inverse" 2. (Float_invertible.get_inverse t 0.5);
      expected "get_inverse" 1. (Float_invertible.get_inverse t 2.5)

  end

  TEST_MODULE "io" = struct
    (* The following tests want exact equality, so they could fail if sexp conversion or
       binary io is inexact.  However, we have used values that should be preserved
       exactly. *)

    open Bigarray

    let t0 = Time_.of_string "2014-05-20 12:34:56-04:00"
    let t1 = Time_.of_string "2014-05-20 15:00:00-04:00"
    let t2 = Time_.of_string "2014-05-20 16:00:00-04:00"

    TEST "sexp" =
      let knots = [(t0, 2.); (t1, 1.5); (t2, 1.)] in
      let t = Or_error.ok_exn (Time.create knots) in
      let sexp = Time.sexp_of_t t in
      let t' = Time.t_of_sexp sexp in
      Time.compare t t' = 0

    (* The following type annotation is included so that if, say, buf changes to
       fortran_layout--not that we expect that--then the code will no longer compile. *)
    let buf_append buf0 buf1 : (char, int8_unsigned_elt, c_layout) Array1.t =
      let len0 = Array1.dim buf0 in
      let len1 = Array1.dim buf1 in
      let result = Bin_prot.Common.create_buf (len0 + len1) in
      Bin_prot.Common.blit_buf ~src_pos:0 ~src:buf0 ~dst_pos:0 ~dst:result len0;
      Bin_prot.Common.blit_buf ~src_pos:0 ~src:buf1 ~dst_pos:len0 ~dst:result len1;
      result

    let reader_of_buf src =
      let current_offset = ref 0 in
      fun dst ~pos ~len -> begin
          Bin_prot.Common.blit_buf ~src_pos:!current_offset ~src ~dst_pos:pos ~dst len;
          current_offset := !current_offset + len
        end

    TEST "bin_io" =
      let knots0 = [(t0, 2.); (t1, 1.5); (t2, 1.)] in
      let knots1 = [(t0, 0.5); (t1, 3.); (t2, 3.5)] in
      let t0 = Or_error.ok_exn (Time.create knots0) in
      let buf0 = Bin_prot.Utils.bin_dump ~header:true Time.bin_writer_t t0 in
      let t1 = Or_error.ok_exn (Time.create knots1) in
      let buf1 = Bin_prot.Utils.bin_dump ~header:true Time.bin_writer_t t1 in
      let buf = buf_append buf0 buf1 in
      let read = reader_of_buf buf in
      let t0' = Bin_prot.Utils.bin_read_stream ~read Time.bin_reader_t in
      let t1' = Bin_prot.Utils.bin_read_stream ~read Time.bin_reader_t in
      Time.compare t0 t0' = 0 && Time.compare t1 t1' = 0
      && Time.compare t0 t1 <> 0

    module Time_invertible = Make_invertible (Time_) (F)

    TEST "invertible sexp" =
      let knots = [(t0, 2.); (t1, 1.5); (t2, 1.)] in
      let t = Or_error.ok_exn (Time_invertible.create knots) in
      let sexp = Time_invertible.sexp_of_t t in
      let t' = Time_invertible.t_of_sexp sexp in
      Time_invertible.compare t t' = 0

    TEST "invertible bin_io" =
      let knots0 = [(t0, 2.); (t1, 1.5); (t2, 1.)] in
      let knots1 = [(t0, 0.5); (t1, 3.); (t2, 3.5)] in
      let t0 = Or_error.ok_exn (Time_invertible.create knots0) in
      let buf0 = Bin_prot.Utils.bin_dump ~header:true Time_invertible.bin_writer_t t0 in
      let t1 = Or_error.ok_exn (Time_invertible.create knots1) in
      let buf1 = Bin_prot.Utils.bin_dump ~header:true Time_invertible.bin_writer_t t1 in
      let buf = buf_append buf0 buf1 in
      let read = reader_of_buf buf in
      let t0' = Bin_prot.Utils.bin_read_stream ~read Time_invertible.bin_reader_t in
      let t1' = Bin_prot.Utils.bin_read_stream ~read Time_invertible.bin_reader_t in
      Time_invertible.compare t0 t0' = 0 && Time_invertible.compare t1 t1' = 0
      && Time_invertible.compare t0 t1 <> 0

  end
end

BENCH_MODULE "Piecewise_linear tests" = struct
  let num_knots = [2; 100; 10_000]

  let create_knots len =
    let a = -2.1 in
    let b = 2.1 in
    (* x is linearly spaced from a to b and y = x ** 3. *)
    let dx = (b -. a) /. (F.of_int len -. 1.) in
    Array.init len ~f:(fun i ->
      let x = a +. F.of_int i *. dx in
      (x, x *. x *. x)
    )
    |> Array.to_list

  (* Some x values can yield faster lookups than others, so we explore the domain a little
     in the benchmarks by leting x' = x ** 2 - 2.  This bounces around in the interval
     [-2, 2].  *)
  BENCH_INDEXED "get" len num_knots =
    let t = Or_error.ok_exn (Float.create (create_knots len)) in
    let x = ref 0.3 in
    (fun () ->
       let x' = !x in
       let x' = x' *. x' -. 2. in
       x := x';
       ignore (Float.get t x'))

  module FI = Make_invertible (F) (F)

  (* This can actually be a little faster than the above, but note that the knots are
     different. *)
  BENCH_INDEXED "get_inverse" len num_knots =
    let t = Or_error.ok_exn (FI.create (create_knots len)) in
    let x = ref 0.3 in
    (fun () ->
       let x' = !x in
       let x' = x' *. x' -. 2. in
       x := x';
       ignore (FI.get_inverse t x'))

  BENCH_FUN "testing overhead" =
    let x = ref 0.3 in
    (fun () ->
       let x' = !x in
       let x' = x' *. x' -. 2. in
       x := x')

end
