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
  (* CR-soon chardin for chardin: Switch to an implementation that uses a pair of float arrays
     (or possibly an Array2.t, but that seems like overkill) and binary search on keys.
     In addition to the obvious benefit of using less memory and being faster for
     nontrivial numbers of knots, the case where Make_invertible is used for an increasing
     function will be able to reuse the same two arrays for the inverse (just swapped).

     yminsky: How about we fix this, at least partially, now?  This is after all a real
     bit of code that we use in PTSs.  Converting to an array representation should
     improve performance and should add very little complexity-wise.  I'm OK with putting
     off the binary search, but the array representation seems like a layup.

     jphillips: assigned

     jphillips: Don't see why we should put off the binary search, once you have the
     array, it's straightforward.

     mrussell: Reassigned. The jphillips-binomial feature has become very long-lived.  I
     would suggest putting off these orthogonal improvements in favor or trying to
     actually release jphillips-binomial.

     jphillips: On second thoughts, I agree with mrussell.

     chardin: Changed to CR-soon after speaking with yminsky.  I have an array
     implementation ready to go, but I'll put it in a separate feature.
  *)
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
    (* CR-soon chardin for chardin: I believe y1 +. weight *. (y2 -. y1) is the better way
       to do the following computation.  E.g., if y1 and y2 are very large compared
       to their difference, it's not obvious to me that
           (1. -. weight) *. y1 +. weight *. y2
       will always be between y1 and y2 (due to roundoff error in each summand).
       It also saves a multiply, though that's less important.

       yminsky: Made this into a CR.  We're already reading the code.  I don't want to
       think about it again later.  Let's just fix it now.

       jphillips: It's not clear which is better.  Lets call two formulations (S)ymmetric
       and (C)hardin

       suppose that weight = 1 and y1 = 1E9. y2 = 1E-9
       then
       y2 -. y1 = -10E9 (rounded)
       so
       (C) = y1 + weight *. (y2 -. y1) = 1E9 *. 1. *. (-1E9) = 0 < y2
       (S) = (1 - weight) * y1 + weight * y2 = 0 * 1E9 + 1 * 1E-9 = 1E-9
       (C) fails while (S) is okay.

       Conversely, I think that (S) can fail when y1 = y2, e.g. take weight =
       ULP(0.5), then 1 - weight = 1, but it's possible that weight * y2 > ULP(y1), so
       1 * y1 + weight * y2 > y1.

       My inclination is that (S) is better.  I think that it only fails when weight is
       very close to ULP(0.5), whereas (C) [corrected, as per chardin's comment below]
       will fail whenever |y2 / y1| < ULP (1).

       ULP(x) = unit of least precision of x

       chardin: Changed to CR-soon after speaking with yminsky.

       (As above, "failing" will mean that the interpolated value does not lie
       between y1 and y2, inclusive.)

       I think jphillips meant "whereas (C) will fail whenever |y2 / y1| < ULP (1)",
       [jphillips: yes] but even then, it should be more like "whereas (C) will fail
       whenever |y2 / y1| < ULP (1) *and* 1 - weight is very close to ULP(0.5)".  Having 1
       - weight very close to  ULP(0.5) seems no more likely than having weight very close
       to ULP(0.5).

       Also, I disagree that (S) fails only when weight is very close to ULP(0.5).
       About 10% of random examples I generated with y1 = y2 made (S) fail, and the
       weights didn't have to be small.  (More precisely, generating a random y1 between
       0. and 100. and a random weight from 0. to 1., interpolating with y1 = y2 failed
       a little over 10% of the time.) E.g., letting
       y1 = y2 = 3.41996554447786449 and weight = 0.360433606780867888
       the interpolated value (1. -. weight) *. y1 +. weight *. y2 is strictly greater
       than y1 and y2.  (When y1 = y2, the interpolated value returned by (C) will always
       be y1.)  Put another way: If using (S) to interpolate a constant function, the
       result is typically not constant.  (Actually, that's subtly different, but a random
       experiment bears that out as well.)

       So far, we have only considered the question of whether or not the interpolated
       value will lie between y1 and y2.  Another property that would seem to be desirable
       from our linear interpolation is the following monotonicity: if y1 <= y2, and we
       increase the weight, then the interpolated value should not decrease.  (Or,
       inversely, if y1 >= y2, and we increase the weight, then the interpolated value
       should not increase.)  (C) will always obey this kind of monotonicity within each
       interval (though, in light of your example, when we go from one interval to the
       next, it would be possible to violate this monotonicity slightly).  (S) can
       fail to have this kind of monotonicity.  E.g., letting y1 = 3. and y2 = 3.1,
       about 17% of random weights between 0 and 1 have the property that increasing
       the weight by Float.epsilon_float decreases the interpolated value.  Also, even if
       you're not intrinsically interested in monotonicity for its own sake, the failure
       of monotonicity demonstrates that (S) is introducing more numerical error than
       necessary.

       So, I still think y1 + weight *. (y2 -. y1) is better overall.  It only runs into
       trouble when the weight is very close to 1 and |y2 / y1| is very close to 0.  That
       seems mild compared to all the situations given above where (S) runs into trouble.
       (Also, in my array implementation--and we intend to end up with an array
       implementation eventually--we will have x1 <= x < x2, which eliminates the most
       common way to get a weight close to 1, which is to have x = x2.  So, weights within
       ULP(0.5) of 1 will be rare.)

       If you are bothered by the behavior of (C) for weights very close to 1, I
       think it would be better to use something like
        if Float.(<=) weight 0.5
        then y1 +. weight *. (y2 -. y1)
        else y2 +. (1. -. weight) *. (y1 -. y2)
       rather than (S).  (Personally, I think the small benefit provided by this
       if-then-else formula is not worth the added complexity; I'd go with (C).)

       jphillips: Interesting.

       I think the question is: Do we want to provide continuity at the nodes (in which
       case, use (S)) or monotonicity between them (in which case, use (C))? But I think
       it's fair to say that if someone's usage of Piecewise_linear is sensitive to either
       of these properties then they are probably doing something wrong.

       I no longer have a preference either way.  Happy if you go with (C).

       An aside: I wonder which gives better relative error (for strictly positive
       endpoints). My intuition would have been (S), but my intuition has already been
       shown to be flawed.

       kmason: FWIW, I would guess weights close to 1 are more likely than weights
       close to 0.5, because knots happen at special times. That said, I think perhaps (C)
       is better just because of the saved multiply .

       chardin: I'm not going to change it in this feature, but I'm going to use (C) in
       the separate feature that uses an array implementation.
    *)
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

  (* CR-someday chardin: Eventually, we should always require keys to be strictly
     increasing, especially since we have an eye toward changing to a representation
     that stores keys and values in arrays and uses binary search--in that case,
     allowing repeated keys would result in either weird semantics or messy code.

     jphillips: binary search could still look for the first key of a given value.

     chardin: I withdraw my claim that allowing repeated keys would result in weird
     semantics or messy code.  I even think we should allow it (though possibly require
     the user to override a default) for the sake of allowing discontinuities, and just
     make sure the behavior when there are repeated keys is unambiguously specified in the
     interface.

     While we're on the topic, I think the better behavior when there are repeated keys is
     to return the *last* instance (as I argue below), even though it would not agree with
     the existing semantics.  (If there is existing code that is relying on this
     particular property of the existing semantics, though, I would consider that a bug,
     since the interface doesn't specify such behavior.  Still, it's a cause for caution.)

     Suppose we have an array k of length l of key values, and that k is nondecreasing.
     Suppose we have an x such that k.(0) < x < k.(l - 1).
     Consider the following possibilities for the value i that our binary search
     will return.
     (a)   The greatest i such that k.(i) <= x.
     (a')  The unique i such that k.(i) <= x < k.(i + 1).
     (b)   The least i such that x <= k.(i + 1).
     (b')  The unique i such that k.(i) < x <= k.(i + 1).
     (b'') The least i such that k.(i) <= x <= k.(i + 1).

     Under the circumstances in question, (a) is equivalent to (a'); (b) is equivalent to
     (b') and (b''). (Note: (b'') mimics the current list-based semantics.)

     I think (a) is more natural than (b), in that the predicate on i involves k.(i),
     rather than k.(i+1).  The binary search will have a cleaner invariant: we will be
     bracketing with m and n where k.(m) <= x < k.(n), with m initialized to 0 and n
     initialized to l - 1.  At any stage, we know that the i we are searching for
     satisfies m <= i < n.

     Now consider the implementation of the binary search for (b). The bracketing will
     maintain the invariant k.(m + 1) < x <= k.(n + 1), with m initialized to -1 and n
     initialized to l - 2, knowing at each stage that m < i <= n.  The awkwardness is
     caused by the fact that (b) is working with left-open right-closed intervals, but
     there is already a sense in which left-closed right-open intervals are built into the
     problem: our index i will satisfy 0 <= i < l - 1.  We can express that as
     -1 < i <= l - 2, but that's awful and essentially a type error: array indices "live"
     in the interval [0, infinity), so we shouldn't have to use -1 as an endpoint when
     describing an interval of indices.

     We could instead maintain the invariant k.(m) < x <= k.(n), with m initialized to 0
     and n initialized to l - 1, knowing at each stage that m <= i < n.  But then we're
     using left-closed right-open intervals in "index-space" and left-open right-closed
     intervals in "key-space", which is awkward.  It's also less obvious that maintaining
     the invariant k.(m) < x <= k.(n) will yield the least i such that x <= k.(i+1).

     I also prefer (a) because, in the "(C) vs. (S)" question in an earlier CR, if we
     resolve in favor of (C), we like it when the weight is in the interval [0, 1)
     (being equal to 1 only from roundoff error).  With weights in (0,1], we would
     want a formula like y2 +. (1 -. weight) *. (y1 -. y2), which is ugly, because
     it requires you to think from right to left.

     jphillips: If we go with (C) then (a) is definitely the correct choice.  If we decide
     to allow repeated keys then we ought to allow the user to specify left- or right-
     continuity semantics (maybe we can easily support both by reversing the arrays).
  *)
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

  (* CR-someday chardin: The automatically generated binary representation will be about
     twice as large as necessary, because [inverse] is redundant information.  However, if
     size is an issue, the higher priority is to switch to a representation that uses
     float arrays instead of (float * float) list.  *)
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

