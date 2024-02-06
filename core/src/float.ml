external format_float : string -> float -> string = "caml_format_float"

(* Stolen from [pervasives.ml].  Adds a "." at the end if needed.  It is in
   [pervasives.mli], but it also says not to use it directly, so we copy and paste the
   code. It makes the assumption on the string passed in argument that it was returned by
   [format_float] *)
let valid_float_lexem s =
  let l = String.length s in
  let rec loop i =
    if i >= l
    then s ^ "."
    else (
      match s.[i] with
      | '0' .. '9' | '-' -> loop (i + 1)
      | _ -> s)
  in
  loop 0
;;

open! Import

module Stable = struct
  module V1 = struct
    module T1 = struct
      include Base.Float

      type t = float
      [@@deriving bin_io ~localize, hash, sexp, stable_witness, typerep, globalize]
    end

    include T1
    include Comparable.Stable.V1.With_stable_witness.Make (T1)
  end
end

module T = Stable.V1
include T
include Hashable.Make_binable (T)
include Comparable.Map_and_set_binable_using_comparator (T)
include Comparable.Validate_with_zero (T)
module Replace_polymorphic_compare : Comparisons.S with type t := t = T

let validate_ordinary t =
  Validate.of_error_opt
    (let module C = Class in
    match classify t with
    | C.Normal | C.Subnormal | C.Zero -> None
    | C.Infinite -> Some "value is infinite"
    | C.Nan -> Some "value is NaN")
;;

module V = struct
  module ZZ = Comparable.Validate (T)

  let validate_bound ~min ~max t =
    Validate.first_failure (validate_ordinary t) (ZZ.validate_bound t ~min ~max)
  ;;

  let validate_lbound ~min t =
    Validate.first_failure (validate_ordinary t) (ZZ.validate_lbound t ~min)
  ;;

  let validate_ubound ~max t =
    Validate.first_failure (validate_ordinary t) (ZZ.validate_ubound t ~max)
  ;;
end

include V

module Robust_compare = struct
  module type S = sig
    (* intended to be a tolerance on human-entered floats *)

    val robust_comparison_tolerance : float

    include Robustly_comparable.S with type t := float
  end

  module Make (T : sig
    val robust_comparison_tolerance : float
  end) : S = struct
    (* We have test in the tree that rely on these functions not allocating, even without
       X_LIBRARY_INLING. The only way to ensure that these don't create temporary boxed
       floats without X_LIBRARY_INLING is for this code to see the float operations as
       externals, as defined in [Pervasives]. That's why we use [Poly] and float
       arithmetic from [Caml]. *)
    open Poly

    let robust_comparison_tolerance = T.robust_comparison_tolerance
    let ( >=. ) x y = x >= Stdlib.( -. ) y robust_comparison_tolerance
    let ( <=. ) x y = y >=. x
    let ( =. ) x y = x >=. y && y >=. x
    let ( >. ) x y = x > Stdlib.( +. ) y robust_comparison_tolerance
    let ( <. ) x y = y >. x
    let ( <>. ) x y = not (x =. y)

    let robustly_compare x y =
      let d = Stdlib.( -. ) x y in
      if d < Stdlib.( ~-. ) robust_comparison_tolerance
      then -1
      else if d > robust_comparison_tolerance
      then 1
      else 0
    ;;
  end
end

module Robustly_comparable = Robust_compare.Make (struct
  let robust_comparison_tolerance = 1E-7
end)

include Robustly_comparable

module O = struct
  include Base.Float.O
  include Robustly_comparable
end

module Terse = struct
  type nonrec t = t [@@deriving bin_io ~localize]

  include (
    Base.Float.Terse :
      module type of struct
        include Base.Float.Terse
      end
      with type t := t)
end

let robust_sign t : Sign.t = if t >. 0. then Pos else if t <. 0. then Neg else Zero

(* There are two issues:
   - Float.sign used to use robust comparison, and users of [Core] might have come to
     depend on this.
   - Robustness aside, what we get from Comparable.With_zero would map nan to Neg.
*)
let sign = robust_sign

(* Standard 12 significant digits, exponential notation used as necessary, guaranteed to
   be a valid OCaml float lexem, not to look like an int. *)
let to_string_12 x = valid_float_lexem (format_float "%.12g" x)
let quickcheck_generator = Base_quickcheck.Generator.float
let quickcheck_observer = Base_quickcheck.Observer.float
let quickcheck_shrinker = Base_quickcheck.Shrinker.float
let gen_uniform_excl = Base_quickcheck.Generator.float_uniform_exclusive
let gen_incl = Base_quickcheck.Generator.float_inclusive
let gen_without_nan = Base_quickcheck.Generator.float_without_nan
let gen_finite = Base_quickcheck.Generator.float_finite
let gen_positive = Base_quickcheck.Generator.float_strictly_positive
let gen_negative = Base_quickcheck.Generator.float_strictly_negative
let gen_zero = Base_quickcheck.Generator.float_of_class Zero
let gen_nan = Base_quickcheck.Generator.float_of_class Nan
let gen_subnormal = Base_quickcheck.Generator.float_of_class Subnormal
let gen_normal = Base_quickcheck.Generator.float_of_class Normal
let gen_infinite = Base_quickcheck.Generator.float_of_class Infinite
