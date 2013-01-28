(** Floating-point representation and utilities. *)

module Binable = Binable0

module type S = sig
  type t
  type outer = t
  with bin_io, sexp


  include Floatable.S with type t := t
  include Stringable.S with type t := t
  include Hashable.S_binable with type t := t
  (* [max] and [min] will return nan if either argument is nan *)
  include Comparable.S_binable with type t := t
  (* The results of robust comparisons on [nan] should be considered undefined. *)
  include Robustly_comparable.S with type t := t

  val nan : t

  val infinity : t
  val neg_infinity : t

  val max_value : t                   (* Float.infinity *)
  val min_value : t                   (* Float.neg_infinity *)
  val zero : t
  val epsilon : t   (* WARNING: This is not [Float.epsilon_float].  See Robust_compare. *)

  (* The difference between 1.0 and the smallest exactly representable floating-point
     number greater than 1.0. *)
  val epsilon_float : t

  val max_finite_value : t
  val min_positive_value : t

  val of_int : int -> t
  val to_int : t -> int
  val of_int64 : int64 -> t
  val to_int64 : t -> int64

  (** iround_towards_zero_exn raises Invalid_argument when either trying to handle nan or
      trying to handle a float outside the range (-. 2. ** 52., 2. ** 52.)
      (since floats have 52 significant bits) or outside the range
      (float min_int, float max_int) *)
  val iround_towards_zero_exn : t -> int           (* closer to 0 *)
  (** iround_towards_zero returns None if iround_towards_zero_exn raises an exception *)
  val iround_towards_zero : t -> int option        (* closer to 0 *)
  (* iround_down[_exn] rounds towards Float.neg_infinity *)
  val iround_down_exn : t -> int
  val iround_down : t -> int option
  (* iround_up[_exn] rounds toward Float.infinity *)
  val iround_up_exn : t -> int
  val iround_up : t -> int option
  (** iround_nearest_exn raises Invalid_argument in the same cases as
      iround_towards_zero_exn *)
  val iround_nearest_exn : t -> int
  (** iround_nearest returns None if iround_nearest_exn raises an exception *)
  (** [iround_nearest t] rounds t to the nearest int.  Returns None when t is too large to
      round to an int. *)
  val iround_nearest : t -> int option

  (* Caveat: If the absolute value of the input float is very large, then it could be that
     |round_down x - round_up x| > 1. *)
  (* round_down ..., [-2.,-1.) to -2., [-1.,0.) to -1., [0.,1.) to 0., [1.,2.) to 1.,...*)
  val round_down : t -> t
  (* round_down ..., (-2.,-1.] to -1., (-1.,0.] to -0., (0.,1.] to 1., (1.,2.] to 2.,...*)
  val round_up  : t -> t
  (* round_towards_zero ..., (-2.,-1.] to -1., (-1.,1.) to 0., [1.,2.) to 1.,...*)
  val round_towards_zero : t -> t
  (* round_nearest ...,[-1.5,-0.5) to -1., [-0.5,0.5) to 0., [0.5,1.5) to 1.,...*)
  val round_nearest : t -> t
  val round : t -> t (* same as [round_nearest] *)

  val is_nan : t -> bool

  (** includes positive and negative Float.infinity *)
  val is_inf : t -> bool

  (** min and max that return the other value if one of the values is a [nan]. Returns
      [nan] if both arguments are [nan]. *)
  val min_inan : t -> t -> t
  val max_inan : t -> t -> t

  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val (/) : t -> t -> t

  (** Returns the fractional part and the whole (i.e. integer) part.  For example, [modf
      (-3.14)] returns [{ fractional = -0.14; integral = -3.; }]! *)
  module Parts : sig
    type t
    val fractional : t -> outer
    val integral : t -> outer
  end
  val modf : t -> Parts.t

  (** [mod_float x y] returns a result with the same sign as [x].  It returns [nan] if [y]
      is [0].  It is basically
      [let mod_float x y = x -. float(truncate(x/.y)) *. y]
      not
      [let mod_float x y = x -. floor(x/.y) *. y]
      and therefore resembles [mod] on integers more than [%].
  *)
  val mod_float : t -> t -> t

  (* mostly for modules that inherit from t, since the infix operators are more
     convenient *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
  val scale : t -> t -> t
  val abs : t -> t

  (** Pretty print float, for example [to_string_hum ~decimals:3 1234.1999 = "1_234.200"]
      [to_string_hum ~decimals:3 ~strip_zero:true 1234.1999 = "1_234.2" ]. No delimiters
      are inserted to the right of the decimal. *)
  val to_string_hum
    :  ?delimiter:char  (* defaults to '_' *)
    -> ?decimals:int    (* defaults to 3 *)
    -> ?strip_zero:bool (* defaults to false *)
    -> float
    -> string

  (* [ldexp x n] returns x *. 2 ** n *)
  val ldexp : t -> int -> t

  (* [frexp f] returns the pair of the significant and the exponent of f. When f is zero,
     the significant x and the exponent n of f are equal to zero. When f is non-zero, they
     are defined by f = x *. 2 ** n and 0.5 <= x < 1.0. *)
  val frexp : t -> t * int

  module Class : sig
    type t =
    | Infinite
    | Nan
    | Normal
    | Subnormal
    | Zero
    with bin_io, sexp

    include Stringable.S with type t := t
  end

  (* return the Class.t.  Excluding nan the floating-point "number line" looks like:
       t                Class.t    example
     ^ neg_infinity     Infinite   neg_infinity
     | neg normals      Normal     -3.14
     | neg subnormals   Subnormal  -.2. ** -1023.
     | (-/+) zero       Zero       0.
     | pos subnormals   Subnormal  2. ** -1023.
     | pos normals      Normal     3.14
     v infinity         Infinite   infinity
  *)
  val classify : t -> Class.t

  (* [is_finite t] returns [true] iff [classify t] is in [Normal; Subnormal; Zero;]. *)
  val is_finite : t -> bool

  module Sign : sig
    type t = Neg | Zero | Pos with sexp
  end

  val sign : t -> Sign.t

  (* S-expressions contain at most 8 significant digits. *)
  module Terse : sig
    type t = outer with bin_io, sexp
    include Stringable.S with type t := t
  end
end
