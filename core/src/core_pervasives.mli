(** This module has exactly the same interface and implementation as INRIA's Pervasives,
    except that some things are deprecated in favor of Core equivalents.

    Functions here are in one of four states:
    1. Deprecated using [@@deprecated].
    2. Accepted as part of Core for the forseeable future.
    3. We haven't yet decided whether to deprecate, as indicated by a CR.
    4. We plan to deprecate eventually, but haven't yet, as indicated by a CR.

    Eventually, this module will be removed, and it will be recommended to compile with
    -nopervasives when using Core.
*)

(** {6 Exceptions} *)

(** Raise the given exception value *)
external raise : exn -> 'a = "%reraise"

(** A faster version [raise] which does not record the backtrace.
    @since 4.02.0
*)
external raise_notrace : exn -> 'a = "%raise_notrace"

(** Raise exception [Invalid_argument] with the given string. *)
val invalid_arg : string -> 'a

(** Raise exception [Failure] with the given string. *)
val failwith : string -> 'a

(** The [Exit] exception is not raised by any library function.  It is
    provided for use in your programs. *)
exception Exit

(** {6 Comparisons} *)

(** [e1 = e2] tests for structural equality of [e1] and [e2].
    Mutable structures (e.g. references and arrays) are equal
    if and only if their current contents are structurally equal,
    even if the two mutable objects are not the same physical object.
    Equality between functional values raises [Invalid_argument].
    Equality between cyclic data structures may not terminate. *)
external ( = ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%equal"

(** Negation of {!Poly.( = )}. *)
external ( <> ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%notequal"

(** See {!Poly.( >= )}. *)
external ( < ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%lessthan"

(** See {!Poly.( >= )}. *)
external ( > ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%greaterthan"

(** See {!Poly.( >= )}. *)
external ( <= ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%lessequal"

(** Structural ordering functions. These functions coincide with
    the usual orderings over integers, characters, strings, byte sequences
    and floating-point numbers, and extend them to a
    total ordering over all types.
    The ordering is compatible with [( = )]. As in the case
    of [( = )], mutable structures are compared by contents.
    Comparison between functional values raises [Invalid_argument].
    Comparison between cyclic structures may not terminate. *)
external ( >= ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%greaterequal"

(** [compare x y] returns [0] if [x] is equal to [y],
    a negative integer if [x] is less than [y], and a positive integer
    if [x] is greater than [y].  The ordering implemented by [compare]
    is compatible with the comparison predicates [=], [<] and [>]
    defined above,  with one difference on the treatment of the float value
    {!Caml.nan}.  Namely, the comparison predicates treat [nan]
    as different from any other float value, including itself;
    while [compare] treats [nan] as equal to itself and less than any
    other float value.  This treatment of [nan] ensures that [compare]
    defines a total ordering relation.

    [compare] applied to functional values may raise [Invalid_argument].
    [compare] applied to cyclic structures may not terminate.

    The [compare] function can be used as the comparison function
    required by the {!Set.Make} and {!Map.Make} functors, as well as
    the {!List.sort} and {!Array.sort} functions. *)
external compare : ('a[@local_opt]) -> ('a[@local_opt]) -> int = "%compare"

(** Return the smaller of the two arguments.
    The result is unspecified if one of the arguments contains
    the float value [nan]. *)
val min : 'a -> 'a -> 'a

(** Return the greater of the two arguments.
    The result is unspecified if one of the arguments contains
    the float value [nan]. *)
val max : 'a -> 'a -> 'a

(** [e1 == e2] tests for physical equality of [e1] and [e2].
    On mutable types such as references, arrays, byte sequences, records with
    mutable fields and objects with mutable instance variables,
    [e1 == e2] is true if and only if physical modification of [e1]
    also affects [e2].
    On non-mutable types, the behavior of [( == )] is
    implementation-dependent; however, it is guaranteed that
    [e1 == e2] implies [compare e1 e2 = 0]. *)
external ( == ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%eq"
  [@@deprecated "[since 2014-10] Use [phys_equal]"]

(** Negation of {!( == )}. *)
external ( != ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%noteq"
  [@@deprecated "[since 2014-10] Use [phys_equal]"]

(** {6 Boolean operations} *)

(** The boolean negation. *)
external not : (bool[@local_opt]) -> bool = "%boolnot"

(** The boolean 'and'. Evaluation is sequential, left-to-right:
    in [e1 && e2], [e1] is evaluated first, and if it returns [false],
    [e2] is not evaluated at all. *)
external ( && ) : (bool[@local_opt]) -> (bool[@local_opt]) -> bool = "%sequand"

(** The boolean 'or'. Evaluation is sequential, left-to-right:
    in [e1 || e2], [e1] is evaluated first, and if it returns [true],
    [e2] is not evaluated at all. *)
external ( || ) : (bool[@local_opt]) -> (bool[@local_opt]) -> bool = "%sequor"

(** {6 Debugging} *)

(** [__LOC__] returns the location at which this expression appears in
    the file currently being parsed by the compiler, with the standard
    error format of OCaml: "File %S, line %d, characters %d-%d" *)
external __LOC__ : string = "%loc_LOC"

(** [__FILE__] returns the name of the file currently being
    parsed by the compiler. *)
external __FILE__ : string = "%loc_FILE"

(** [__LINE__] returns the line number at which this expression
    appears in the file currently being parsed by the compiler. *)
external __LINE__ : int = "%loc_LINE"

(** [__MODULE__] returns the module name of the file being
    parsed by the compiler. *)
external __MODULE__ : string = "%loc_MODULE"

(** [__POS__] returns a tuple [(file,lnum,cnum,enum)], corresponding
    to the location at which this expression appears in the file
    currently being parsed by the compiler. [file] is the current
    filename, [lnum] the line number, [cnum] the character position in
    the line and [enum] the last character position in the line. *)
external __POS__ : string * int * int * int = "%loc_POS"

[%%if ocaml_version >= (4, 12, 0)]

(** [__FUNCTION__] returns the name of the current function or method, including
    any enclosing modules or classes. *)
external __FUNCTION__ : string = "%loc_FUNCTION"

[%%else]

val __FUNCTION__ : string

[%%endif]

(** [__LOC_OF__ expr] returns a pair [(loc, expr)] where [loc] is the
    location of [expr] in the file currently being parsed by the
    compiler, with the standard error format of OCaml: "File %S, line
    %d, characters %d-%d" *)
external __LOC_OF__ : ('a[@local_opt]) -> (string * 'a[@local_opt]) = "%loc_LOC"

(** [__LINE_OF__ expr] returns a pair [(line, expr)], where [line] is the
    line number at which the expression [expr] appears in the file
    currently being parsed by the compiler. *)
external __LINE_OF__ : ('a[@local_opt]) -> (int * 'a[@local_opt]) = "%loc_LINE"

(** [__POS_OF__ expr] returns a pair [(expr,loc)], where [loc] is a
    tuple [(file,lnum,cnum,enum)] corresponding to the location at
    which the expression [expr] appears in the file currently being
    parsed by the compiler. [file] is the current filename, [lnum] the
    line number, [cnum] the character position in the line and [enum]
    the last character position in the line. *)
external __POS_OF__
  :  ('a[@local_opt])
  -> ((string * int * int * int) * 'a[@local_opt])
  = "%loc_POS"

(** {6 Composition operators} *)

(** Reverse-application operator: [x |> f |> g] is exactly equivalent
    to [g (f (x))].
    @since 4.01
*)
external ( |> ) : 'a -> (('a -> 'b)[@local_opt]) -> 'b = "%revapply"

(** Application operator: [g @@ f @@ x] is exactly equivalent to
    [g (f (x))].
    @since 4.01
*)
external ( @@ ) : (('a -> 'b)[@local_opt]) -> 'a -> 'b = "%apply"

(** {6 Integer arithmetic} *)

(** Integers are 31 bits wide (or 63 bits on 64-bit processors).
    All operations are taken modulo 2{^31} (or 2{^63}).
    They do not fail on overflow. *)

(** Unary negation. You can also write [- e] instead of [~- e]. *)
external ( ~- ) : (int[@local_opt]) -> int = "%negint"

(** Unary addition. You can also write [+ e] instead of [~+ e].
    @since 3.12.0
*)
external ( ~+ ) : (int[@local_opt]) -> int = "%identity"

(** [succ x] is [x + 1]. *)
external succ : (int[@local_opt]) -> int = "%succint"

(** [pred x] is [x - 1]. *)
external pred : (int[@local_opt]) -> int = "%predint"

(** Integer addition. *)
external ( + ) : (int[@local_opt]) -> (int[@local_opt]) -> int = "%addint"

(** Integer subtraction. *)
external ( - ) : (int[@local_opt]) -> (int[@local_opt]) -> int = "%subint"

(** Integer multiplication. *)
external ( * ) : (int[@local_opt]) -> (int[@local_opt]) -> int = "%mulint"

(** Integer division.
    Raise [Division_by_zero] if the second argument is 0.
    Integer division rounds the real quotient of its arguments towards zero.
    More precisely, if [x >= 0] and [y > 0], [x / y] is the greatest integer
    less than or equal to the real quotient of [x] by [y].  Moreover,
    [(- x) / y = x / (- y) = - (x / y)].  *)
external ( / ) : (int[@local_opt]) -> (int[@local_opt]) -> int = "%divint"

(** Integer remainder.  If [y] is not zero, the result
    of [x mod y] satisfies the following properties:
    [x = (x / y) * y + x mod y] and
    [abs(x mod y) <= abs(y) - 1].
    If [y = 0], [x mod y] raises [Division_by_zero].
    Note that [x mod y] is negative only if [x < 0].
    Raise [Division_by_zero] if [y] is zero. *)
external ( mod ) : (int[@local_opt]) -> (int[@local_opt]) -> int = "%modint"

(** Return the absolute value of the argument.  Note that this may be
    negative if the argument is [min_int]. *)
val abs : int -> int

(** The greatest representable integer. *)
val max_int : int
  [@@deprecated "[since 2014-10] Use [Int.max_value]"]

(** The smallest representable integer. *)
val min_int : int
  [@@deprecated "[since 2014-10] Use [Int.min_value]"]

(** {7 Bitwise operations} *)

(** Bitwise logical and. *)
external ( land ) : (int[@local_opt]) -> (int[@local_opt]) -> int = "%andint"

(** Bitwise logical or. *)
external ( lor ) : (int[@local_opt]) -> (int[@local_opt]) -> int = "%orint"

(** Bitwise logical exclusive or. *)
external ( lxor ) : (int[@local_opt]) -> (int[@local_opt]) -> int = "%xorint"

(** Bitwise logical negation. *)
val lnot : int -> int

(** [n lsl m] shifts [n] to the left by [m] bits.
    The result is unspecified if [m < 0] or [m >= bitsize],
    where [bitsize] is [32] on a 32-bit platform and
    [64] on a 64-bit platform. *)
external ( lsl ) : (int[@local_opt]) -> (int[@local_opt]) -> int = "%lslint"

(** [n lsr m] shifts [n] to the right by [m] bits.
    This is a logical shift: zeroes are inserted regardless of
    the sign of [n].
    The result is unspecified if [m < 0] or [m >= bitsize]. *)
external ( lsr ) : (int[@local_opt]) -> (int[@local_opt]) -> int = "%lsrint"

(** [n asr m] shifts [n] to the right by [m] bits.
    This is an arithmetic shift: the sign bit of [n] is replicated.
    The result is unspecified if [m < 0] or [m >= bitsize]. *)
external ( asr ) : (int[@local_opt]) -> (int[@local_opt]) -> int = "%asrint"

(** {6 Floating-point arithmetic}

    OCaml's floating-point numbers follow the
    IEEE 754 standard, using double precision (64 bits) numbers.
    Floating-point operations never raise an exception on overflow,
    underflow, division by zero, etc.  Instead, special IEEE numbers
    are returned as appropriate, such as [infinity] for [1.0 /. 0.0],
    [neg_infinity] for [-1.0 /. 0.0], and [nan] ('not a number')
    for [0.0 /. 0.0].  These special numbers then propagate through
    floating-point computations as expected: for instance,
    [1.0 /. infinity] is [0.0], and any arithmetic operation with [nan]
    as argument returns [nan] as result.
*)

(** Unary negation. You can also write [-. e] instead of [~-. e]. *)
external ( ~-. ) : (float[@local_opt]) -> (float[@local_opt]) = "%negfloat"

(** Unary addition. You can also write [+. e] instead of [~+. e].
    @since 3.12.0
*)
external ( ~+. ) : (float[@local_opt]) -> (float[@local_opt]) = "%identity"

(** Floating-point addition *)
external ( +. )
  :  (float[@local_opt])
  -> (float[@local_opt])
  -> (float[@local_opt])
  = "%addfloat"

(** Floating-point subtraction *)
external ( -. )
  :  (float[@local_opt])
  -> (float[@local_opt])
  -> (float[@local_opt])
  = "%subfloat"

(** Floating-point multiplication *)
external ( *. )
  :  (float[@local_opt])
  -> (float[@local_opt])
  -> (float[@local_opt])
  = "%mulfloat"

(** Floating-point division. *)
external ( /. )
  :  (float[@local_opt])
  -> (float[@local_opt])
  -> (float[@local_opt])
  = "%divfloat"

(** Exponentiation. *)
external ( ** ) : float -> float -> float = "caml_power_float" "pow"
  [@@unboxed] [@@noalloc]

(** Square root. *)
external sqrt : float -> float = "caml_sqrt_float" "sqrt"
  [@@unboxed] [@@noalloc]

(** Exponential. *)
external exp : float -> float = "caml_exp_float" "exp"
  [@@unboxed] [@@noalloc]

(** Natural logarithm. *)
external log : float -> float = "caml_log_float" "log"
  [@@unboxed] [@@noalloc]

(** Base 10 logarithm. *)
external log10 : float -> float = "caml_log10_float" "log10"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2016-07] Use [Float.log10]"]

(** [expm1 x] computes [exp x -. 1.0], giving numerically-accurate results
    even if [x] is close to [0.0].
    @since 3.12.0
*)
external expm1 : float -> float = "caml_expm1_float" "caml_expm1"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2016-07] Use [Float.expm1]"]

(** [log1p x] computes [log(1.0 +. x)] (natural logarithm),
    giving numerically-accurate results even if [x] is close to [0.0].
    @since 3.12.0
*)
external log1p : float -> float = "caml_log1p_float" "caml_log1p"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2016-07] Use [Float.log1p]"]

(** Cosine.  Argument is in radians. *)
external cos : float -> float = "caml_cos_float" "cos"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2016-07] Use [Float.cos]"]

(** Sine.  Argument is in radians. *)
external sin : float -> float = "caml_sin_float" "sin"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2016-07] Use [Float.sin]"]

(** Tangent.  Argument is in radians. *)
external tan : float -> float = "caml_tan_float" "tan"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2016-07] Use [Float.tan]"]

(** Arc cosine.  The argument must fall within the range [[-1.0, 1.0]].
    Result is in radians and is between [0.0] and [pi]. *)
external acos : float -> float = "caml_acos_float" "acos"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2016-07] Use [Float.acos]"]

(** Arc sine.  The argument must fall within the range [[-1.0, 1.0]].
    Result is in radians and is between [-pi/2] and [pi/2]. *)
external asin : float -> float = "caml_asin_float" "asin"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2016-07] Use [Float.asin]"]

(** Arc tangent.
    Result is in radians and is between [-pi/2] and [pi/2]. *)
external atan : float -> float = "caml_atan_float" "atan"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2016-07] Use [Float.atan]"]

(** [atan2 y x] returns the arc tangent of [y /. x].  The signs of [x]
    and [y] are used to determine the quadrant of the result.
    Result is in radians and is between [-pi] and [pi]. *)
external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2016-07] Use [Float.atan2]"]

(** [hypot x y] returns [sqrt(x *. x + y *. y)], that is, the length
    of the hypotenuse of a right-angled triangle with sides of length
    [x] and [y], or, equivalently, the distance of the point [(x,y)]
    to origin.
    @since 4.00.0  *)
external hypot : float -> float -> float = "caml_hypot_float" "caml_hypot"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2016-07] Use [Float.hypot]"]

(** Hyperbolic cosine.  Argument is in radians. *)
external cosh : float -> float = "caml_cosh_float" "cosh"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2016-07] Use [Float.cosh]"]

(** Hyperbolic sine.  Argument is in radians. *)
external sinh : float -> float = "caml_sinh_float" "sinh"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2016-07] Use [Float.sinh]"]

(** Hyperbolic tangent.  Argument is in radians. *)
external tanh : float -> float = "caml_tanh_float" "tanh"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2016-07] Use [Float.tanh]"]

(** Hyperbolic arc cosine.  The argument must fall within the range
    [[1.0, inf]].
    Result is in radians and is between [0.0] and [inf].

    @since 4.13.0
*)
external acosh : float -> float = "caml_acosh_float" "caml_acosh"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2022-11] Use [Float.acosh]"]

(** Hyperbolic arc sine.  The argument and result range over the entire
    real line.
    Result is in radians.

    @since 4.13.0
*)
external asinh : float -> float = "caml_asinh_float" "caml_asinh"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2022-11] Use [Float.asinh]"]

(** Hyperbolic arc tangent.  The argument must fall within the range
    [[-1.0, 1.0]].
    Result is in radians and ranges over the entire real line.

    @since 4.13.0
*)
external atanh : float -> float = "caml_atanh_float" "caml_atanh"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2022-11] Use [Float.atanh]"]

(** Round above to an integer value.
    [ceil f] returns the least integer value greater than or equal to [f].
    The result is returned as a float. *)
external ceil : float -> float = "caml_ceil_float" "ceil"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2014-10] Use [Float.round_up]"]

(** Round below to an integer value.
    [floor f] returns the greatest integer value less than or
    equal to [f].
    The result is returned as a float. *)
external floor : float -> float = "caml_floor_float" "floor"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2014-10] Use [Float.round_down]"]

(** [abs_float f] returns the absolute value of [f]. *)
external abs_float : float -> float = "%absfloat"
  [@@deprecated "[since 2014-10] Use [Float.abs]"]

(** [copysign x y] returns a float whose absolute value is that of [x]
    and whose sign is that of [y].  If [x] is [nan], returns [nan].
    If [y] is [nan], returns either [x] or [-. x], but it is not
    specified which.
    @since 4.00.0  *)
external copysign : float -> float -> float = "caml_copysign_float" "caml_copysign"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2016-07] Use [Float.copysign]"]

(** [mod_float a b] returns the remainder of [a] with respect to
    [b].  The returned value is [a -. n *. b], where [n]
    is the quotient [a /. b] rounded towards zero to an integer. *)
external mod_float : float -> float -> float = "caml_fmod_float" "fmod"
  [@@unboxed] [@@noalloc] [@@deprecated "[since 2014-10] Use [Float.mod_float]"]

(** [frexp f] returns the pair of the significant
    and the exponent of [f].  When [f] is zero, the
    significant [x] and the exponent [n] of [f] are equal to
    zero.  When [f] is non-zero, they are defined by
    [f = x *. 2 ** n] and [0.5 <= x < 1.0]. *)
external frexp : float -> float * int = "caml_frexp_float"
  [@@deprecated "[since 2014-10] Use [Float.frexp]"]

(** [ldexp x n] returns [x *. 2 ** n]. *)
external ldexp
  :  (float[@unboxed])
  -> (int[@untagged])
  -> (float[@unboxed])
  = "caml_ldexp_float" "caml_ldexp_float_unboxed"
  [@@noalloc] [@@deprecated "[since 2014-10] Use [Float.ldexp]"]

(** [modf f] returns the pair of the fractional and integral
    part of [f]. *)
external modf : float -> float * float = "caml_modf_float"
  [@@deprecated "[since 2014-10] Use [Float.modf]"]

(** Same as {!Caml.float_of_int}. *)
external float : (int[@local_opt]) -> (float[@local_opt]) = "%floatofint"

(** Convert an integer to floating-point. *)
external float_of_int : (int[@local_opt]) -> (float[@local_opt]) = "%floatofint"

(** Same as {!Caml.int_of_float}. *)
external truncate : (float[@local_opt]) -> int = "%intoffloat"
  [@@deprecated "[since 2014-10] Use [Float.iround_towards_zero_exn]"]

(** Truncate the given floating-point number to an integer.
    The result is unspecified if the argument is [nan] or falls outside the
    range of representable integers. *)
external int_of_float : (float[@local_opt]) -> int = "%intoffloat"

(** Positive infinity. *)
val infinity : float
  [@@deprecated "[since 2014-10] Use [Float.infinity]"]

(** Negative infinity. *)
val neg_infinity : float
  [@@deprecated "[since 2014-10] Use [Float.neg_infinity]"]

(** A special floating-point value denoting the result of an
    undefined operation such as [0.0 /. 0.0].  Stands for
    'not a number'.  Any floating-point operation with [nan] as
    argument returns [nan] as result.  As for floating-point comparisons,
    [=], [<], [<=], [>] and [>=] return [false] and [<>] returns [true]
    if one or both of their arguments is [nan]. *)
val nan : float
  [@@deprecated "[since 2014-10] Use [Float.nan]"]

(** The largest positive finite value of type [float]. *)
val max_float : float
  [@@deprecated "[since 2014-10] Use [Float.max_value]"]

(** The smallest positive, non-zero, non-denormalized value of type [float]. *)
val min_float : float
  [@@deprecated "[since 2014-10] Use [Float.min_value]"]

(** The difference between [1.0] and the smallest exactly representable
    floating-point number greater than [1.0]. *)
val epsilon_float : float
  [@@deprecated "[since 2014-10] Use [Float.epsilon_float]"]

(** The five classes of floating-point numbers, as determined by
    the {!Caml.classify_float} function. *)
type fpclass = Stdlib.fpclass =
  | FP_normal (** Normal number, none of the below *)
  | FP_subnormal (** Number very close to 0.0, has reduced precision *)
  | FP_zero (** Number is 0.0 or -0.0 *)
  | FP_infinite (** Number is positive or negative infinity *)
  | FP_nan (** Not a number: result of an undefined operation *)

(** Return the class of the given floating-point number:
    normal, subnormal, zero, infinite, or not a number. *)
external classify_float
  :  (float[@unboxed])
  -> fpclass
  = "caml_classify_float" "caml_classify_float_unboxed"
  [@@noalloc] [@@deprecated "[since 2014-10] Use [Float.classify]"]

(** {6 String operations}

    More string operations are provided in module {!String}.
*)

(** String concatenation. *)
val ( ^ ) : string -> string -> string

(** {6 Character operations}

    More character operations are provided in module {!Char}.
*)

(** Return the ASCII code of the argument. *)
external int_of_char : (char[@local_opt]) -> int = "%identity"

(** Return the character with the given ASCII code.
    Raise [Invalid_argument "char_of_int"] if the argument is
    outside the range 0--255. *)
val char_of_int : int -> char

(** {6 Unit operations} *)

(** Discard the value of its argument and return [()].
    For instance, [ignore(f x)] discards the result of
    the side-effecting function [f].  It is equivalent to
    [f x; ()], except that the latter may generate a
    compiler warning; writing [ignore(f x)] instead
    avoids the warning. *)
external ignore : ('a[@local_opt]) -> unit = "%ignore"

(** {6 String conversion functions} *)

(** Return the string representation of a boolean. As the returned values
    may be shared, the user should not modify them directly.
*)
val string_of_bool : bool -> string

(** Convert the given string to a boolean.
    Raise [Invalid_argument "bool_of_string"] if the string is not
    ["true"] or ["false"]. *)
val bool_of_string : string -> bool

(** Return the string representation of an integer, in decimal. *)
val string_of_int : int -> string

(** Convert the given string to an integer.
    The string is read in decimal (by default) or in hexadecimal (if it
    begins with [0x] or [0X]), octal (if it begins with [0o] or [0O]),
    or binary (if it begins with [0b] or [0B]).
    Raise [Failure "int_of_string"] if the given string is not
    a valid representation of an integer, or if the integer represented
    exceeds the range of integers representable in type [int]. *)
external int_of_string : string -> int = "caml_int_of_string"

(** Return the string representation of a floating-point number. *)
val string_of_float : float -> string

(** Convert the given string to a float.  Raise [Failure "float_of_string"]
    if the given string is not a valid representation of a float. *)
external float_of_string : string -> float = "caml_float_of_string"

(** {6 Pair operations} *)

[%%if flambda_backend]

(** Return the first component of a pair. *)
external fst : ('a * 'b[@local_opt]) -> ('a[@local_opt]) = "%field0_immut"

(** Return the second component of a pair. *)
external snd : ('a * 'b[@local_opt]) -> ('b[@local_opt]) = "%field1_immut"

[%%else]

(** Return the first component of a pair. *)
external fst : ('a * 'b[@local_opt]) -> ('a[@local_opt]) = "%field0"

(** Return the second component of a pair. *)
external snd : ('a * 'b[@local_opt]) -> ('b[@local_opt]) = "%field1"

[%%endif]

(** {6 List operations}

    More list operations are provided in module {!List}.
*)

(** List concatenation. *)
val ( @ ) : 'a list -> 'a list -> 'a list
  [@@deprecated "[since 2014-10] Use [List.Infix]"]

(** {6 Input/output}
    Note: all input/output functions can raise [Sys_error] when the system
    calls they invoke fail. *)

(** The type of input channel. *)
type in_channel = Stdlib.in_channel [@@deprecated "[since 2016-04] Use [In_channel.t]"]

(** The type of output channel. *)
type out_channel = Stdlib.out_channel [@@deprecated "[since 2016-04] Use [Out_channel.t]"]

(** The standard input for the process. *)
val stdin : Stdlib.in_channel
  [@@deprecated "[since 2016-04] Use [In_channel.stdin]"]

(** The standard output for the process. *)
val stdout : Stdlib.out_channel

(** The standard error output for the process. *)
val stderr : Stdlib.out_channel

(** {7 Output functions on standard output} *)

(** Print a character on standard output. *)
val print_char : char -> unit
  [@@deprecated "[since 2016-04] Use [Out_channel.output_char stdout]"]

(** Print a string on standard output. *)
val print_string : string -> unit

(** Print a byte sequence on standard output. *)
val print_bytes : bytes -> unit
  [@@deprecated "[since 2016-04] Core doesn't support [bytes] yet."]

(** Print an integer, in decimal, on standard output. *)
val print_int : int -> unit
  [@@deprecated "[since 2016-04] Use [Out_channel.output_string stdout]"]

(** Print a floating-point number, in decimal, on standard output. *)
val print_float : float -> unit
  [@@deprecated "[since 2016-04] Use [Out_channel.output_string stdout]"]

(** Print a string, followed by a newline character, on
    standard output and flush standard output. *)
val print_endline : string -> unit

(** Print a newline character on standard output, and flush
    standard output. This can be used to simulate line
    buffering of standard output. *)
val print_newline : unit -> unit
  [@@deprecated "[since 2016-04] Use [Out_channel.newline stdout]"]

(** {7 Output functions on standard error} *)

(** Print a character on standard error. *)
val prerr_char : char -> unit
  [@@deprecated "[since 2016-04] Use [Out_channel.output_char stderr]"]

(** Print a string on standard error. *)
val prerr_string : string -> unit
  [@@deprecated "[since 2016-04] Use [Out_channel.output_string stderr]"]

(** Print a byte sequence on standard error. *)
val prerr_bytes : bytes -> unit
  [@@deprecated "[since 2016-04] Core doesn't support [bytes] yet"]

(** Print an integer, in decimal, on standard error. *)
val prerr_int : int -> unit
  [@@deprecated "[since 2016-04] Use [Out_channel.output_string stderr]"]

(** Print a floating-point number, in decimal, on standard error. *)
val prerr_float : float -> unit
  [@@deprecated "[since 2016-04] Use [Out_channel.output_string stderr]"]

(** Print a string, followed by a newline character on standard
    error and flush standard error. *)
val prerr_endline : string -> unit

(** Print a newline character on standard error, and flush
    standard error. *)
val prerr_newline : unit -> unit
  [@@deprecated "[since 2016-04] Use [Out_channel.newline stderr]"]

(** {7 Input functions on standard input} *)

(** Flush standard output, then read characters from standard input
    until a newline character is encountered. Return the string of
    all characters read, without the newline character at the end. *)
val read_line : unit -> string
  [@@deprecated
    "[since 2016-04] Use\n[Out_channel.(flush stdout); In_channel.(input_line_exn stdin)]"]

(** Flush standard output, then read one line from standard input
    and convert it to an integer. Raise [Failure "int_of_string"]
    if the line read is not a valid representation of an integer. *)
val read_int : unit -> int
  [@@deprecated
    "[since 2016-04] Use\n\
     [Out_channel.(flush stdout); Int.of_string In_channel.(input_line_exn stdin)]"]

(** Flush standard output, then read one line from standard input
    and convert it to a floating-point number.
    The result is unspecified if the line read is not a valid
    representation of a floating-point number. *)
val read_float : unit -> float
  [@@deprecated
    "[since 2016-04] Use\n\
     [Out_channel.(flush stdout); Float.of_string In_channel.(input_line_exn stdin)]"]

(** {7 General output functions} *)

(** Opening modes for {!Caml.open_out_gen} and
    {!Caml.open_in_gen}. *)
type open_flag = Stdlib.open_flag =
  | Open_rdonly (** open for reading. *)
  | Open_wronly (** open for writing. *)
  | Open_append (** open for appending: always write at end of file. *)
  | Open_creat (** create the file if it does not exist. *)
  | Open_trunc (** empty the file if it already exists. *)
  | Open_excl (** fail if Open_creat and the file already exists. *)
  | Open_binary (** open in binary mode (no conversion). *)
  | Open_text (** open in text mode (may perform conversions). *)
  | Open_nonblock (** open in non-blocking mode. *)
[@@deprecated "[since 2016-04] Use [In_channel.create] and [Out_channel.create]"]

(** Open the named file for writing, and return a new output channel
    on that file, positionned at the beginning of the file. The
    file is truncated to zero length if it already exists. It
    is created if it does not already exists. *)
val open_out : string -> Stdlib.out_channel
  [@@deprecated "[since 2016-04] Use [Out_channel.create]"]

(** Same as {!Caml.open_out}, but the file is opened in binary mode,
    so that no translation takes place during writes. On operating
    systems that do not distinguish between text mode and binary
    mode, this function behaves like {!Caml.open_out}. *)
val open_out_bin : string -> Stdlib.out_channel
  [@@deprecated "[since 2016-04] Use [Out_channel.create]"]

(** [open_out_gen mode perm filename] opens the named file for writing,
    as described above. The extra argument [mode]
    specify the opening mode. The extra argument [perm] specifies
    the file permissions, in case the file must be created.
    {!Caml.open_out} and {!Caml.open_out_bin} are special
    cases of this function. *)
val open_out_gen : Stdlib.open_flag list -> int -> string -> Stdlib.out_channel
  [@@deprecated "[since 2016-04] Use [Out_channel.create]"]

(** Flush the buffer associated with the given output channel,
    performing all pending writes on that channel.
    Interactive programs must be careful about flushing standard
    output and standard error at the right time. *)
val flush : Stdlib.out_channel -> unit
  [@@deprecated "[since 2016-04] Use [Out_channel.flush]"]

(** Flush all open output channels; ignore errors. *)
val flush_all : unit -> unit
  [@@deprecated "[since 2016-04]"]

(** Write the character on the given output channel. *)
val output_char : Stdlib.out_channel -> char -> unit
  [@@deprecated "[since 2016-04] Use [Out_channel.output_char]"]

(** Write the string on the given output channel. *)
val output_string : Stdlib.out_channel -> string -> unit
  [@@deprecated "[since 2016-04] Use [Out_channel.output_string]"]

(** Write the byte sequence on the given output channel. *)
val output_bytes : Stdlib.out_channel -> bytes -> unit
  [@@deprecated "[since 2016-04] Core doesn't yet support bytes."]

(** [output oc buf pos len] writes [len] characters from byte sequence [buf],
    starting at offset [pos], to the given output channel [oc].
    Raise [Invalid_argument "output"] if [pos] and [len] do not
    designate a valid range of [buf]. *)
val output : Stdlib.out_channel -> bytes -> int -> int -> unit
  [@@deprecated "[since 2016-04] Core doesn't yet support bytes."]

(** Same as [output] but take a string as argument instead of
    a byte sequence. *)
val output_substring : Stdlib.out_channel -> string -> int -> int -> unit
  [@@deprecated "[since 2016-04] Use [Out_channel.output]"]

(** Write one 8-bit integer (as the single character with that code)
    on the given output channel. The given integer is taken modulo
    256. *)
val output_byte : Stdlib.out_channel -> int -> unit
  [@@deprecated "[since 2016-04] Use [Out_channel.output_byte]"]

(** Write one integer in binary format (4 bytes, big-endian)
    on the given output channel.
    The given integer is taken modulo 2{^32}.
    The only reliable way to read it back is through the
    {!Caml.input_binary_int} function. The format is compatible across
    all machines for a given version of OCaml. *)
val output_binary_int : Stdlib.out_channel -> int -> unit
  [@@deprecated "[since 2016-04] Use [Out_channel.output_binary_int]"]

(** Write the representation of a structured value of any type
    to a channel. Circularities and sharing inside the value
    are detected and preserved. The object can be read back,
    by the function {!Caml.input_value}. See the description of module
    {!Marshal} for more information. {!Caml.output_value} is equivalent
    to {!Marshal.to_channel} with an empty list of flags. *)
val output_value : Stdlib.out_channel -> 'a -> unit
  [@@deprecated "[since 2016-04] Use [Out_channel.output_value]"]

(** [seek_out chan pos] sets the current writing position to [pos]
    for channel [chan]. This works only for regular files. On
    files of other kinds (such as terminals, pipes and sockets),
    the behavior is unspecified. *)
val seek_out : Stdlib.out_channel -> int -> unit
  [@@deprecated "[since 2014-10] Use [Out_channel.seek]"]

(** Return the current writing position for the given channel.  Does
    not work on channels opened with the [Open_append] flag (returns
    unspecified results). *)
val pos_out : Stdlib.out_channel -> int
  [@@deprecated "[since 2014-10] Use [Out_channel.pos]"]

(** Return the size (number of characters) of the regular file
    on which the given channel is opened.  If the channel is opened
    on a file that is not a regular file, the result is meaningless. *)
val out_channel_length : Stdlib.out_channel -> int
  [@@deprecated "[since 2014-10] Use [Out_channel.length]"]

(** Close the given channel, flushing all buffered write operations.
    Output functions raise a [Sys_error] exception when they are
    applied to a closed output channel, except [close_out] and [flush],
    which do nothing when applied to an already closed channel.
    Note that [close_out] may raise [Sys_error] if the operating
    system signals an error when flushing or closing. *)
val close_out : Stdlib.out_channel -> unit
  [@@deprecated "[since 2014-10] Use [Out_channel.close]"]

(** Same as [close_out], but ignore all errors. *)
val close_out_noerr : Stdlib.out_channel -> unit
  [@@deprecated "[since 2016-04] Use [Out_channel.close] and catch exceptions"]

(** [set_binary_mode_out oc true] sets the channel [oc] to binary
    mode: no translations take place during output.
    [set_binary_mode_out oc false] sets the channel [oc] to text
    mode: depending on the operating system, some translations
    may take place during output.  For instance, under Windows,
    end-of-lines will be translated from [\n] to [\r\n].
    This function has no effect under operating systems that
    do not distinguish between text mode and binary mode. *)
val set_binary_mode_out : Stdlib.out_channel -> bool -> unit
  [@@deprecated "[since 2016-04] Use [Out_channel.set_binary_mode]"]

(** {7 General input functions} *)

(** Open the named file for reading, and return a new input channel
    on that file, positionned at the beginning of the file. *)
val open_in : string -> Stdlib.in_channel
  [@@deprecated "[since 2016-04] Use [In_channel.create]"]

(** Same as {!Caml.open_in}, but the file is opened in binary mode,
    so that no translation takes place during reads. On operating
    systems that do not distinguish between text mode and binary
    mode, this function behaves like {!Caml.open_in}. *)
val open_in_bin : string -> Stdlib.in_channel
  [@@deprecated "[since 2016-04] Use [In_channel.create]"]

(** [open_in_gen mode perm filename] opens the named file for reading,
    as described above. The extra arguments
    [mode] and [perm] specify the opening mode and file permissions.
    {!Caml.open_in} and {!Caml.open_in_bin} are special
    cases of this function. *)
val open_in_gen : Stdlib.open_flag list -> int -> string -> Stdlib.in_channel
  [@@deprecated "[since 2016-04] Use [In_channel.create]"]

(** Read one character from the given input channel.
    Raise [End_of_file] if there are no more characters to read. *)
val input_char : Stdlib.in_channel -> char
  [@@deprecated "[since 2016-04] Use [In_channel.input_char]"]

(** Read characters from the given input channel, until a
    newline character is encountered. Return the string of
    all characters read, without the newline character at the end.
    Raise [End_of_file] if the end of the file is reached
    at the beginning of line. *)
val input_line : Stdlib.in_channel -> string
  [@@deprecated "[since 2016-04] Use [In_channel.input_line]"]

(** [input ic buf pos len] reads up to [len] characters from
    the given channel [ic], storing them in byte sequence [buf], starting at
    character number [pos].
    It returns the actual number of characters read, between 0 and
    [len] (inclusive).
    A return value of 0 means that the end of file was reached.
    A return value between 0 and [len] exclusive means that
    not all requested [len] characters were read, either because
    no more characters were available at that time, or because
    the implementation found it convenient to do a partial read;
    [input] must be called again to read the remaining characters,
    if desired.  (See also {!Caml.really_input} for reading
    exactly [len] characters.)
    Exception [Invalid_argument "input"] is raised if [pos] and [len]
    do not designate a valid range of [buf]. *)
val input : Stdlib.in_channel -> bytes -> int -> int -> int
  [@@deprecated "[since 2016-04] Core doesn't yet support bytes."]

(** [really_input ic buf pos len] reads [len] characters from channel [ic],
    storing them in byte sequence [buf], starting at character number [pos].
    Raise [End_of_file] if the end of file is reached before [len]
    characters have been read.
    Raise [Invalid_argument "really_input"] if
    [pos] and [len] do not designate a valid range of [buf]. *)
val really_input : Stdlib.in_channel -> bytes -> int -> int -> unit
  [@@deprecated "[since 2016-04] Core doesn't yet support bytes."]

(** [really_input_string ic len] reads [len] characters from channel [ic]
    and returns them in a new string.
    Raise [End_of_file] if the end of file is reached before [len]
    characters have been read. *)
val really_input_string : Stdlib.in_channel -> int -> string
  [@@deprecated "[since 2016-04] Use [In_channel.really_input_exn ~pos:0]"]

(** Same as {!Caml.input_char}, but return the 8-bit integer representing
    the character.
    Raise [End_of_file] if an end of file was reached. *)
val input_byte : Stdlib.in_channel -> int
  [@@deprecated "[since 2016-04] Use [In_channel.input_byte]"]

(** Read an integer encoded in binary format (4 bytes, big-endian)
    from the given input channel. See {!Caml.output_binary_int}.
    Raise [End_of_file] if an end of file was reached while reading the
    integer. *)
val input_binary_int : Stdlib.in_channel -> int
  [@@deprecated "[since 2016-04] Use [In_channel.input_binary_int]"]

(** Read the representation of a structured value, as produced
    by {!Caml.output_value}, and return the corresponding value.
    This function is identical to {!Marshal.from_channel};
    see the description of module {!Marshal} for more information,
    in particular concerning the lack of type safety. *)
val input_value : Stdlib.in_channel -> 'a
  [@@deprecated "[since 2016-04] Use [In_channel.unsafe_input_value]"]

(** [seek_in chan pos] sets the current reading position to [pos]
    for channel [chan]. This works only for regular files. On
    files of other kinds, the behavior is unspecified. *)
val seek_in : Stdlib.in_channel -> int -> unit
  [@@deprecated "[since 2014-10] Use [In_channel.seek]"]

(** Return the current reading position for the given channel. *)
val pos_in : Stdlib.in_channel -> int
  [@@deprecated "[since 2014-10] Use [In_channel.pos]"]

(** Return the size (number of characters) of the regular file
    on which the given channel is opened.  If the channel is opened
    on a file that is not a regular file, the result is meaningless.
    The returned size does not take into account the end-of-line
    translations that can be performed when reading from a channel
    opened in text mode. *)
val in_channel_length : Stdlib.in_channel -> int
  [@@deprecated "[since 2014-10] Use [In_channel.length]"]

(** Close the given channel.  Input functions raise a [Sys_error]
    exception when they are applied to a closed input channel,
    except [close_in], which does nothing when applied to an already
    closed channel. *)
val close_in : Stdlib.in_channel -> unit
  [@@deprecated "[since 2014-10] Use [In_channel.close]"]

(** Same as [close_in], but ignore all errors. *)
val close_in_noerr : Stdlib.in_channel -> unit
  [@@deprecated "[since 2016-04] Use [In_channel.close] and catch exceptions"]

(** [set_binary_mode_in ic true] sets the channel [ic] to binary
    mode: no translations take place during input.
    [set_binary_mode_out ic false] sets the channel [ic] to text
    mode: depending on the operating system, some translations
    may take place during input.  For instance, under Windows,
    end-of-lines will be translated from [\r\n] to [\n].
    This function has no effect under operating systems that
    do not distinguish between text mode and binary mode. *)
val set_binary_mode_in : Stdlib.in_channel -> bool -> unit
  [@@deprecated "[since 2016-04] Use [In_channel.set_binary_mode]"]

(** {7 Operations on large files} *)

(** Operations on large files.
    This sub-module provides 64-bit variants of the channel functions
    that manipulate file positions and file sizes.  By representing
    positions and sizes by 64-bit integers (type [int64]) instead of
    regular integers (type [int]), these alternate functions allow
    operating on files whose sizes are greater than [max_int]. *)
module LargeFile : sig
  val seek_out : Stdlib.out_channel -> int64 -> unit
  val pos_out : Stdlib.out_channel -> int64
  val out_channel_length : Stdlib.out_channel -> int64
  val seek_in : Stdlib.in_channel -> int64 -> unit
  val pos_in : Stdlib.in_channel -> int64
  val in_channel_length : Stdlib.in_channel -> int64
end
[@@deprecated "[since 2016-04] Use [In_channel] and [Out_channel]"]

(** {6 References} *)

(** The type of references (mutable indirection cells) containing
    a value of type ['a]. *)
type 'a ref = 'a Stdlib.ref = { mutable contents : 'a }

(** Return a fresh reference containing the given value. *)
external ref : 'a -> ('a ref[@local_opt]) = "%makemutable"

(** [!r] returns the current contents of reference [r].
    Equivalent to [fun r -> r.contents]. *)
external ( ! ) : ('a ref[@local_opt]) -> 'a = "%field0"

(** [r := a] stores the value of [a] in reference [r].
    Equivalent to [fun r v -> r.contents <- v]. *)
external ( := ) : ('a ref[@local_opt]) -> 'a -> unit = "%setfield0"

(** Increment the integer contained in the given reference.
    Equivalent to [fun r -> r := succ !r]. *)
external incr : (int ref[@local_opt]) -> unit = "%incr"

(** Decrement the integer contained in the given reference.
    Equivalent to [fun r -> r := pred !r]. *)
external decr : (int ref[@local_opt]) -> unit = "%decr"

(** Result type *)

type ('a, 'b) result = ('a, 'b) Stdlib.result =
  | Ok of 'a
  | Error of 'b

(** {6 Operations on format strings} *)

(** Format strings are character strings with special lexical conventions
    that defines the functionality of formatted input/output functions. Format
    strings are used to read data with formatted input functions from module
    {!Scanf} and to print data with formatted output functions from modules
    {!Printf} and {!Format}.

    Format strings are made of three kinds of entities:
    - {e conversions specifications}, introduced by the special character ['%']
      followed by one or more characters specifying what kind of argument to
      read or print,
    - {e formatting indications}, introduced by the special character ['@']
      followed by one or more characters specifying how to read or print the
      argument,
    - {e plain characters} that are regular characters with usual lexical
      conventions. Plain characters specify string literals to be read in the
      input or printed in the output.

    There is an additional lexical rule to escape the special characters ['%']
    and ['@'] in format strings: if a special character follows a ['%']
    character, it is treated as a plain character. In other words, ["%%"] is
    considered as a plain ['%'] and ["%@"] as a plain ['@'].

    For more information about conversion specifications and formatting
    indications available, read the documentation of modules {!Scanf},
    {!Printf} and {!Format}.
*)

(** Format strings have a general and highly polymorphic type
    [('a, 'b, 'c, 'd, 'e, 'f) format6].
    The two simplified types, [format] and [format4] below are
    included for backward compatibility with earlier releases of
    OCaml.

    The meaning of format string type parameters is as follows:

    - ['a] is the type of the parameters of the format for formatted output
      functions ([printf]-style functions);
      ['a] is the type of the values read by the format for formatted input
      functions ([scanf]-style functions).

    - ['b] is the type of input source for formatted input functions and the
      type of output target for formatted output functions.
      For [printf]-style functions from module [Printf], ['b] is typically
      [out_channel];
      for [printf]-style functions from module [Format], ['b] is typically
      [Format.formatter];
      for [scanf]-style functions from module [Scanf], ['b] is typically
      [Scanf.Scanning.in_channel].

    Type argument ['b] is also the type of the first argument given to
    user's defined printing functions for [%a] and [%t] conversions,
    and user's defined reading functions for [%r] conversion.

    - ['c] is the type of the result of the [%a] and [%t] printing
      functions, and also the type of the argument transmitted to the
      first argument of [kprintf]-style functions or to the
      [kscanf]-style functions.

    - ['d] is the type of parameters for the [scanf]-style functions.

    - ['e] is the type of the receiver function for the [scanf]-style functions.

    - ['f] is the final result type of a formatted input/output function
      invocation: for the [printf]-style functions, it is typically [unit];
      for the [scanf]-style functions, it is typically the result type of the
      receiver function.
*)
type ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.format6

type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6
type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4

(** Converts a format string into a string. *)
val string_of_format : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string

(** [format_of_string s] returns a format string read from the string
    literal [s].
    Note: [format_of_string] can not convert a string argument that is not a
    literal. If you need this functionality, use the more general
    {!Scanf.format_from_string} function.
*)
external format_of_string
  :  (('a, 'b, 'c, 'd, 'e, 'f) format6[@local_opt])
  -> (('a, 'b, 'c, 'd, 'e, 'f) format6[@local_opt])
  = "%identity"

(** [f1 ^^ f2] catenates format strings [f1] and [f2]. The result is a
    format string that behaves as the concatenation of format strings [f1] and
    [f2]: in case of formatted output, it accepts arguments from [f1], then
    arguments from [f2]; in case of formatted input, it returns results from
    [f1], then results from [f2].
*)
val ( ^^ )
  :  ('a, 'b, 'c, 'd, 'e, 'f) format6
  -> ('f, 'b, 'c, 'e, 'g, 'h) format6
  -> ('a, 'b, 'c, 'd, 'g, 'h) format6

(** {6 Program termination} *)

(** Terminate the process, returning the given status code
    to the operating system: usually 0 to indicate no errors,
    and a small positive integer to indicate failure.
    All open output channels are flushed with [flush_all].
    An implicit [exit 0] is performed each time a program
    terminates normally.  An implicit [exit 2] is performed if the program
    terminates early because of an uncaught exception. *)
val exit : int -> 'a

(** Register the given function to be called at program
    termination time. The functions registered with [at_exit]
    will be called when the program executes {!Caml.exit},
    or terminates, either normally or because of an uncaught exception.
    The functions are called in 'last in, first out' order:
    the function most recently added with [at_exit] is called first. *)
val at_exit : (unit -> unit) -> unit

(**/**)

(** The following is for system use only. Do not call directly. *)

val valid_float_lexem : string -> string [@@deprecated "[since 2015-11] Do not use."]

val unsafe_really_input : Stdlib.in_channel -> bytes -> int -> int -> unit
  [@@deprecated "[since 2015-11] Do not use."]

val do_at_exit : unit -> unit [@@deprecated "[since 2015-11] Do not use."]
