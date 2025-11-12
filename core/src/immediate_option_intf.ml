(** A non-allocating alternative to the standard Option type. *)

open! Import

[%%template
[@@@mode.default m = (global, local)]

module type S_without_immediate_plain = sig
  (** The immediate value carried by the immediate option.

      Given the presence of {!unchecked_value}, the [value] type should not have
      operations that depend on the value's validity for memory safety. In particular,
      [unchecked_value] is not called [unsafe_value] as it would be if it could return a
      value that later resulted in a segmentation fault. For pointer-like values, use
      {!Ext.Nullable}, for example. *)
  type value

  (** Represents [value option] without allocating a [Some] tag. The interface does not
      enforce that [t] is immediate because some types, like [Int63.t], are only immediate
      on 64-bit platforms. For representations whose type is immediate, use [S] below
      which adds the [[@@immediate]] annotation. *)
  type t

  module Optional_syntax :
    Optional_syntax.S [@mode m] with type t := t with type value := value

  (** Constructors analogous to [None] and [Some]. If [not (some_is_representable x)] then
      [some x] may raise or return [none]. *)

  val none : t

  (** For some representations of immediate options, the encodings of [none] and [some]
      overlap. For these representations, [some_is_representable value = false] if [value]
      cannot be represented as an option. For example, [Int.Option] uses [min_value] to
      represent [none]. For other representations, [some_is_representable] always returns
      [true]. *)
  val some_is_representable : value @ local -> bool

  val is_none : t @ local -> bool
  val is_some : t @ local -> bool

  [@@@mode.default m = (global, m)]

  val some : value @ m -> t @ m

  (** [value (some x) ~default = x] and [value none ~default = default]. *)
  val value : t @ m -> default:value @ m -> value @ m

  (** [value_exn (some x) = x]. [value_exn none] raises. Unlike [Option.value_exn], there
      is no [?message] argument, so that calls to [value_exn] that do not raise also do
      not have to allocate. *)
  val value_exn : t @ m -> value @ m

  (** [unchecked_value (some x) = x]. [unchecked_value none] returns an unspecified value.
      [unchecked_value t] is intended as an optimization of [value_exn t] when [is_some t]
      is known to be true. *)
  val unchecked_value : t @ m -> value @ m

  val to_option : t @ m -> value option @ m
  val of_option : value option @ m -> t @ m
end

module type S_without_immediate_plain_zero_alloc = sig
  type value
  type t

  module Optional_syntax :
    Optional_syntax.S_zero_alloc [@mode m] with type t := t with type value := value

  val none : t
  val some_is_representable : value @ local -> bool [@@zero_alloc]
  val is_none : t @ local -> bool [@@zero_alloc]
  val is_some : t @ local -> bool [@@zero_alloc]

  [@@@mode.default m = (global, m)]

  val some : value @ m -> t @ m [@@zero_alloc]
  val value : t @ m -> default:value @ m -> value @ m [@@zero_alloc]
  val value_exn : t @ m -> value @ m [@@zero_alloc]
  val unchecked_value : t @ m -> value @ m [@@zero_alloc]
  val to_option : t @ m -> value option @ m
  val of_option : value option @ m -> t @ m [@@zero_alloc]
end]

module type S_plain = sig
  type t : immediate

  include S_without_immediate_plain with type t := t
end

module type S_plain_zero_alloc = sig
  type t : immediate

  include S_without_immediate_plain_zero_alloc with type t := t
end

module type S_int63_plain = sig
  type t : immediate64

  include S_without_immediate_plain with type t := t
end

[%%template
[@@@mode.default m = (global, local)]

module type S_derivers = sig
  type t [@@deriving (compare [@mode m]), hash, sexp_of, typerep]
end

module type S_without_immediate = sig
  include S_without_immediate_plain [@mode m]
  include S_derivers [@mode m] with type t := t
end

module type S_without_immediate_zero_alloc = sig
  include S_without_immediate_plain_zero_alloc [@mode m]
  include S_derivers [@mode m] with type t := t
end

(** For interfaces that expose that [t] is immediate, the only additional function
    provided in the [local] versions is [compare]. This is because [t] crosses locality,
    and so existing functions can all be used with [local] [t]s. We provide compare
    specially only because [compare__local] is a name needed by downstream users of
    [[@@deriving compare ~localize]].

    There are some clients of the [S_*] family where [value] is non-immediate and does not
    cross, and so local-allocating versions of e.g. [value_exn] could plausibly be useful:
    we can add templating over those functions if someone asks. *)

module type S = sig
  include S_plain
  include S_derivers [@mode m] with type t := t
end

module type S_zero_alloc = sig
  include S_plain_zero_alloc
  include S_derivers [@mode m] with type t := t
end

module type S_int63 = sig
  include S_int63_plain
  include S_derivers [@mode m] with type t := t
end

module type S_int63_zero_alloc = sig
  type t : immediate64

  include S_without_immediate_plain_zero_alloc with type t := t
  include S_derivers [@mode m] with type t := t
end]

module type S_unboxed_float64 = sig
  type t : float64 mod global
  type value : float64

  val typerep_of_t : t Typerep_lib.Std.Typerep.t
  val typename_of_t : t Typerep_lib.Std.Typename.t
  val sexp_of_t : t -> Sexp.t
  val none : unit -> t [@@zero_alloc]
  val some : value -> t [@@zero_alloc]
  val some_is_representable : value @ local -> bool [@@zero_alloc]
  val is_none : t @ local -> bool [@@zero_alloc]
  val is_some : t @ local -> bool [@@zero_alloc]
  val value : t -> default:value -> value [@@zero_alloc]
  val value_exn : t -> value [@@zero_alloc]
  val unchecked_value : t -> value [@@zero_alloc]

  module Optional_syntax : sig
    type nonrec t = t
    type nonrec value = value

    module Optional_syntax : sig
      val is_none : t @ local -> bool [@@zero_alloc]
      val unsafe_value : t -> value [@@zero_alloc]
    end
  end
end

module type Immediate_option = sig
  (** Always immediate. *)

  module type S_plain = S_plain
  module type S_plain_zero_alloc = S_plain_zero_alloc

  [%%template:
  [@@@mode.default m = (global, local)]

  module type S = S [@mode m]
  module type S_zero_alloc = S_zero_alloc [@mode m]]

  (** Immediate only on 64-bit machines. *)

  module type S_int63_plain = S_int63_plain

  [%%template:
  [@@@mode.default m = (global, local)]

  module type S_int63 = S_int63 [@mode m]
  module type S_int63_zero_alloc = S_int63_zero_alloc [@mode m]]

  (** Unboxed types *)
  module type S_unboxed_float64 = S_unboxed_float64

  (** Never immediate. *)

  [%%template:
  [@@@mode.default m = (global, local)]

  module type S_without_immediate_plain = S_without_immediate_plain [@mode m]

  module type S_without_immediate_plain_zero_alloc = S_without_immediate_plain_zero_alloc
  [@mode m]

  module type S_without_immediate = S_without_immediate [@mode m]
  module type S_without_immediate_zero_alloc = S_without_immediate_zero_alloc [@mode m]]
end
