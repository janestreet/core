@@ portable

(** A ['a t] represents a non-empty list, as evidenced by the fact that there is no [[]]
    variant. The sexp representation is as a regular list (i.e., the same as the
    [Stable.V3] module below).

    For operations on a locally allocated ['a t], see [Local_nonempty_list]. For
    convenience, some functions here are also ppx_template'd over local, and we hope to
    fully merge the two after list is localized *)
include module type of struct
  include Base.Nonempty_list
end

[%%rederive:
  type nonrec 'a t = 'a t = ( :: ) of 'a * 'a list
  [@@deriving bin_io ~localize, quickcheck ~portable, typerep]]

type 'a nonempty_list := 'a t

(** validates a list, naming each element by its position in the list (where the first
    position is 1, not 0). *)
val validate_indexed : 'a Validate.check -> 'a t Validate.check

(** validates a list, naming each element using a user-defined function for computing the
    name. *)
val validate : name:('a -> string) -> 'a Validate.check -> 'a t Validate.check

(** Returns a flag that must be passed one or more times. See
    [Command.Param.one_or_more_as_pair]. *)
val flag : 'a Command.Param.Arg_type.t -> 'a t Command.Flag.t @@ nonportable

(** Accepts comma-separated lists of arguments parsed by [t]. See
    [Command.Param.Arg_type.comma_separated]. *)
val comma_separated_argtype
  :  ?key:'a t Univ_map.Multi.Key.t
  -> ?strip_whitespace:bool
  -> ?unique_values:bool
  -> 'a Command.Param.Arg_type.t
  -> 'a t Command.Param.Arg_type.t
  @@ nonportable

(** Requires one or more of an anonymous argument. *)
val anons : 'a Command.Anons.t -> 'a t Command.Anons.t @@ nonportable

(** This module provides 0-alloc versions of [to_list] and [of_list], via [some] and
    allowing you to [match%optional] on a list, respectively. *)
module Option : sig
  type 'a t = 'a list
  [@@deriving
    compare ~localize, equal ~localize, sexp, sexp_grammar, hash, quickcheck, typerep]

  (** Constructors analogous to [None] and [Some]. *)

  val none : _ t
  val some : 'a nonempty_list -> 'a t
  val is_none : _ t -> bool
  val is_some : _ t -> bool

  (** [value (some x) ~default = x] and [value none ~default = default]. *)
  val value : 'a t -> default:'a nonempty_list -> 'a nonempty_list

  (** [value_exn (some x) = x]. [value_exn none] raises. Unlike [Option.value_exn], there
      is no [?message] argument, so that calls to [value_exn] that do not raise also do
      not have to allocate. *)
  val value_exn : 'a t -> 'a nonempty_list

  (** [unchecked_value (some x) = x]. [unchecked_value none] returns an unspecified value.
      [unchecked_value t] is intended as an optimization of [value_exn t] when [is_some t]
      is known to be true. *)
  val unchecked_value : 'a t -> 'a nonempty_list

  val to_option : 'a t -> 'a nonempty_list option
  val of_option : 'a nonempty_list option -> 'a t

  module Optional_syntax :
    Optional_syntax.S1 with type 'a t := 'a t and type 'a value := 'a nonempty_list
end

module Unstable : sig
  type nonrec 'a t = 'a t
  [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp, sexp_grammar]
end

module Stable : sig
  (** Represents a [t] as an ordinary list for sexp and bin_io conversions, e.g. [1::2] is
      represented as [(1 2)]. *)
  module V3 : sig
    type nonrec 'a t = 'a t
    [@@deriving
      bin_io
      , compare ~localize
      , equal ~localize
      , globalize
      , sexp
      , sexp_grammar
      , hash
      , stable_witness]
  end

  (** Represents a [t] as an ordinary list for sexp conversions, but uses a record
      [{hd : 'a; tl ; 'a list}] for bin_io conversions. This module is provided for
      compatibility with existing protocols; there's no reason not to use the latest
      version if you're writing a new protocol. *)
  module V2 : sig
    type nonrec 'a t = 'a t
    [@@deriving
      bin_io, compare ~localize, equal ~localize, sexp, sexp_grammar, hash, stable_witness]
  end

  (** Represents a [t] as an ordinary list for sexps, but as a pair for bin_io conversions
      (i.e., a ['a t] is represented as the type ['a * 'a list]). This module is provided
      for compatibility with existing protocols; there's no reason not to use the latest
      version if you're writing a new protocol. *)
  module V1 : sig
    type nonrec 'a t = 'a t
    [@@deriving
      bin_io, compare ~localize, equal ~localize, sexp, sexp_grammar, stable_witness]
  end
end
