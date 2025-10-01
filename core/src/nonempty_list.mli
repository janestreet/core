@@ portable

open Std_internal

(** A ['a t] represents a non-empty list, as evidenced by the fact that there is no [[]]
    variant. The sexp representation is as a regular list (i.e., the same as the
    [Stable.V3] module below).

    For operations on a locally allocated ['a t], see [Local_nonempty_list]. For
    convenience, some functions here are also ppx_template'd over local, and we hope to
    fully merge the two after list is localized *)

type%template ('a : k) t = ( :: ) of 'a * ('a List.t[@kind k])
[@@deriving compare ~localize, equal ~localize]
[@@kind k = (float64, bits64, bits32, word)]

type ('a : value_or_null) t = ( :: ) of 'a * 'a list
[@@deriving
  compare ~localize, equal ~localize, hash, quickcheck ~portable, typerep, globalize]

[%%rederive:
  type nonrec 'a t = 'a t = ( :: ) of 'a * 'a list
  [@@deriving bin_io ~localize, sexp, sexp_grammar]]

type 'a nonempty_list := 'a t

include Comparator.Derived with type 'a t := 'a t
include Container.S1 with type 'a t := 'a t
include Base.Invariant.S1 with type 'a t := 'a t

include%template
  Base.Monad.S
  [@kind value_or_null mod maybe_null] [@mode local]
  with type ('a : value_or_null) t := 'a t

include Indexed_container.S1 with type 'a t := 'a t

[%%template:
[@@@kind k = (float64, bits32, bits64, word, value)]

type 'a t := ('a t[@kind k])

[@@@kind.default k]

val create : 'a -> ('a List.t[@kind k]) -> 'a t
val init : int -> f:local_ (int -> 'a) -> 'a t

val of_list : ('a List.t[@kind k]) @ m -> 'a t option @ m
[@@mode m = (global, local)] [@@zero_alloc_if_local m]

val of_list_error : ('a List.t[@kind k]) @ m -> 'a t Or_error.t @ m
[@@mode m = (global, local)]

val of_list_exn : ('a List.t[@kind k]) @ m -> 'a t @ m
[@@mode m = (global, local)] [@@zero_alloc]

val to_list : 'a t @ m -> ('a List.t[@kind k]) @ m
[@@mode m = (global, local)] [@@zero_alloc]

val singleton : 'a -> 'a t
val cons : 'a -> 'a t -> 'a t
val hd : 'a t -> 'a
val tl : 'a t -> ('a List.t[@kind k])
val nth : 'a t -> int -> ('a option[@kind k])
val nth_exn : 'a t -> int -> 'a
val reduce : 'a t -> f:local_ ('a -> 'a -> 'a) -> 'a
val reverse : 'a t -> 'a t
val append : 'a t -> ('a List.t[@kind k]) -> 'a t
val ( @ ) : 'a t -> 'a t -> 'a t
val filter : 'a t -> f:local_ ('a -> bool) -> ('a List.t[@kind k])
val filteri : 'a t -> f:local_ (int -> 'a -> bool) -> ('a List.t[@kind k])
val last : 'a t -> 'a
val iter : 'a t @ m -> f:local_ ('a @ m -> unit) -> unit [@@mode m = (global, local)]
val iteri : 'a t -> f:local_ (int -> 'a -> unit) -> unit
val length : _ t -> int]

[%%template:
[@@@kind.default
  ka = (float64, bits32, bits64, word, value_or_null)
  , kb = (float64, bits32, bits64, word, value_or_null)]

val map : ('a : ka) ('b : kb). ('a t[@kind ka]) -> f:local_ ('a -> 'b) -> ('b t[@kind kb])

val mapi
  : ('a : ka) ('b : kb).
  ('a t[@kind ka]) -> f:local_ (int -> 'a -> 'b) -> ('b t[@kind kb])

val filter_map
  : ('a : ka) ('b : kb).
  ('a t[@kind ka]) -> f:local_ ('a -> ('b Option.t[@kind kb])) -> ('b List.t[@kind kb])

val filter_mapi
  : ('a : ka) ('b : kb).
  ('a t[@kind ka])
  -> f:local_ (int -> 'a -> ('b Option.t[@kind kb]))
  -> ('b List.t[@kind kb])

val concat_map
  : ('a : ka) ('b : kb).
  ('a t[@kind ka]) -> f:local_ ('a -> ('b t[@kind kb])) -> ('b t[@kind kb])]

val reduce : 'a t -> f:local_ ('a -> 'a -> 'a) -> 'a
val append' : 'a list -> 'a t -> 'a t
val unzip : ('a * 'b) t -> 'a t * 'b t
val unzip3 : ('a * 'b * 'c) t -> 'a t * 'b t * 'c t
val zip : 'a t -> 'b t -> ('a * 'b) t List.Or_unequal_lengths.t
val zip_exn : 'a t -> 'b t -> ('a * 'b) t
val zip3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t List.Or_unequal_lengths.t
val zip3_exn : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val map2 : 'a t -> 'b t -> f:local_ ('a -> 'b -> 'c) -> 'c t List.Or_unequal_lengths.t
val map2_exn : 'a t -> 'b t -> f:local_ ('a -> 'b -> 'c) -> 'c t
val filter_opt : 'a option t -> 'a list
val concat : 'a t nonempty_list -> 'a t
val drop_last : 'a t -> 'a list
val to_sequence : 'a t -> 'a Sequence.t
val sort : 'a t -> compare:local_ ('a -> 'a -> int) -> 'a t
val group : 'a t -> break:local_ ('a -> 'a -> bool) -> 'a t t
val sort_and_group : 'a t -> compare:local_ ('a -> 'a -> int) -> 'a t t
val stable_sort : 'a t -> compare:local_ ('a -> 'a -> int) -> 'a t
val stable_dedup : 'a t -> compare:('a -> 'a -> int) -> 'a t
val dedup_and_sort : 'a t -> compare:local_ ('a -> 'a -> int) -> 'a t
val permute : ?random_state:Base.Random.State.t -> 'a t -> 'a t
val random_element : ?random_state:Base.Random.State.t -> 'a t -> 'a
val cartesian_product : 'a t -> 'b t -> ('a * 'b) t
val fold_nonempty : 'a t -> init:local_ ('a -> 'acc) -> f:('acc -> 'a -> 'acc) -> 'acc
val fold_right : 'a t -> init:'b -> f:local_ ('a -> 'b -> 'b) -> 'b
val folding_map : 'a t -> init:'b -> f:local_ ('b -> 'a -> 'b * 'c) -> 'c t
val fold_map : 'a t -> init:'acc -> f:local_ ('acc -> 'a -> 'acc * 'b) -> 'acc * 'b t
val findi_exn : 'a t -> f:local_ (int -> 'a -> bool) -> int * 'a

(** [all_equal] returns a single element of the list that is equal to all other elements,
    or [None] if no such element exists. *)
val all_equal : 'a t -> equal:local_ ('a -> 'a -> bool) -> 'a option

(** [min_elt'] and [max_elt'] differ from [min_elt] and [max_elt] (included in
    [Container.S1]) in that they don't return options. *)
val min_elt' : 'a t -> compare:local_ ('a -> 'a -> int) -> 'a

val max_elt' : 'a t -> compare:local_ ('a -> 'a -> int) -> 'a

(** [transpose] takes an n x m list of lists to an m x n list of lists. Hence, if the
    input lists are all non-empty, the output lists will also all be non-empty. *)
val transpose : 'a t t -> 'a t t option

val transpose_exn : 'a t t -> 'a t t

(** Like [Map.add_multi], but comes with a guarantee that the range of the returned map is
    all nonempty lists. *)
val map_add_multi : ('k, 'v t, 'cmp) Map.t -> key:'k -> data:'v -> ('k, 'v t, 'cmp) Map.t

(** Like [Hashtbl.add_multi], but comes with a guarantee that list that's added to or
    created is a nonempty list. *)
val hashtbl_add_multi : ('k, 'v t) Hashtbl.t -> key:'k -> data:'v -> unit

(** Like [Map.of_alist_multi], but comes with a guarantee that the range of the returned
    map is all nonempty lists. *)
val map_of_alist_multi
  :  ('k * 'v) list
  -> comparator:('k, 'cmp) Comparator.Module.t
  -> ('k, 'v t, 'cmp) Map.t

(** Like [map_of_alist_multi] but the elements in the resulting nonempty lists will be in
    the reverse order of the input list. Uses of this function should probably be replaced
    with [map_of_alist_multi] *)
val map_of_alist_multi_rev
  :  ('k * 'v) list
  -> comparator:('k, 'cmp) Comparator.Module.t
  -> ('k, 'v t, 'cmp) Map.t

(** Like [Map.of_sequence_multi], but comes with a guarantee that the range of the
    returned map is all nonempty lists. *)
val map_of_sequence_multi
  :  ('k * 'v) Sequence.t
  -> comparator:('k, 'cmp) Comparator.Module.t
  -> ('k, 'v t, 'cmp) Map.t

(** Like [map_of_sequence_multi] but the elements in the resulting nonempty lists will be
    in the reverse order of the input sequence. Uses of this function should probably be
    replaced with [map_of_sequence_multi] *)
val map_of_sequence_multi_rev
  :  ('k * 'v) Sequence.t
  -> comparator:('k, 'cmp) Comparator.Module.t
  -> ('k, 'v t, 'cmp) Map.t

(** Like [Map.of_list_with_key_multi], but comes with a guarantee that the range of the
    returned map is all nonempty lists. *)
val map_of_list_with_key_multi
  :  'v list
  -> comparator:('k, 'cmp) Comparator.Module.t
  -> get_key:('v -> 'k)
  -> ('k, 'v t, 'cmp) Map.t

(** Like [map_of_list_with_key_multi] but the elements in the resulting nonempty lists
    will be in the reverse order of the input list. Uses of this function should probably
    be replaced with [map_of_list_with_key_multi] *)
val map_of_list_with_key_multi_rev
  :  'v list
  -> comparator:('k, 'cmp) Comparator.Module.t
  -> get_key:('v -> 'k)
  -> ('k, 'v t, 'cmp) Map.t

(** Like [Result.combine_errors] but for non-empty lists *)
val combine_errors : ('ok, 'err) Result.t t -> ('ok t, 'err t) Result.t

(** Like [Result.combine_errors_unit] but for non-empty lists *)
val combine_errors_unit : (unit, 'err) Result.t t -> (unit, 'err t) Result.t

(** Like [Or_error.combine_errors] but for non-empty lists *)
val combine_or_errors : 'a Or_error.t t -> 'a t Or_error.t

(** Like [Or_error.combine_errors_unit] but for non-empty lists *)
val combine_or_errors_unit : unit Or_error.t t -> unit Or_error.t

(** Like [Or_error.filter_ok_at_least_one] but for non-empty lists.

    In particular it:

    + Returns a ['a t Or_error.t], statically ensuring that in the [Ok l] case, [l] is
      nonempty.

    + Takes a ['a Or_error.t t], ensuring that in the [Error e] case, [e] is informative,
      rather than having been constructed from an empty list. *)
val filter_ok_at_least_one : 'a Or_error.t t -> 'a t Or_error.t

(** Like [Option.all] but for non-empty lists *)
val option_all : 'a option t -> 'a t option

(** Returns the given t with consecutive duplicates removed. The relative order of the
    other elements is unaffected. The element kept from a run of duplicates is determined
    by [which_to_keep]. *)
val remove_consecutive_duplicates
  :  ?which_to_keep:[ `First | `Last ] (** default = `Last *)
  -> 'a t
  -> equal:local_ ('a -> 'a -> bool)
  -> 'a t

module Partition : sig
  (** A [Partition] represents a splitting of a nonempty list into two parts, a "fst" and
      a "snd" part.

      Unlike a [List], partitioning a nonempty list does not produce two nonempty lists,
      because one of the parts could be empty. However, we know for sure that at least one
      of the parts is nonempty, so we represent this information in a structured type
      indicating which parts are nonempty.

      The [partition] functions below have two variations, one which safely provides a
      [Partition], and another [partition'] variation which returns two possibly empty
      lists instead. The latter loses some type information but may be more convenient in
      some cases. *)

  type ('fst, 'snd) t =
    | Fst of 'fst nonempty_list
    | Snd of 'snd nonempty_list
    | Both of ('fst nonempty_list * 'snd nonempty_list)
  [@@deriving sexp_of]

  (** Returns the fst part of the partition. *)
  val fst : ('fst, _) t -> 'fst nonempty_list option

  (** Returns the snd part of the partition. *)
  val snd : (_, 'snd) t -> 'snd nonempty_list option
end

(** [partition_tf t ~f] returns a pair [t1, t2], where [t1] is all elements of [t] that
    satisfy [f], and [t2] is all elements of [t] that do not satisfy [f]. The "tf" suffix
    is mnemonic to remind readers that the result is (trues, falses).

    At least one of the two parts must be nonempty, which is represented by the type of
    [Partition.t]. *)
val partition_tf : 'a t -> f:local_ ('a -> bool) -> ('a, 'a) Partition.t

(** Like [partition_tf], but returns the parts in two lists instead. *)
val partition_tf' : 'a t -> f:local_ ('a -> bool) -> 'a list * 'a list

(** [partition_map t ~f] partitions [t] according to [f].

    At least one of the two parts must be nonempty, which is represented by the type of
    [Partition.t]. *)
val partition_map
  :  'a t
  -> f:local_ ('a -> ('fst, 'snd) Either.t)
  -> ('fst, 'snd) Partition.t

(** Like [partition_map], but returns the parts in two lists instead. *)
val partition_map'
  :  'a t
  -> f:local_ ('a -> ('fst, 'snd) Either.t)
  -> 'fst list * 'snd list

(** [partition_result t] returns a pair [t1, t2], where [t1] is the all [Ok] elements in
    [t] and [t2] is the list of all [Error] elements. The order of elements in the input
    list is preserved.

    At least one of the two parts must be nonempty, which is represented by the type of
    [Partition.t]. *)
val partition_result : ('ok, 'error) Result.t t -> ('ok, 'error) Partition.t

(** Like [partition_result], but returns the parts in two lists instead. *)
val partition_result' : ('ok, 'error) Result.t t -> 'ok list * 'error list

module Partition3 : sig
  (** A [Partition3] represents a splitting of a nonempty list into three parts, a "fst",
      a "snd", and a "trd" part.

      Unlike [List], partitioning a nonempty list does not produce three nonempty lists,
      because one of the parts could be empty. However, we know for sure that at least one
      of the parts is nonempty, so we represent this information in a structured type
      indicating which parts are nonempty. *)

  type ('fst, 'snd, 'trd) t =
    | Fst of 'fst nonempty_list
    | Snd of 'snd nonempty_list
    | Trd of 'trd nonempty_list
    | Fst_snd of 'fst nonempty_list * 'snd nonempty_list
    | Fst_trd of 'fst nonempty_list * 'trd nonempty_list
    | Snd_trd of 'snd nonempty_list * 'trd nonempty_list
    | Fst_snd_trd of 'fst nonempty_list * 'snd nonempty_list * 'trd nonempty_list
  [@@deriving sexp_of]
end

val partition3_map
  :  'a t
  -> f:local_ ('a -> [ `Fst of 'fst | `Snd of 'snd | `Trd of 'trd ])
  -> ('fst, 'snd, 'trd) Partition3.t

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
