@@ portable

(** Fixed-length, mutable vector of elements with O(1) [get] and [set] operations.

    This module extends {{!Base.Array} [Base.Array]}. *)

open Import
open Perms.Export

(** {2 The [Array] type} *)

type ('a : any_non_null) t = 'a Base.Array.t

[%%rederive:
  type nonrec 'a t = 'a t [@@deriving bin_io ~localize, quickcheck ~portable, typerep]]

(** {2 The signature included from [Base.Array]} *)

include Base.Array.Public with type ('a : any_non_null) t := 'a t (** @inline *)

(** {2 Extensions}

    We add extensions for [Int] and [Float] arrays to make them bin-able, comparable,
    sexpable, and blit-able (via [Blit.S]). [Permissioned] provides fine-grained access
    control for arrays.

    Operations supporting "normalized" indexes are also available. *)

module Int : sig
  type nonrec t = int t
  [@@deriving bin_io ~localize, compare ~localize, sexp, sexp_grammar]

  include Blit.S with type t := t

  external unsafe_blit
    :  src:(t[@local_opt])
    -> src_pos:int
    -> dst:(t[@local_opt])
    -> dst_pos:int
    -> len:int
    -> unit
    = "core_array_unsafe_int_blit"
  [@@noalloc]
end

module Float : sig
  type nonrec t = float t
  [@@deriving bin_io ~localize, compare ~localize, sexp, sexp_grammar]

  include Blit.S with type t := t

  external unsafe_blit
    :  src:(t[@local_opt])
    -> src_pos:int
    -> dst:(t[@local_opt])
    -> dst_pos:int
    -> len:int
    -> unit
    = "core_array_unsafe_float_blit"
  [@@noalloc]
end

(** [normalize array index] returns a new index into the array such that if the index is
    less than zero, the returned index will "wrap around" -- i.e.,
    [array.(normalize array (-1))] returns the last element of the array. *)
val normalize : 'a t -> int -> int

(** [slice t start stop] returns a new array including elements [t.(start)] through
    [t.(stop-1)], normalized Python-style with the exception that [stop = 0] is treated as
    [stop = length t]. *)
val slice : 'a t -> int -> int -> 'a t

(** Array access with [normalize]d index. *)
val nget : 'a t -> int -> 'a

(** Array modification with [normalize]d index. *)
val nset : 'a t -> int -> 'a -> unit

(** The [Permissioned] module gives the ability to restrict permissions on an array, so
    you can give a function read-only access to an array, create an immutable array, etc. *)
module Permissioned : sig
  (** The meaning of the ['perms] parameter is as usual (see the [Perms] module for more
      details) with the non-obvious difference that you don't need any permissions to
      extract the length of an array. This was done for simplicity because some
      information about the length of an array can leak out even if you only have write
      permissions since you can catch out-of-bounds errors. *)
  type ('a : any_non_null, -'perms) t

  [%%rederive:
    type nonrec ('a, -'perms) t = ('a, 'perms) t
    [@@deriving bin_io ~localize, compare ~localize, sexp, sexp_grammar]]

  module Int : sig
    type nonrec -'perms t = (int, 'perms) t
    [@@deriving bin_io ~localize, compare ~localize, sexp, sexp_grammar]

    include Blit.S_permissions with type 'perms t := 'perms t

    external unsafe_blit
      :  src:([> read ] t[@local_opt])
      -> src_pos:int
      -> dst:([> write ] t[@local_opt])
      -> dst_pos:int
      -> len:int
      -> unit
      = "core_array_unsafe_int_blit"
    [@@noalloc]
  end

  module Float : sig
    type nonrec -'perms t = (float, 'perms) t
    [@@deriving bin_io ~localize, compare ~localize, sexp, sexp_grammar]

    include Blit.S_permissions with type 'perms t := 'perms t

    external get
      :  ([> read ] t[@local_opt])
      -> (int[@local_opt])
      -> float
      = "%floatarray_safe_get"

    external set
      :  ([> write ] t[@local_opt])
      -> (int[@local_opt])
      -> (float[@local_opt])
      -> unit
      = "%floatarray_safe_set"

    external unsafe_get
      :  ([> read ] t[@local_opt])
      -> (int[@local_opt])
      -> float
      = "%floatarray_unsafe_get"

    external unsafe_set
      :  ([> write ] t[@local_opt])
      -> (int[@local_opt])
      -> (float[@local_opt])
      -> unit
      = "%floatarray_unsafe_set"

    external unsafe_blit
      :  src:([> read ] t[@local_opt])
      -> src_pos:int
      -> dst:([> write ] t[@local_opt])
      -> dst_pos:int
      -> len:int
      -> unit
      = "core_array_unsafe_float_blit"
    [@@noalloc]
  end

  (** [of_array_id] and [to_array_id] return the same underlying array. On the other hand,
      [to_array] (inherited from [Container.S1_permissions] below) makes a copy.

      To create a new (possibly immutable) copy of an array [a], use
      [copy (of_array_id a)]. More generally, any function that takes a (possibly mutable)
      [t] can be called on an array by calling [of_array_id] on it first.

      There is a conceptual type equality between ['a Array.t] and
      [('a, read_write) Array.Permissioned.t]. The reason for not exposing this as an
      actual type equality is that we also want:

      - The type equality ['a Array.t = 'a array] for interoperability with code which
        does not use Core.
      - The type [('a, 'perms) Array.Permissioned.t] to be abstract, so that the
        permission phantom type will have an effect.

      Since we don't control the definition of ['a array], this would require a type
      [('a, 'perms) Array.Permissioned.t] which is abstract, except that
      [('a, read_write) Array.Permissioned.t] is concrete, which is not possible. *)
  val of_array_id : 'a array -> ('a, [< read_write ]) t

  val to_array_id : ('a, [> read_write ]) t -> 'a array

  (** [to_sequence_immutable t] converts [t] to a sequence. Unlike [to_sequence],
      [to_sequence_immutable] does not need to copy [t] since it is immutable. *)
  val to_sequence_immutable : ('a, [> immutable ]) t -> 'a Sequence.t

  include
    Indexed_container.S1_with_creators_permissions
    with type ('a, 'perms) t := ('a, 'perms) t

  include Blit.S1_permissions with type ('a, 'perms) t := ('a, 'perms) t
  include Binary_searchable.S1_permissions with type ('a, 'perms) t := ('a, 'perms) t

  (** These functions are in [Container.S1_permissions], but they are re-exposed here so
      that their types can be changed to make them more permissive (see comment above). *)

  val length : (_, _) t -> int
  val is_empty : (_, _) t -> bool

  (** counterparts of regular array functions above *)

  external get
    : ('a : any_non_null).
    (('a, [> read ]) t[@local_opt]) -> (int[@local_opt]) -> 'a
    = "%array_safe_get"
  [@@layout_poly]

  external set
    : ('a : any_non_null).
    (('a, [> write ]) t[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
    = "%array_safe_set"
  [@@layout_poly]

  external unsafe_get
    : ('a : any_non_null).
    (('a, [> read ]) t[@local_opt]) -> (int[@local_opt]) -> 'a
    = "%array_unsafe_get"
  [@@layout_poly]

  external unsafe_set
    : ('a : any_non_null).
    (('a, [> write ]) t[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
    = "%array_unsafe_set"
  [@@layout_poly]

  [%%template:
    external create
      : ('a : any_non_null) 'perm.
      len:int -> 'a -> ('a, [< 'perm perms ]) t @ m
      = "%makearray_dynamic"
    [@@ocaml.doc
      " [create ~len x] creates an array of length [len] with the value [x] populated in\n\
      \        each element. "]
    [@@layout_poly]
    [@@alloc __ @ m = (heap_global, stack_local)]]

  external create_local
    : ('a : any_non_null) 'perm.
    len:int -> 'a -> local_ ('a, [< 'perm perms ]) t
    = "%makearray_dynamic"
  [@@ocaml.doc
    " [create_local ~len x] is like [create]. It allocates the array on the local stack.\n\
    \        The array's elements are still global. "]
  [@@layout_poly]

  external magic_create_uninitialized
    : ('a : any_non_null) 'perm.
    len:int -> (('a, [< 'perm perms ]) t[@local_opt])
    = "%makearray_dynamic_uninit"
  [@@ocaml.doc
    " [magic_create_uninitialized ~len] creates an array of length [len] with\n\
    \        uninitialized elements -- that is, they may contain arbitrary, \
     nondeterministic 'a\n\
    \        values. This can be significantly faster than using [create].\n\n\
    \        [magic_create_uninitialized] can only be used for GC-ignorable arrays not\n\
    \        involving tagged immediates and arrays of elements with unboxed number \
     layout. The\n\
    \        compiler rejects attempts to use [magic_create_uninitialized] to produce \
     e.g. an\n\
    \        [('a : value) array].\n\n\
    \        [magic_create_uninitialized] can break abstraction boundaries and type safety\n\
    \        (e.g. by creating phony witnesses to type equality) and so should be used \
     with\n\
    \        caution. "]
  [@@layout_poly]

  val create_float_uninitialized : len:int -> (float, [< _ perms ]) t
  val init : int -> f:local_ (int -> 'a) -> ('a, [< _ perms ]) t
  val make_matrix : dimx:int -> dimy:int -> 'a -> (('a, [< _ perms ]) t, [< _ perms ]) t

  val copy_matrix
    :  local_ (('a, [> read ]) t, [> read ]) t
    -> (('a, [< _ perms ]) t, [< _ perms ]) t

  val append : ('a, [> read ]) t -> ('a, [> read ]) t -> ('a, [< _ perms ]) t
  val concat : local_ ('a, [> read ]) t list -> ('a, [< _ perms ]) t
  val copy : local_ ('a, [> read ]) t -> ('a, [< _ perms ]) t
  val fill : local_ ('a, [> write ]) t -> pos:int -> len:int -> 'a -> unit
  val of_list : 'a list -> ('a, [< _ perms ]) t
  val map : ('a, [> read ]) t -> f:local_ ('a -> 'b) -> ('b, [< _ perms ]) t
  val mapi : ('a, [> read ]) t -> f:local_ (int -> 'a -> 'b) -> ('b, [< _ perms ]) t

  val folding_map
    :  ('a, [> read ]) t
    -> init:'b
    -> f:local_ ('b -> 'a -> 'b * 'c)
    -> ('c, [< _ perms ]) t

  val fold_map
    :  ('a, [> read ]) t
    -> init:'b
    -> f:local_ ('b -> 'a -> 'b * 'c)
    -> 'b * ('c, [< _ perms ]) t

  val iteri : ('a, [> read ]) t -> f:local_ (int -> 'a -> unit) -> unit
  val foldi : ('a, [> read ]) t -> init:'b -> f:local_ (int -> 'b -> 'a -> 'b) -> 'b

  val folding_mapi
    :  ('a, [> read ]) t
    -> init:'b
    -> f:local_ (int -> 'b -> 'a -> 'b * 'c)
    -> ('c, [< _ perms ]) t

  val fold_mapi
    :  ('a, [> read ]) t
    -> init:'b
    -> f:local_ (int -> 'b -> 'a -> 'b * 'c)
    -> 'b * ('c, [< _ perms ]) t

  val fold_right : ('a, [> read ]) t -> f:local_ ('a -> 'b -> 'b) -> init:'b -> 'b

  val sort
    :  ?pos:int
    -> ?len:int
    -> local_ ('a, [> read_write ]) t
    -> compare:local_ ('a -> 'a -> int)
    -> unit

  val stable_sort : ('a, [> read_write ]) t -> compare:('a -> 'a -> int) -> unit
  val is_sorted : local_ ('a, [> read ]) t -> compare:local_ ('a -> 'a -> int) -> bool

  val is_sorted_strictly
    :  local_ ('a, [> read ]) t
    -> compare:local_ ('a -> 'a -> int)
    -> bool

  val merge
    :  ('a, [> read ]) t
    -> ('a, [> read ]) t
    -> compare:local_ ('a -> 'a -> int)
    -> ('a, [< _ perms ]) t

  val concat_map
    :  ('a, [> read ]) t
    -> f:local_ ('a -> ('b, [> read ]) t)
    -> ('b, [< _ perms ]) t

  val concat_mapi
    :  ('a, [> read ]) t
    -> f:local_ (int -> 'a -> ('b, [> read ]) t)
    -> ('b, [< _ perms ]) t

  val partition_tf
    :  ('a, [> read ]) t
    -> f:local_ ('a -> bool)
    -> ('a, [< _ perms ]) t * ('a, [< _ perms ]) t

  val partitioni_tf
    :  ('a, [> read ]) t
    -> f:local_ (int -> 'a -> bool)
    -> ('a, [< _ perms ]) t * ('a, [< _ perms ]) t

  val cartesian_product
    :  ('a, [> read ]) t
    -> ('b, [> read ]) t
    -> ('a * 'b, [< _ perms ]) t

  val transpose
    :  (('a, [> read ]) t, [> read ]) t
    -> (('a, [< _ perms ]) t, [< _ perms ]) t option

  val transpose_exn
    :  (('a, [> read ]) t, [> read ]) t
    -> (('a, [< _ perms ]) t, [< _ perms ]) t

  val normalize : (_, _) t -> int -> int
  val slice : ('a, [> read ]) t -> int -> int -> ('a, [< _ perms ]) t
  val nget : ('a, [> read ]) t -> int -> 'a
  val nset : ('a, [> write ]) t -> int -> 'a -> unit
  val filter_opt : ('a option, [> read ]) t -> ('a, [< _ perms ]) t
  val filter_map : ('a, [> read ]) t -> f:local_ ('a -> 'b option) -> ('b, [< _ perms ]) t

  val filter_mapi
    :  ('a, [> read ]) t
    -> f:local_ (int -> 'a -> 'b option)
    -> ('b, [< _ perms ]) t

  val for_alli : ('a, [> read ]) t -> f:local_ (int -> 'a -> bool) -> bool
  val existsi : ('a, [> read ]) t -> f:local_ (int -> 'a -> bool) -> bool
  val counti : ('a, [> read ]) t -> f:local_ (int -> 'a -> bool) -> int

  val iter2_exn
    :  ('a, [> read ]) t
    -> ('b, [> read ]) t
    -> f:local_ ('a -> 'b -> unit)
    -> unit

  val map2_exn
    :  ('a, [> read ]) t
    -> ('b, [> read ]) t
    -> f:local_ ('a -> 'b -> 'c)
    -> ('c, [< _ perms ]) t

  val fold2_exn
    :  ('a, [> read ]) t
    -> ('b, [> read ]) t
    -> init:'c
    -> f:local_ ('c -> 'a -> 'b -> 'c)
    -> 'c

  val for_all2_exn
    :  ('a, [> read ]) t
    -> ('b, [> read ]) t
    -> f:local_ ('a -> 'b -> bool)
    -> bool

  val exists2_exn
    :  ('a, [> read ]) t
    -> ('b, [> read ]) t
    -> f:local_ ('a -> 'b -> bool)
    -> bool

  val filter : ('a, [> read ]) t -> f:local_ ('a -> bool) -> ('a, [< _ perms ]) t
  val filteri : ('a, [> read ]) t -> f:local_ (int -> 'a -> bool) -> ('a, [< _ perms ]) t
  val swap : local_ ('a, [> read_write ]) t -> int -> int -> unit
  val rev_inplace : local_ ('a, [> read_write ]) t -> unit
  val of_list_rev : 'a list -> ('a, [< _ perms ]) t
  val of_list_map : 'a list -> f:local_ ('a -> 'b) -> ('b, [< _ perms ]) t
  val of_list_mapi : 'a list -> f:local_ (int -> 'a -> 'b) -> ('b, [< _ perms ]) t
  val of_list_rev_map : 'a list -> f:local_ ('a -> 'b) -> ('b, [< _ perms ]) t
  val of_list_rev_mapi : 'a list -> f:local_ (int -> 'a -> 'b) -> ('b, [< _ perms ]) t
  val map_inplace : local_ ('a, [> read_write ]) t -> f:local_ ('a -> 'a) -> unit
  val find_exn : ('a, [> read ]) t -> f:local_ ('a -> bool) -> 'a
  val find_map_exn : ('a, [> read ]) t -> f:local_ ('a -> 'b option) -> 'b
  val findi : ('a, [> read ]) t -> f:local_ (int -> 'a -> bool) -> (int * 'a) option
  val findi_exn : ('a, [> read ]) t -> f:local_ (int -> 'a -> bool) -> int * 'a
  val find_mapi : ('a, [> read ]) t -> f:local_ (int -> 'a -> 'b option) -> 'b option
  val find_mapi_exn : ('a, [> read ]) t -> f:local_ (int -> 'a -> 'b option) -> 'b

  val find_consecutive_duplicate
    :  ('a, [> read ]) t
    -> equal:local_ ('a -> 'a -> bool)
    -> ('a * 'a) option

  val reduce : ('a, [> read ]) t -> f:local_ ('a -> 'a -> 'a) -> 'a option
  val reduce_exn : ('a, [> read ]) t -> f:local_ ('a -> 'a -> 'a) -> 'a

  val permute
    :  ?random_state:Random.State.t
    -> ?pos:int
    -> ?len:int
    -> local_ ('a, [> read_write ]) t
    -> unit

  val zip : ('a, [> read ]) t -> ('b, [> read ]) t -> ('a * 'b, [< _ perms ]) t option
  val zip_exn : ('a, [> read ]) t -> ('b, [> read ]) t -> ('a * 'b, [< _ perms ]) t
  val unzip : ('a * 'b, [> read ]) t -> ('a, [< _ perms ]) t * ('b, [< _ perms ]) t

  val sorted_copy
    :  local_ ('a, [> read ]) t
    -> compare:local_ ('a -> 'a -> int)
    -> ('a, [< _ perms ]) t

  val last : ('a, [> read ]) t -> 'a
  [@@deprecated "[since 2024-07] This was renamed to [last_exn]"]

  val last_exn : ('a, [> read ]) t -> 'a
  val equal : ('a -> 'a -> bool) -> ('a, [> read ]) t -> ('a, [> read ]) t -> bool
  val to_sequence : ('a, [> read ]) t -> 'a Sequence.t
  val to_sequence_mutable : ('a, [> read ]) t -> 'a Sequence.t
end
