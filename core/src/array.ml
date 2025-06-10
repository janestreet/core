open! Import
open Base_quickcheck.Export
open Perms.Export
module Array = Base.Array
module Core_sequence = Sequence

include (
  Base.Array :
  sig
  @@ portable
    type ('a : any_non_null) t = 'a array

    [%%rederive:
      type nonrec 'a t = 'a t
      [@@deriving compare ~localize, globalize, sexp ~localize, sexp_grammar]]
  end)

[%%rederive.portable
  type 'a t = 'a array [@@deriving bin_io ~localize, quickcheck ~portable, typerep]]

module T = struct
  include Base.Array

  let normalize t i = Ordered_collection_common.normalize ~length_fun:length t i

  let slice t start stop =
    Ordered_collection_common.slice
      ~length_fun:length
      ~sub_fun:(fun t ~pos ~len -> sub t ~pos ~len)
      t
      start
      stop
  ;;

  let nget t i = t.(normalize t i)
  let nset t i v = t.(normalize t i) <- v

  module Sequence = struct
    open Base.Array

    let length = length
    let get = get
    let set = set
  end

  (* See OCaml perf notes for why these array blits are special cased -- in particular,
     the section entitled "Fast, Slow and Incorrect Array blits" of
     https://web.archive.org/web/20130220000229/http://janestreet.github.com/ocaml-perf-notes.html *)
  module Int = struct
    type t_ = int array
    [@@deriving bin_io ~localize, compare ~localize, sexp, sexp_grammar]

    module Unsafe_blit = struct
      external unsafe_blit
        :  src:(t_[@local_opt])
        -> src_pos:int
        -> dst:(t_[@local_opt])
        -> dst_pos:int
        -> len:int
        -> unit
        @@ portable
        = "core_array_unsafe_int_blit"
      [@@noalloc]
    end

    include%template
      Test_blit.Make_and_test [@modality portable]
        (struct
          type t = int

          let equal = ( = )
          let of_bool b = if b then 1 else 0
        end)
        (struct
          type t = t_ [@@deriving sexp_of]

          include Sequence

          let create ~len = create ~len 0

          include Unsafe_blit
        end)

    include Unsafe_blit
  end

  module Float = struct
    type t_ = float array
    [@@deriving bin_io ~localize, compare ~localize, sexp, sexp_grammar]

    module Unsafe_blit = struct
      external unsafe_blit
        :  src:(t_[@local_opt])
        -> src_pos:int
        -> dst:(t_[@local_opt])
        -> dst_pos:int
        -> len:int
        -> unit
        @@ portable
        = "core_array_unsafe_float_blit"
      [@@noalloc]
    end

    external get
      :  (t_[@local_opt])
      -> (int[@local_opt])
      -> float
      @@ portable
      = "%floatarray_safe_get"

    external set
      :  (t_[@local_opt])
      -> (int[@local_opt])
      -> (float[@local_opt])
      -> unit
      @@ portable
      = "%floatarray_safe_set"

    external unsafe_get
      :  (t_[@local_opt])
      -> (int[@local_opt])
      -> float
      @@ portable
      = "%floatarray_unsafe_get"

    external unsafe_set
      :  (t_[@local_opt])
      -> (int[@local_opt])
      -> (float[@local_opt])
      -> unit
      @@ portable
      = "%floatarray_unsafe_set"

    include%template
      Test_blit.Make_and_test [@modality portable]
        (struct
          type t = float

          let equal = Base.Float.equal
          let of_bool b = if b then 1. else 0.
        end)
        (struct
          type t = t_ [@@deriving sexp_of]

          include Sequence

          let create ~len = create ~len 0.

          include Unsafe_blit
        end)

    include Unsafe_blit
  end
end

module type Permissioned = sig @@ portable
  type ('a : any_non_null, -'perms) t

  include
    Indexed_container.S1_with_creators_permissions
    with type ('a, 'perms) t := ('a, 'perms) t

  include Blit.S1_permissions with type ('a, 'perms) t := ('a, 'perms) t
  include Binary_searchable.S1_permissions with type ('a, 'perms) t := ('a, 'perms) t

  external length
    : ('a : any_non_null) 'perms.
    (('a, 'perms) t[@local_opt]) @ contended -> int
    @@ portable
    = "%array_length"
  [@@layout_poly]

  val is_empty : (_, _) t -> bool

  external%template get
    : ('a : any_non_null).
    (('a, [> read ]) t[@local_opt]) @ m -> (int[@local_opt]) -> 'a @ m
    @@ portable
    = "%array_safe_get"
  [@@layout_poly] [@@mode m = (uncontended, shared)]

  external set
    : ('a : any_non_null).
    (('a, [> write ]) t[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
    @@ portable
    = "%array_safe_set"
  [@@layout_poly]

  external%template unsafe_get
    : ('a : any_non_null).
    (('a, [> read ]) t[@local_opt]) @ m -> (int[@local_opt]) -> 'a @ m
    @@ portable
    = "%array_unsafe_get"
  [@@layout_poly] [@@mode m = (uncontended, shared)]

  external unsafe_set
    : ('a : any_non_null).
    (('a, [> write ]) t[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
    @@ portable
    = "%array_unsafe_set"
  [@@layout_poly]

  val create_float_uninitialized : len:int -> (float, [< _ perms ]) t

  [%%template:
    external create
      : ('a : any_non_null) 'perm.
      len:int -> 'a -> ('a, [< 'perm perms ]) t @ m
      @@ portable
      = "%makearray_dynamic"
    [@@ocaml.doc
      " [create ~len x] creates an array of length [len] with the value [x] populated in\n\
      \        each element. "]
    [@@alloc __ @ m = (heap_global, stack_local)]
    [@@layout_poly]]

  external create_local
    : ('a : any_non_null) 'perm.
    len:int -> 'a -> local_ ('a, [< 'perm perms ]) t
    @@ portable
    = "%makearray_dynamic"
  [@@ocaml.doc
    " [create_local ~len x] is like [create]. It allocates the array on the local stack.\n\
    \        The array's elements are still global. "]
  [@@layout_poly]

  external magic_create_uninitialized
    : ('a : any_non_null) 'perm.
    len:int -> (('a, [< 'perm perms ]) t[@local_opt])
    @@ portable
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

  val%template init : int -> f:local_ (int -> 'a) -> ('a, [< _ perms ]) t @ m
  [@@alloc __ @ m = (heap_global, stack_local)]

  val make_matrix : dimx:int -> dimy:int -> 'a -> (('a, [< _ perms ]) t, [< _ perms ]) t

  val copy_matrix
    :  local_ (('a, [> read ]) t, [> read ]) t
    -> (('a, [< _ perms ]) t, [< _ perms ]) t

  val append : ('a, [> read ]) t -> ('a, [> read ]) t -> ('a, [< _ perms ]) t
  val concat : local_ ('a, [> read ]) t list -> ('a, [< _ perms ]) t
  val copy : local_ ('a, [> read ]) t -> ('a, [< _ perms ]) t
  val fill : local_ ('a, [> write ]) t -> pos:int -> len:int -> 'a -> unit
  val of_list : 'a list -> ('a, [< _ perms ]) t

  [@@@warning "-incompatible-with-upstream"]

  val%template map
    : ('a : ki) ('b : ko) 'p.
    ('a, [> read ]) t -> f:local_ ('a -> 'b) -> ('b, [< 'p perms ]) t
  [@@kind
    ki = (value, float64, bits32, bits64, word, immediate, immediate64)
    , ko = (value, float64, bits32, bits64, word, immediate, immediate64)]

  val folding_map
    :  ('a, [> read ]) t
    -> init:'acc
    -> f:local_ ('acc -> 'a -> 'acc * 'b)
    -> ('b, [< _ perms ]) t

  val fold_map
    :  ('a, [> read ]) t
    -> init:'acc
    -> f:local_ ('acc -> 'a -> 'acc * 'b)
    -> 'acc * ('b, [< _ perms ]) t

  val mapi : ('a, [> read ]) t -> f:local_ (int -> 'a -> 'b) -> ('b, [< _ perms ]) t
  val iteri : ('a, [> read ]) t -> f:local_ (int -> 'a -> unit) -> unit

  val foldi
    :  ('a, [> read ]) t
    -> init:'acc
    -> f:local_ (int -> 'acc -> 'a -> 'acc)
    -> 'acc

  val%template foldi_right
    :  ('a, [> read ]) t @ local
    -> init:'acc @ m
    -> f:(int -> 'a -> 'acc @ m -> 'acc @ m)
    -> 'acc @ m
  [@@alloc a @ m = (stack_local, heap_global)]

  val folding_mapi
    :  ('a, [> read ]) t
    -> init:'acc
    -> f:local_ (int -> 'acc -> 'a -> 'acc * 'b)
    -> ('b, [< _ perms ]) t

  val fold_mapi
    :  ('a, [> read ]) t
    -> init:'acc
    -> f:local_ (int -> 'acc -> 'a -> 'acc * 'b)
    -> 'acc * ('b, [< _ perms ]) t

  val%template fold_right
    :  ('a, [> read ]) t @ m
    -> f:local_ ('a @ m -> 'acc -> 'acc)
    -> init:'acc
    -> 'acc
  [@@mode m = (uncontended, shared)]

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

  val partition_map
    :  ('a, [> read ]) t
    -> f:local_ ('a -> ('b, 'c) Either.t)
    -> ('b, [< _ perms ]) t * ('c, [< _ perms ]) t

  val partition_mapi
    :  ('a, [> read ]) t
    -> f:local_ (int -> 'a -> ('b, 'c) Either.t)
    -> ('b, [< _ perms ]) t * ('c, [< _ perms ]) t

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

  val normalize : ('a, _) t -> int -> int
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
    -> init:'acc
    -> f:local_ ('acc -> 'a -> 'b -> 'acc)
    -> 'acc

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
  val rev : ('a, [> read ]) t -> ('a, [< _ perms ]) t
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

  val random_element : ?random_state:Random.State.t -> ('a, [> read ]) t -> 'a option
  val random_element_exn : ?random_state:Random.State.t -> ('a, [> read ]) t -> 'a
  val zip : ('a, [> read ]) t -> ('b, [> read ]) t -> ('a * 'b, [< _ perms ]) t option
  val zip_exn : ('a, [> read ]) t -> ('b, [> read ]) t -> ('a * 'b, [< _ perms ]) t
  val unzip : ('a * 'b, [> read ]) t -> ('a, [< _ perms ]) t * ('b, [< _ perms ]) t

  val sorted_copy
    :  local_ ('a, [> read ]) t
    -> compare:local_ ('a -> 'a -> int)
    -> ('a, [< _ perms ]) t

  val last : ('a, [> read ]) t -> 'a
  val last_exn : ('a, [> read ]) t -> 'a
  val equal : ('a -> 'a -> bool) -> ('a, [> read ]) t -> ('a, [> read ]) t -> bool

  val equal__local
    :  (local_ 'a -> local_ 'a -> bool)
    -> local_ ('a, [> read ]) t
    -> local_ ('a, [> read ]) t
    -> bool

  val to_sequence : ('a, [> read ]) t -> 'a Sequence.t
  val to_sequence_mutable : ('a, [> read ]) t -> 'a Sequence.t
end

module Permissioned : sig @@ portable
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

  val of_array_id : 'a array -> ('a, [< read_write ]) t
  val to_array_id : ('a, [> read_write ]) t -> 'a array
  val to_sequence_immutable : ('a, [> immutable ]) t -> 'a Sequence.t

  include Permissioned with type ('a : any_non_null, 'perms) t := ('a, 'perms) t
end = struct
  type ('a : any_non_null, -'perms) t = 'a array

  [%%rederive.portable
    type ('a, _) t = 'a array
    [@@deriving bin_io ~localize, compare ~localize, sexp, sexp_grammar, typerep]]

  module Int = struct
    include T.Int

    type -'perms t = t_
    [@@deriving bin_io ~localize, compare ~localize, sexp, sexp_grammar]
  end

  module Float = struct
    include T.Float

    type -'perms t = t_
    [@@deriving bin_io ~localize, compare ~localize, sexp, sexp_grammar]
  end

  let to_array_id = Fn.id
  let of_array_id = Fn.id

  include (T : Permissioned with type ('a : any_non_null, 'b) t := ('a, 'b) t)
  [@ocaml.warning "-3"]

  let to_array = copy
  let to_sequence_immutable = to_sequence_mutable
end

module type S = sig @@ portable
  type ('a : any_non_null) t

  include Binary_searchable.S1 with type 'a t := 'a t
  include Indexed_container.S1_with_creators with type 'a t := 'a t

  [@@@warning "-incompatible-with-upstream"]

  val%template map : ('a : ki) ('b : ko). 'a t -> f:local_ ('a -> 'b) -> 'b t
  [@@kind
    ki = (value, float64, bits32, bits64, word, immediate, immediate64)
    , ko = (value, float64, bits32, bits64, word, immediate, immediate64)]

  external length
    : ('a : any_non_null) 'perms.
    ('a t[@local_opt]) @ contended -> int
    @@ portable
    = "%array_length"
  [@@layout_poly]

  external%template get
    : ('a : any_non_null).
    ('a t[@local_opt]) @ m -> (int[@local_opt]) -> 'a @ m
    @@ portable
    = "%array_safe_get"
  [@@layout_poly] [@@mode m = (uncontended, shared)]

  external set
    : ('a : any_non_null).
    ('a t[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
    @@ portable
    = "%array_safe_set"
  [@@layout_poly]

  external%template unsafe_get
    : ('a : any_non_null).
    ('a t[@local_opt]) @ m -> (int[@local_opt]) -> 'a @ m
    @@ portable
    = "%array_unsafe_get"
  [@@layout_poly] [@@mode m = (uncontended, shared)]

  external unsafe_set
    : ('a : any_non_null).
    ('a t[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
    @@ portable
    = "%array_unsafe_set"
  [@@layout_poly]

  [%%template:
    external create
      : ('a : any_non_null).
      len:int -> 'a -> 'a t @ m
      @@ portable
      = "%makearray_dynamic"
    [@@ocaml.doc
      " [create ~len x] creates an array of length [len] with the value [x] populated in\n\
      \        each element. "]
    [@@alloc __ @ m = (heap_global, stack_local)]
    [@@layout_poly]]

  external create_local
    : ('a : any_non_null).
    len:int -> 'a -> local_ 'a t
    @@ portable
    = "%makearray_dynamic"
  [@@ocaml.doc
    " [create_local ~len x] is like [create]. It allocates the array on the local stack.\n\
    \        The array's elements are still global. "]
  [@@layout_poly]

  external magic_create_uninitialized
    : ('a : any_non_null).
    len:int -> ('a t[@local_opt])
    @@ portable
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

  val create_float_uninitialized : len:int -> float t

  val%template init : int -> f:local_ (int -> 'a) -> 'a t @ m
  [@@alloc __ @ m = (heap_global, stack_local)]

  val make_matrix : dimx:int -> dimy:int -> 'a -> 'a t t
  val copy_matrix : local_ 'a t t -> 'a t t
  val append : 'a t -> 'a t -> 'a t
  val concat : local_ 'a t list -> 'a t
  val copy : local_ 'a t -> 'a t
  val fill : local_ 'a t -> pos:int -> len:int -> 'a -> unit

  include Blit.S1 with type 'a t := 'a t

  val of_list : 'a list -> 'a t
  val map : 'a t -> f:local_ ('a -> 'b) -> 'b t
  val folding_map : 'a t -> init:'acc -> f:local_ ('acc -> 'a -> 'acc * 'b) -> 'b t
  val fold_map : 'a t -> init:'acc -> f:local_ ('acc -> 'a -> 'acc * 'b) -> 'acc * 'b t
  val mapi : 'a t -> f:local_ (int -> 'a -> 'b) -> 'b t
  val iteri : 'a t -> f:local_ (int -> 'a -> unit) -> unit
  val foldi : 'a t -> init:'b -> f:local_ (int -> 'b -> 'a -> 'b) -> 'b

  val%template foldi_right
    :  'a t @ local
    -> init:'acc @ m
    -> f:(int -> 'a -> 'acc @ m -> 'acc @ m)
    -> 'acc @ m
  [@@alloc a @ m = (stack_local, heap_global)]

  val folding_mapi
    :  'a t
    -> init:'acc
    -> f:local_ (int -> 'acc -> 'a -> 'acc * 'b)
    -> 'b t

  val fold_mapi
    :  'a t
    -> init:'acc
    -> f:local_ (int -> 'acc -> 'a -> 'acc * 'b)
    -> 'acc * 'b t

  val%template fold_right
    :  'a t @ m
    -> f:local_ ('a @ m -> 'acc -> 'acc)
    -> init:'acc
    -> 'acc
  [@@mode m = (uncontended, shared)]

  val sort
    :  ?pos:int
    -> ?len:int
    -> local_ 'a t
    -> compare:local_ ('a -> 'a -> int)
    -> unit

  val stable_sort : 'a t -> compare:('a -> 'a -> int) -> unit
  val is_sorted : local_ 'a t -> compare:local_ ('a -> 'a -> int) -> bool
  val is_sorted_strictly : local_ 'a t -> compare:local_ ('a -> 'a -> int) -> bool
  val merge : 'a t -> 'a t -> compare:local_ ('a -> 'a -> int) -> 'a t
  val concat_map : 'a t -> f:local_ ('a -> 'b t) -> 'b t
  val concat_mapi : 'a t -> f:local_ (int -> 'a -> 'b t) -> 'b t
  val partition_tf : 'a t -> f:local_ ('a -> bool) -> 'a t * 'a t
  val partitioni_tf : 'a t -> f:local_ (int -> 'a -> bool) -> 'a t * 'a t
  val cartesian_product : 'a t -> 'b t -> ('a * 'b) t
  val transpose : 'a t t -> 'a t t option
  val transpose_exn : 'a t t -> 'a t t
  val normalize : 'a t -> int -> int
  val slice : 'a t -> int -> int -> 'a t
  val nget : 'a t -> int -> 'a
  val nset : 'a t -> int -> 'a -> unit
  val filter_opt : 'a option t -> 'a t
  val filter_map : 'a t -> f:local_ ('a -> 'b option) -> 'b t
  val filter_mapi : 'a t -> f:local_ (int -> 'a -> 'b option) -> 'b t
  val for_alli : 'a t -> f:local_ (int -> 'a -> bool) -> bool
  val existsi : 'a t -> f:local_ (int -> 'a -> bool) -> bool
  val counti : 'a t -> f:local_ (int -> 'a -> bool) -> int
  val iter2_exn : 'a t -> 'b t -> f:local_ ('a -> 'b -> unit) -> unit
  val map2_exn : 'a t -> 'b t -> f:local_ ('a -> 'b -> 'c) -> 'c t
  val fold2_exn : 'a t -> 'b t -> init:'acc -> f:local_ ('acc -> 'a -> 'b -> 'acc) -> 'acc
  val for_all2_exn : 'a t -> 'b t -> f:local_ ('a -> 'b -> bool) -> bool
  val exists2_exn : 'a t -> 'b t -> f:local_ ('a -> 'b -> bool) -> bool
  val filter : 'a t -> f:local_ ('a -> bool) -> 'a t
  val filteri : 'a t -> f:local_ (int -> 'a -> bool) -> 'a t
  val swap : local_ 'a t -> int -> int -> unit
  val rev_inplace : local_ 'a t -> unit
  val rev : 'a t -> 'a t
  val of_list_rev : 'a list -> 'a t
  val of_list_map : 'a list -> f:local_ ('a -> 'b) -> 'b t
  val of_list_mapi : 'a list -> f:local_ (int -> 'a -> 'b) -> 'b t
  val of_list_rev_map : 'a list -> f:local_ ('a -> 'b) -> 'b t
  val of_list_rev_mapi : 'a list -> f:local_ (int -> 'a -> 'b) -> 'b t
  val map_inplace : local_ 'a t -> f:local_ ('a -> 'a) -> unit
  val find_exn : 'a t -> f:local_ ('a -> bool) -> 'a
  val find_map_exn : 'a t -> f:local_ ('a -> 'b option) -> 'b
  val findi : 'a t -> f:local_ (int -> 'a -> bool) -> (int * 'a) option
  val findi_exn : 'a t -> f:local_ (int -> 'a -> bool) -> int * 'a
  val find_mapi : 'a t -> f:local_ (int -> 'a -> 'b option) -> 'b option
  val find_mapi_exn : 'a t -> f:local_ (int -> 'a -> 'b option) -> 'b

  val find_consecutive_duplicate
    :  'a t
    -> equal:local_ ('a -> 'a -> bool)
    -> ('a * 'a) option

  val reduce : 'a t -> f:local_ ('a -> 'a -> 'a) -> 'a option
  val reduce_exn : 'a t -> f:local_ ('a -> 'a -> 'a) -> 'a

  val permute
    :  ?random_state:Random.State.t
    -> ?pos:int
    -> ?len:int
    -> local_ 'a t
    -> unit

  val random_element : ?random_state:Random.State.t -> 'a t -> 'a option
  val random_element_exn : ?random_state:Random.State.t -> 'a t -> 'a
  val zip : 'a t -> 'b t -> ('a * 'b) t option
  val zip_exn : 'a t -> 'b t -> ('a * 'b) t
  val unzip : ('a * 'b) t -> 'a t * 'b t
  val sorted_copy : local_ 'a t -> compare:local_ ('a -> 'a -> int) -> 'a t
  val last : 'a t -> 'a
  val last_exn : 'a t -> 'a
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val equal__local
    :  (local_ 'a -> local_ 'a -> bool)
    -> local_ 'a t
    -> local_ 'a t
    -> bool

  val to_sequence : 'a t -> 'a Core_sequence.t
  val to_sequence_mutable : 'a t -> 'a Core_sequence.t
end

include (T : S with type ('a : any_non_null) t := 'a array) [@ocaml.warning "-3"]

let invariant invariant_a t = iter t ~f:invariant_a
let max_length = Sys.max_array_length

module Int = struct
  include T.Int

  type t = t_ [@@deriving bin_io ~localize, compare ~localize, sexp, sexp_grammar]
end

module Float = struct
  include T.Float

  type t = t_ [@@deriving bin_io ~localize, compare ~localize, sexp, sexp_grammar]
end

module _ (M : S) : sig
  type ('a : any_non_null, -'perm) t_

  include Permissioned with type ('a : any_non_null, 'perm) t := ('a, 'perm) t_
end = struct
  include M

  type ('a : any_non_null, -'perm) t_ = 'a t
end

module _ (M : Permissioned) : sig
  type ('a : any_non_null) t_

  include S with type ('a : any_non_null) t := 'a t_
end = struct
  include M

  type ('a : any_non_null) t_ = ('a, read_write) t
end
