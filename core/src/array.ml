open! Import
open Base_quickcheck.Export
open Perms.Export
module Array = Base.Array
module Core_sequence = Sequence

[%%template
[@@@kind_set.define base_or_null_with_ext = (base_or_null, value_or_null mod external64)]

include (
  Base.Array :
  sig
  @@ portable
    type ('a : any mod separable) t = 'a array

    [%%rederive:
      type nonrec ('a : value_or_null mod separable) t = 'a t
      [@@deriving compare ~localize, globalize, sexp ~stackify, sexp_grammar]]
  end)

[%%template
type ('a : k mod separable) array = 'a array
[@@kind k = (base_non_value, value_or_null mod external64)]

[@@@kind k = (base_or_null, value_or_null mod external64)]

[%%rederive.portable
  type ('a : k mod separable) t = ('a array[@kind k])
  [@@deriving bin_io ~localize] [@@kind k]]]

[%%rederive.portable type 'a t = 'a array [@@deriving quickcheck ~portable, typerep]]

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
        = "base_array_unsafe_float_blit"
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
  type ('a, -'perms) t

  include
    Indexed_container.S1_with_creators_permissions
    with type ('a, 'perms) t := ('a, 'perms) t

  include Blit.S1_permissions with type ('a, 'perms) t := ('a, 'perms) t
  include Binary_searchable.S1_permissions with type ('a, 'perms) t := ('a, 'perms) t

  val length : ('a, 'perms) t @ contended local -> int
  val is_empty : (_, _) t -> bool

  external%template get
    :  (('a, [> read ]) t[@local_opt]) @ m
    -> (int[@local_opt])
    -> 'a @ m
    = "%array_safe_get"
  [@@mode m = (uncontended, shared)]

  val%template get_opt : ('a, [> read ]) t @ c local -> int -> 'a Option.t @ c m
  [@@mode c = (uncontended, shared)] [@@alloc __ @ m = (heap_global, stack_local)]

  external set
    :  (('a, [> write ]) t[@local_opt])
    -> (int[@local_opt])
    -> 'a
    -> unit
    = "%array_safe_set"

  external%template unsafe_get
    :  (('a, [> read ]) t[@local_opt]) @ m
    -> (int[@local_opt])
    -> 'a @ m
    = "%array_unsafe_get"
  [@@mode m = (uncontended, shared)]

  external unsafe_set
    :  (('a, [> write ]) t[@local_opt])
    -> (int[@local_opt])
    -> 'a
    -> unit
    = "%array_unsafe_set"

  val create_float_uninitialized : len:int -> (float, [< _ perms ]) t

  [%%template:
    external create
      :  len:int
      -> 'a
      -> ('a, [< 'perm perms ]) t @ m unique
      = "%makearray_dynamic"
    [@@ocaml.doc
      {| [create ~len x] creates an array of length [len] with the value [x] populated in
        each element. |}]
    [@@alloc __ @ m = (heap_global, stack_local)]]

  external create_local
    :  len:int
    -> 'a
    -> ('a, [< 'perm perms ]) t @ local unique
    = "%makearray_dynamic"
  [@@ocaml.doc
    {| [create_local ~len x] is like [create]. It allocates the array on the local stack.
        The array's elements are still global. |}]

  external magic_create_uninitialized
    :  len:int
    -> (('a, [< 'perm perms ]) t[@local_opt]) @ unique
    = "%makearray_dynamic_uninit"
  [@@ocaml.doc
    {| [magic_create_uninitialized ~len] creates an array of length [len] with
        uninitialized elements -- that is, they may contain arbitrary, nondeterministic 'a
        values. This can be significantly faster than using [create].

        [magic_create_uninitialized] can only be used for GC-ignorable arrays not
        involving tagged immediates and arrays of elements with unboxed number layout. The
        compiler rejects attempts to use [magic_create_uninitialized] to produce e.g. an
        [('a : value) array].

        [magic_create_uninitialized] can break abstraction boundaries and type safety
        (e.g. by creating phony witnesses to type equality) and so should be used with
        caution. |}]

  val%template init : int -> f:(int -> 'a) @ local -> ('a, [< _ perms ]) t @ m
  [@@alloc __ @ m = (heap_global, stack_local)]

  val make_matrix : dimx:int -> dimy:int -> 'a -> (('a, [< _ perms ]) t, [< _ perms ]) t

  val copy_matrix
    :  (('a, [> read ]) t, [> read ]) t @ local
    -> (('a, [< _ perms ]) t, [< _ perms ]) t

  val append : ('a, [> read ]) t -> ('a, [> read ]) t -> ('a, [< _ perms ]) t
  val concat : ('a, [> read ]) t list @ local -> ('a, [< _ perms ]) t
  val copy : ('a, [> read ]) t @ local -> ('a, [< _ perms ]) t
  val fill : ('a, [> write ]) t @ local -> pos:int -> len:int -> 'a -> unit
  val of_list : 'a list -> ('a, [< _ perms ]) t
  val map : ('a, [> read ]) t -> f:('a -> 'b) @ local -> ('b, [< _ perms ]) t

  val folding_map
    :  ('a, [> read ]) t
    -> init:'acc
    -> f:('acc -> 'a -> 'acc * 'b) @ local
    -> ('b, [< _ perms ]) t

  val fold_map
    :  ('a, [> read ]) t
    -> init:'acc
    -> f:('acc -> 'a -> 'acc * 'b) @ local
    -> 'acc * ('b, [< _ perms ]) t

  val mapi : ('a, [> read ]) t -> f:(int -> 'a -> 'b) @ local -> ('b, [< _ perms ]) t
  val iteri : ('a, [> read ]) t -> f:(int -> 'a -> unit) @ local -> unit

  val foldi
    :  ('a, [> read ]) t
    -> init:'acc
    -> f:(int -> 'acc -> 'a -> 'acc) @ local
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
    -> f:(int -> 'acc -> 'a -> 'acc * 'b) @ local
    -> ('b, [< _ perms ]) t

  val fold_mapi
    :  ('a, [> read ]) t
    -> init:'acc
    -> f:(int -> 'acc -> 'a -> 'acc * 'b) @ local
    -> 'acc * ('b, [< _ perms ]) t

  val%template fold_right
    :  ('a, [> read ]) t @ m
    -> f:('a @ m -> 'acc -> 'acc) @ local
    -> init:'acc
    -> 'acc
  [@@mode m = (uncontended, shared)]

  val sort
    :  ?pos:int
    -> ?len:int
    -> ('a, [> read_write ]) t @ local
    -> compare:('a -> 'a -> int) @ local
    -> unit

  val stable_sort : ('a, [> read_write ]) t -> compare:('a -> 'a -> int) -> unit
  val is_sorted : ('a, [> read ]) t @ local -> compare:('a -> 'a -> int) @ local -> bool

  val is_sorted_strictly
    :  ('a, [> read ]) t @ local
    -> compare:('a -> 'a -> int) @ local
    -> bool

  val merge
    :  ('a, [> read ]) t
    -> ('a, [> read ]) t
    -> compare:('a -> 'a -> int) @ local
    -> ('a, [< _ perms ]) t

  val concat_map
    :  ('a, [> read ]) t
    -> f:('a -> ('b, [> read ]) t) @ local
    -> ('b, [< _ perms ]) t

  val concat_mapi
    :  ('a, [> read ]) t
    -> f:(int -> 'a -> ('b, [> read ]) t) @ local
    -> ('b, [< _ perms ]) t

  val partition_tf
    :  ('a, [> read ]) t
    -> f:('a -> bool) @ local
    -> ('a, [< _ perms ]) t * ('a, [< _ perms ]) t

  val partitioni_tf
    :  ('a, [> read ]) t
    -> f:(int -> 'a -> bool) @ local
    -> ('a, [< _ perms ]) t * ('a, [< _ perms ]) t

  val partition_map
    :  ('a, [> read ]) t
    -> f:('a -> ('b, 'c) Either.t) @ local
    -> ('b, [< _ perms ]) t * ('c, [< _ perms ]) t

  val partition_mapi
    :  ('a, [> read ]) t
    -> f:(int -> 'a -> ('b, 'c) Either.t) @ local
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

  val filter_map
    :  ('a, [> read ]) t
    -> f:('a -> 'b option) @ local
    -> ('b, [< _ perms ]) t

  val filter_mapi
    :  ('a, [> read ]) t
    -> f:(int -> 'a -> 'b option) @ local
    -> ('b, [< _ perms ]) t

  val for_alli : ('a, [> read ]) t -> f:(int -> 'a -> bool) @ local -> bool
  val existsi : ('a, [> read ]) t -> f:(int -> 'a -> bool) @ local -> bool
  val counti : ('a, [> read ]) t -> f:(int -> 'a -> bool) @ local -> int

  val iter2_exn
    :  ('a, [> read ]) t
    -> ('b, [> read ]) t
    -> f:('a -> 'b -> unit) @ local
    -> unit

  val map2_exn
    :  ('a, [> read ]) t
    -> ('b, [> read ]) t
    -> f:('a -> 'b -> 'c) @ local
    -> ('c, [< _ perms ]) t

  val fold2_exn
    :  ('a, [> read ]) t
    -> ('b, [> read ]) t
    -> init:'acc
    -> f:('acc -> 'a -> 'b -> 'acc) @ local
    -> 'acc

  val for_all2_exn
    :  ('a, [> read ]) t
    -> ('b, [> read ]) t
    -> f:('a -> 'b -> bool) @ local
    -> bool

  val exists2_exn
    :  ('a, [> read ]) t
    -> ('b, [> read ]) t
    -> f:('a -> 'b -> bool) @ local
    -> bool

  val filter : ('a, [> read ]) t -> f:('a -> bool) @ local -> ('a, [< _ perms ]) t
  val filteri : ('a, [> read ]) t -> f:(int -> 'a -> bool) @ local -> ('a, [< _ perms ]) t
  val swap : ('a, [> read_write ]) t @ local -> int -> int -> unit
  val rev_inplace : ('a, [> read_write ]) t @ local -> unit
  val rev : ('a, [> read ]) t -> ('a, [< _ perms ]) t
  val of_list_rev : 'a list -> ('a, [< _ perms ]) t
  val of_list_map : 'a list -> f:('a -> 'b) @ local -> ('b, [< _ perms ]) t
  val of_list_mapi : 'a list -> f:(int -> 'a -> 'b) @ local -> ('b, [< _ perms ]) t
  val of_list_rev_map : 'a list -> f:('a -> 'b) @ local -> ('b, [< _ perms ]) t
  val of_list_rev_mapi : 'a list -> f:(int -> 'a -> 'b) @ local -> ('b, [< _ perms ]) t
  val map_inplace : ('a, [> read_write ]) t @ local -> f:('a -> 'a) @ local -> unit
  val find_exn : ('a, [> read ]) t -> f:('a -> bool) @ local -> 'a
  val find_map_exn : ('a, [> read ]) t -> f:('a -> 'b option) @ local -> 'b
  val findi : ('a, [> read ]) t -> f:(int -> 'a -> bool) @ local -> (int * 'a) option
  val findi_exn : ('a, [> read ]) t -> f:(int -> 'a -> bool) @ local -> int * 'a
  val find_mapi : ('a, [> read ]) t -> f:(int -> 'a -> 'b option) @ local -> 'b option
  val find_mapi_exn : ('a, [> read ]) t -> f:(int -> 'a -> 'b option) @ local -> 'b

  val find_consecutive_duplicate
    :  ('a, [> read ]) t
    -> equal:('a -> 'a -> bool) @ local
    -> ('a * 'a) option

  val reduce : ('a, [> read ]) t -> f:('a -> 'a -> 'a) @ local -> 'a option
  val reduce_exn : ('a, [> read ]) t -> f:('a -> 'a -> 'a) @ local -> 'a

  val permute
    :  ?random_state:Random.State.t @ local
    -> ?pos:int
    -> ?len:int
    -> ('a, [> read_write ]) t @ local
    -> unit

  val random_element
    :  ?random_state:Random.State.t @ local
    -> ('a, [> read ]) t
    -> 'a option

  val random_element_exn : ?random_state:Random.State.t @ local -> ('a, [> read ]) t -> 'a
  val zip : ('a, [> read ]) t -> ('b, [> read ]) t -> ('a * 'b, [< _ perms ]) t option
  val zip_exn : ('a, [> read ]) t -> ('b, [> read ]) t -> ('a * 'b, [< _ perms ]) t
  val unzip : ('a * 'b, [> read ]) t -> ('a, [< _ perms ]) t * ('b, [< _ perms ]) t

  val sorted_copy
    :  ('a, [> read ]) t @ local
    -> compare:('a -> 'a -> int) @ local
    -> ('a, [< _ perms ]) t

  val last : ('a, [> read ]) t -> 'a
  val last_exn : ('a, [> read ]) t -> 'a
  val equal : ('a -> 'a -> bool) -> ('a, [> read ]) t -> ('a, [> read ]) t -> bool

  val equal__local
    :  ('a @ local -> 'a @ local -> bool)
    -> ('a, [> read ]) t @ local
    -> ('a, [> read ]) t @ local
    -> bool

  val to_sequence : ('a, [> read ]) t -> 'a Sequence.t
  val to_sequence_mutable : ('a, [> read ]) t -> 'a Sequence.t
end

module Permissioned : sig @@ portable
  type ('a, -'perms) t

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
      = "base_array_unsafe_float_blit"
    [@@noalloc]
  end

  external of_array_id
    :  ('a array[@local_opt])
    -> (('a, [< read_write ]) t[@local_opt])
    = "%identity"

  external to_array_id
    :  (('a, [> read_write ]) t[@local_opt])
    -> ('a array[@local_opt])
    = "%identity"

  val to_sequence_immutable : ('a, [> immutable ]) t -> 'a Sequence.t

  include Permissioned with type ('a, 'perms) t := ('a, 'perms) t
end = struct
  type ('a, -'perms) t = 'a array

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

  external to_array_id
    :  (('a, [> read_write ]) t[@local_opt])
    -> ('a array[@local_opt])
    @@ portable
    = "%identity"

  external of_array_id
    :  ('a array[@local_opt])
    -> (('a, [< read_write ]) t[@local_opt])
    @@ portable
    = "%identity"

  module T' = struct
    (* Replace externals in [T] with non-layout poly versions *)
    include T

    external length : ('a t[@local_opt]) @ immutable -> int @@ portable = "%array_length"

    external%template get
      :  ('a t[@local_opt]) @ m
      -> (int[@local_opt])
      -> 'a @ m
      @@ portable
      = "%array_safe_get"
    [@@mode m = (uncontended, shared)]

    external%template unsafe_get
      :  ('a t[@local_opt]) @ m
      -> (int[@local_opt])
      -> 'a @ m
      @@ portable
      = "%array_unsafe_get"
    [@@mode m = (uncontended, shared)]

    external unsafe_set
      :  ('a t[@local_opt])
      -> (int[@local_opt])
      -> 'a
      -> unit
      @@ portable
      = "%array_unsafe_set"

    external set
      :  ('a t[@local_opt])
      -> (int[@local_opt])
      -> 'a
      -> unit
      @@ portable
      = "%array_safe_set"

    [%%template
      external create
        :  len:int
        -> 'a
        -> 'a t @ m unique
        @@ portable
        = "%makearray_dynamic"
      [@@alloc __ @ m = (heap_global, stack_local)]]

    external create_local
      :  len:int
      -> 'a
      -> 'a t @ local unique
      @@ portable
      = "%makearray_dynamic"

    external magic_create_uninitialized
      :  len:int
      -> ('a t[@local_opt]) @ unique
      @@ portable
      = "%makearray_dynamic_uninit"
  end

  include (T' : Permissioned with type ('a, 'b) t := ('a, 'b) t) [@ocaml.warning "-3"]

  let to_array = copy
  let to_sequence_immutable = to_sequence_mutable
end

module type S = sig @@ portable
  type ('a : any mod separable) t

  val array_should_be_polymorphic_over_value_or_null : unit

  type%template ('a : k mod separable) t
  [@@kind k = (base_non_value, value_or_null mod external64)]
  [@@deriving compare ~localize, equal ~localize, sexp ~stackify, globalize]

  include Binary_searchable.S1 with type 'a t := 'a t

  include%template
    Indexed_container.S1_with_creators
  [@kind_set.explicit base_or_null_with_ext]
  [@with: type ('a : any) t := 'a t [@@kind base_or_null_with_ext]]

  include
    Indexed_container.S1_with_creators
    [@kind_set.explicit value_or_null]
    with type ('a : value_or_null mod separable) t := 'a t

  val%template map
    : ('a : ki mod separable) ('b : ko mod separable).
    'a t -> f:('a -> 'b) @ local -> 'b t
  [@@kind ki = base_or_null_with_ext, ko = base_or_null_with_ext]

  external length
    : ('a : any mod separable) 'perms.
    ('a t[@local_opt]) @ immutable -> int
    = "%array_length"
  [@@layout_poly]

  external%template get
    : ('a : any mod separable).
    ('a t[@local_opt]) @ m -> (int[@local_opt]) -> 'a @ m
    = "%array_safe_get"
  [@@layout_poly] [@@mode m = (uncontended, shared)]

  val%template get_opt
    : ('a : k).
    'a t @ c local -> int -> ('a Option.t[@kind k or value_or_null]) @ c m
  [@@mode c = (uncontended, shared)]
  [@@kind k = base]
  [@@alloc __ @ m = (heap_global, stack_local)]

  val%template get_or_null : ('a : value). 'a t @ c local -> int -> 'a or_null @ c
  [@@mode c = (uncontended, shared)]

  external set
    : ('a : any mod separable).
    ('a t[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
    = "%array_safe_set"
  [@@layout_poly]

  external%template unsafe_get
    : ('a : any mod separable).
    ('a t[@local_opt]) @ m -> (int[@local_opt]) -> 'a @ m
    = "%array_unsafe_get"
  [@@layout_poly] [@@mode m = (uncontended, shared)]

  external unsafe_set
    : ('a : any mod separable).
    ('a t[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
    = "%array_unsafe_set"
  [@@layout_poly]

  [%%template:
    external create
      : ('a : any mod separable).
      len:int -> 'a -> 'a t @ m unique
      = "%makearray_dynamic"
    [@@ocaml.doc
      {| [create ~len x] creates an array of length [len] with the value [x] populated in
        each element. |}]
    [@@alloc __ @ m = (heap_global, stack_local)]
    [@@layout_poly]]

  external create_local
    : ('a : any mod separable).
    len:int -> 'a -> 'a t @ local unique
    = "%makearray_dynamic"
  [@@ocaml.doc
    {| [create_local ~len x] is like [create]. It allocates the array on the local stack.
        The array's elements are still global. |}]
  [@@layout_poly]

  external magic_create_uninitialized
    : ('a : any mod separable).
    len:int -> ('a t[@local_opt]) @ unique
    = "%makearray_dynamic_uninit"
  [@@ocaml.doc
    {| [magic_create_uninitialized ~len] creates an array of length [len] with
        uninitialized elements -- that is, they may contain arbitrary, nondeterministic 'a
        values. This can be significantly faster than using [create].

        [magic_create_uninitialized] can only be used for GC-ignorable arrays not
        involving tagged immediates and arrays of elements with unboxed number layout. The
        compiler rejects attempts to use [magic_create_uninitialized] to produce e.g. an
        [('a : value) array].

        [magic_create_uninitialized] can break abstraction boundaries and type safety
        (e.g. by creating phony witnesses to type equality) and so should be used with
        caution. |}]
  [@@layout_poly]

  val create_float_uninitialized : len:int -> float t

  val%template init
    : ('a : value_or_null mod separable).
    int -> f:(int -> 'a) @ local -> 'a t @ m
  [@@alloc __ @ m = (heap_global, stack_local)]

  val make_matrix : dimx:int -> dimy:int -> 'a -> 'a t t
  val copy_matrix : 'a t t @ local -> 'a t t

  [%%template:
  [@@@kind.default k' = base_or_null_with_ext]
  [@@@kind k = k' mod separable]

  val concat : ('a : k). 'a t list @ local -> 'a t
  val copy : ('a : k). 'a t @ local -> 'a t
  val fill : ('a : k). 'a t @ local -> pos:int -> len:int -> 'a -> unit]

  include
    Blit.S1 [@kind.explicit value_or_null mod separable] with type ('a : any) t := 'a t

  val%template unsafe_blit : ('a : k mod separable). ('a array, 'a array) Blit.blit
  [@@kind k = base_or_null_with_ext]

  val%template sub : ('a : k mod separable). ('a array, 'a array) Blit.sub
  [@@kind k = base_or_null_with_ext]

  include sig
    [@@@implicit_kind: ('a : value_or_null mod separable)]

    val folding_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) @ local -> 'b t
    val fold_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) @ local -> 'acc * 'b t

    val%template foldi_right
      :  'a t @ local
      -> init:'acc @ m
      -> f:(int -> 'a -> 'acc @ m -> 'acc @ m)
      -> 'acc @ m
    [@@alloc a @ m = (stack_local, heap_global)]

    val folding_mapi
      :  'a t
      -> init:'acc
      -> f:(int -> 'acc -> 'a -> 'acc * 'b) @ local
      -> 'b t

    val fold_mapi
      :  'a t
      -> init:'acc
      -> f:(int -> 'acc -> 'a -> 'acc * 'b) @ local
      -> 'acc * 'b t

    val%template fold_right
      :  'a t @ m
      -> f:('a @ m -> 'acc -> 'acc) @ local
      -> init:'acc
      -> 'acc
    [@@mode m = (uncontended, shared)]
  end

  val%template sort
    : ('a : k mod separable).
    ?pos:int -> ?len:int -> 'a t @ local -> compare:('a -> 'a -> int) @ local -> unit
  [@@kind k = base_or_null_with_ext]

  val stable_sort : 'a t -> compare:('a -> 'a -> int) -> unit

  [%%template:
  [@@@kind.default k = base_or_null_with_ext]
  [@@@kind k = k mod separable]

  val is_sorted : ('a : k). 'a t @ local -> compare:('a -> 'a -> int) @ local -> bool

  val is_sorted_strictly
    : ('a : k).
    'a t @ local -> compare:('a -> 'a -> int) @ local -> bool

  val merge : ('a : k). 'a t -> 'a t -> compare:('a -> 'a -> int) @ local -> 'a t]

  include sig
    [@@@implicit_kind: ('a : value_or_null mod separable)]
    [@@@implicit_kind: ('b : value_or_null mod separable)]

    val partitioni_tf : 'a t -> f:(int -> 'a -> bool) @ local -> 'a t * 'a t
    val cartesian_product : 'a t -> 'b t -> ('a * 'b) t
    val transpose : 'a t t -> 'a t t option
    val transpose_exn : 'a t t -> 'a t t
  end

  val normalize : 'a t -> int -> int
  val slice : 'a t -> int -> int -> 'a t
  val nget : 'a t -> int -> 'a
  val nset : 'a t -> int -> 'a -> unit

  include sig
    [@@@implicit_kind: ('a : value_or_null mod separable)]
    [@@@implicit_kind: ('b : value_or_null mod separable)]

    val filter_opt : 'a option t -> 'a t
    val iter2_exn : 'a t -> 'b t -> f:('a -> 'b -> unit) @ local -> unit
    val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) @ local -> 'c t

    val fold2_exn
      :  'a t
      -> 'b t
      -> init:'acc
      -> f:('acc -> 'a -> 'b -> 'acc) @ local
      -> 'acc
  end

  [%%template:
  [@@@kind.default k1' = base_or_null_with_ext, k2' = base_or_null_with_ext]
  [@@@kind k1 = k1' mod separable, k2 = k2' mod separable]

  val of_list_map
    : ('a : k1') ('b : k2).
    ('a List.t[@kind k1' or value_or_null]) -> f:('a -> 'b) @ local -> 'b t

  val of_list_mapi
    : ('a : k1') ('b : k2).
    ('a List.t[@kind k1' or value_or_null]) -> f:(int -> 'a -> 'b) @ local -> 'b t

  val of_list_rev_map
    : ('a : k1') ('b : k2).
    ('a List.t[@kind k1' or value_or_null]) -> f:('a -> 'b) @ local -> 'b t

  val of_list_rev_mapi
    : ('a : k1') ('b : k2).
    ('a List.t[@kind k1' or value_or_null]) -> f:(int -> 'a -> 'b) @ local -> 'b t

  [@@@mode.default m = (global, local)]

  val for_all2_exn
    : ('a : k1) ('b : k2).
    'a t @ m -> 'b t @ m -> f:('a @ m -> 'b @ m -> bool) @ local -> bool

  val exists2_exn
    : ('a : k1) ('b : k2).
    'a t @ m -> 'b t @ m -> f:('a @ m -> 'b @ m -> bool) @ local -> bool]

  val map_inplace
    : ('a : value_or_null mod separable).
    'a t @ local -> f:('a -> 'a) @ local -> unit

  [%%template:
  [@@@kind.default k1' = base_or_null_with_ext]
  [@@@kind k1 = k1' mod separable]

  val swap : ('a : k1). 'a t @ local -> int -> int -> unit
  val rev_inplace : ('a : k1). 'a t @ local -> unit
  val rev : ('a : k1). 'a t -> 'a t
  val of_list_rev : ('a : k1). ('a List.t[@kind k1' or value_or_null]) -> 'a t
  val find_exn : ('a : k1). 'a t -> f:('a -> bool) @ local -> 'a

  [@@@kind.default k2 = base_or_null]

  val find_map_exn
    : ('a : k1) ('b : k2).
    'a t -> f:('a -> ('b Option.t[@kind k2 or value_or_null])) @ local -> 'b

  val find_mapi_exn
    : ('a : k1) ('b : k2).
    'a t -> f:(int -> 'a -> ('b Option.t[@kind k2 or value_or_null])) @ local -> 'b]

  [%%template:
  [@@@kind.default k' = (base_non_value, value_or_null mod external64)]
  [@@@kind k = k' mod separable]

  val findi_exn : ('a : k). 'a t -> f:(int -> 'a -> bool) @ local -> #(int * 'a)]

  val find_or_null : ('a : value). 'a t -> f:('a -> bool) @ local -> 'a or_null

  val findi_or_null
    : ('a : value).
    'a t -> f:(int -> 'a -> bool) @ local -> (int * 'a) or_null

  include sig
    [@@@implicit_kind: ('a : value_or_null mod separable)]

    val findi_exn : 'a t -> f:(int -> 'a -> bool) @ local -> int * 'a

    val find_consecutive_duplicate
      :  'a t
      -> equal:('a -> 'a -> bool) @ local
      -> ('a * 'a) option

    val reduce : 'a t -> f:('a -> 'a -> 'a) @ local -> 'a option
    val reduce_exn : 'a t -> f:('a -> 'a -> 'a) @ local -> 'a

    val permute
      :  ?random_state:Random.State.t @ local
      -> ?pos:int
      -> ?len:int
      -> 'a t @ local
      -> unit

    val random_element : ?random_state:Random.State.t @ local -> 'a t -> 'a option
    val random_element_exn : ?random_state:Random.State.t @ local -> 'a t -> 'a
    val zip : 'a t -> 'b t -> ('a * 'b) t option
    val zip_exn : 'a t -> 'b t -> ('a * 'b) t
    val unzip : ('a * 'b) t -> 'a t * 'b t
    val sorted_copy : 'a t @ local -> compare:('a -> 'a -> int) @ local -> 'a t
    val last : 'a t -> 'a
    val last_exn : 'a t -> 'a

    val equal
      : ('a : value_or_null mod separable).
      ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val equal__local
      : ('a : value_or_null mod separable).
      ('a @ local -> 'a @ local -> bool) -> 'a t @ local -> 'a t @ local -> bool

    val to_sequence : 'a t -> 'a Core_sequence.t
    val to_sequence_mutable : 'a t -> 'a Core_sequence.t
    val split_n : 'a t -> int -> 'a t * 'a t
    val chunks_of : 'a t -> length:int -> 'a t t
  end
end

include%template (
  T :
    S
    [@with:
      type ('a : any mod separable) t := 'a array

      type ('a : k mod separable) t = 'a array
      [@@kind k = (base_non_value, value_or_null mod external64)]])
[@ocaml.warning "-3"]

let invariant invariant_a t = iter t ~f:invariant_a
let max_length = Sys.max_array_length

module Int = struct
  include T.Int

  type t = t_ [@@deriving bin_io ~localize, compare ~localize, sexp, sexp_grammar]
end

module Float = struct
  include T.Float

  type t = t_ [@@deriving bin_io ~localize, compare ~localize, sexp, sexp_grammar]
end]
