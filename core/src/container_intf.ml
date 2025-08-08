(** This module extends {!Base.Container}. *)

open! Import
open Perms.Export
open Base.Container
module Continue_or_stop = Continue_or_stop

module type%template S0_permissions = sig
  type elt
  type -'permissions t

  (** Checks whether the provided element is there. *)
  val mem : [> read ] t -> elt -> bool
  [@@mode m = (global, m)]

  val length : [> read ] t -> int
  val is_empty : [> read ] t -> bool

  (** [iter t ~f] calls [f] on each element of [t]. *)
  val iter : [> read ] t -> f:(elt -> unit) -> unit
  [@@mode m = (global, m)]

  (** [iter_until t ~f ~finish] is a short-circuiting version of [iter]. If [f] returns
      [Stop x] the computation ceases and returns [x]. If [f] always returns [Continue ()]
      the final result is computed by [finish]. *)
  val iter_until
    :  [> read ] t
    -> f:(elt -> (unit, 'final) Continue_or_stop.t)
    -> finish:(unit -> 'final)
    -> 'final
  [@@mode mi = (global, m), mo = (global, m)]

  (** [fold t ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where [e1..en]
      are the elements of [t] *)
  val fold : [> read ] t -> init:'acc -> f:('acc -> elt -> 'acc) -> 'acc
  [@@mode mi = (global, m), mo = (global, m)]

  (** [fold_result t ~init ~f] is a short-circuiting version of [fold] that runs in the
      [Result] monad. If [f] returns an [Error _], that value is returned without any
      additional invocations of [f]. *)
  val fold_result
    :  [> read ] t
    -> init:'acc
    -> f:('acc -> elt -> ('acc, 'e) Result.t)
    -> ('acc, 'e) Result.t
  [@@mode mi = (global, m), mo = (global, m)]

  (** [fold_until t ~init ~f ~finish] is a short-circuiting version of [fold]. If [f]
      returns [Stop _] the computation ceases and results in that value. If [f] returns
      [Continue _], the fold will proceed. If [f] never returns [Stop _], the final result
      is computed by [finish]. *)
  val fold_until
    :  [> read ] t
    -> init:'acc
    -> f:('acc -> elt -> ('acc, 'final) Continue_or_stop.t)
    -> finish:('acc -> 'final)
    -> 'final
  [@@mode mi = (global, m), mo = (global, m)]

  (** Returns [true] if and only if there exists an element for which the provided
      function evaluates to [true]. This is a short-circuiting operation. *)
  val exists : [> read ] t -> f:(elt -> bool) -> bool
  [@@mode m = (global, m)]

  (** Returns [true] if and only if the provided function evaluates to [true] for all
      elements. This is a short-circuiting operation. *)
  val for_all : [> read ] t -> f:(elt -> bool) -> bool
  [@@mode m = (global, m)]

  (** Returns the number of elements for which the provided function evaluates to true. *)
  val count : [> read ] t -> f:(elt -> bool) -> int
  [@@mode m = (global, m)]

  (** Returns the sum of [f i] for i in the container *)
  val sum
    :  ((module Summable with type t = 'sum)[@mode mo])
    -> [> read ] t
    -> f:(elt -> 'sum)
    -> 'sum
  [@@mode mi = (global, m), mo = (global, m)]

  (** Returns as an [option] the first element for which [f] evaluates to true. *)
  val find : [> read ] t -> f:(elt -> bool) -> elt option
  [@@mode m = (global, m)]

  (** Returns the first evaluation of [f] that returns [Some], and returns [None] if there
      is no such element. *)
  val find_map : [> read ] t -> f:(elt -> 'b option) -> 'b option
  [@@mode mi = (global, m), mo = (global, m)]

  val to_list : [> read ] t -> elt list [@@alloc __ @ m = (heap_global, a @ m)]

  (*_ There's no local version of this because array elements must be global. *)
  val to_array : [> read ] t -> elt array

  (** Returns a min (resp max) element from the collection using the provided [compare]
      function. In case of a tie, the first element encountered while traversing the
      collection is returned. The implementation uses [fold] so it has the same complexity
      as [fold]. Returns [None] iff the collection is empty. *)
  val min_elt : [> read ] t -> compare:(elt -> elt -> int) -> elt option
  [@@mode m = (global, m)]

  val max_elt : [> read ] t -> compare:(elt -> elt -> int) -> elt option
  [@@mode m = (global, m)]
end
[@@alloc a @ m = (heap_global, stack_local)]

module type%template S1_permissions = sig
  type ('a, -'permissions) t

  (** Checks whether the provided element is there. *)
  val mem : ('a, [> read ]) t -> 'a -> equal:('a -> 'a -> bool) -> bool
  [@@mode m = (global, m)]

  val length : (_, [> read ]) t -> int
  val is_empty : (_, [> read ]) t -> bool

  (** [iter t ~f] calls [f] on each element of [t]. *)
  val iter : ('a, [> read ]) t -> f:('a -> unit) -> unit
  [@@mode m = (global, m)]

  (** [iter_until t ~f ~finish] is a short-circuiting version of [iter]. If [f] returns
      [Stop x] the computation ceases and returns [x]. If [f] always returns [Continue ()]
      the final result is computed by [finish]. *)
  val iter_until
    :  ('a, [> read ]) t
    -> f:('a -> (unit, 'final) Continue_or_stop.t)
    -> finish:(unit -> 'final)
    -> 'final
  [@@mode mi = (global, m), mo = (global, m)]

  (** [fold t ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where [e1..en]
      are the elements of [t] *)
  val fold : ('a, [> read ]) t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
  [@@mode mi = (global, m), mo = (global, m)]

  (** [fold_result t ~init ~f] is a short-circuiting version of [fold] that runs in the
      [Result] monad. If [f] returns an [Error _], that value is returned without any
      additional invocations of [f]. *)
  val fold_result
    :  ('a, [> read ]) t
    -> init:'acc
    -> f:('acc -> 'a -> ('acc, 'e) Result.t)
    -> ('acc, 'e) Result.t
  [@@mode mi = (global, m), mo = (global, m)]

  (** [fold_until t ~init ~f ~finish] is a short-circuiting version of [fold]. If [f]
      returns [Stop _] the computation ceases and results in that value. If [f] returns
      [Continue _], the fold will proceed. If [f] never returns [Stop _], the final result
      is computed by [finish]. *)
  val fold_until
    :  ('a, [> read ]) t
    -> init:'acc
    -> f:('acc -> 'a -> ('acc, 'final) Continue_or_stop.t)
    -> finish:('acc -> 'final)
    -> 'final
  [@@mode mi = (global, m), mo = (global, m)]

  (** Returns [true] if and only if there exists an element for which the provided
      function evaluates to [true]. This is a short-circuiting operation. *)
  val exists : ('a, [> read ]) t -> f:('a -> bool) -> bool
  [@@mode m = (global, m)]

  (** Returns [true] if and only if the provided function evaluates to [true] for all
      elements. This is a short-circuiting operation. *)
  val for_all : ('a, [> read ]) t -> f:('a -> bool) -> bool
  [@@mode m = (global, m)]

  (** Returns the number of elements for which the provided function evaluates to true. *)
  val count : ('a, [> read ]) t -> f:('a -> bool) -> int
  [@@mode m = (global, m)]

  (** Returns the sum of [f i] for i in the container *)
  val sum
    :  ((module Summable with type t = 'sum)[@mode mo])
    -> ('a, [> read ]) t
    -> f:('a -> 'sum)
    -> 'sum
  [@@mode mi = (global, m), mo = (global, m)]

  (** Returns as an [option] the first element for which [f] evaluates to true. *)
  val find : ('a, [> read ]) t -> f:('a -> bool) -> 'a option
  [@@mode m = (global, m)]

  (** Returns the first evaluation of [f] that returns [Some], and returns [None] if there
      is no such element. *)
  val find_map : ('a, [> read ]) t -> f:('a -> 'b option) -> 'b option
  [@@mode mi = (global, m), mo = (global, m)]

  val to_list : ('a, [> read ]) t -> 'a list [@@alloc __ @ m = (heap_global, a @ m)]

  (*_ There's no local version of this because array elements must be global. *)
  val to_array : ('a, [> read ]) t -> 'a array

  (** Returns a min (resp max) element from the collection using the provided [compare]
      function. In case of a tie, the first element encountered while traversing the
      collection is returned. The implementation uses [fold] so it has the same complexity
      as [fold]. Returns [None] iff the collection is empty. *)
  val min_elt : ('a, [> read ]) t -> compare:('a -> 'a -> int) -> 'a option
  [@@mode m = (global, m)]

  val max_elt : ('a, [> read ]) t -> compare:('a -> 'a -> int) -> 'a option
  [@@mode m = (global, m)]
end
[@@alloc a @ m = (heap_global, stack_local)]

module type%template S1_with_creators_permissions = sig
  include S1_permissions [@alloc a]

  val of_list : 'a list -> ('a, [< _ perms ]) t [@@alloc __ @ m = (heap_global, a @ m)]
  val of_array : 'a array -> ('a, [< _ perms ]) t [@@alloc __ @ m = (heap_global, a @ m)]

  val append : ('a, [> read ]) t -> ('a, [> read ]) t -> ('a, [< _ perms ]) t
  [@@alloc __ @ m = (heap_global, a @ m)]

  val concat : (('a, [> read ]) t, [> read ]) t -> ('a, [< _ perms ]) t
  [@@alloc __ @ m = (heap_global, a @ m)]

  val map : ('a, [> read ]) t -> f:('a -> 'b) -> ('b, [< _ perms ]) t
  [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

  val filter : ('a, [> read ]) t -> f:('a -> bool) -> ('a, [< _ perms ]) t
  [@@alloc __ @ m = (heap_global, a @ m)]

  val filter_map : ('a, [> read ]) t -> f:('a -> 'b option) -> ('b, [< _ perms ]) t
  [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

  val concat_map
    :  ('a, [> read ]) t
    -> f:('a -> ('b, [> read ]) t)
    -> ('b, [< _ perms ]) t
  [@@mode mi = (global, m)] [@@alloc a @ mo = (heap_global, a @ m)]

  val partition_tf
    :  ('a, [> read ]) t
    -> f:('a -> bool)
    -> ('a, [< _ perms ]) t * ('a, [< _ perms ]) t
  [@@alloc __ @ m = (heap_global, a @ m)]

  val partition_map
    :  ('a, [> read ]) t
    -> f:('a -> ('b, 'c) Either.t)
    -> ('b, [< _ perms ]) t * ('c, [< _ perms ]) t
  [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]
end
[@@alloc a @ m = (heap_global, stack_local)]

module type Container = sig
  (** @open *)
  include module type of struct
    include Base.Container
  end

  [%%template:
  [@@@alloc.default a = (heap, stack)]

  module type S0_permissions = S0_permissions [@alloc a]
  module type S1_permissions = S1_permissions [@alloc a]
  module type S1_with_creators_permissions = S1_with_creators_permissions [@alloc a]]
end
