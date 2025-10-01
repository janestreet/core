(** This module extends {!Base.Container}. *)

open! Import
open Perms.Export
open Base.Container
module Continue_or_stop = Continue_or_stop

module type%template S0_permissions = sig
  type elt
  type -'permissions t

  (** Checks whether the provided element is there. *)
  val mem : [> read ] t @ m -> elt @ m -> bool
  [@@mode m = (global, m)]

  val length : [> read ] t @ m -> int
  val is_empty : [> read ] t @ m -> bool

  (** [iter t ~f] calls [f] on each element of [t]. *)
  val iter : [> read ] t @ m -> f:(elt @ m -> unit) @ local -> unit
  [@@mode m = (global, m)]

  (** [iter_until t ~f ~finish] is a short-circuiting version of [iter]. If [f] returns
      [Stop x] the computation ceases and returns [x]. If [f] always returns [Continue ()]
      the final result is computed by [finish]. *)
  val iter_until
    :  [> read ] t @ mi
    -> f:(elt @ mi -> (unit, 'final) Continue_or_stop.t @ mo) @ local
    -> finish:(unit -> 'final @ mo) @ local
    -> 'final @ mo
  [@@mode mi = (global, m), mo = (global, m)]

  (** [fold t ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where [e1..en]
      are the elements of [t] *)
  val fold
    :  [> read ] t @ mi
    -> init:'acc @ mo
    -> f:('acc @ mo -> elt @ mi -> 'acc @ mo) @ local
    -> 'acc @ mo
  [@@mode mi = (global, m), mo = (global, m)]

  (** [fold_result t ~init ~f] is a short-circuiting version of [fold] that runs in the
      [Result] monad. If [f] returns an [Error _], that value is returned without any
      additional invocations of [f]. *)
  val fold_result
    :  [> read ] t @ mi
    -> init:'acc @ mo
    -> f:('acc @ mo -> elt @ mi -> ('acc, 'e) Result.t @ mo) @ local
    -> ('acc, 'e) Result.t @ mo
  [@@mode mi = (global, m), mo = (global, m)]

  (** [fold_until t ~init ~f ~finish] is a short-circuiting version of [fold]. If [f]
      returns [Stop _] the computation ceases and results in that value. If [f] returns
      [Continue _], the fold will proceed. If [f] never returns [Stop _], the final result
      is computed by [finish]. *)
  val fold_until
    :  [> read ] t @ mi
    -> init:'acc @ mo
    -> f:('acc @ mo -> elt @ mi -> ('acc, 'final) Continue_or_stop.t @ mo) @ local
    -> finish:('acc @ mo -> 'final @ mo) @ local
    -> 'final @ mo
  [@@mode mi = (global, m), mo = (global, m)]

  (** Returns [true] if and only if there exists an element for which the provided
      function evaluates to [true]. This is a short-circuiting operation. *)
  val exists : [> read ] t @ m -> f:(elt @ m -> bool) @ local -> bool
  [@@mode m = (global, m)]

  (** Returns [true] if and only if the provided function evaluates to [true] for all
      elements. This is a short-circuiting operation. *)
  val for_all : [> read ] t @ m -> f:(elt @ m -> bool) @ local -> bool
  [@@mode m = (global, m)]

  (** Returns the number of elements for which the provided function evaluates to true. *)
  val count : [> read ] t @ m -> f:(elt @ m -> bool) @ local -> int
  [@@mode m = (global, m)]

  (** Returns the sum of [f i] for i in the container *)
  val sum
    :  ((module Summable with type t = 'sum)[@mode mo])
    -> [> read ] t @ mi
    -> f:(elt @ mi -> 'sum @ mo) @ local
    -> 'sum @ mo
  [@@mode mi = (global, m), mo = (global, m)]

  (** Returns as an [option] the first element for which [f] evaluates to true. *)
  val find : [> read ] t @ m -> f:(elt @ m -> bool) @ local -> elt option @ m
  [@@mode m = (global, m)]

  (** Returns the first evaluation of [f] that returns [Some], and returns [None] if there
      is no such element. *)
  val find_map
    :  [> read ] t @ mi
    -> f:(elt @ mi -> 'b option @ mo) @ local
    -> 'b option @ mo
  [@@mode mi = (global, m), mo = (global, m)]

  val to_list : [> read ] t @ m -> elt list @ m [@@alloc __ @ m = (heap_global, a @ m)]

  (*_ There's no local version of this because array elements must be global. *)
  val to_array : [> read ] t -> elt array

  (** Returns a min (resp max) element from the collection using the provided [compare]
      function. In case of a tie, the first element encountered while traversing the
      collection is returned. The implementation uses [fold] so it has the same complexity
      as [fold]. Returns [None] iff the collection is empty. *)
  val min_elt
    :  [> read ] t @ m
    -> compare:(elt @ m -> elt @ m -> int) @ local
    -> elt option @ m
  [@@mode m = (global, m)]

  val max_elt
    :  [> read ] t @ m
    -> compare:(elt @ m -> elt @ m -> int) @ local
    -> elt option @ m
  [@@mode m = (global, m)]
end
[@@alloc a @ m = (heap_global, stack_local)]

module type%template S1_permissions = sig
  type ('a, -'permissions) t

  (** Checks whether the provided element is there. *)
  val mem
    :  ('a, [> read ]) t @ m
    -> 'a @ m
    -> equal:('a @ m -> 'a @ m -> bool) @ local
    -> bool
  [@@mode m = (global, m)]

  val length : (_, [> read ]) t @ m -> int
  val is_empty : (_, [> read ]) t @ m -> bool

  (** [iter t ~f] calls [f] on each element of [t]. *)
  val iter : ('a, [> read ]) t @ m -> f:('a @ m -> unit) @ local -> unit
  [@@mode m = (global, m)]

  (** [iter_until t ~f ~finish] is a short-circuiting version of [iter]. If [f] returns
      [Stop x] the computation ceases and returns [x]. If [f] always returns [Continue ()]
      the final result is computed by [finish]. *)
  val iter_until
    :  ('a, [> read ]) t @ mi
    -> f:('a @ mi -> (unit, 'final) Continue_or_stop.t @ mo) @ local
    -> finish:(unit -> 'final @ mo) @ local
    -> 'final @ mo
  [@@mode mi = (global, m), mo = (global, m)]

  (** [fold t ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where [e1..en]
      are the elements of [t] *)
  val fold
    :  ('a, [> read ]) t @ mi
    -> init:'acc @ mo
    -> f:('acc @ mo -> 'a @ mi -> 'acc @ mo) @ local
    -> 'acc @ mo
  [@@mode mi = (global, m), mo = (global, m)]

  (** [fold_result t ~init ~f] is a short-circuiting version of [fold] that runs in the
      [Result] monad. If [f] returns an [Error _], that value is returned without any
      additional invocations of [f]. *)
  val fold_result
    :  ('a, [> read ]) t @ mi
    -> init:'acc @ mo
    -> f:('acc @ mo -> 'a @ mi -> ('acc, 'e) Result.t @ mo) @ local
    -> ('acc, 'e) Result.t @ mo
  [@@mode mi = (global, m), mo = (global, m)]

  (** [fold_until t ~init ~f ~finish] is a short-circuiting version of [fold]. If [f]
      returns [Stop _] the computation ceases and results in that value. If [f] returns
      [Continue _], the fold will proceed. If [f] never returns [Stop _], the final result
      is computed by [finish]. *)
  val fold_until
    :  ('a, [> read ]) t @ mi
    -> init:'acc @ mo
    -> f:('acc @ mo -> 'a @ mi -> ('acc, 'final) Continue_or_stop.t @ mo) @ local
    -> finish:('acc @ mo -> 'final @ mo) @ local
    -> 'final @ mo
  [@@mode mi = (global, m), mo = (global, m)]

  (** Returns [true] if and only if there exists an element for which the provided
      function evaluates to [true]. This is a short-circuiting operation. *)
  val exists : ('a, [> read ]) t @ m -> f:('a @ m -> bool) @ local -> bool
  [@@mode m = (global, m)]

  (** Returns [true] if and only if the provided function evaluates to [true] for all
      elements. This is a short-circuiting operation. *)
  val for_all : ('a, [> read ]) t @ m -> f:('a @ m -> bool) @ local -> bool
  [@@mode m = (global, m)]

  (** Returns the number of elements for which the provided function evaluates to true. *)
  val count : ('a, [> read ]) t @ m -> f:('a @ m -> bool) @ local -> int
  [@@mode m = (global, m)]

  (** Returns the sum of [f i] for i in the container *)
  val sum
    :  ((module Summable with type t = 'sum)[@mode mo])
    -> ('a, [> read ]) t @ mi
    -> f:('a @ mi -> 'sum @ mo) @ local
    -> 'sum @ mo
  [@@mode mi = (global, m), mo = (global, m)]

  (** Returns as an [option] the first element for which [f] evaluates to true. *)
  val find : ('a, [> read ]) t @ m -> f:('a @ m -> bool) @ local -> 'a option @ m
  [@@mode m = (global, m)]

  (** Returns the first evaluation of [f] that returns [Some], and returns [None] if there
      is no such element. *)
  val find_map
    :  ('a, [> read ]) t @ mi
    -> f:('a @ mi -> 'b option @ mo) @ local
    -> 'b option @ mo
  [@@mode mi = (global, m), mo = (global, m)]

  val to_list : ('a, [> read ]) t @ m -> 'a list @ m
  [@@alloc __ @ m = (heap_global, a @ m)]

  (*_ There's no local version of this because array elements must be global. *)
  val to_array : ('a, [> read ]) t -> 'a array

  (** Returns a min (resp max) element from the collection using the provided [compare]
      function. In case of a tie, the first element encountered while traversing the
      collection is returned. The implementation uses [fold] so it has the same complexity
      as [fold]. Returns [None] iff the collection is empty. *)
  val min_elt
    :  ('a, [> read ]) t @ m
    -> compare:('a @ m -> 'a @ m -> int) @ local
    -> 'a option @ m
  [@@mode m = (global, m)]

  val max_elt
    :  ('a, [> read ]) t @ m
    -> compare:('a @ m -> 'a @ m -> int) @ local
    -> 'a option @ m
  [@@mode m = (global, m)]
end
[@@alloc a @ m = (heap_global, stack_local)]

module type%template S1_with_creators_permissions = sig
  include S1_permissions [@alloc a]

  val of_list : 'a list @ m -> ('a, [< _ perms ]) t @ m
  [@@alloc __ @ m = (heap_global, a @ m)]

  val of_array : 'a array @ m -> ('a, [< _ perms ]) t @ m
  [@@alloc __ @ m = (heap_global, a @ m)]

  val append : ('a, [> read ]) t @ m -> ('a, [> read ]) t @ m -> ('a, [< _ perms ]) t @ m
  [@@alloc __ @ m = (heap_global, a @ m)]

  val concat : (('a, [> read ]) t, [> read ]) t @ m -> ('a, [< _ perms ]) t @ m
  [@@alloc __ @ m = (heap_global, a @ m)]

  val map
    :  ('a, [> read ]) t @ mi
    -> f:('a @ mi -> 'b @ mo) @ local
    -> ('b, [< _ perms ]) t @ mo
  [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

  val filter
    :  ('a, [> read ]) t @ m
    -> f:('a @ m -> bool) @ local
    -> ('a, [< _ perms ]) t @ m
  [@@alloc __ @ m = (heap_global, a @ m)]

  val filter_map
    :  ('a, [> read ]) t @ mi
    -> f:('a @ mi -> 'b option @ mo) @ local
    -> ('b, [< _ perms ]) t @ mo
  [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

  val concat_map
    :  ('a, [> read ]) t @ mi
    -> f:('a @ mi -> ('b, [> read ]) t @ mo) @ local
    -> ('b, [< _ perms ]) t @ mo
  [@@mode mi = (global, m)] [@@alloc a @ mo = (heap_global, a @ m)]

  val partition_tf
    :  ('a, [> read ]) t @ m
    -> f:('a @ m -> bool) @ local
    -> ('a, [< _ perms ]) t * ('a, [< _ perms ]) t @ m
  [@@alloc __ @ m = (heap_global, a @ m)]

  val partition_map
    :  ('a, [> read ]) t @ mi
    -> f:('a @ mi -> ('b, 'c) Either.t @ mo) @ local
    -> ('b, [< _ perms ]) t * ('c, [< _ perms ]) t @ mo
  [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]
end
[@@alloc a @ m = (heap_global, stack_local)]

module type Container = sig @@ portable
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
