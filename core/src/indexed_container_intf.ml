(** This module extends {!Base.Indexed_container}. *)

open! Import
open Perms.Export

module type%template S1_permissions = sig
  include Container.S1_permissions [@alloc a]

  val foldi : ('a, [> read ]) t -> init:'acc -> f:(int -> 'acc -> 'a -> 'acc) -> 'acc
  [@@mode mi = (global, m), mo = (global, m)]

  val foldi_until
    :  ('a, [> read ]) t
    -> init:'acc
    -> f:(int -> 'acc -> 'a -> ('acc, 'final) Continue_or_stop.t)
    -> finish:(int -> 'acc -> 'final)
    -> 'final
  [@@mode mi = (global, m), mo = (global, m)]

  val iteri : ('a, [> read ]) t -> f:(int -> 'a -> unit) -> unit [@@mode m = (global, m)]

  val iteri_until
    :  ('a, [> read ]) t
    -> f:(int -> 'a -> (unit, 'final) Continue_or_stop.t)
    -> finish:(int -> 'final)
    -> 'final
  [@@mode mi = (global, m), mo = (global, m)]

  val existsi : ('a, [> read ]) t -> f:(int -> 'a -> bool) -> bool
  [@@mode m = (global, m)]

  val for_alli : ('a, [> read ]) t -> f:(int -> 'a -> bool) -> bool
  [@@mode m = (global, m)]

  val counti : ('a, [> read ]) t -> f:(int -> 'a -> bool) -> int [@@mode m = (global, m)]

  val findi : ('a, [> read ]) t -> f:(int -> 'a -> bool) -> (int * 'a) option
  [@@mode m = (global, m)]

  val find_mapi : ('a, [> read ]) t -> f:(int -> 'a -> 'b option) -> 'b option
  [@@mode mi = (global, m), mo = (global, m)]
end
[@@alloc a @ m = (heap_global, stack_local)]

module type%template S1_with_creators_permissions = sig
  include Container.S1_with_creators_permissions [@alloc a]
  include S1_permissions [@alloc a] with type ('a, 'perms) t := ('a, 'perms) t

  val init : int -> f:(int -> 'a) -> ('a, [< _ perms ]) t
  [@@alloc __ @ m = (heap_global, a @ m)]

  val mapi : ('a, [> read ]) t -> f:(int -> 'a -> 'b) -> ('b, [< _ perms ]) t
  [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

  val filteri : ('a, [> read ]) t -> f:(int -> 'a -> bool) -> ('a, [< _ perms ]) t
  [@@alloc __ @ m = (heap_global, a @ m)]

  val filter_mapi
    :  ('a, [> read ]) t
    -> f:(int -> 'a -> 'b option)
    -> ('b, [< _ perms ]) t
  [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

  val concat_mapi
    :  ('a, [> read ]) t
    -> f:(int -> 'a -> ('b, [> read ]) t)
    -> ('b, [< _ perms ]) t
  [@@mode mi = (global, m)] [@@alloc a @ mo = (heap_global, a @ m)]

  val partitioni_tf
    :  ('a, [> read ]) t
    -> f:(int -> 'a -> bool)
    -> ('a, [< _ perms ]) t * ('a, [< _ perms ]) t
  [@@alloc __ @ m = (heap_global, a @ m)]

  val partition_mapi
    :  ('a, [> read ]) t
    -> f:(int -> 'a -> ('b, 'c) Either.t)
    -> ('b, [< _ perms ]) t * ('c, [< _ perms ]) t
  [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]
end
[@@alloc a @ m = (heap_global, stack_local)]

module type Indexed_container = sig
  (** @open *)
  include module type of struct
    include Base.Indexed_container
  end

  [%%template:
  [@@@alloc.default a = (heap, stack)]

  module type S1_permissions = S1_permissions [@alloc a]
  module type S1_with_creators_permissions = S1_with_creators_permissions [@alloc a]]
end
