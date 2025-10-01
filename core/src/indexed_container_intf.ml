(** This module extends {!Base.Indexed_container}. *)

open! Import
open Perms.Export

module type%template S1_permissions = sig
  include Container.S1_permissions [@alloc a]

  val foldi
    :  ('a, [> read ]) t @ mi
    -> init:'acc @ mo
    -> f:(int -> 'acc @ mo -> 'a @ mi -> 'acc @ mo) @ local
    -> 'acc @ mo
  [@@mode mi = (global, m), mo = (global, m)]

  val foldi_until
    :  ('a, [> read ]) t @ mi
    -> init:'acc @ mo
    -> f:(int -> 'acc @ mo -> 'a @ mi -> ('acc, 'final) Continue_or_stop.t @ mo) @ local
    -> finish:(int -> 'acc @ mo -> 'final @ mo) @ local
    -> 'final @ mo
  [@@mode mi = (global, m), mo = (global, m)]

  val iteri : ('a, [> read ]) t @ m -> f:(int -> 'a @ m -> unit) @ local -> unit
  [@@mode m = (global, m)]

  val iteri_until
    :  ('a, [> read ]) t @ mi
    -> f:(int -> 'a @ mi -> (unit, 'final) Continue_or_stop.t @ mo) @ local
    -> finish:(int -> 'final @ mo) @ local
    -> 'final @ mo
  [@@mode mi = (global, m), mo = (global, m)]

  val existsi : ('a, [> read ]) t @ m -> f:(int -> 'a @ m -> bool) @ local -> bool
  [@@mode m = (global, m)]

  val for_alli : ('a, [> read ]) t @ m -> f:(int -> 'a @ m -> bool) @ local -> bool
  [@@mode m = (global, m)]

  val counti : ('a, [> read ]) t @ m -> f:(int -> 'a @ m -> bool) @ local -> int
  [@@mode m = (global, m)]

  val findi
    :  ('a, [> read ]) t @ m
    -> f:(int -> 'a @ m -> bool) @ local
    -> (int * 'a) option @ m
  [@@mode m = (global, m)]

  val find_mapi
    :  ('a, [> read ]) t @ mi
    -> f:(int -> 'a @ mi -> 'b option @ mo) @ local
    -> 'b option @ mo
  [@@mode mi = (global, m), mo = (global, m)]
end
[@@alloc a @ m = (heap_global, stack_local)]

module type%template S1_with_creators_permissions = sig
  include Container.S1_with_creators_permissions [@alloc a]
  include S1_permissions [@alloc a] with type ('a, 'perms) t := ('a, 'perms) t

  val init : int -> f:(int -> 'a @ m) @ local -> ('a, [< _ perms ]) t @ m
  [@@alloc __ @ m = (heap_global, a @ m)]

  val mapi
    :  ('a, [> read ]) t @ mi
    -> f:(int -> 'a @ mi -> 'b @ mo) @ local
    -> ('b, [< _ perms ]) t @ mo
  [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

  val filteri
    :  ('a, [> read ]) t @ m
    -> f:(int -> 'a @ m -> bool) @ local
    -> ('a, [< _ perms ]) t @ m
  [@@alloc __ @ m = (heap_global, a @ m)]

  val filter_mapi
    :  ('a, [> read ]) t @ mi
    -> f:(int -> 'a @ mi -> 'b option @ mo) @ local
    -> ('b, [< _ perms ]) t @ mo
  [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

  val concat_mapi
    :  ('a, [> read ]) t @ mi
    -> f:(int -> 'a @ mi -> ('b, [> read ]) t @ mo) @ local
    -> ('b, [< _ perms ]) t @ mo
  [@@mode mi = (global, m)] [@@alloc a @ mo = (heap_global, a @ m)]

  val partitioni_tf
    :  ('a, [> read ]) t @ m
    -> f:(int -> 'a @ m -> bool) @ local
    -> ('a, [< _ perms ]) t * ('a, [< _ perms ]) t @ m
  [@@alloc __ @ m = (heap_global, a @ m)]

  val partition_mapi
    :  ('a, [> read ]) t @ mi
    -> f:(int -> 'a @ mi -> ('b, 'c) Either.t @ mo) @ local
    -> ('b, [< _ perms ]) t * ('c, [< _ perms ]) t @ mo
  [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]
end
[@@alloc a @ m = (heap_global, stack_local)]

module type Indexed_container = sig @@ portable
  (** @open *)
  include module type of struct
    include Base.Indexed_container
  end

  [%%template:
  [@@@alloc.default a = (heap, stack)]

  module type S1_permissions = S1_permissions [@alloc a]
  module type S1_with_creators_permissions = S1_with_creators_permissions [@alloc a]]
end
