(** This module extends {!Base.Container}. *)

open! Import
open Perms.Export
open Base.Container
module Continue_or_stop = Continue_or_stop

module type S1_permissions = sig
  type ('a, -'permissions) t

  (** Checks whether the provided element is there, using polymorphic compare if [equal]
      is not provided. *)
  val mem : ('a, [> read ]) t -> 'a -> equal:('a -> 'a -> bool) -> bool

  val length : (_, [> read ]) t -> int
  val is_empty : (_, [> read ]) t -> bool

  (** [iter t ~f] calls [f] on each element of [t]. *)
  val iter : ('a, [> read ]) t -> f:('a -> unit) -> unit

  (** [fold t ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where [e1..en]
      are the elements of [t]  *)
  val fold : ('a, [> read ]) t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum

  (** [fold_result t ~init ~f] is a short-circuiting version of [fold] that runs in the
      [Result] monad.  If [f] returns an [Error _], that value is returned without any
      additional invocations of [f]. *)
  val fold_result
    :  ('a, [> read ]) t
    -> init:'accum
    -> f:('accum -> 'a -> ('accum, 'e) Result.t)
    -> ('accum, 'e) Result.t

  (** [fold_until t ~init ~f ~finish] is a short-circuiting version of [fold]. If [f]
      returns [Stop _] the computation ceases and results in that value. If [f] returns
      [Continue _], the fold will proceed. If [f] never returns [Stop _], the final result
      is computed by [finish]. *)
  val fold_until
    :  ('a, [> read ]) t
    -> init:'accum
    -> f:('accum -> 'a -> ('accum, 'final) Continue_or_stop.t)
    -> finish:('accum -> 'final)
    -> 'final

  (** Returns [true] if and only if there exists an element for which the provided
      function evaluates to [true].  This is a short-circuiting operation. *)
  val exists : ('a, [> read ]) t -> f:('a -> bool) -> bool

  (** Returns [true] if and only if the provided function evaluates to [true] for all
      elements.  This is a short-circuiting operation. *)
  val for_all : ('a, [> read ]) t -> f:('a -> bool) -> bool

  (** Returns the number of elements for which the provided function evaluates to true. *)
  val count : ('a, [> read ]) t -> f:('a -> bool) -> int

  (** Returns the sum of [f i] for i in the container *)
  val sum
    :  (module Summable with type t = 'sum)
    -> ('a, [> read ]) t
    -> f:('a -> 'sum)
    -> 'sum

  (** Returns as an [option] the first element for which [f] evaluates to true. *)
  val find : ('a, [> read ]) t -> f:('a -> bool) -> 'a option

  (** Returns the first evaluation of [f] that returns [Some], and returns [None] if there
      is no such element.  *)
  val find_map : ('a, [> read ]) t -> f:('a -> 'b option) -> 'b option

  val to_list : ('a, [> read ]) t -> 'a list
  val to_array : ('a, [> read ]) t -> 'a array

  (** Returns a min (resp max) element from the collection using the provided [compare]
      function. In case of a tie, the first element encountered while traversing the
      collection is returned. The implementation uses [fold] so it has the same complexity
      as [fold]. Returns [None] iff the collection is empty. *)
  val min_elt : ('a, [> read ]) t -> compare:('a -> 'a -> int) -> 'a option

  val max_elt : ('a, [> read ]) t -> compare:('a -> 'a -> int) -> 'a option
end

module type Container = sig
  (** @open *)
  include module type of struct
    include Base.Container
  end

  module type S1_permissions = S1_permissions
end
