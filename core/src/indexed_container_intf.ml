(** This module extends {!Base.Indexed_container}. *)

open! Import
open Perms.Export

module type S1_permissions = sig
  include Container.S1_permissions

  val foldi
    :  ('a, [> read ]) t
    -> init:'acc
    -> f:((int -> 'acc -> 'a -> 'acc)[@local])
    -> 'acc

  val iteri : ('a, [> read ]) t -> f:((int -> 'a -> unit)[@local]) -> unit
  val existsi : ('a, [> read ]) t -> f:((int -> 'a -> bool)[@local]) -> bool
  val for_alli : ('a, [> read ]) t -> f:((int -> 'a -> bool)[@local]) -> bool
  val counti : ('a, [> read ]) t -> f:((int -> 'a -> bool)[@local]) -> int
  val findi : ('a, [> read ]) t -> f:((int -> 'a -> bool)[@local]) -> (int * 'a) option
  val find_mapi : ('a, [> read ]) t -> f:((int -> 'a -> 'b option)[@local]) -> 'b option
end

module type S1_with_creators_permissions = sig
  include Container.S1_with_creators_permissions
  include S1_permissions with type ('a, 'perms) t := ('a, 'perms) t

  val init : int -> f:((int -> 'a)[@local]) -> ('a, [< _ perms ]) t
  val mapi : ('a, [> read ]) t -> f:((int -> 'a -> 'b)[@local]) -> ('b, [< _ perms ]) t

  val filteri
    :  ('a, [> read ]) t
    -> f:((int -> 'a -> bool)[@local])
    -> ('a, [< _ perms ]) t

  val filter_mapi
    :  ('a, [> read ]) t
    -> f:((int -> 'a -> 'b option)[@local])
    -> ('b, [< _ perms ]) t

  val concat_mapi
    :  ('a, [> read ]) t
    -> f:((int -> 'a -> ('b, [> read ]) t)[@local])
    -> ('b, [< _ perms ]) t
end

module type Indexed_container = sig
  (** @open *)
  include module type of struct
    include Base.Indexed_container
  end

  module type S1_permissions = S1_permissions
  module type S1_with_creators_permissions = S1_with_creators_permissions
end
