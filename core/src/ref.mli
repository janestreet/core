(** This module extends {{!Base.Ref}[Base.Ref]}. *)

open! Import
open Perms.Export

type 'a t = 'a Base.Ref.t = { mutable contents : 'a }
[@@deriving bin_io ~localize, quickcheck, typerep]

(** @inline *)
include module type of struct
    include Base.Ref
  end
  with type 'a t := 'a t

module Permissioned : sig
  type (!'a, -'perms) t [@@deriving sexp, bin_io ~localize]

  val create : 'a -> ('a, [< _ perms ]) t
  val read_only : ('a, [> read ]) t -> ('a, read) t

  (** [get] and [(!)] are two names for the same function. *)
  val ( ! ) : ('a, [> read ]) t -> 'a

  val get : ('a, [> read ]) t -> 'a

  (** [set] and [(:=)] are two names for the same function. *)
  val set : ('a, [> write ]) t -> 'a -> unit

  val ( := ) : ('a, [> write ]) t -> 'a -> unit
  val of_ref : 'a ref -> ('a, [< read_write ]) t
  val to_ref : ('a, [> read_write ]) t -> 'a ref
  val swap : ('a, [> read_write ]) t -> ('a, [> read_write ]) t -> unit
  val replace : ('a, [> read_write ]) t -> ('a -> 'a) -> unit
  val set_temporarily : ('a, [> read_write ]) t -> 'a -> f:(unit -> 'b) -> 'b
end
