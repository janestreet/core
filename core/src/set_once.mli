(** A ['a Set_once.t] is like an ['a option ref] that can only be set once.  A
    [Set_once.t] starts out as [None], the first [set] transitions it to [Some], and
    subsequent [set]s fail. *)

open! Import

type 'a t [@@deriving sexp_of]

(** Passes when unset. *)
include Invariant.S1 with type 'a t := 'a t

val create : unit -> _ t
val set : 'a t -> Source_code_position.t -> 'a -> unit Or_error.t
val set_exn : 'a t -> Source_code_position.t -> 'a -> unit

(** [set_if_none t here a] will do nothing if [is_some t], otherwise it will [set_exn t
    here a]. *)
val set_if_none : 'a t -> Source_code_position.t -> 'a -> unit

val get : 'a t -> 'a option
val get_exn : 'a t -> Source_code_position.t -> 'a
val is_none : _ t -> bool
val is_some : _ t -> bool
val iter : 'a t -> f:('a -> unit) -> unit

module Optional_syntax :
  Optional_syntax.S1 with type 'a t := 'a t with type 'a value := 'a identity

module Unstable : sig
  type nonrec 'a t = 'a t [@@deriving bin_io, sexp]
end

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving bin_io, sexp]
  end
end
