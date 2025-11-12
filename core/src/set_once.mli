@@ portable

(** A ['a Set_once.t] is like an ['a option ref] that can only be set once. A [Set_once.t]
    starts out as [None], the first [set] transitions it to [Some], and subsequent [set]s
    fail.

    Equality is determined only by the internal value and not the source code position of
    where the value was set. *)

open! Import

[%%template:
type ('a : k) t
[@@deriving compare ~localize, equal ~localize, sexp_of] [@@kind k = base_non_value]

type 'a t [@@deriving compare ~localize, equal ~localize, quickcheck, sexp_of]

[@@@kind k = base]

type ('a : k) t := ('a t[@kind k])

[@@@kind.default k]

(** Passes when unset. *)
include Invariant.S1 [@kind k] with type 'a t := 'a t

val create : unit -> _ t
val create_full : here:[%call_pos] -> 'a -> 'a t
val set : 'a t -> here:[%call_pos] -> 'a -> unit Or_error.t
val set_exn : 'a t -> here:[%call_pos] -> 'a -> unit

(** [set_if_none t a] will do nothing if [is_some t], otherwise it will [set_exn t a]. *)
val set_if_none : 'a t -> here:[%call_pos] -> 'a -> unit

val get : 'a t -> ('a option[@kind k])
val get_exn : here:[%call_pos] -> 'a t -> 'a

(** Get the value. If it's not set, [f ()] will be called to initialize it. *)
val get_or_set_thunk : here:[%call_pos] -> 'a t -> f:local_ (unit -> 'a) -> 'a

val is_none : _ t -> bool
val is_some : _ t -> bool
val iter : 'a t -> f:local_ ('a -> unit) -> unit

module Optional_syntax :
  Optional_syntax.S1 [@kind k] with type 'a t := 'a t with type 'a value := 'a identity]

module Unstable : sig
  type nonrec 'a t = 'a t
  [@@deriving bin_io, compare ~localize, equal ~localize, sexp, sexp_grammar]
end

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t
    [@@deriving bin_io, compare ~localize, equal ~localize, sexp, sexp_grammar]
  end
end
