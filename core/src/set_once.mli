(** A ['a Set_once.t] is like an ['a option ref] that can only be set once. A [Set_once.t]
    starts out as [None], the first [set] transitions it to [Some], and subsequent [set]s
    fail.

    Equality is determined only by the internal value and not the source code position of
    where the value was set. *)

open! Import

[%%template:
type 'a t
[@@deriving compare ~localize, equal ~localize, sexp_of]
[@@kind k = (float64, bits32, bits64, word)]

type 'a t [@@deriving compare ~localize, equal ~localize, quickcheck, sexp_of]

[@@@kind k = (float64, bits32, bits64, word, value)]

type 'a t := ('a t[@kind k])

[@@@kind.default k]

(** Passes when unset. *)
include Invariant.S1 [@kind k] with type 'a t := 'a t

val create : unit -> _ t
val set : 'a t -> ?here:Stdlib.Lexing.position -> 'a -> unit Or_error.t
val set_exn : 'a t -> ?here:Stdlib.Lexing.position -> 'a -> unit

(** [set_if_none t a] will do nothing if [is_some t], otherwise it will [set_exn t a]. *)
val set_if_none : 'a t -> ?here:Stdlib.Lexing.position -> 'a -> unit

val get : 'a t -> ('a option[@kind k])
val get_exn : ?here:Stdlib.Lexing.position -> 'a t -> 'a

(** Get the value. If it's not set, [f ()] will be called to initialize it. *)
val get_or_set_thunk : ?here:Stdlib.Lexing.position -> 'a t -> f:(unit -> 'a) -> 'a

val is_none : _ t -> bool
val is_some : _ t -> bool
val iter : 'a t -> f:('a -> unit) -> unit

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
