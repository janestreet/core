(** A ['a Set_once.t] is like an ['a option ref] that can only be set once. A [Set_once.t]
    starts out as [None], the first [set] transitions it to [Some], and subsequent [set]s
    fail.

    Equality is determined only by the internal value and not the source code position of
    where the value was set. *)

open! Import

type 'a t [@@deriving compare, equal, sexp_of]

(*_ Non-value [t]s don't support quickcheck *)
[%%rederive: type nonrec 'a t = 'a t [@@deriving quickcheck]]

(*_ Unmangled [t] is shadowed below, so define [t] with explicit mangling. *)
type 'a t := 'a t
type 'a t := 'a t

(** Passes when unset. *)
include Invariant.S1 with type 'a t := 'a t

val create : unit -> _ t
val create_full : ?here:Stdlib.Lexing.position -> 'a -> 'a t
val set : 'a t -> ?here:Stdlib.Lexing.position -> 'a -> unit Or_error.t
val set_exn : 'a t -> ?here:Stdlib.Lexing.position -> 'a -> unit

(** [set_if_none t a] will do nothing if [is_some t], otherwise it will [set_exn t a]. *)
val set_if_none : 'a t -> ?here:Stdlib.Lexing.position -> 'a -> unit

val get : 'a t -> 'a option [@@zero_alloc]
val get_exn : ?here:Stdlib.Lexing.position -> 'a t -> 'a [@@zero_alloc]

(** Get the value. If it's not set, [f ()] will be called to initialize it. *)
val get_or_set_thunk : ?here:Stdlib.Lexing.position -> 'a t -> f:(unit -> 'a) -> 'a

val is_none : _ t -> bool
val is_some : _ t -> bool
val iter : 'a t -> f:('a -> unit) -> unit

module Optional_syntax :
  Optional_syntax.S1_zero_alloc with type 'a t := 'a t with type 'a value := 'a identity

module Unstable : sig
  type nonrec 'a t = 'a t [@@deriving bin_io, compare, equal, sexp, sexp_grammar]
end

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving bin_io, compare, equal, sexp, sexp_grammar]
  end
end

module Atomic : sig
  (** A ['a Set_once.Atomic.t] is like ['a Set_once.t], except it's backed by an
      [Atomic.t] instead of a [ref]. This means it can be safely mutated and shared
      between threads. *)

  type 'a t

  [%%rederive: type nonrec 'a t = 'a t [@@deriving sexp_of]]

  (** Passes when unset. *)
  val invariant : 'a. ('a -> unit) -> 'a t -> unit

  val create : unit -> _ t
  val create_full : ?here:Stdlib.Lexing.position -> 'a -> 'a t
  val set : 'a t -> ?here:Stdlib.Lexing.position -> 'a -> unit Or_error.t
  val set_exn : 'a t -> ?here:Stdlib.Lexing.position -> 'a -> unit

  (** [set_if_none t a] will do nothing if [is_some t], otherwise it will [set_exn t a]. *)
  val set_if_none : 'a t -> ?here:Stdlib.Lexing.position -> 'a -> unit

  val get : 'a t -> 'a option [@@zero_alloc]
  val get_exn : ?here:Stdlib.Lexing.position -> 'a t -> 'a [@@zero_alloc]
  val is_none : _ t -> bool
  val is_some : _ t -> bool
  val iter : 'a t -> f:('a -> unit) -> unit
end
