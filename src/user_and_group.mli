(** A pair of Unix username and primary Unix group. *)

open! Import

type t

(** The string/sexp converters follow the usual Unix convention of '<user>:<group>'. *)
include Identifiable.S with type t := t

val create : user:string -> group:string -> t
val user : t -> string
val group : t -> string

(** Get the [t] for the current process.  If you're using Async, there is a wrapper,
    [Async.User_and_group], that doesn't do blocking calls. *)
val for_this_process     : unit -> t Or_error.t
val for_this_process_exn : unit -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare]
    include Stringable with type t := t
  end
end
