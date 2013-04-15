(** A pair of unix username and primary unix group. *)

open Std_internal

type t

(** The string/sexp converters follow the usual unix convention of '<user>:<group>'. *)
include Identifiable.S with type t := t

val create : user:string -> group:string -> t
val user : t -> string
val group : t -> string

(** Get the [t] for the current process.  If you're using async, there is a wrapper,
    [Async.Std.User_and_group], that doesn't do blocking calls. *)
val for_this_process     : unit -> t Or_error.t
val for_this_process_exn : unit -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t with sexp, bin_io, compare
    include Stringable with type t := t
  end
end
