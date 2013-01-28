(** Type for tracking errors in an Error.t. This is a specialization of the Result type,
    where the Error constructor carries an Error.t.

    A common idiom is to wrap a function that is not implemented on all platforms, e.g.:

    val do_something_linux_specific : (unit -> unit) Or_error.t
*)

open Sexplib

type 'a t = ('a, Error.t) Result.t

include Sexpable.S1 with type 'a t := 'a t
include Binable .S1 with type 'a t := 'a t
include Monad   .S  with type 'a t := 'a t

(** [try_with f] catches exceptions thrown by [f] and returns them in the Result.t as an
    Error.t.  [try_with_bind] is like [try_with], except that [f] can throw exceptions or
    return an Error directly, without ending up with a nested error; it is equivalent to
    [Result.join (try_with f)]. *)
val try_with      : ?backtrace:bool -> (unit -> 'a  ) -> 'a t
val try_with_bind : ?backtrace:bool -> (unit -> 'a t) -> 'a t

(** [ok_exn t] throws an exception if [t] is an [Error], and otherwise returns the
    contents of the [Ok] constructor. *)
val ok_exn : 'a t -> 'a

(** [of_exn exn] is [Error (Error.of_exn exn)]. *)
val of_exn : ?backtrace:[ `Get | `This of string ] -> exn -> 'a t

(** [error message value sexp_of_value] constructs an [Error.t] and returns it as a
    [Result.Error]. [error_string] is a sepcial case with ['a = unit]. *)
val error        : string -> 'a -> ('a -> Sexp.t) -> _ t
val error_string : string                         -> _ t

(** [unimplemented name] returns a standard error value for an unimplemented value. *)
val unimplemented : string -> _ t
