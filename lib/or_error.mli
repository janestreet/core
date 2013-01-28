(** Type for tracking errors in an Error.t. This is a specialization of the Result type,
    where the Error constructor carries an Error.t.

    A common idiom is to wrap a function that is not implemented on all platforms, e.g.:

    val do_something_linux_specific : (unit -> unit) Or_error.t
*)

open Sexplib

type 'a t = ('a, Error.t) Result.t with bin_io, sexp

include Monad.S  with type 'a t := 'a t

(** [try_with f] catches exceptions thrown by [f] and returns them in the Result.t as an
    Error.t.  [try_with_join] is like [try_with], except that [f] can throw exceptions or
    return an Error directly, without ending up with a nested error; it is equivalent to
    [Result.join (try_with f)]. *)
val try_with      : ?backtrace:bool (* defaults to false *) -> (unit -> 'a  ) -> 'a t
val try_with_join : ?backtrace:bool (* defaults to false *) -> (unit -> 'a t) -> 'a t

(** [ok_exn t] throws an exception if [t] is an [Error], and otherwise returns the
    contents of the [Ok] constructor. *)
val ok_exn : 'a t -> 'a

(** [of_exn exn] is [Error (Error.of_exn exn)]. *)
val of_exn : ?backtrace:[ `Get | `This of string ] -> exn -> _ t

(** [of_exn_result (Ok a) = Ok a], [of_exn_result (Error exn) = of_exn exn] *)
val of_exn_result : ('a, exn) Result.t -> 'a t

(** [error message value sexp_of_value] constructs an [Error.t] and returns it as a
    [Result.Error]. *)
val error        : string -> 'a -> ('a -> Sexp.t) -> _ t

(** [error_string message] is [Error (Error.of_string message)] *)
val error_string : string -> _ t

(** [unimplemented name] returns a standard error value for an unimplemented value. *)
val unimplemented : string -> _ t

(** [combine_errors ts] returns [Ok] if every element in [ts] is [Ok], else it returns
    [Error] with all the errors in [ts].  More precisely:

    | combine_errors [Ok a1; ...; Ok an] = Ok [a1; ...; an]
    | combine_errors [...; Error e1; ...; Error en; ...]
    |   = Error (Error.of_list [e1; ...; en]) *)
val combine_errors : 'a t list -> 'a list t

(** [combine_errors_unit] returns [Ok] if every element in [ts] is [Ok ()], else it
    returns [Error] with all the errors in [ts], like [combine_errors]. *)
val combine_errors_unit : unit t list -> unit t
