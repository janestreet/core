(** Library for lazily constructing string error messages.  This is better than using
    strings for error messages because you don't have to eagerly construct the string at
    error reporting time --- you only need to pay when you actually serialize it, which
    for many applications is rare.  It's better than reporting errors by returning an
    exception because you have more control of the format of the error.

    Error messages are intended to be constructed in the following style; for simple
    errors, you write:

    {[Error.of_string "Unable to find file"]}

    For errors where you want to attach some content, you would write:

    {[Error.create "Unable to find file" filename <:sexp_of<string>>]}

    Or even,

    {[
    Error.create "price too big" (price, [`Max max_price])
    (<:sexp_of<float * [`Max of float]>>)
    ]}
*)
open Sexplib

type t
include Sexpable.S with type t := t
include Binable.S  with type t := t

(** [to_sexp_hum t] returns [t] formatted as sexp that aims to be readable.  This loses
    information about the structure of the error, so if the result is converted back using
    [t_of_sexp], it will not be able to pretty print as nicely.  That is, the following
    equalities do not necessarily hold:

    to_sexp_hum   (t_of_sexp (to_sexp_hum t)) = to_sexp_hum t
    to_string_hum (t_of_sexp (to_sexp_hum t)) = to_string_hum t *)
val to_sexp_hum : t -> Sexp.t

(** might be an expensive operation *)
val to_string_hum : t -> string

val of_string : string -> t

(** Be careful that the body of the lazy or thunk does not access mutable data, since it
    will only be called at an undetermined later point. *)
val of_lazy  : string Lazy.t    -> t
val of_thunk : (unit -> string) -> t

(** For [create msg z sexp_of_z], be careful to use only immutable values for z, or be
    aware that [z] will be lazily converted to a sexp at a later point in time, which will
    pick up the then-current state of [z]. *)
val create : string -> 'a -> ('a -> Sexp.t) -> t

(** Functions for transforming errors *)

(* Add a string to the front of an error *)
val tag : t -> string -> t

(* Add a string and some other data in the form of an s-expression in front of an error *)
val tag_arg : t -> string -> 'a -> ('a -> Sexp.t) -> t

(* Combine multiple errors into one *)
val of_list : ?trunc_after:int -> t list -> t

val of_exn : ?backtrace:[ `Get | `This of string ] -> exn -> t
val to_exn : t -> exn

(* Note that the exception holds onto the [t]. *)
val raise : t -> _

(** [failwiths message value sexp_of_value] raises an exception with the supplied
    [message] and [value], by constructing an [Error.t] and using [Error.raise].  As
    usual, the [sexp_of_value] is only applied when the value is converted to a sexp or a
    string.  So, if you mutate [value] in between the time you call [failwiths] and the
    time the error is displayed, those mutations will be reflected in the error message.

    [failwiths s a f] = [Error.raise (Error.create s a f)] *)
val failwiths : string -> 'a -> ('a -> Sexp.t) -> _

val pp : Format.formatter -> t -> unit
