(** [Info] is a library for lazily constructing human-readable information as a string or
    sexp, with a primary use being error messages.  Using [Info] is often preferable to
    [sprintf] or manually constructing strings because you don't have to eagerly construct
    the string --- you only need to pay when you actually want to display the info.  which
    for many applications is rare.  Using [Info] is also better than creating custom
    exceptions because you have more control over the format.

    Info is intended to be constructed in the following style; for simple info, you write:

    {[Info.of_string "Unable to find file"]}

    For info where you want to attach some content, you would write:

    {[Info.create "Unable to find file" filename <:sexp_of< string >>]}

    Or even,

    {[
    Info.create "price too big" (price, [`Max max_price])
      (<:sexp_of< float * [`Max of float] >>)
    ]}

    Note that an [Info.t] can be created from any arbritrary sexp with [Info.t_of_sexp].
*)

open Sexplib

type t with bin_io, sexp

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
val create : ?here:Source_code_position0.t -> string -> 'a -> ('a -> Sexp.t) -> t

(* Add a string to the front. *)
val tag : t -> string -> t

(* Add a string and some other data in the form of an s-expression at the front. *)
val tag_arg : t -> string -> 'a -> ('a -> Sexp.t) -> t

(* Combine multiple infos into one *)
val of_list : ?trunc_after:int -> t list -> t

(* [of_exn] and [to_exn] are primarily used with [Error], but their definitions have to be
   here because they refer to the underlying representation. *)
val of_exn : ?backtrace:[ `Get | `This of string ] -> exn -> t
val to_exn : t -> exn
