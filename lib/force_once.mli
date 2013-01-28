open Sexplib

(** A "force_once" is a thunk that can only be forced once.  Subsequent forces
    will raise an exception. *)
type 'a t

(** [create f] creates a new force_once *)
val create : (unit -> 'a) -> 'a t

(** [force t] runs the thunk if it hadn't already been forced, else it raises an
    exception. *)
val force : 'a t -> 'a

(** [ignore ()] = create (fun () -> ()) *)
val ignore : unit -> unit t

val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
