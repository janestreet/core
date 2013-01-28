(** various combinators for functions *)

(** A 'pipe' operator. *)
val ( |! ) : 'a -> ( 'a -> 'b) -> 'b

(** produces a function that just returns its first argument *)
val const : 'a -> _ -> 'a

(**
 * [ignore] is the same as [Pervasives.ignore].  It is useful to have here so
 * that code that rebinds [ignore] can still refer to [Fn.ignore].
 *)
external ignore : _ -> unit = "%ignore"

(** Negates a function *)
val non : ('a -> bool) -> 'a -> bool

(** [forever f] runs [f ()] until it throws an exception and returns the
    exception. This function is useful for read_line loops, etc. *)
val forever : (unit -> unit) -> exn

(** The identity function*)
external id : 'a -> 'a = "%identity"

(** [compose f g x] is [f (g x)] *)
val compose : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)

(** reverse the order of arguments for a binary function *)
val flip : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)
