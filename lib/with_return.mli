(** This is [include]'d and documented in {! module: Common}.  It is defined here to avoid
    circular dependencies. *)

type 'a return = private { return : 'b. 'a -> 'b }

val with_return : ('a return -> 'a) -> 'a
