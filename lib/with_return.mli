(* belongs in Common, but moved here to avoid circular dependencies *)
type 'a return = private { return : 'b. 'a -> 'b }

val with_return : ('a return -> 'a) -> 'a
