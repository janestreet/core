(** Like {!Identifiable}, but with [t = private string] and stable modules. *)

include String_id_intf.String_id (** @inline *)
