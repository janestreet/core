(** A substring is a contiguous set of characters within a string. Creating a substring
    does not copy. Therefore modifying the string also modifies the substring. *)

module type S = Make_substring.S

include S with type base = bytes (** @inline *)
