(* A substring is a contiguous set of characters within a string. Creating a substring
   does not copy. Therefore modifying the string also modifies the substring. *)

module type S = sig
  (* the type of strings that type t is a substring of *)
  type base

  type t

  (* The pos refers to the position in the base string, not any other substring that this
     substring was generated from. *)
  val base : t -> base
  val pos : t -> int
  val length : t -> int

  (** [create ?pos ?len base] creates a substring of the base sequence of
   * length [len] starting at position [pos], i.e.
   *
   *   base.[pos], base.[pos + 1], ... base.[pos + len - 1]
   *
   * It is required that:
   *   0 <= pos
   *   0 <= len
   *   pos + len <= length base
   *
   * It does not copy the characters.
   *)
  val create : ?pos:int -> ?len:int -> base -> t

  (* copies the characters *)
  val blit_to_string : t -> dst:string -> dst_pos:int -> unit
  val blit_to_bigstring : t -> dst:Bigstring.t -> dst_pos:int -> unit
  val blit_from_string : t -> src:string -> src_pos:int -> len:int -> unit
  val blit_from_bigstring : t -> src:Bigstring.t -> src_pos:int -> len:int -> unit

  (* copies the characters *)
  val concat : t list -> t
  val concat_string : t list -> string
  val concat_bigstring : t list -> Bigstring.t

  (* do not copy the characters *)
  val of_bigstring : Bigstring.t -> t
  val of_string : string -> t

  (* these two functions perform a copy *)
  val to_bigstring : t -> Bigstring.t
  val to_string : t -> string

  (* no copying *)
  val drop_prefix : t -> int -> t
  val drop_suffix : t -> int -> t
  val prefix : t -> int -> t
  val suffix : t -> int -> t
end
