(** Interface for {{!Core.Substring} [Substring]}. *)

open! Import

module type S = sig
  (** The type of strings that type [t] is a substring of. *)
  type base

  (** [sexp_of_t] is equivalent to [String.sexp_of_t (to_string t)] *)
  type t [@@deriving sexp_of, quickcheck]

  include Indexed_container.S0 with type t := t with type elt := char

  (*_ Shadows [length] from [Indexed_container.S0] to allow [t @ immutable]. *)
  val length : t @ immutable local -> int

  (*_ Shadows [is_empty] from [Indexed_container.S0] to allow [t @ immutable]. *)
  val is_empty : t @ immutable local -> bool

  [%%template:
  [@@@mode.default l = (global, local)]
  [@@@mode.default v = (read_write, read)]

  val base : t @ l v -> base @ l v]

  (** [pos] refers to the position in the base string, not any other substring that this
      substring was generated from. *)
  val pos : t @ immutable local -> int

  (** Per [String.get] and [Bigstring.get], this raises an exception if the index is out
      of bounds. *)
  val get : t @ local read -> int -> char

  [%%template:
  [@@@alloc.default a @ l = (heap_global, stack_local)]
  [@@@mode.default v = (read_write, read)]

  (** [create ?pos ?len base] creates a substring of the base sequence of length [len]
      starting at position [pos], i.e.,

      {[
        base.[pos], base.[pos + 1], ... base.[pos + len - 1]
      ]}

      An exception is raised if any of those indices into [base] is invalid.

      It does not copy the characters, so mutating [base] mutates [t] and vice versa. *)
  val create : ?pos:int -> ?len:int -> base @ l v -> t @ l v

  val sub : ?pos:int -> ?len:int -> t @ l v -> t @ l v]

  (** {2 Blit functions}

      For copying characters from a substring to and from both strings and substrings. *)

  val blit_to_string : t @ local read -> dst:bytes @ local -> dst_pos:int -> unit
  val blit_to_bytes : t @ local read -> dst:bytes @ local -> dst_pos:int -> unit
  val blit_to_bigstring : t @ local read -> dst:Bigstring.t @ local -> dst_pos:int -> unit
  val blit_from_string : t @ local -> src:string @ local -> src_pos:int -> len:int -> unit

  val blit_from_bigstring
    :  t @ local
    -> src:Bigstring.t @ local
    -> src_pos:int
    -> len:int
    -> unit

  (** {2 String concatenation} *)

  (** These functions always copy. *)

  val concat : t list -> t
  val concat_string : t list -> string
  val concat_bigstring : t list -> Bigstring.t

  (** {2 Conversion to/from substrings} *)

  (** These functions always copy. *)

  val%template to_string : t @ local read -> string @ l
  [@@alloc a @ l = (heap_global, stack_local)]

  val to_bigstring : t @ local read -> Bigstring.t

  (** These functions always copy. Use [create] if you want sharing. *)

  val of_string : string -> t [@@deprecated "[since 2017-11] use [create] instead"]

  val of_bigstring : Bigstring.t -> t
  [@@deprecated "[since 2017-11] use [create] instead"]

  (** {2 Prefixes and suffixes}

      The result of these functions share data with their input, but don't mutate the
      underlying string. *)

  [%%template:
  [@@@alloc.default a @ l = (heap_global, stack_local)]
  [@@@mode.default v = (read_write, read)]

  val drop_prefix : t @ l v -> int -> t @ l v
  val drop_suffix : t @ l v -> int -> t @ l v
  val prefix : t @ l v -> int -> t @ l v
  val suffix : t @ l v -> int -> t @ l v]
end
