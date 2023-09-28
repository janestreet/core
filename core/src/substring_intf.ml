(** Interface for {{!Core.Substring}[Substring]}. *)

open! Import

module type S = sig
  (** The type of strings that type [t] is a substring of. *)
  type base

  (** [sexp_of_t] is equivalent to [String.sexp_of_t (to_string t)] *)
  type t [@@deriving sexp_of, quickcheck]

  include Indexed_container.S0 with type t := t with type elt := char

  val base : t -> base

  (** [pos] refers to the position in the base string, not any other substring that this
      substring was generated from. *)
  val pos : t -> int

  (** Per [String.get] and [Bigstring.get], this raises an exception if the index is out
      of bounds. *)
  val get : t -> int -> char

  (** [create ?pos ?len base] creates a substring of the base sequence of
      length [len] starting at position [pos], i.e.,

      {[ base.[pos], base.[pos + 1], ... base.[pos + len - 1] ]}

      An exception is raised if any of those indices into [base] is invalid.

      It does not copy the characters, so mutating [base] mutates [t] and vice versa.
  *)
  val create : ?pos:int -> ?len:int -> base -> t

  val sub : ?pos:int -> ?len:int -> t -> t

  (** {2 Blit functions}

      For copying characters from a substring to and from both strings and substrings. *)

  val blit_to_string : t -> dst:bytes -> dst_pos:int -> unit
  val blit_to_bytes : t -> dst:bytes -> dst_pos:int -> unit
  val blit_to_bigstring : t -> dst:Bigstring.t -> dst_pos:int -> unit
  val blit_from_string : t -> src:string -> src_pos:int -> len:int -> unit
  val blit_from_bigstring : t -> src:Bigstring.t -> src_pos:int -> len:int -> unit

  (** {2 String concatenation} *)

  (** These functions always copy. *)

  val concat : t list -> t
  val concat_string : t list -> string
  val concat_bigstring : t list -> Bigstring.t

  (** {2 Conversion to/from substrings} *)

  (** These functions always copy. *)

  val to_string : t -> string
  val to_bigstring : t -> Bigstring.t

  (** These functions always copy. Use [create] if you want sharing. *)

  val of_string : string -> t [@@deprecated "[since 2017-11] use [create] instead"]

  val of_bigstring : Bigstring.t -> t
    [@@deprecated "[since 2017-11] use [create] instead"]

  (** {2 Prefixes and suffixes}

      The result of these functions share data with their input, but don't mutate the
      underlying string. *)

  val drop_prefix : t -> int -> t
  val drop_suffix : t -> int -> t
  val prefix : t -> int -> t
  val suffix : t -> int -> t
end
