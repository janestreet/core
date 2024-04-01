(** String type based on [Bigarray], for use in I/O and C-bindings. *)

open! Import
open Bigarray

(** {2 Types and exceptions} *)

(** Type of bigstrings *)
type t = (char, int8_unsigned_elt, c_layout) Array1.t
[@@deriving compare, quickcheck, sexp_of]

(** Type of bigstrings which support hashing. Note that mutation invalidates previous hashes. *)
type t_frozen = t [@@deriving compare, hash, sexp_of]

include module type of Base_bigstring with type t := t and type t_frozen := t_frozen
include Hexdump.S with type t := t

(** {2 Creation and string conversion} *)

(** [create length]
    @return a new bigstring having [length].
    Content is undefined. *)
val create : int -> t

(** [sub_shared ?pos ?len bstr] @return the sub-bigstring in [bstr]
    that starts at position [pos] and has length [len].  The sub-bigstring
    shares the same memory region, i.e. modifying it will modify the
    original bigstring.  Holding on to the sub-bigstring will also keep
    the (usually bigger) original one around.

    @param pos default = 0
    @param len default = [Bigstring.length bstr - pos] *)
val sub_shared : ?pos:int -> ?len:int -> t -> t

(** {2 Reading/writing bin-prot} *)

(** These functions write the "size-prefixed" bin-prot format that is used by, e.g.,
    async's [Writer.write_bin_prot], [Reader.read_bin_prot] and
    [Unpack_buffer.Unpack_one.create_bin_prot]. *)

(** [write_bin_prot t writer a] writes [a] to [t] starting at [pos], and returns the index
    in [t] immediately after the last byte written.  It raises if [pos < 0] or if [a]
    doesn't fit in [t]. *)
val write_bin_prot
  :  t
  -> ?pos:int (** default is 0 *)
  -> 'a Bin_prot.Type_class.writer
  -> 'a
  -> int

(** Same as [write_bin_prot], with the difference that [size] is pre-computed by the
    caller. [size] is assumed to be the result of calling the bin prot sizer on the value
    being written. *)
val write_bin_prot_known_size
  :  t
  -> ?pos:int (** default is 0 *)
  -> 'a Bin_prot.Write.writer
  -> size:int
  -> 'a
  -> int

(** The [read_bin_prot*] functions read from the region of [t] starting at [pos] of length
    [len].  They return the index in [t] immediately after the last byte read.  They raise
    if [pos] and [len] don't describe a region of [t]. *)
val read_bin_prot
  :  t
  -> ?pos:int
  -> ?len:int
  -> 'a Bin_prot.Type_class.reader
  -> ('a * int) Or_error.t

val read_bin_prot_verbose_errors
  :  t
  -> ?pos:int
  -> ?len:int
  -> 'a Bin_prot.Type_class.reader
  -> [ `Invalid_data of Error.t | `Not_enough_data | `Ok of 'a * int ]

(** {2 Destruction} *)

(** [unsafe_destroy bstr] destroys the bigstring by deallocating its associated data or,
    if memory-mapped, unmapping the corresponding file, and setting all dimensions to
    zero.  This effectively frees the associated memory or address-space resources
    instantaneously.  This feature helps reclaim the resources sooner than they are
    automatically reclaimed by the GC.

    This operation is safe unless you have passed the bigstring to another thread that is
    performing operations on it at the same time.  Access to the bigstring after this
    operation will yield array bounds exceptions.

    @raise Failure if the bigstring has already been deallocated (or deemed "external",
    which is treated equivalently), or if it has proxies, i.e. other bigstrings referring
    to the same data. *)
external unsafe_destroy : t -> unit = "bigstring_destroy_stub"

(** [unsafe_destroy_and_resize bstr ~len] reallocates the memory backing
    [bstr] and returns a new bigstring that starts at position 0 and has
    length [len]. If [len] is greater than [length bstr] then the newly
    allocated memory will not be initialized.

    Similar to [unsafe_destroy], this operation is safe unless you have passed
    the bigstring to another thread that is performing operations on it at the
    same time.  Access to [bstr] after this operation will yield array bounds
    exceptions.

    @raise Failure if the bigstring has already been deallocated (or deemed
    "external", which is treated equivalently), if it is backed by a memory
    map, or if it has proxies, i.e. other bigstrings referring to the same
    data. *)
external unsafe_destroy_and_resize : t -> len:int -> t = "bigstring_realloc"

(** Similar to [Binary_packing.unpack_tail_padded_fixed_string] and
    [.pack_tail_padded_fixed_string]. *)
val get_tail_padded_fixed_string
  :  padding:char
  -> t
  -> pos:int
  -> len:int
  -> unit
  -> string

val get_tail_padded_fixed_string_local
  :  padding:char
  -> t
  -> pos:int
  -> len:int
  -> unit
  -> string

val set_tail_padded_fixed_string
  :  padding:char
  -> t
  -> pos:int
  -> len:int
  -> string
  -> unit

val get_head_padded_fixed_string
  :  padding:char
  -> t
  -> pos:int
  -> len:int
  -> unit
  -> string

val get_head_padded_fixed_string_local
  :  padding:char
  -> t
  -> pos:int
  -> len:int
  -> unit
  -> string

val set_head_padded_fixed_string
  :  padding:char
  -> t
  -> pos:int
  -> len:int
  -> string
  -> unit

module Unstable : sig
  type nonrec t = t [@@deriving bin_io ~localize, compare, equal, sexp_of]
  type nonrec t_frozen = t_frozen [@@deriving bin_io ~localize, compare, hash, sexp_of]
end

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io ~localize, stable_witness, compare, equal, sexp]

    type nonrec t_frozen = t_frozen
    [@@deriving bin_io ~localize, stable_witness, compare, hash, sexp]
  end
end
