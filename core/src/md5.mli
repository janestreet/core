(** This module implements the [MD5] message-digest algorithm as described IETF RFC 1321.
    [t] is the result type and [val digest_string : string -> t] is the implementation of
    the algorithm itself.

    This is currently a thin wrapper over the [Digest] module in INRIA's standard
    library. *)

open Interfaces

module Stable : sig
  module V1 : sig
    type t = Md5_lib.t
    [@@deriving sexp, sexp_grammar, bin_io, compare, equal, hash, stable_witness]
  end
end

(** Both bin_io and sexp serializations produce a binary 16-character string. *)
module As_binary_string : sig
  module Stable : sig
    module V1 : sig
      type t = Md5_lib.t
      [@@deriving sexp, sexp_grammar, bin_io, compare, equal, hash, stable_witness]
    end
  end

  type t = Stable.V1.t [@@deriving bin_io, sexp, sexp_grammar, hash]

  include Comparable with type t := t
  include Binable with type t := t
  include Hashable with type t := t
end

(** Intended to represent a 16-byte string that is the output of MD5 algorithm.

    Note that any 16-byte string can be converted to this type, so a value of type [t] is
    not an evidence of someone having found an input corresponding to this output. *)
type t = Stable.V1.t [@@deriving bin_io, sexp, sexp_grammar, hash]

include Comparable with type t := t
include Binable with type t := t
include Hashable with type t := t

(** [digest_num_bytes = 16] is the size of the digest in bytes. *)
val digest_num_bytes : int

(** Binary representations are 16 bytes long, and not human readable. *)
val to_binary : t -> string

val of_binary_exn : string -> t

(** [to_hex] prints each byte of [t] as a big-endian sequence of 2 hex digits
    (e.g. byte 31 is written as "1f") and then concatenates them.
    For example,
    {[
      Md5.to_hex (Md5.digest_string "a") =
      Md5.to_hex (
        Md5.of_binary_exn
          "\x0c\xc1\x75\xb9\xc0\xf1\xb6\xa8\x31\xc3\x99\xe2\x69\x77\x26\x61") =
      "0cc175b9c0f1b6a831c399e269772661"
    ]}
*)
val to_hex : t -> string

(** The inverse of [to_hex]. This function ignores case. It will raise an
    exception if the string is not a 32-byte-long string of hex digits. *)
val of_hex_exn : string -> t

val digest_string : string -> t
val digest_bytes : bytes -> t

(** [digest_subbytes m ~pos ~len] computes Md5 digest of the substring of [m] of length
    [len] starting at [pos]. *)
val digest_subbytes : bytes -> pos:int -> len:int -> t

(** [digest_file_blocking filename] reads the contents of file [filename] and computes its
    digest. *)
val digest_file_blocking : string -> t

(** Reads [len] bytes from the given channel and computes md5 digest of that.

    WARNING: This function does digest computation with OCaml global lock held, so it can
    be slow and make the other threads starve. See [digest_file_blocking]. *)
val digest_channel_blocking_without_releasing_runtime_lock : in_channel -> len:int -> t

(** Reads an Md5 digest from the given channel (in a format written by [output_blocking])
*)
val input_blocking : in_channel -> t

(** Writes the Md5 digest to the given channel. *)
val output_blocking : t -> out_channel -> unit

val string : string -> t [@@ocaml.deprecated "[since 2017-07] use [Md5.digest_string]."]
val bytes : bytes -> t [@@ocaml.deprecated "[since 2017-07] use [Md5.digest_bytes]."]

val subbytes : bytes -> int -> int -> t
  [@@ocaml.deprecated "[since 2017-07] use [Md5.digest_subbytes]."]

val from_hex : string -> t [@@ocaml.deprecated "[since 2017-07] use [of_hex_exn]."]

val file : string -> t
  [@@ocaml.deprecated
    "[since 2017-07] blocking functions should be avoided. Use [file_blocking] if you \
     really want this."]

val channel : in_channel -> int -> t
  [@@ocaml.deprecated
    "[since 2017-07] blocking functions should be avoided. Use [channel_blocking] if you \
     really want this."]

val output : out_channel -> t -> unit
  [@@ocaml.deprecated
    "[since 2017-07] Use [to_binary] together with [Out_channel.output_string]"]

val input : in_channel -> t
  [@@ocaml.deprecated
    "[since 2017-07] blocking functions should be avoided. Use [input_blocking] if you \
     really want this."]

(** [digest_bin_prot w x] digests the serialization of [x] by [w].
    It is a cheap way (in dev time) to compute the digest of an ocaml value, for a
    fixed and deterministic serialization function.
    It is currently implemented inefficiently and allocates large strings.

    For a more efficient and resource-aware version, use [Bigbuffer.add_bin_prot]
    and [Bigbuffer_blocking.md5]. *)
val digest_bin_prot : 'a Bin_prot.Type_class.writer -> 'a -> t

val digest_bigstring : Bigstring.t -> t
val digest_subbigstring : Bigstring.t -> pos:int -> len:int -> t
