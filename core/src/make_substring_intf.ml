open! Import

module type Base = sig
  type t [@@deriving quickcheck]

  val create : int -> t
  val length : t -> int
  val blit : (t, t) Blit.blito
  val blit_to_bytes : (t, bytes) Blit.blito
  val blit_to_bigstring : (t, bigstring) Blit.blito
  val blit_from_string : (string, t) Blit.blito
  val blit_from_bigstring : (bigstring, t) Blit.blito

  val blit_to_string : (t, bytes) Blit.blito
    [@@deprecated "[since 2017-10] use [blit_to_bytes] instead"]

  val get : t -> int -> char
end

module type S = Substring_intf.S

module type Make_substring = sig
  module type Base = Base
  module type S = S

  type bigstring = Bigstring.t

  module Blit : sig
    type ('src, 'dst) t = ('src, 'dst) Blit.blito

    val string_string : (string, bytes) t
      [@@deprecated "[since 2017-10] use [string_bytes] instead"]

    val bigstring_string : (bigstring, bytes) t
      [@@deprecated "[since 2017-10] use [bigstring_bytes] instead"]

    val string_bytes : (string, bytes) t
    val bytes_bytes : (bytes, bytes) t
    val bigstring_bytes : (bigstring, bytes) t
    val string_bigstring : (string, bigstring) t
    val bytes_bigstring : (bytes, bigstring) t
    val bigstring_bigstring : (bigstring, bigstring) t
  end

  module F (Base : Base) : S with type base = Base.t
end
