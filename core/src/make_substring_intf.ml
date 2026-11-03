open! Import

module type Base = sig
  type t [@@deriving quickcheck]

  val create : int -> t
  val length : t @ immutable local -> int

  [%%template:
  val blit : ((t, t) Blit.blito[@mode read])
  val blit_to_bytes : ((t, bytes) Blit.blito[@mode read])
  val blit_to_bigstring : ((t, bigstring) Blit.blito[@mode read])
  val blit_from_bigstring : ((bigstring, t) Blit.blito[@mode read])

  val blit_to_string : ((t, bytes) Blit.blito[@mode read])
  [@@deprecated "[since 2017-10] use [blit_to_bytes] instead"]]

  val blit_from_string : (string, t) Blit.blito
  val get : t @ local read -> int -> char
end

module type S = Substring_intf.S

module type Make_substring = sig @@ portable
  module type Base = Base
  module type S = S

  type bigstring = Bigstring.t

  module Blit : sig
    [%%template:
    [@@@mode.default v = (read_write, read)]

    type ('src, 'dst) t = (('src, 'dst) Blit.blito[@mode v])]

    val string_string : (string, bytes) t
    [@@deprecated "[since 2017-10] use [string_bytes] instead"]

    val string_bytes : (string, bytes) t
    val string_bigstring : (string, bigstring) t

    [%%template:
    val bigstring_string : ((bigstring, bytes) t[@mode read])
    [@@deprecated "[since 2017-10] use [bigstring_bytes] instead"]

    val bytes_bytes : ((bytes, bytes) t[@mode read])
    val bigstring_bytes : ((bigstring, bytes) t[@mode read])
    val bytes_bigstring : ((bytes, bigstring) t[@mode read])
    val bigstring_bigstring : ((bigstring, bigstring) t[@mode read])]
  end

  module%template.portable F (Base : Base) : S with type base = Base.t
end
