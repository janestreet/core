open! Import

include Make_substring.F (struct
  type t = Bigstring.t [@@deriving quickcheck]

  let create = Bigstring.create
  let length = Bigstring.length
  let get = Bigstring.get

  module Blit = Make_substring.Blit

  let blit = Blit.bigstring_bigstring
  let blit_to_string = Blit.bigstring_bytes
  let blit_to_bytes = Blit.bigstring_bytes
  let blit_to_bigstring = Blit.bigstring_bigstring
  let blit_from_string = Blit.string_bigstring
  let blit_from_bigstring = Blit.bigstring_bigstring
end)
