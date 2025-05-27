open! Import

module type S = Make_substring.S

include%template Make_substring.F [@modality portable] (struct
    type t = Bytes.t [@@deriving quickcheck]

    let create = Bytes.create
    let length = Bytes.length
    let get t i = Bytes.get t i

    module Blit = Make_substring.Blit

    let blit = Blit.bytes_bytes
    let blit_to_string = Blit.bytes_bytes
    let blit_to_bytes = Blit.bytes_bytes
    let blit_to_bigstring = Blit.bytes_bigstring
    let blit_from_string = Blit.string_bytes
    let blit_from_bigstring = Blit.bigstring_bytes
  end)
