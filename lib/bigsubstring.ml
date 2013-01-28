include Make_substring.F (struct
  type t = Bigstring.t

  let create = Bigstring.create
  let length = Bigstring.length
  module Blit = Make_substring.Blit
  let blit = Blit.bigstring_bigstring
  let blit_to_string = Blit.bigstring_string
  let blit_to_bigstring = Blit.bigstring_bigstring
  let blit_from_string = Blit.string_bigstring
  let blit_from_bigstring = Blit.bigstring_bigstring
  let of_bigstring t = t
  let of_string s = Bigstring.of_string s
end)
