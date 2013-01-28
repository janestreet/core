include Make_substring.F (struct
  type t = string

  let create = String.create
  let length = String.length
  module Blit = Make_substring.Blit
  let blit = Blit.string_string
  let blit_to_string = Blit.string_string
  let blit_to_bigstring = Blit.string_bigstring
  let blit_from_string = Blit.string_string
  let blit_from_bigstring = Blit.bigstring_string
  let of_bigstring bs = Bigstring.to_string bs
  let of_string s = s
end)
