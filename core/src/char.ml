open! Import

type t = char [@@deriving typerep]

include
  Identifiable.Extend
    (Base.Char)
    (struct
      type t = char [@@deriving bin_io]
    end)

(* include [Base.Char] after the application of [Identifiable.Extend] to replace the
   [Comparable] functions with the pervasive versions *)
include (
  Base.Char :
    module type of struct
    include Base.Char
  end
  with type t := t)

module Caseless = struct
  module T = struct
    include Caseless

    type t = char [@@deriving bin_io]
  end

  include T
  include Comparable.Make_binable_using_comparator (T)
  include Hashable.Make_binable (T)
end

module Replace_polymorphic_compare = Base.Char

let quickcheck_generator = Base_quickcheck.Generator.char
let quickcheck_observer = Base_quickcheck.Observer.char
let quickcheck_shrinker = Base_quickcheck.Shrinker.char
let gen_digit = Base_quickcheck.Generator.char_digit
let gen_lowercase = Base_quickcheck.Generator.char_lowercase
let gen_uppercase = Base_quickcheck.Generator.char_uppercase
let gen_alpha = Base_quickcheck.Generator.char_alpha
let gen_alphanum = Base_quickcheck.Generator.char_alphanum
let gen_print = Base_quickcheck.Generator.char_print
let gen_whitespace = Base_quickcheck.Generator.char_whitespace
let gen_uniform_inclusive = Base_quickcheck.Generator.char_uniform_inclusive
