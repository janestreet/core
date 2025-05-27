open! Import

module Stable = struct
  module V1 = struct
    module T = struct
      include Base.Char

      type t = char
      [@@deriving bin_io ~localize, sexp, sexp_grammar, stable_witness, typerep]
    end

    include T

    include%template Comparable.Stable.V1.With_stable_witness.Make [@modality portable] (T)
  end
end

type t = char [@@deriving typerep, bin_io ~localize]

include%template
  Identifiable.Extend [@mode local] [@modality portable]
    (Base.Char)
    (struct
      type t = char [@@deriving bin_io ~localize]
    end)

(* include [Base.Char] after the application of [Identifiable.Extend] to replace the
   [Comparable] functions with the pervasive versions *)
include (
  Base.Char :
  sig
  @@ portable
    include module type of struct
        include Base.Char
      end
      with type t := t
  end)

module Caseless = struct
  module T = struct
    include Caseless

    type t = char [@@deriving bin_io ~localize]
  end

  include T

  include%template
    Comparable.Make_binable_using_comparator [@mode local] [@modality portable] (T)

  include%template Hashable.Make_binable [@modality portable] (T)
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
