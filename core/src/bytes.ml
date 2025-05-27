open! Import
open Base_quickcheck.Export

module Stable = struct
  module V1 = struct
    include Base.Bytes

    type t = bytes
    [@@deriving bin_io ~localize, globalize, quickcheck, stable_witness, typerep]
  end
end

include Stable.V1

include%template Comparable.Validate [@modality portable] (Base.Bytes)

include%template Hexdump.Of_indexable [@modality portable] (struct
    type t = bytes

    let length = length
    let get t i = get t i
  end)

let gen' char_gen = String.gen' char_gen |> Quickcheck.Generator.map ~f:of_string

let gen_with_length len char_gen =
  String.gen_with_length len char_gen |> Quickcheck.Generator.map ~f:of_string
;;
