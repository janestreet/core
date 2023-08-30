open! Import
open Base_quickcheck.Export

module Stable = struct
  module V1 = struct
    include Base.Bytes

    type t = bytes [@@deriving bin_io ~localize, quickcheck, typerep, stable_witness]
  end
end

include Stable.V1
include Comparable.Validate (Base.Bytes)

include Hexdump.Of_indexable (struct
  type t = bytes

  let length = length
  let get = get
end)

let gen' char_gen = String.gen' char_gen |> Quickcheck.Generator.map ~f:of_string

let gen_with_length len char_gen =
  String.gen_with_length len char_gen |> Quickcheck.Generator.map ~f:of_string
;;
