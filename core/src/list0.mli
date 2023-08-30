open! Import

include module type of struct
  include Base.List
end

type 'a t = 'a Base.List.t [@@deriving bin_io ~localize, typerep]

include Comparator.Derived with type 'a t := 'a t
include Quickcheckable.S1 with type 'a t := 'a t

val stable_witness : 'a Stable_witness.t -> 'a t Stable_witness.t
  [@@alert
    for_internal_use_only
      "[Core.List0.stable_witness] is only exported for use in [Core.List.Stable]"]

val to_string : f:('a -> string) -> 'a t -> string
val gen_non_empty : 'a Quickcheck.Generator.t -> 'a t Quickcheck.Generator.t
val gen_with_length : int -> 'a Quickcheck.Generator.t -> 'a t Quickcheck.Generator.t
val gen_filtered : 'a t -> 'a t Quickcheck.Generator.t
val gen_permutations : 'a t -> 'a t Quickcheck.Generator.t

module Assoc : sig
  include module type of struct
    include Base.List.Assoc
  end

  type ('a, 'b) t = ('a, 'b) Base.List.Assoc.t [@@deriving bin_io ~localize]

  val compare : ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
    [@@deprecated
      "[since 2016-06] This does not respect the equivalence class promised by \
       List.Assoc. Use List.compare directly if that's what you want."]
end
