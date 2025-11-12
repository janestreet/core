@@ portable

open! Import

include module type of struct
  include Base.List
end

[%%rederive:
  type ('a : value_or_null) t = 'a Base.List.t
  [@@deriving bin_io ~localize, quickcheck ~portable, typerep]]

include%template Comparator.Derived [@modality portable] with type 'a t := 'a t

val stable_witness : 'a Stable_witness.t -> 'a t Stable_witness.t
[@@alert
  for_internal_use_only
    "[Core.List0.stable_witness] is only exported for use in [Core.List.Stable]"]

val to_string : ('a : value_or_null). f:('a -> string) -> 'a t -> string

val%template gen_non_empty
  : ('a : value_or_null).
  'a Quickcheck.Generator.t @ p -> 'a t Quickcheck.Generator.t @ p
[@@mode p = (portable, nonportable)]

val%template gen_with_length
  : ('a : value_or_null).
  int -> 'a Quickcheck.Generator.t @ p -> 'a t Quickcheck.Generator.t @ p
[@@mode p = (portable, nonportable)]

val gen_filtered : 'a t -> 'a t Quickcheck.Generator.t
val gen_permutations : 'a t -> 'a t Quickcheck.Generator.t

module Assoc : sig
  include module type of struct
    include Base.List.Assoc
  end

  type ('a : value_or_null, 'b : value_or_null) t = ('a, 'b) Base.List.Assoc.t
  [@@deriving bin_io ~localize]

  val compare : ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
  [@@deprecated
    "[since 2016-06] This does not respect the equivalence class promised by List.Assoc. \
     Use List.compare directly if that's what you want."]
end
