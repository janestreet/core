open! Core

module Foo (X : sig
  type t

  include Comparable.S with type t := t
  include Quickcheckable with type t := t
end) =
struct
  type t1 = Set.M(X).t [@@deriving quickcheck]
  type t2 = unit Map.M(X).t [@@deriving quickcheck]
end
