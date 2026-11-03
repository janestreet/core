open! Import

(** @inline *)
include module type of struct
  include Base.Portable_lazy
end

[%%rederive: type nonrec 'a t = 'a t [@@deriving bin_io, quickcheck]]

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving sexp_grammar]

    [%%rederive: type nonrec 'a t = 'a t [@@deriving compare, equal, hash, sexp_of]]
    [%%rederive: type nonrec 'a t = 'a t [@@deriving of_sexp]]
    [%%rederive: type nonrec 'a t = 'a t [@@deriving bin_io]]
  end
end
