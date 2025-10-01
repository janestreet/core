@@ portable

(** This module extends {{!Base.Uniform_array} [Base.Uniform_array]} with bin_io. *)

open! Import

type ('a : value_or_null) t = 'a Base.Uniform_array.t
[@@deriving compare ~localize, sexp, sexp_grammar, stable_witness]

[%%rederive:
  type nonrec 'a t = 'a Base.Uniform_array.t [@@deriving bin_io ~localize, quickcheck]]

include module type of struct
    include Base.Uniform_array
  end
  with type ('a : value_or_null) t := 'a t
