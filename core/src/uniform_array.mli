@@ portable

(** This module extends {{!Base.Uniform_array} [Base.Uniform_array]} with bin_io. *)

open! Import

type 'a t = 'a Base.Uniform_array.t
[@@deriving bin_io ~localize, quickcheck, sexp, sexp_grammar]

include module type of struct
    include Base.Uniform_array
  end
  with type 'a t := 'a t
