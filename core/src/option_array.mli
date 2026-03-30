(** This module extends {{!Base.Option_array} [Base.Option_array]} with bin_io. *)

open! Import

type 'a t = 'a Base.Option_array.t

[%%rederive: type nonrec 'a t = 'a t [@@deriving bin_io, quickcheck, sexp, sexp_grammar]]

include module type of struct
    include Base.Option_array
  end
  with type 'a t := 'a t
