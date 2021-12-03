(** This module extends {{!Base.Uniform_array}[Base.Uniform_array]} with
    bin_io. *)

open! Import

type 'a t = 'a Base.Uniform_array.t [@@deriving bin_io, sexp]

include module type of struct
  include Base.Uniform_array
end
with type 'a t := 'a t
