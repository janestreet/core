(** This module extends {{!Base.Queue}[Base.Queue]} with bin_io. *)

open! Import

type 'a t = 'a Base.Queue.t [@@deriving sexp_of, bin_io]

(** {2 The interface from Base} *)

include module type of Base.Queue with type 'a t := 'a t (** @inline *)

(** {2 Extensions} *)

include Binary_searchable.S1 with type 'a t := 'a t

module Stable : sig
  module V1 : Stable_module_types.S1 with type 'a t = 'a t
end
