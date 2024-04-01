open! Import

(** Extension to the base signature *)
module type Extension = sig
  type t [@@deriving bin_io, diff ~how:"atomic" ~extra_derive:[ sexp ]]

  module Stable : sig
    (** [Info.t] is wire-compatible with [V2.t], but not [V1.t].  [V1] bin-prots a sexp of
        the underlying message, whereas [V2] bin-prots the underlying message. *)
    module V1 : Stable_module_types.With_stable_witness.S0 with type t = t

    module V2 : sig
      type nonrec t = t
      [@@deriving equal, hash, sexp_grammar, diff ~extra_derive:[ sexp; bin_io ]]

      include Stable_module_types.With_stable_witness.S0 with type t := t
    end
  end
end

module type S = sig
  include Base.Info.S
  include Extension with type t := t
end

module type Info = sig
  (** @inline *)
  include module type of struct
    include Base.Info
  end

  module Internal_repr : module type of Base.Info.Internal_repr
  include Extension with type t := t
  module Extend (Info : Base.Info.S) : Extension with type t := Info.t
end
