open! Import

module type S = sig
  type t [@@deriving hash]
end

module type Deriving_hash = sig
  module Of_deriving_hash
      (Repr : S) (M : sig
                    type t

                    val to_repr : t -> Repr.t
                  end) : S with type t := M.t
end
