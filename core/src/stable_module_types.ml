open! Import

[%%template
[@@@mode.default m = (global, local)]

module type S0_without_comparator = sig
  type t [@@deriving (bin_io [@mode m]), (compare [@mode m]), sexp]
end

module type [@modality p = (portable, nonportable)] S0 = sig
  include S0_without_comparator [@mode m]
  include Comparator.Stable.V1.S [@modality p] with type t := t
end

(** The polymorphic signatures require a mapping function so people can write conversion
    functions without either (1) re-implementing the mapping function inline or (2)
    reaching into the unstable part of the module. *)

module type S1 = sig
  type 'a t [@@deriving (bin_io [@mode m]), (compare [@mode m]), sexp]

  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module type S2 = sig
  type ('a1, 'a2) t [@@deriving (bin_io [@mode m]), (compare [@mode m]), sexp]

  val map : ('a1, 'a2) t -> f1:('a1 -> 'b1) -> f2:('a2 -> 'b2) -> ('b1, 'b2) t
end

module type S3 = sig
  type ('a1, 'a2, 'a3) t [@@deriving (bin_io [@mode m]), (compare [@mode m]), sexp]

  val map
    :  ('a1, 'a2, 'a3) t
    -> f1:('a1 -> 'b1)
    -> f2:('a2 -> 'b2)
    -> f3:('a3 -> 'b3)
    -> ('b1, 'b2, 'b3) t
end

module type S4 = sig
  type ('a1, 'a2, 'a3, 'a4) t [@@deriving (bin_io [@mode m]), (compare [@mode m]), sexp]

  val map
    :  ('a1, 'a2, 'a3, 'a4) t
    -> f1:('a1 -> 'b1)
    -> f2:('a2 -> 'b2)
    -> f3:('a3 -> 'b3)
    -> f4:('a4 -> 'b4)
    -> ('b1, 'b2, 'b3, 'b4) t
end]

module%template With_stable_witness = struct
  [@@@mode.default m = (global, local)]

  module type S0_without_comparator = sig
    type t [@@deriving stable_witness]

    include S0_without_comparator [@mode m] with type t := t
  end

  module type [@modality p = (portable, nonportable)] S0 = sig
    type t [@@deriving stable_witness]

    include S0 [@mode m] [@modality p] with type t := t
  end

  module type S1 = sig
    type 'a t [@@deriving stable_witness]

    include S1 [@mode m] with type 'a t := 'a t
  end

  module type S2 = sig
    type ('a, 'b) t [@@deriving stable_witness]

    include S2 [@mode m] with type ('a, 'b) t := ('a, 'b) t
  end

  module type S3 = sig
    type ('a, 'b, 'c) t [@@deriving stable_witness]

    include S3 [@mode m] with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
  end

  module type S4 = sig
    type ('a, 'b, 'c, 'd) t [@@deriving stable_witness]

    include S4 [@mode m] with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) t
  end
end
