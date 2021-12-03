(** This module extends {!Base.Sexpable}. *)

open! Import

(** @inline *)
include module type of struct
  include Base.Sexpable
end

module To_stringable (M : S) : Stringable.S with type t := M.t

(** The following functors preserve stability: if applied to stable types with stable
    (de)serializations, they will produce stable types with stable (de)serializations.

    Note: In all cases, stability of the input (and therefore the output) depends on the
    semantics of all conversion functions (e.g. to_string, to_sexpable) not changing in
    the future.
*)
module Stable : sig
  module Of_sexpable : sig
    module V1 : module type of Of_sexpable
  end

  module Of_sexpable1 : sig
    module V1 : module type of Of_sexpable1
  end

  module Of_sexpable2 : sig
    module V1 : module type of Of_sexpable2
  end

  module Of_sexpable3 : sig
    module V1 : module type of Of_sexpable3
  end

  module Of_stringable : sig
    module V1 : module type of Of_stringable
  end

  module To_stringable : sig
    module V1 : module type of To_stringable
  end
end
