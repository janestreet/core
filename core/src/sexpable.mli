@@ portable

(** This module extends {!Base.Sexpable}. *)

open! Import

(** @inline *)
include module type of struct
  include Base.Sexpable
end

module%template.portable To_stringable (M : S) : Stringable.S with type t := M.t
module%template.portable To_stringable_utf8 (M : S) : Stringable.S with type t := M.t

(** The following functors preserve stability: if applied to stable types with stable
    (de)serializations, they will produce stable types with stable (de)serializations.

    Note: In all cases, stability of the input (and therefore the output) depends on the
    semantics of all conversion functions (e.g. to_string, to_sexpable) not changing in
    the future. *)
module%template Stable : sig
  module Of_sexpable : sig
    module%template
      [@alloc a = (heap, stack)] [@modality p = (portable, nonportable)] V1 :
        module type of Of_sexpable [@alloc a] [@modality p]
  end

  module Of_sexpable1 : sig
    module%template
      [@alloc a = (heap, stack)]
      [@modality p = (portable, nonportable)]
      [@kind ka = (value, any)] V1 :
        module type of Of_sexpable1 [@kind ka] [@alloc a] [@modality p]
  end

  module Of_sexpable2 : sig
    module%template
      [@alloc a = (heap, stack)]
      [@modality p = (portable, nonportable)]
      [@kind ka = (value, any), kb = (value, any)] V1 :
        module type of Of_sexpable2 [@kind ka kb] [@alloc a] [@modality p]
  end

  module Of_sexpable3 : sig
    module%template
      [@alloc a = (heap, stack)]
      [@modality p = (portable, nonportable)]
      [@kind ka = (value, any), kb = (value, any), kc = (value, any)] V1 :
        module type of Of_sexpable3 [@kind ka kb kc] [@alloc a] [@modality p]
  end

  module Of_stringable : sig
    module%template
      [@alloc a = (heap, stack)] [@modality p = (portable, nonportable)] V1 :
        module type of Of_stringable [@alloc a] [@modality p]
  end

  module To_stringable : sig
    module%template [@modality p = (portable, nonportable)] V1 :
        module type of To_stringable [@modality p]
  end
end
