(** For making an abstract version of a type that ensures a validation check has passed.

    Suppose one wants to have a type of positive integers:

    {[
      module Positive_int = Validated.Make (struct
          type t = int

          let here = [%here]
          let validate = Int.validate_positive
        end)
    ]}

    With this, one is certain that any value of type [Positive_int.t] has passed
    [Int.validate_positive].

    One can call [Positive_int.create_exn n] to create a new positive int from an [n],
    which will of course raise if [n <= 0]. One can call [Positive_int.raw positive_int]
    to get the [int] from a [Positive_int.t]. *)

open! Import

module type Raw = sig
  type t [@@deriving sexp]

  (** [here] will appear in validation-failure error messages. *)
  val here : Source_code_position.t

  val validate : t Validate.check
end

[%%template
[@@@mode.default m = (global, local)]

module type Raw_bin_io = sig
  type t [@@deriving bin_io [@mode m]]

  include Raw with type t := t

  (** [validate_binio_deserialization] controls whether when the binio representation of a
      value is deserialized, the resulting value is validated. Whether one needs to
      validate values upon deserialization depends on how serialization is being used. If
      one only ever serializes/deserializes so that the validation function is the same on
      both ends, then one need not validate upon deserialization, because only values that
      already pass the validation function can be serialized.

      If the validation functions in the serializer and deserializer may be different,
      e.g. because of two different versions of the code compiled at different times, then
      it is possible to serialize a value that may fail validation upon deserialization.
      In that case, having [validate_binio_deserialization = true] is necessary to prevent
      creating values that don't pass the validation function. *)
  val validate_binio_deserialization : bool
end

module type Raw_bin_io_compare_hash_sexp = sig
  type t [@@deriving (compare [@mode m]), hash]

  include Raw_bin_io [@mode m] with type t := t
end

module type Raw_bin_io_compare_globalize_hash_sexp = sig
  type t [@@deriving globalize]

  include Raw_bin_io_compare_hash_sexp [@mode m] with type t := t
end]

(** [S_allowing_substitution] is the same as [S], but allows writing interfaces like:

    {[
      type t [@@deriving bin_io, compare, ...]

      include Validated.S_allowing_substitution with type t := t and type raw := my_raw_type
    ]}

    which is not possible with [S] due to the fact that it constrains [t]. The downside is
    that you can no longer directly coerce [t] to be a [raw] but:

    + You can use the [raw] function instead
    + You can match on [type_equal] to do the coercion if you really need to (although
      that's still a bit clunkier than a direct coercion would be) *)
module type S_allowing_substitution = sig
  type ('raw, 'witness) validated
  type witness
  type raw
  type t [@@deriving sexp]

  val create : raw -> t Or_error.t
  val create_exn : raw -> t

  val%template raw : t @ m -> raw @ m [@@mode m = (global, local)]

  val create_stable_witness : raw Stable_witness.t -> t Stable_witness.t
  val type_equal : (t, (raw, witness) validated) Type_equal.t
end

module type S = sig
  type ('raw, 'witness) validated
  type witness
  type raw
  type t = (raw, witness) validated

  include
    S_allowing_substitution
    with type t := t
     and type raw := raw
     and type witness := witness
     and type ('raw, 'witness) validated := ('raw, 'witness) validated
end

[%%template
[@@@mode.default m = (global, local)]

module type S_bin_io = sig
  include S

  include sig
      type t = (raw, witness) validated [@@deriving bin_io [@mode m]]
    end
    with type t := t
end

module type S_bin_io_compare_hash_sexp = sig
  include S

  include sig
      type t = (raw, witness) validated
      [@@deriving (bin_io [@mode m]), (compare [@mode m]), hash]
    end
    with type t := t
end

module type S_bin_io_compare_globalize_hash_sexp = sig
  include S_bin_io_compare_hash_sexp [@mode m]

  include sig
      type t = (raw, witness) validated [@@deriving globalize]
    end
    with type t := t
end]

module type Validated = sig @@ portable
  type ('raw, 'witness) t = private 'raw

  val%template raw : ('raw, _) t @ m -> 'raw @ m [@@mode m = (global, local)]

  module type Raw = Raw
  module type S = S with type ('a, 'b) validated := ('a, 'b) t

  module type S_allowing_substitution =
    S_allowing_substitution with type ('a, 'b) validated := ('a, 'b) t

  module%template.portable Make (Raw : Raw) : S with type raw := Raw.t

  [%%template:
  [@@@mode.default m = (global, local)]

  module type S_bin_io = S_bin_io [@mode m] with type ('a, 'b) validated := ('a, 'b) t

  module type S_bin_io_compare_hash_sexp =
    S_bin_io_compare_hash_sexp [@mode m] with type ('a, 'b) validated := ('a, 'b) t

  module type S_bin_io_compare_globalize_hash_sexp =
    S_bin_io_compare_globalize_hash_sexp
    [@mode m]
    with type ('a, 'b) validated := ('a, 'b) t

  module%template.portable Make_binable (Raw : Raw_bin_io [@mode m]) :
    S_bin_io [@mode m] with type raw := Raw.t

  (** [Make_bin_io_compare_hash_sexp] is useful for stable types. *)
  module%template.portable Make_bin_io_compare_hash_sexp
      (Raw : Raw_bin_io_compare_hash_sexp
    [@mode m]) : S_bin_io_compare_hash_sexp [@mode m] with type raw := Raw.t

  module%template.portable Make_bin_io_compare_globalize_hash_sexp
      (Raw : Raw_bin_io_compare_globalize_hash_sexp
    [@mode m]) : S_bin_io_compare_globalize_hash_sexp [@mode m] with type raw := Raw.t

  module%template.portable Add_bin_io
      (Raw : Raw_bin_io
    [@mode m])
      (Validated : S with type raw := Raw.t) : sig
      type t [@@deriving bin_io]
    end
    with type t := Validated.t

  module%template.portable Add_compare
      (Raw : sig
         type t [@@deriving compare [@mode m]]

         include Raw with type t := t
       end)
      (Validated : S with type raw := Raw.t) : sig
      type t [@@deriving compare [@mode m]]
    end
    with type t := Validated.t]

  module%template.portable Add_hash
      (Raw : sig
         type t [@@deriving hash]

         include Raw with type t := t
       end)
      (Validated : S with type raw := Raw.t) : sig
      type t [@@deriving hash]
    end
    with type t := Validated.t

  module%template.portable Add_typerep
      (Raw : sig
         type t [@@deriving typerep]

         include Raw with type t := t
       end)
      (Validated : S with type raw := Raw.t) : sig
      type t [@@deriving typerep]
    end
    with type t := Validated.t
end
