(** For making an abstract version of a type that ensures a validation check has passed.

    Suppose one wants to have a type of positive integers:

    {[
      module Positive_int = Validated.Make (struct
        type t = int
        let here = _here_
        let validate = Int.validate_positive
      end)
    ]}

    With this, one is certain that any value of type [Positive_int.t] has passed
    [Int.validate_positive].

    One can call [Positive_int.create_exn n] to create a new positive int from an [n],
    which will of course raise if [n <= 0].  One can call [Positive_int.raw positive_int]
    to get the [int] from a [Positive_int.t].
*)

module type Raw = sig
  type t with sexp

  (** [here] will appear in validation-failure error messages. *)
  val here : Source_code_position.t

  val validate : t Validate.check
end

module type Raw_binable = sig
  type t with bin_io
  include Raw with type t := t
end

module type Validated = sig
  type raw
  type t (* = private raw *) with sexp

  val create     : raw -> t Or_error.t
  val create_exn : raw -> t

  val raw : t -> raw
end

module type Validated_binable = sig
  type t with bin_io
  include Validated with type t := t
end

module type S = sig
  module type Raw       = Raw
  module type Validated = Validated

  module Make         (Raw : Raw)         : Validated         with type raw := Raw.t
  module Make_binable (Raw : Raw_binable) : Validated_binable with type raw := Raw.t
end
