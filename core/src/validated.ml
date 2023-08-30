open! Import
open Std_internal
open Validated_intf

module type Raw = Raw

type ('raw, 'witness) t = 'raw

module type S = S with type ('a, 'b) validated := ('a, 'b) t

module type S_allowing_substitution =
  S_allowing_substitution with type ('a, 'b) validated := ('a, 'b) t

module type S_bin_io = S_bin_io with type ('a, 'b) validated := ('a, 'b) t

module type S_bin_io_compare_hash_sexp =
  S_bin_io_compare_hash_sexp with type ('a, 'b) validated := ('a, 'b) t

module type S_bin_io_compare_globalize_hash_sexp =
  S_bin_io_compare_globalize_hash_sexp with type ('a, 'b) validated := ('a, 'b) t

let raw t = t
let raw_local t = t

module Make (Raw : Raw) = struct
  type witness
  type t = Raw.t [@@deriving sexp_of]

  let validation_failed t error =
    Error.create
      "validation failed"
      (t, error, Raw.here)
      [%sexp_of: Raw.t * Error.t * Source_code_position.t]
  ;;

  let create_exn t =
    match Validate.result (Raw.validate t) with
    | Ok () -> t
    | Error error -> Error.raise (validation_failed t error)
  ;;

  let create t =
    match Validate.result (Raw.validate t) with
    | Ok () -> Ok t
    | Error error -> Error (validation_failed t error)
  ;;

  let t_of_sexp sexp = create_exn (Raw.t_of_sexp sexp)
  let raw t = t
  let raw_local t = t

  let create_stable_witness raw_stable_witness =
    Stable_witness.of_serializable raw_stable_witness create_exn raw
  ;;

  let type_equal = Type_equal.T
end

module Add_bin_io (Raw : sig
  type t [@@deriving bin_io]

  include Raw_bin_io with type t := t
end)
(Validated : S with type raw := Raw.t) =
struct
  include
    Binable.Of_binable_without_uuid [@alert "-legacy"]
      (Raw)
      (struct
        type t = Raw.t

        let of_binable raw =
          if Raw.validate_binio_deserialization then Validated.create_exn raw else raw
        ;;

        let to_binable = Fn.id
      end)
end

module Add_compare (Raw : sig
  type t [@@deriving compare]

  include Raw with type t := t
end)
(_ : S with type raw := Raw.t) =
struct
  let compare t1 t2 = [%compare: Raw.t] (raw t1) (raw t2)
end

(* We don't expose [Validated.Add_globalize] because it can be used in a way that can
   create an unvalidated [Validated.t]:

   {[
     let backdoor (example_validated : Validated.t) (unvalidated : Your_raw.t) : Validated.t =
       let module M = Add_globalize (struct type include Your_raw let globalize _ = unvalidated end) (Your_validated) in
       M.globalize example_validated
     ;;
   ]} *)
module Add_globalize (Raw : sig
  type t [@@deriving globalize]

  include Raw with type t := t
end)
(_ : S with type raw := Raw.t) =
struct
  let globalize t = Raw.globalize t
end

module Add_hash (Raw : sig
  type t [@@deriving hash]

  include Raw with type t := t
end)
(Validated : S with type raw := Raw.t) =
struct
  let hash_fold_t state t = Raw.hash_fold_t state (Validated.raw t)
  let hash t = Raw.hash (Validated.raw t)
end

module Add_typerep (Raw : sig
  type t [@@deriving typerep]

  include Raw with type t := t
end)
(_ : S with type raw := Raw.t) =
struct
  type t = Raw.t [@@deriving typerep]
end

module Make_binable (Raw : Raw_bin_io) = struct
  module T0 = Make (Raw)
  include T0
  include Add_bin_io (Raw) (T0)
end

module Make_bin_io_compare_hash_sexp (Raw : sig
  type t [@@deriving compare, hash]

  include Raw_bin_io with type t := t
end) =
struct
  module T = Make_binable (Raw)
  include T
  include Add_compare (Raw) (T)

  include (
    Add_hash (Raw) (T) :
        sig
          type t [@@deriving hash]
        end
        with type t := t)
end

module Make_bin_io_compare_globalize_hash_sexp
  (Raw : Raw_bin_io_compare_globalize_hash_sexp) =
struct
  module T1 = Make_bin_io_compare_hash_sexp (Raw)
  include T1
  include Add_globalize (Raw) (T1)
end
