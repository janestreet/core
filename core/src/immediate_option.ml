open! Import
include Immediate_option_intf

module
  [@inline]
  Provide_or_null_conversions (Option : sig
    type t

    include S_minimal with type t := t
  end) =
struct
  let[@inline] to_or_null t =
    Or_null.this_if (not (Option.is_none t)) (Option.unchecked_value t)
  ;;

  let[@inline] of_or_null = function
    | Null -> Option.none
    | This value -> Option.some value
  ;;
end

module
  [@inline]
  Provide_or_null_conversions_zero_alloc (Option : sig
    type t

    include S_minimal_zero_alloc with type t := t
  end) =
struct
  let[@inline] to_or_null t =
    Or_null.this_if (not (Option.is_none t)) (Option.unchecked_value t)
  ;;

  let[@inline] of_or_null = function
    | Null -> Option.none
    | This value -> Option.some value
  ;;
end
