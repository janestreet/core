open! Import
include Immediate_option_intf

[%%template
[@@@mode.default m = (global, local)]
[@@@mode.default (p, c) = ((nonportable, uncontended), (portable, contended))]

module
  [@inline] Provide_or_null_conversions (Option : sig
    type t

    include S_minimal [@mode m] with type t := t
  end) =
struct
  [@@@mode.default m = (global, m)]

  let[@inline] to_or_null t =
    (Or_null.this_if [@mode m])
      (not (Option.is_none t))
      ((Option.unchecked_value [@mode m]) t)
    [@exclave_if_local m ~reasons:[ May_return_regional ]]
  ;;

  let[@inline] of_or_null = function
    | Null -> Option.none
    | This value -> (Option.some [@mode m]) value [@exclave_if_local m]
  ;;
end

module
  [@inline] Provide_or_null_conversions_zero_alloc (Option : sig
    type t

    include S_minimal_zero_alloc [@mode m] with type t := t
  end) =
struct
  [@@@mode.default m = (global, m)]

  let[@inline] to_or_null t =
    (Or_null.this_if [@mode m])
      (not (Option.is_none t))
      ((Option.unchecked_value [@mode m]) t)
    [@exclave_if_local m ~reasons:[ May_return_regional ]]
  ;;

  let[@inline] of_or_null = function
    | Null -> Option.none
    | This value -> (Option.some [@mode m]) value [@exclave_if_local m]
  ;;
end]
