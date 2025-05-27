@@ portable

open! Base

type t =
  { first_transition : Timezone_types.Transition.t
  ; remaining_transitions : Timezone_types.Transition.t list
  }

module Load_error : sig
  type t =
    | Disabled
    | Platform_not_supported
    | Failed of exn
  [@@deriving sexp_of]
end

val load : string -> (t, Load_error.t) Result.t

module For_testing : sig
  val disable : unit -> unit
  val enable : unit -> unit
end

module For_advanced_timezone_feature_detection : sig
  val should_use_timezone_js_loader
    :  unit
    -> [ `Yes | `Platform_not_supported | `Disabled ]
end
