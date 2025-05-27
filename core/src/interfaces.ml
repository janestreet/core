(** Various interface exports. *)

open! Import

module type Applicative = Applicative.S

module type%template Comparable = Comparable.S [@mode m] [@modality p]
[@@mode m = (global, local)] [@@modality p = (portable, nonportable)]

module type%template Comparable_binable = Comparable.S_binable [@mode m] [@modality p]
[@@mode m = (global, local)] [@@modality p = (portable, nonportable)]

module type Floatable = Floatable.S
module type Hashable = Hashable.S
module type Hashable_binable = Hashable.S_binable
module type Identifiable = Identifiable.S
module type Infix_comparators = Comparable.Infix
module type Intable = Intable.S
module type Monad = Monad.S
module type Quickcheckable = Quickcheckable.S
module type Robustly_comparable = Robustly_comparable.S
module type Sexpable = Sexpable.S
module type Stringable = Stringable.S
module type Unit = Unit.S

[%%template
[@@@mode.default m = (global, local)]

module type Binable = Binable0.S [@mode m]
module type Stable = Stable_module_types.S0 [@mode m]
module type Stable_int63able = Stable_int63able.S [@mode m]

module type Stable_int63able_without_comparator = Stable_int63able.Without_comparator.S
[@mode m]

module type Stable_without_comparator = Stable_module_types.S0_without_comparator
[@mode m]

module type Stable1 = Stable_module_types.S1 [@mode m]
module type Stable2 = Stable_module_types.S2 [@mode m]
module type Stable3 = Stable_module_types.S3 [@mode m]
module type Stable4 = Stable_module_types.S4 [@mode m]

(* Versions of the stable module types that expose a stable_witness *)
module type Stable_with_witness = Stable_module_types.With_stable_witness.S0 [@mode m]

module type Stable_int63able_with_witness = Stable_int63able.With_stable_witness.S
[@mode m]

module type Stable_without_comparator_with_witness =
  Stable_module_types.With_stable_witness.S0_without_comparator [@mode m]

module type Stable1_with_witness = Stable_module_types.With_stable_witness.S1 [@mode m]
module type Stable2_with_witness = Stable_module_types.With_stable_witness.S2 [@mode m]
module type Stable3_with_witness = Stable_module_types.With_stable_witness.S3 [@mode m]
module type Stable4_with_witness = Stable_module_types.With_stable_witness.S4 [@mode m]]
