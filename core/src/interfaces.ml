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

module type%template Identifiable = Identifiable.S [@mode m] [@modality p]
[@@mode m = (global, local)] [@@modality p = (portable, nonportable)]

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

module type Binable = Binable0.S [@mode m]]

include Stable_int63able.Module_types_for_export
include Stable_module_types.For_export
