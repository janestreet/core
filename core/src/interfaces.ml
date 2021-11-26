(** Various interface exports. *)

open! Import

module type Applicative = Applicative.S
module type Binable = Binable0.S
module type Comparable = Comparable.S
module type Comparable_binable = Comparable.S_binable
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
module type Stable = Stable_module_types.S0
module type Stable_int63able = Stable_int63able.S
module type Stable_without_comparator = Stable_module_types.S0_without_comparator
module type Stable1 = Stable_module_types.S1
module type Stable2 = Stable_module_types.S2
module type Stable3 = Stable_module_types.S3
module type Stable4 = Stable_module_types.S4
module type Stringable = Stringable.S
module type Unit = Unit.S
