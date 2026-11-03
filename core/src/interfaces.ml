(** Various interface exports. *)

open! Import

module type Applicative = Applicative.S
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
module type Stringable = Stringable.S
module type Unit = Unit.S
module type Binable = Binable0.S

include Stable_int63able.Module_types_for_export
include Stable_module_types.For_export
