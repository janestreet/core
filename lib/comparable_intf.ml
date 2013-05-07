module type Infix               = Polymorphic_compare_intf.Infix
module type Polymorphic_compare = Polymorphic_compare_intf.S

(** Used for specifying a bound (either upper or lower) as inclusive, exclusive, or
    unbounded. *)
type 'a bound = Incl of 'a | Excl of 'a | Unbounded

module type Validate = sig
  type t

  val validate_lbound : min : t bound                  -> t Validate.check
  val validate_ubound :                  max : t bound -> t Validate.check
  val validate_bound  : min : t bound -> max : t bound -> t Validate.check
end

module type With_zero = sig
  type t

  val validate_positive     : t Validate.check
  val validate_non_negative : t Validate.check
  val validate_negative     : t Validate.check
  val validate_non_positive : t Validate.check
  val is_positive     : t -> bool
  val is_non_negative : t -> bool
  val is_negative     : t -> bool
  val is_non_positive : t -> bool
end

module type S_common = sig
  include Polymorphic_compare
  (** [ascending] is identical to [compare]. [descending x y = ascending y x].  These are
      intended to be mnemonic when used like [List.sort ~cmp:ascending] and [List.sort
      ~cmp:descending], since they cause the list to be sorted in ascending or descending
      order, respectively. *)
  val ascending : t -> t -> int
  val descending : t -> t -> int

  val between : t -> low:t -> high:t -> bool

  module Replace_polymorphic_compare : sig
    include Polymorphic_compare with type t := t
    val _squelch_unused_module_warning_ : unit
  end

  type comparator
  val comparator : (t, comparator) Comparator.t

  include Validate with type t := t
end

module type S = sig
  include S_common

  module Map : Core_map.S
    with type Key.t = t
    with type Key.comparator = comparator
  module Set : Core_set.S
    with type Elt.t = t
    with type Elt.comparator = comparator
end

module type Map_and_set_binable = sig
  type t
  include Comparator.S_binable with type t := t
  module Map : Core_map.S_binable
    with type Key.t = t
    with type Key.comparator = comparator
  module Set : Core_set.S_binable
    with type Elt.t = t
    with type Elt.comparator = comparator
end

module type S_binable = sig
  include S_common
  include Map_and_set_binable
    with type t := t
    with type comparator := comparator
end

