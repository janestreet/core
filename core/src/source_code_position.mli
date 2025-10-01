@@ portable

(** This module extends {{!Base.Source_code_position} [Base.Source_code_position]}. *)

module type S := sig
  (** @inline *)
  include module type of struct
    include Base.Source_code_position
  end

  type t = Base.Source_code_position.t =
    { pos_fname : string
    ; pos_lnum : int
    ; pos_bol : int
    ; pos_cnum : int
    }
  [@@deriving bin_io ~localize, fields ~getters, globalize]

  include%template
    Comparable.S
    [@mode local]
    with type t := t
     and type comparator_witness := comparator_witness

  include Hashable.S with type t := t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving equal ~localize]

      include%template
        Stable_module_types.With_stable_witness.S0 [@mode local] with type t := t
    end
  end
end

include S (** @inline *)

(** [With_hiding] differs in that [to_string] and [sexp_of_t], in test, show [LINE] and
    [COL] rather than the actual line and column. Eliding the numbers makes tests that
    includes source-code positions more robust because output doesn't change unless
    filenames change. [Source_code_position_with_hiding] makes this behavior automatic,
    which is easier than manually hiding via other mechanisms, e.g. using
    [Expect_test_helpers] with [~hide_positions:true].

    Idiomatic usage is:

    {[
      module Source_code_position = Source_code_position.With_hiding
    ]} *)
module With_hiding : S
