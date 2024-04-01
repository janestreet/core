open! Import

module Bin : Binable0.S with type t := Base.Int63.t = struct
  module Bin_emulated = struct
    type t = Base.Int63.Private.Emul.t

    include
      Binable0.Of_binable_without_uuid [@alert "-legacy"]
        (Int64)
        (struct
          type nonrec t = t

          let of_binable = Base.Int63.Private.Emul.W.wrap_exn
          let to_binable = Base.Int63.Private.Emul.W.unwrap
        end)
  end

  type 'a binable = (module Binable0.S with type t = 'a)

  let binable_of_repr : type a b. (a, b) Base.Int63.Private.Repr.t -> b binable = function
    | Base.Int63.Private.Repr.Int -> (module Int)
    | Base.Int63.Private.Repr.Int64 -> (module Bin_emulated)
  ;;

  let binable : Base.Int63.t binable = binable_of_repr Base.Int63.Private.repr

  include (val binable)

  let bin_shape_t = Bin_prot.Shape.bin_shape_int63
end

module Stable = struct
  module V1 = struct
    module T = struct
      type t = Base.Int63.t [@@deriving equal, hash, sexp, sexp_grammar]

      include Bin

      include (
        Base.Int63 :
          Base.Comparable.S
            with type t := t
            with type comparator_witness = Base.Int63.comparator_witness)

      (* This serialization is stable, since it either delegates to [int] or
         [Int63_emul]. *)
      let stable_witness : t Stable_witness.t = Stable_witness.assert_stable
    end

    include T
    include Comparable.Stable.V1.With_stable_witness.Make (T)
  end
end

(* This [include struct] is required because it lets us shadow [t] when we include
   [Base.Int63] later on. *)
include struct
  type t = Base.Int63.t
end

let typerep_of_t = typerep_of_int63
let typename_of_t = typename_of_int63

include
  Identifiable.Extend
    (Base.Int63)
    (struct
      type nonrec t = t

      include Bin
    end)

module Replace_polymorphic_compare : Comparable.Comparisons with type t := t = Base.Int63
include Base.Int63
include Comparable.Validate_with_zero (Base.Int63)

module Binary = struct
  include Binary

  type nonrec t = t [@@deriving typerep, bin_io]
end

module Hex = struct
  include Hex

  type nonrec t = t [@@deriving typerep, bin_io]
end

let quickcheck_generator = Base_quickcheck.Generator.int63
let quickcheck_observer = Base_quickcheck.Observer.int63
let quickcheck_shrinker = Base_quickcheck.Shrinker.int63
let gen_incl = Base_quickcheck.Generator.int63_inclusive
let gen_uniform_incl = Base_quickcheck.Generator.int63_uniform_inclusive
let gen_log_incl = Base_quickcheck.Generator.int63_log_inclusive
let gen_log_uniform_incl = Base_quickcheck.Generator.int63_log_uniform_inclusive
