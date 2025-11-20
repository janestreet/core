open! Import

module%template Bin : sig @@ portable
  include Binable0.S [@mode local] with type t := Base.Int63.t
end = struct
  module Bin_emulated = struct
    type t = Base.Int63.Private.Emul.t

    include
      Binable0.Of_binable_without_uuid
        [@mode local]
        [@modality portable]
        [@alert "-legacy"]
        (Int64)
        (struct
          type nonrec t = t

          let of_binable i =
            Base.Int63.Private.Emul.globalize
              (Base.Int63.Private.Emul.W.wrap_exn i) [@nontail]
          ;;

          external unbox_int64
            :  local_ int64
            -> (int64#[@unboxed])
            @@ portable
            = "%unbox_int64"

          external box_int64
            :  (int64#[@unboxed])
            -> (int64[@local_opt])
            @@ portable
            = "%box_int64"

          let[@mode m = (global, local)] to_binable t =
            let i = unbox_int64 (Base.Int63.Private.Emul.W.unwrap t) in
            box_int64 i [@exclave_if_local m]
          ;;
        end)
  end

  module type S = sig @@ portable
    include Binable0.S [@mode local]
  end

  type 'a binable = (module S with type t = 'a)

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
      type t = Base.Int63.t
      [@@deriving compare ~localize, equal ~localize, globalize, hash, sexp, sexp_grammar]

      include Bin

      include (
        Base.Int63 :
        sig
        @@ portable
          include
            Base.Comparable.S
            with type t := t
            with type comparator_witness = Base.Int63.comparator_witness
        end)

      (* This serialization is stable, since it either delegates to [int] or [Int63_emul]. *)
      let stable_witness : t Stable_witness.t = Stable_witness.assert_stable
    end

    include T

    include%template Comparable.Stable.V1.With_stable_witness.Make [@modality portable] (T)
  end
end

(* This [include struct] is required because it lets us shadow [t] when we include
   [Base.Int63] later on. *)
include struct
  type t = Base.Int63.t
end

let typerep_of_t = typerep_of_int63
let typename_of_t = typename_of_int63

include%template
  Identifiable.Extend [@mode local] [@modality portable]
    (Base.Int63)
    (struct
      type nonrec t = t

      include Bin
    end)

include Bin

module Replace_polymorphic_compare : sig @@ portable
  include%template Comparable.Comparisons [@mode local] with type t := t
end =
  Base.Int63

include Base.Int63

include%template Comparable.Validate_with_zero [@modality portable] (Base.Int63)

module Binary = struct
  include Binary

  type nonrec t = t [@@deriving typerep, bin_io ~localize]
end

module Hex = struct
  include Hex

  type nonrec t = t [@@deriving typerep, bin_io ~localize]
end

let quickcheck_generator = Base_quickcheck.Generator.int63
let quickcheck_observer = Base_quickcheck.Observer.int63
let quickcheck_shrinker = Base_quickcheck.Shrinker.int63
let gen_incl = Base_quickcheck.Generator.int63_inclusive
let gen_uniform_incl = Base_quickcheck.Generator.int63_uniform_inclusive
let gen_log_incl = Base_quickcheck.Generator.int63_log_inclusive
let gen_log_uniform_incl = Base_quickcheck.Generator.int63_log_uniform_inclusive
