open! Import
open! Stable_internal

module Stable = struct
  module V1 = struct
    module Make (M : sig
      val label : string
    end) =
    struct
      type t =
        { index : int
        ; min_index : int
        ; max_index : int
        }
      [@@deriving bin_io, compare, hash, stable_witness]

      let create index ~min ~max =
        if index < min || index > max
        then
          Error.raise_s
            [%message "index out of bounds" (index : int) (min : int) (max : int)]
        else { index; min_index = min; max_index = max }
      ;;

      module For_sexpable = struct
        type t = string * int * string * int * string * int [@@deriving sexp]
      end

      include
        Sexpable.Stable.Of_sexpable.V1
          (For_sexpable)
          (struct
            type nonrec t = t

            let to_sexpable t = M.label, t.index, "of", t.min_index, "to", t.max_index

            let of_sexpable (label, index, of_, min, to_, max) =
              if String.equal label M.label
                 && String.equal of_ "of"
                 && String.equal to_ "to"
              then create index ~min ~max
              else Error.raise_s [%message "invalid sexp for index" ~label:M.label]
            ;;
          end)

      include Comparator.Stable.V1.Make (struct
        type nonrec t = t [@@deriving sexp_of, compare]
      end)

      include Comparable.Stable.V1.With_stable_witness.Make (struct
        type nonrec t = t [@@deriving sexp, compare, bin_io, stable_witness]
        type nonrec comparator_witness = comparator_witness

        let comparator = comparator
      end)
    end
  end
end

open! Std_internal

module type S = Bounded_index_intf.S

module Make (M : sig
  val label : string
  val module_name : string
end) =
struct
  module Stable = struct
    module V1 = Stable.V1.Make (M)
  end

  open Stable.V1

  type t = Stable.V1.t [@@deriving bin_io, compare, hash, sexp]
  type comparator_witness = Stable.V1.comparator_witness

  let create = Stable.V1.create

  let create_all ~min ~max =
    Sequence.unfold ~init:min ~f:(fun index ->
      if index < min || index > max then None else Some (create index ~min ~max, index + 1))
    |> Sequence.to_list
  ;;

  let index t = t.index
  let max_index t = t.max_index
  let min_index t = t.min_index
  let zero_based_index t = index t - min_index t
  let num_indexes t = max_index t - min_index t + 1

  include Sexpable.To_stringable (struct
    type nonrec t = t [@@deriving sexp]
  end)

  include Identifiable.Make_using_comparator (struct
    type nonrec t = t [@@deriving bin_io, compare, hash, sexp]
    type nonrec comparator_witness = comparator_witness

    let comparator = comparator
    let of_string = of_string
    let to_string = to_string
    let module_name = M.module_name
  end)
end
