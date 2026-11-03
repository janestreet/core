open! Import
include Base.Uniform_array

include
  Binable.Of_binable1_without_uuid [@alert "-legacy"]
    (Array)
    (struct
      type nonrec 'a t = 'a t

      let to_binable = to_array
      let of_binable = of_array
    end)

let stable_witness : 'a Stable_witness.t -> 'a t Stable_witness.t =
  fun stable_witness_a ->
  Stable_witness.of_serializable (stable_witness_array stable_witness_a) of_array to_array
;;

include
  Quickcheckable.Of_quickcheckable1
    (Array)
    (struct
      type nonrec 'a t = 'a t

      let to_quickcheckable = to_array
      let of_quickcheckable = of_array
    end)
