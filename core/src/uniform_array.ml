open! Import
include Base.Uniform_array

include%template
  Binable.Of_binable1_without_uuid [@modality portable] [@mode local] [@alert "-legacy"]
    (Array)
    (struct
      type nonrec 'a t = 'a t

      let to_binable = to_array
      let%template[@mode local] to_binable = (to_array [@alloc stack])
      let of_binable = of_array
    end)

let stable_witness : 'a Stable_witness.t -> 'a t Stable_witness.t =
  fun stable_witness_a ->
  Stable_witness.of_serializable (stable_witness_array stable_witness_a) of_array to_array
;;

include%template
  Quickcheckable.Of_quickcheckable1 [@modality portable]
    (Array)
    (struct
      type nonrec 'a t = 'a t

      let to_quickcheckable = to_array
      let of_quickcheckable = of_array
    end)
