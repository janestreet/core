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

include%template
  Quickcheckable.Of_quickcheckable1 [@modality portable]
    (Array)
    (struct
      type nonrec 'a t = 'a t

      let to_quickcheckable = to_array
      let of_quickcheckable = of_array
    end)
