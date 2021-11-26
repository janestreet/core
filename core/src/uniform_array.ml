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
