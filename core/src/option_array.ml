open! Import
open! Base_quickcheck.Export
include Base.Option_array

module Array_of_options = struct
  type 'a t = 'a option array [@@deriving sexp, bin_io, quickcheck ~portable]
end

include%template
  Binable.Of_binable1_without_uuid [@modality portable] [@alert "-legacy"]
    (Array_of_options)
    (struct
      type nonrec 'a t = 'a t

      let to_binable = to_array
      let of_binable = of_array
    end)

include%template
  Quickcheckable.Of_quickcheckable1 [@modality portable]
    (Array_of_options)
    (struct
      type nonrec 'a t = 'a t

      let to_quickcheckable = to_array
      let of_quickcheckable = of_array
    end)
