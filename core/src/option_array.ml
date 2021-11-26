open! Import
include Base.Option_array

include
  Binable.Of_binable1_without_uuid [@alert "-legacy"]
    (struct
      type 'a t = 'a option array [@@deriving sexp, bin_io]
    end)
    (struct
      type nonrec 'a t = 'a t

      let to_binable = to_array
      let of_binable = of_array
    end)
