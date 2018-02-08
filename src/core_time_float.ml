open! Import
open Core_kernel.Core_kernel_private

module T = Core_time.Make (Time_float0) (Time_float)

(* Previous versions rendered hash-based containers using float serialization rather than
   time serialization, so when reading hash-based containers in we accept either
   serialization. *)
include Hashable.Make_binable (struct
    include T

    let t_of_sexp sexp =
      match Float.t_of_sexp sexp with
      | float       -> of_span_since_epoch (Span.of_sec float)
      | exception _ -> t_of_sexp sexp
  end)

module Stable = struct
  module V1 = struct
    (* There is no simple, pristine implementation of "stable time", and in fact
       [Time.Stable.V1] has always called out to "unstable" string conversions.
       For a complicated "stable" story like this, we rely on comprehensive tests
       of stability; see [lib/core/test/src/test_time.ml]. *)
    include T
  end

  module With_utc_sexp = struct
    module V1 = struct
      module C = struct
        include (V1 : module type of V1
                 with module Map := V1.Map
                  and module Set := V1.Set)

        let sexp_of_t t = sexp_of_t_abs t ~zone:Zone.utc
      end
      include C

      module Map = Map.Make_binable_using_comparator (C)
      module Set = Set.Make_binable_using_comparator (C)
    end
    module V2 = struct
      module C = struct
        include Time.Stable.With_utc_sexp.V2

        type comparator_witness = T.comparator_witness

        let comparator = T.comparator
      end
      include C
      include Comparable.Stable.V1.Make (C)
    end
  end

  module With_t_of_sexp_abs = struct
    module V1 = struct
      include (V1 : module type of V1 with module Map := V1.Map and module Set := V1.Set)

      let t_of_sexp = t_of_sexp_abs
    end
  end

  module Span = Time.Stable.Span

  module Ofday = struct
    include Time_float.Stable.Ofday

    module Zoned = struct
      module V1 = struct
        open T.Ofday.Zoned
        type nonrec t = t
        let compare = With_nonchronological_compare.compare

        module Bin_repr = struct
          type t =
            { ofday : Time_float.Stable.Ofday.V1.t;
              zone  : Core_zone.Stable.V1.t;
            } [@@deriving bin_io]
        end

        include Binable.Of_binable (Bin_repr) (struct
            type nonrec t = t

            let to_binable t : Bin_repr.t =
              { ofday = ofday t; zone = zone t }

            let of_binable (repr : Bin_repr.t) =
              create repr.ofday repr.zone
          end)

        type sexp_repr = Time_float.Stable.Ofday.V1.t * Core_zone.Stable.V1.t
        [@@deriving sexp]

        let sexp_of_t t = [%sexp_of: sexp_repr] (ofday t, zone t)

        let t_of_sexp sexp =
          let (ofday, zone) = [%of_sexp: sexp_repr] sexp in
          create ofday zone
        ;;
      end
    end
  end

  module Zone = Core_zone.Stable
end

include (T : module type of struct include T end
         with module Table      := T.Table
          and module Hash_set   := T.Hash_set
          and module Hash_queue := T.Hash_queue)
