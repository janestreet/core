open! Import

module T = Core_time.Make (Time) (Time)

(* Previous versions rendered hash-based containers using float serialization rather than
   time serialization, so when reading hash-based containers in we accept either
   serialization. *)
include Hashable.Make_binable (struct
    type t = Time.t [@@deriving bin_io, compare, hash]

    let sexp_of_t = T.sexp_of_t

    let t_of_sexp sexp =
      match Float.t_of_sexp sexp with
      | float       -> Time.of_span_since_epoch (Time.Span.of_sec float)
      | exception _ -> T.t_of_sexp sexp
  end)

module Span = struct
  include Time.Span

  let arg_type = T.Span.arg_type
end

module Ofday = struct
  include Time.Ofday

  module Zoned = T.Ofday.Zoned

  let now = T.Ofday.now
  let arg_type = T.Ofday.arg_type
end

module Zone = struct
  include (
    T.Zone : module type of struct include T.Zone end
    with module Index := T.Zone.Index
    with type   t     := T.Zone.t
  )

  include Time.Zone
end

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
    include Time.Stable.Ofday

    module Zoned = struct
      module V1 = struct
        open T.Ofday.Zoned
        type nonrec t = t [@@deriving hash]
        let compare = With_nonchronological_compare.compare

        module Bin_repr = struct
          type t =
            { ofday : Time.Stable.Ofday.V1.t;
              zone  : Timezone.Stable.V1.t;
            } [@@deriving bin_io]
        end

        include (Binable.Of_binable_without_uuid [@alert "-legacy"]) (Bin_repr) (struct
            type nonrec t = t

            let to_binable t : Bin_repr.t =
              { ofday = ofday t; zone = zone t }

            let of_binable (repr : Bin_repr.t) =
              create repr.ofday repr.zone
          end)

        type sexp_repr = Time.Stable.Ofday.V1.t * Timezone.Stable.V1.t
        [@@deriving sexp]

        let sexp_of_t t = [%sexp_of: sexp_repr] (ofday t, zone t)

        let t_of_sexp sexp =
          let (ofday, zone) = [%of_sexp: sexp_repr] sexp in
          create ofday zone
        ;;
      end
    end
  end

  module Zone = Timezone.Stable
end

include (
  T : module type of struct include T end
  with module Table                       := T.Table
  with module Hash_set                    := T.Hash_set
  with module Hash_queue                  := T.Hash_queue
  with module Span                        := T.Span
  with module Ofday                       := T.Ofday
  with module Replace_polymorphic_compare := T.Replace_polymorphic_compare
  with module Date_and_ofday              := T.Date_and_ofday
  with module Zone                        := T.Zone
  with type   underlying                  := T.underlying
  with type   t                           := T.t
  with type   comparator_witness          := T.comparator_witness
)

include (
  Time : module type of struct include Time end
  with module Ofday  := Time.Ofday
  with module Span   := Time.Span
  with module Zone   := Time.Zone
  with module Stable := Time.Stable
)

let to_string     = T.to_string
let of_string     = T.of_string
let of_string_gen = T.of_string_gen
