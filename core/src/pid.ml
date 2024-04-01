module Stable = struct
  module V1 = struct
    module Without_containers = struct
      type nonrec t = Int.Stable.V1.t [@@deriving compare, equal, stable_witness]

      exception Pid_must_be_positive of Int.Stable.V1.t [@@deriving sexp]

      let ensure i = if i <= 0 then raise (Pid_must_be_positive i) else i

      include
        Sexpable.Stable.Of_sexpable.V1
          (Int.Stable.V1)
          (struct
            type t = Int.Stable.V1.t

            let to_sexpable = Fn.id
            let of_sexpable = ensure
          end)

      include
        Binable.Stable.Of_binable.V1 [@alert "-legacy"]
          (Int.Stable.V1)
          (struct
            type t = Int.Stable.V1.t

            let to_binable = Fn.id
            let of_binable = ensure
          end)

      include (val Comparator.Stable.V1.make ~compare ~sexp_of_t)
    end

    include Comparable.Stable.V1.With_stable_witness.Make (Without_containers)
    include Without_containers
  end

  module Latest = V1
end

open! Import
include Stable.Latest.Without_containers

type t = int [@@deriving hash]

let of_int i = ensure i
let to_int = Fn.id
let of_string string = ensure (Int.of_string string)
let to_string = Int.to_string
let init = of_int 1

include
  Quickcheckable.Of_quickcheckable_filtered
    (Int)
    (struct
      type nonrec t = t

      let of_quickcheckable n = Option.some_if (n > 0) n
      let to_quickcheckable = to_int
    end)

include Identifiable.Make_using_comparator (struct
  type nonrec t = t [@@deriving bin_io, compare, hash, sexp]
  type nonrec comparator_witness = comparator_witness

  let comparator = comparator
  let of_string = of_string
  let to_string = to_string
  let module_name = "Core.Pid"
end)
