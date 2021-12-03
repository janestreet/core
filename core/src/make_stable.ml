open Stable_module_types
module Binable = Binable.Stable
module Comparator = Comparator.Stable
module Sexpable = Sexpable.Stable

module Of_stable_format = struct
  module V1 (Stable_format : sig
      type t [@@deriving bin_io, sexp]
    end) (M : sig
            type t [@@deriving compare]

            val to_stable_format : t -> Stable_format.t
            val of_stable_format : Stable_format.t -> t
          end) : S0 with type t = M.t = struct
    module T1 = struct
      module T2 = struct
        include M

        let to_sexpable = to_stable_format
        let of_sexpable = of_stable_format
        let to_binable = to_stable_format
        let of_binable = of_stable_format
      end

      include T2
      include Sexpable.Of_sexpable.V1 (Stable_format) (T2)
      include Binable.Of_binable.V1 [@alert "-legacy"] (Stable_format) (T2)
    end

    include T1
    include Comparator.V1.Make (T1)
  end
end

module Of_stable_format1 = struct
  module V1 (Stable_format : sig
      type 'a t [@@deriving bin_io, sexp]
    end) (M : sig
            type 'a t [@@deriving compare]

            val map : 'a t -> f:('a -> 'b) -> 'b t
            val to_stable_format : 'a t -> 'a Stable_format.t
            val of_stable_format : 'a Stable_format.t -> 'a t
          end) : S1 with type 'a t = 'a M.t = struct
    module T = struct
      include M

      let to_sexpable = to_stable_format
      let of_sexpable = of_stable_format
      let to_binable = to_stable_format
      let of_binable = of_stable_format
    end

    include T
    include Sexpable.Of_sexpable1.V1 (Stable_format) (T)
    include Binable.Of_binable1.V1 [@alert "-legacy"] (Stable_format) (T)
  end
end

module Of_stable_format2 = struct
  module V1 (Stable_format : sig
      type ('a1, 'a2) t [@@deriving bin_io, sexp]
    end) (M : sig
            type ('a1, 'a2) t [@@deriving compare]

            val map : ('a1, 'a2) t -> f1:('a1 -> 'b1) -> f2:('a2 -> 'b2) -> ('b1, 'b2) t
            val to_stable_format : ('a1, 'a2) t -> ('a1, 'a2) Stable_format.t
            val of_stable_format : ('a1, 'a2) Stable_format.t -> ('a1, 'a2) t
          end) : S2 with type ('a1, 'a2) t = ('a1, 'a2) M.t = struct
    module T = struct
      include M

      let to_sexpable = to_stable_format
      let of_sexpable = of_stable_format
      let to_binable = to_stable_format
      let of_binable = of_stable_format
    end

    include T
    include Sexpable.Of_sexpable2.V1 (Stable_format) (T)
    include Binable.Of_binable2.V1 [@alert "-legacy"] (Stable_format) (T)
  end
end
