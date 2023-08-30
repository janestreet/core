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

  module V2 (Stable_format : sig
    type t [@@deriving bin_io, sexp]
  end) (M : sig
    type t [@@deriving compare]

    val to_stable_format : t -> Stable_format.t
    val of_stable_format : Stable_format.t -> t
    val caller_identity : Bin_shape.Uuid.t
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
      include Binable.Of_binable.V2 (Stable_format) (T2)
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

  module V2 (Stable_format : sig
    type 'a t [@@deriving bin_io, sexp]
  end) (M : sig
    type 'a t [@@deriving compare]

    val map : 'a t -> f:('a -> 'b) -> 'b t
    val to_stable_format : 'a t -> 'a Stable_format.t
    val of_stable_format : 'a Stable_format.t -> 'a t
    val caller_identity : Bin_shape.Uuid.t
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
    include Binable.Of_binable1.V2 (Stable_format) (T)
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

  module V2 (Stable_format : sig
    type ('a1, 'a2) t [@@deriving bin_io, sexp]
  end) (M : sig
    type ('a1, 'a2) t [@@deriving compare]

    val map : ('a1, 'a2) t -> f1:('a1 -> 'b1) -> f2:('a2 -> 'b2) -> ('b1, 'b2) t
    val to_stable_format : ('a1, 'a2) t -> ('a1, 'a2) Stable_format.t
    val of_stable_format : ('a1, 'a2) Stable_format.t -> ('a1, 'a2) t
    val caller_identity : Bin_shape.Uuid.t
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
    include Binable.Of_binable2.V2 (Stable_format) (T)
  end
end

module Of_stable_format3 = struct
  module V1 (Stable_format : sig
    type ('a1, 'a2, 'a3) t [@@deriving bin_io, sexp]
  end) (M : sig
    type ('a1, 'a2, 'a3) t [@@deriving compare]

    val map
      :  ('a1, 'a2, 'a3) t
      -> f1:('a1 -> 'b1)
      -> f2:('a2 -> 'b2)
      -> f3:('a3 -> 'b3)
      -> ('b1, 'b2, 'b3) t

    val to_stable_format : ('a1, 'a2, 'a3) t -> ('a1, 'a2, 'a3) Stable_format.t
    val of_stable_format : ('a1, 'a2, 'a3) Stable_format.t -> ('a1, 'a2, 'a3) t
  end) : S3 with type ('a1, 'a2, 'a3) t = ('a1, 'a2, 'a3) M.t = struct
    module T = struct
      include M

      let to_sexpable = to_stable_format
      let of_sexpable = of_stable_format
      let to_binable = to_stable_format
      let of_binable = of_stable_format
    end

    include T
    include Sexpable.Of_sexpable3.V1 (Stable_format) (T)
    include Binable.Of_binable3.V1 [@alert "-legacy"] (Stable_format) (T)
  end

  module V2 (Stable_format : sig
    type ('a1, 'a2, 'a3) t [@@deriving bin_io, sexp]
  end) (M : sig
    type ('a1, 'a2, 'a3) t [@@deriving compare]

    val map
      :  ('a1, 'a2, 'a3) t
      -> f1:('a1 -> 'b1)
      -> f2:('a2 -> 'b2)
      -> f3:('a3 -> 'b3)
      -> ('b1, 'b2, 'b3) t

    val to_stable_format : ('a1, 'a2, 'a3) t -> ('a1, 'a2, 'a3) Stable_format.t
    val of_stable_format : ('a1, 'a2, 'a3) Stable_format.t -> ('a1, 'a2, 'a3) t
    val caller_identity : Bin_shape.Uuid.t
  end) : S3 with type ('a1, 'a2, 'a3) t = ('a1, 'a2, 'a3) M.t = struct
    module T = struct
      include M

      let to_sexpable = to_stable_format
      let of_sexpable = of_stable_format
      let to_binable = to_stable_format
      let of_binable = of_stable_format
    end

    include T
    include Sexpable.Of_sexpable3.V1 (Stable_format) (T)
    include Binable.Of_binable3.V2 (Stable_format) (T)
  end
end

module With_stable_witness = struct
  module Of_stable_format = struct
    module V1 (Stable_format : sig
      type t [@@deriving bin_io, sexp, stable_witness]
    end) (M : sig
      type t [@@deriving compare]

      val to_stable_format : t -> Stable_format.t
      val of_stable_format : Stable_format.t -> t
    end) : With_stable_witness.S0 with type t = M.t = struct
      include Of_stable_format.V1 (Stable_format) (M)

      let stable_witness =
        Stable_witness.of_serializable
          Stable_format.stable_witness
          M.of_stable_format
          M.to_stable_format
      ;;
    end

    module V2 (Stable_format : sig
      type t [@@deriving bin_io, sexp, stable_witness]
    end) (M : sig
      type t [@@deriving compare]

      val to_stable_format : t -> Stable_format.t
      val of_stable_format : Stable_format.t -> t
      val caller_identity : Bin_shape.Uuid.t
    end) : With_stable_witness.S0 with type t = M.t = struct
      include Of_stable_format.V2 (Stable_format) (M)

      let stable_witness =
        Stable_witness.of_serializable
          Stable_format.stable_witness
          M.of_stable_format
          M.to_stable_format
      ;;
    end
  end

  module Of_stable_format1 = struct
    module V1 (Stable_format : sig
      type 'a t [@@deriving bin_io, sexp, stable_witness]
    end) (M : sig
      type 'a t [@@deriving compare]

      val map : 'a t -> f:('a -> 'b) -> 'b t
      val to_stable_format : 'a t -> 'a Stable_format.t
      val of_stable_format : 'a Stable_format.t -> 'a t
    end) : With_stable_witness.S1 with type 'a t = 'a M.t = struct
      include Of_stable_format1.V1 (Stable_format) (M)

      let stable_witness (type a) : a Stable_witness.t -> a M.t Stable_witness.t =
        fun witness ->
        let module Stable_witness = Stable_witness.Of_serializable1 (Stable_format) (M) in
        Stable_witness.of_serializable
          Stable_format.stable_witness
          M.of_stable_format
          M.to_stable_format
          witness
      ;;
    end

    module V2 (Stable_format : sig
      type 'a t [@@deriving bin_io, sexp, stable_witness]
    end) (M : sig
      type 'a t [@@deriving compare]

      val map : 'a t -> f:('a -> 'b) -> 'b t
      val to_stable_format : 'a t -> 'a Stable_format.t
      val of_stable_format : 'a Stable_format.t -> 'a t
      val caller_identity : Bin_shape.Uuid.t
    end) : With_stable_witness.S1 with type 'a t = 'a M.t = struct
      include Of_stable_format1.V2 (Stable_format) (M)

      let stable_witness (type a) : a Stable_witness.t -> a M.t Stable_witness.t =
        fun witness ->
        let module Stable_witness = Stable_witness.Of_serializable1 (Stable_format) (M) in
        Stable_witness.of_serializable
          Stable_format.stable_witness
          M.of_stable_format
          M.to_stable_format
          witness
      ;;
    end
  end

  module Of_stable_format2 = struct
    module V1 (Stable_format : sig
      type ('a1, 'a2) t [@@deriving bin_io, sexp, stable_witness]
    end) (M : sig
      type ('a1, 'a2) t [@@deriving compare]

      val map : ('a1, 'a2) t -> f1:('a1 -> 'b1) -> f2:('a2 -> 'b2) -> ('b1, 'b2) t
      val to_stable_format : ('a1, 'a2) t -> ('a1, 'a2) Stable_format.t
      val of_stable_format : ('a1, 'a2) Stable_format.t -> ('a1, 'a2) t
    end) : With_stable_witness.S2 with type ('a1, 'a2) t = ('a1, 'a2) M.t = struct
      include Of_stable_format2.V1 (Stable_format) (M)

      let stable_witness witness =
        let module Stable_witness = Stable_witness.Of_serializable2 (Stable_format) (M) in
        Stable_witness.of_serializable
          Stable_format.stable_witness
          M.of_stable_format
          M.to_stable_format
          witness
      ;;
    end

    module V2 (Stable_format : sig
      type ('a1, 'a2) t [@@deriving bin_io, sexp, stable_witness]
    end) (M : sig
      type ('a1, 'a2) t [@@deriving compare]

      val map : ('a1, 'a2) t -> f1:('a1 -> 'b1) -> f2:('a2 -> 'b2) -> ('b1, 'b2) t
      val to_stable_format : ('a1, 'a2) t -> ('a1, 'a2) Stable_format.t
      val of_stable_format : ('a1, 'a2) Stable_format.t -> ('a1, 'a2) t
      val caller_identity : Bin_shape.Uuid.t
    end) : With_stable_witness.S2 with type ('a1, 'a2) t = ('a1, 'a2) M.t = struct
      include Of_stable_format2.V2 (Stable_format) (M)

      let stable_witness witness =
        let module Stable_witness = Stable_witness.Of_serializable2 (Stable_format) (M) in
        Stable_witness.of_serializable
          Stable_format.stable_witness
          M.of_stable_format
          M.to_stable_format
          witness
      ;;
    end
  end

  module Of_stable_format3 = struct
    module V1 (Stable_format : sig
      type ('a1, 'a2, 'a3) t [@@deriving bin_io, sexp, stable_witness]
    end) (M : sig
      type ('a1, 'a2, 'a3) t [@@deriving compare]

      val map
        :  ('a1, 'a2, 'a3) t
        -> f1:('a1 -> 'b1)
        -> f2:('a2 -> 'b2)
        -> f3:('a3 -> 'b3)
        -> ('b1, 'b2, 'b3) t

      val to_stable_format : ('a1, 'a2, 'a3) t -> ('a1, 'a2, 'a3) Stable_format.t
      val of_stable_format : ('a1, 'a2, 'a3) Stable_format.t -> ('a1, 'a2, 'a3) t
    end) : With_stable_witness.S3 with type ('a1, 'a2, 'a3) t = ('a1, 'a2, 'a3) M.t =
    struct
      include Of_stable_format3.V1 (Stable_format) (M)

      let stable_witness witness =
        let module Stable_witness = Stable_witness.Of_serializable3 (Stable_format) (M) in
        Stable_witness.of_serializable
          Stable_format.stable_witness
          M.of_stable_format
          M.to_stable_format
          witness
      ;;
    end

    module V2 (Stable_format : sig
      type ('a1, 'a2, 'a3) t [@@deriving bin_io, sexp, stable_witness]
    end) (M : sig
      type ('a1, 'a2, 'a3) t [@@deriving compare]

      val map
        :  ('a1, 'a2, 'a3) t
        -> f1:('a1 -> 'b1)
        -> f2:('a2 -> 'b2)
        -> f3:('a3 -> 'b3)
        -> ('b1, 'b2, 'b3) t

      val to_stable_format : ('a1, 'a2, 'a3) t -> ('a1, 'a2, 'a3) Stable_format.t
      val of_stable_format : ('a1, 'a2, 'a3) Stable_format.t -> ('a1, 'a2, 'a3) t
      val caller_identity : Bin_shape.Uuid.t
    end) : With_stable_witness.S3 with type ('a1, 'a2, 'a3) t = ('a1, 'a2, 'a3) M.t =
    struct
      include Of_stable_format3.V2 (Stable_format) (M)

      let stable_witness witness =
        let module Stable_witness = Stable_witness.Of_serializable3 (Stable_format) (M) in
        Stable_witness.of_serializable
          Stable_format.stable_witness
          M.of_stable_format
          M.to_stable_format
          witness
      ;;
    end
  end
end
