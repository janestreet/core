open Stable_module_types
module Binable = Binable.Stable
module Comparator = Comparator.Stable
module Sexpable = Sexpable.Stable

module%template Of_stable_format = struct
  [@@@mode.default m = (global, local)]

  module%template.portable
    [@modality p] V1
      (Stable_format : sig
         type t [@@deriving (bin_io [@mode m]), sexp]
       end)
      (M : sig
         type t [@@deriving compare [@mode m]]

         val to_stable_format : t @ m -> Stable_format.t @ m [@@mode m = (m, global)]
         val of_stable_format : Stable_format.t -> t
       end) : S0 [@mode m] [@modality p] with type t = M.t = struct
    module T1 = struct
      module T2 = struct
        include M

        let to_sexpable = to_stable_format
        let of_sexpable = of_stable_format
        let[@mode m = (m, global)] to_binable = (to_stable_format [@mode m])
        let of_binable = of_stable_format
      end

      include T2
      include Sexpable.Of_sexpable.V1 [@modality p] (Stable_format) (T2)

      include
        Binable.Of_binable.V1 [@modality p] [@mode m] [@alert "-legacy"]
          (Stable_format)
          (T2)
    end

    include T1
    include Comparator.V1.Make [@modality p] (T1)
  end

  module%template.portable
    [@modality p] V2
      (Stable_format : sig
         type t [@@deriving (bin_io [@mode m]), sexp]
       end)
      (M : sig
         type t [@@deriving compare [@mode m]]

         val to_stable_format : t @ m -> Stable_format.t @ m [@@mode m = (m, global)]
         val of_stable_format : Stable_format.t -> t
         val caller_identity : Bin_shape.Uuid.t
       end) : S0 [@mode m] [@modality p] with type t = M.t = struct
    module T1 = struct
      module T2 = struct
        include M

        let to_sexpable = to_stable_format
        let of_sexpable = of_stable_format
        let[@mode m = (m, global)] to_binable = (to_stable_format [@mode m])
        let of_binable = of_stable_format
      end

      include T2
      include Sexpable.Of_sexpable.V1 [@modality p] (Stable_format) (T2)
      include Binable.Of_binable.V2 [@modality p] [@mode m] (Stable_format) (T2)
    end

    include T1
    include Comparator.V1.Make [@modality p] (T1)
  end
end

module%template Of_stable_format1 = struct
  [@@@mode.default m = (global, local)]

  module%template.portable
    [@modality p] V1
      (Stable_format : sig
         type 'a t [@@deriving (bin_io [@mode m]), sexp]
       end)
      (M : sig
         type 'a t [@@deriving compare [@mode m]]

         val map : 'a t -> f:('a -> 'b) -> 'b t

         val to_stable_format : 'a t @ m -> 'a Stable_format.t @ m
         [@@mode m = (m, global)]

         val of_stable_format : 'a Stable_format.t -> 'a t
       end) : S1 [@mode m] with type 'a t = 'a M.t = struct
    module T = struct
      include M

      let to_sexpable = to_stable_format
      let of_sexpable = of_stable_format
      let[@mode m = (m, global)] to_binable = (to_stable_format [@mode m])
      let of_binable = of_stable_format
    end

    include T
    include Sexpable.Of_sexpable1.V1 [@modality p] (Stable_format) (T)

    include
      Binable.Of_binable1.V1 [@modality p] [@mode m] [@alert "-legacy"]
        (Stable_format)
        (T)
  end

  module%template.portable
    [@modality p] V2
      (Stable_format : sig
         type 'a t [@@deriving (bin_io [@mode m]), sexp]
       end)
      (M : sig
         type 'a t [@@deriving compare [@mode m]]

         val map : 'a t -> f:('a -> 'b) -> 'b t

         val to_stable_format : 'a t @ m -> 'a Stable_format.t @ m
         [@@mode m = (m, global)]

         val of_stable_format : 'a Stable_format.t -> 'a t
         val caller_identity : Bin_shape.Uuid.t
       end) : S1 [@mode m] with type 'a t = 'a M.t = struct
    module T = struct
      include M

      let to_sexpable = to_stable_format
      let of_sexpable = of_stable_format
      let[@mode m = (m, global)] to_binable = (to_stable_format [@mode m])
      let of_binable = of_stable_format
    end

    include T
    include Sexpable.Of_sexpable1.V1 [@modality p] (Stable_format) (T)
    include Binable.Of_binable1.V2 [@modality p] [@mode m] (Stable_format) (T)
  end
end

module%template Of_stable_format2 = struct
  [@@@mode.default m = (global, local)]

  module%template.portable
    [@modality p] V1
      (Stable_format : sig
         type ('a1, 'a2) t [@@deriving (bin_io [@mode m]), sexp]
       end)
      (M : sig
         type ('a1, 'a2) t [@@deriving compare [@mode m]]

         val map : ('a1, 'a2) t -> f1:('a1 -> 'b1) -> f2:('a2 -> 'b2) -> ('b1, 'b2) t

         val to_stable_format : ('a1, 'a2) t @ m -> ('a1, 'a2) Stable_format.t @ m
         [@@mode m = (m, global)]

         val of_stable_format : ('a1, 'a2) Stable_format.t -> ('a1, 'a2) t
       end) : S2 [@mode m] with type ('a1, 'a2) t = ('a1, 'a2) M.t = struct
    module T = struct
      include M

      let to_sexpable = to_stable_format
      let of_sexpable = of_stable_format
      let[@mode m = (m, global)] to_binable = (to_stable_format [@mode m])
      let of_binable = of_stable_format
    end

    include T
    include Sexpable.Of_sexpable2.V1 [@modality p] (Stable_format) (T)

    include
      Binable.Of_binable2.V1 [@modality p] [@mode m] [@alert "-legacy"]
        (Stable_format)
        (T)
  end

  module%template.portable
    [@modality p] V2
      (Stable_format : sig
         type ('a1, 'a2) t [@@deriving (bin_io [@mode m]), sexp]
       end)
      (M : sig
         type ('a1, 'a2) t [@@deriving compare [@mode m]]

         val map : ('a1, 'a2) t -> f1:('a1 -> 'b1) -> f2:('a2 -> 'b2) -> ('b1, 'b2) t

         val to_stable_format : ('a1, 'a2) t @ m -> ('a1, 'a2) Stable_format.t @ m
         [@@mode m = (m, global)]

         val of_stable_format : ('a1, 'a2) Stable_format.t -> ('a1, 'a2) t
         val caller_identity : Bin_shape.Uuid.t
       end) : S2 [@mode m] with type ('a1, 'a2) t = ('a1, 'a2) M.t = struct
    module T = struct
      include M

      let to_sexpable = to_stable_format
      let of_sexpable = of_stable_format
      let[@mode m = (m, global)] to_binable = (to_stable_format [@mode m])
      let of_binable = of_stable_format
    end

    include T
    include Sexpable.Of_sexpable2.V1 [@modality p] (Stable_format) (T)
    include Binable.Of_binable2.V2 [@modality p] [@mode m] (Stable_format) (T)
  end
end

module%template Of_stable_format3 = struct
  [@@@mode.default m = (global, local)]

  module%template.portable
    [@modality p] V1
      (Stable_format : sig
         type ('a1, 'a2, 'a3) t [@@deriving (bin_io [@mode m]), sexp]
       end)
      (M : sig
         type ('a1, 'a2, 'a3) t [@@deriving compare [@mode m]]

         val map
           :  ('a1, 'a2, 'a3) t
           -> f1:('a1 -> 'b1)
           -> f2:('a2 -> 'b2)
           -> f3:('a3 -> 'b3)
           -> ('b1, 'b2, 'b3) t

         val to_stable_format
           :  ('a1, 'a2, 'a3) t @ m
           -> ('a1, 'a2, 'a3) Stable_format.t @ m
         [@@mode m = (m, global)]

         val of_stable_format : ('a1, 'a2, 'a3) Stable_format.t -> ('a1, 'a2, 'a3) t
       end) : S3 [@mode m] with type ('a1, 'a2, 'a3) t = ('a1, 'a2, 'a3) M.t = struct
    module T = struct
      include M

      let to_sexpable = to_stable_format
      let of_sexpable = of_stable_format
      let[@mode m = (m, global)] to_binable = (to_stable_format [@mode m])
      let of_binable = of_stable_format
    end

    include T
    include Sexpable.Of_sexpable3.V1 [@modality p] (Stable_format) (T)

    include
      Binable.Of_binable3.V1 [@modality p] [@mode m] [@alert "-legacy"]
        (Stable_format)
        (T)
  end

  module%template.portable
    [@modality p] V2
      (Stable_format : sig
         type ('a1, 'a2, 'a3) t [@@deriving (bin_io [@mode m]), sexp]
       end)
      (M : sig
         type ('a1, 'a2, 'a3) t [@@deriving compare [@mode m]]

         val map
           :  ('a1, 'a2, 'a3) t
           -> f1:('a1 -> 'b1)
           -> f2:('a2 -> 'b2)
           -> f3:('a3 -> 'b3)
           -> ('b1, 'b2, 'b3) t

         val to_stable_format
           :  ('a1, 'a2, 'a3) t @ m
           -> ('a1, 'a2, 'a3) Stable_format.t @ m
         [@@mode m = (m, global)]

         val of_stable_format : ('a1, 'a2, 'a3) Stable_format.t -> ('a1, 'a2, 'a3) t
         val caller_identity : Bin_shape.Uuid.t
       end) : S3 [@mode m] with type ('a1, 'a2, 'a3) t = ('a1, 'a2, 'a3) M.t = struct
    module T = struct
      include M

      let to_sexpable = to_stable_format
      let of_sexpable = of_stable_format
      let[@mode m = (m, global)] to_binable = (to_stable_format [@mode m])
      let of_binable = of_stable_format
    end

    include T
    include Sexpable.Of_sexpable3.V1 [@modality p] (Stable_format) (T)
    include Binable.Of_binable3.V2 [@modality p] [@mode m] (Stable_format) (T)
  end
end

module With_stable_witness = struct
  module%template Of_stable_format = struct
    [@@@mode.default m = (global, local)]

    module%template.portable
      [@modality p] V1
        (Stable_format : sig
           type t [@@deriving (bin_io [@mode m]), sexp, stable_witness]
         end)
        (M : sig
           type t [@@deriving compare [@mode m]]

           val to_stable_format : t @ m -> Stable_format.t @ m [@@mode m = (m, global)]
           val of_stable_format : Stable_format.t -> t
         end) : With_stable_witness.S0 [@mode m] [@modality p] with type t = M.t = struct
      include Of_stable_format.V1 [@modality p] [@mode m] (Stable_format) (M)

      let stable_witness =
        Stable_witness.of_serializable
          Stable_format.stable_witness
          M.of_stable_format
          M.to_stable_format
      ;;
    end

    module%template.portable
      [@modality p] V2
        (Stable_format : sig
           type t [@@deriving (bin_io [@mode m]), sexp, stable_witness]
         end)
        (M : sig
           type t [@@deriving compare [@mode m]]

           val to_stable_format : t @ m -> Stable_format.t @ m [@@mode m = (m, global)]
           val of_stable_format : Stable_format.t -> t
           val caller_identity : Bin_shape.Uuid.t
         end) : With_stable_witness.S0 [@mode m] [@modality p] with type t = M.t = struct
      include Of_stable_format.V2 [@modality p] [@mode m] (Stable_format) (M)

      let stable_witness =
        Stable_witness.of_serializable
          Stable_format.stable_witness
          M.of_stable_format
          M.to_stable_format
      ;;
    end
  end

  module%template Of_stable_format1 = struct
    [@@@mode.default m = (global, local)]

    module%template.portable
      [@modality p] V1
        (Stable_format : sig
           type 'a t [@@deriving (bin_io [@mode m]), sexp, stable_witness]
         end)
        (M : sig
           type 'a t [@@deriving compare [@mode m]]

           val map : 'a t -> f:('a -> 'b) -> 'b t

           val to_stable_format : 'a t @ m -> 'a Stable_format.t @ m
           [@@mode m = (m, global)]

           val of_stable_format : 'a Stable_format.t -> 'a t
         end) : With_stable_witness.S1 [@mode m] with type 'a t = 'a M.t = struct
      include Of_stable_format1.V1 [@modality p] [@mode m] (Stable_format) (M)
      module Witness = Stable_witness.Of_serializable1 (Stable_format) (M)

      let stable_witness (type a) : a Stable_witness.t -> a M.t Stable_witness.t =
        fun witness ->
        Witness.of_serializable
          Stable_format.stable_witness
          M.of_stable_format
          M.to_stable_format
          witness
      ;;
    end

    module%template.portable
      [@modality p] V2
        (Stable_format : sig
           type 'a t [@@deriving (bin_io [@mode m]), sexp, stable_witness]
         end)
        (M : sig
           type 'a t [@@deriving compare [@mode m]]

           val map : 'a t -> f:('a -> 'b) -> 'b t

           val to_stable_format : 'a t @ m -> 'a Stable_format.t @ m
           [@@mode m = (m, global)]

           val of_stable_format : 'a Stable_format.t -> 'a t
           val caller_identity : Bin_shape.Uuid.t
         end) : With_stable_witness.S1 [@mode m] with type 'a t = 'a M.t = struct
      include Of_stable_format1.V2 [@modality p] [@mode m] (Stable_format) (M)
      module Witness = Stable_witness.Of_serializable1 (Stable_format) (M)

      let stable_witness (type a) : a Stable_witness.t -> a M.t Stable_witness.t =
        fun witness ->
        Witness.of_serializable
          Stable_format.stable_witness
          M.of_stable_format
          M.to_stable_format
          witness
      ;;
    end
  end

  module%template Of_stable_format2 = struct
    [@@@mode.default m = (global, local)]

    module%template.portable
      [@modality p] V1
        (Stable_format : sig
           type ('a1, 'a2) t [@@deriving (bin_io [@mode m]), sexp, stable_witness]
         end)
        (M : sig
           type ('a1, 'a2) t [@@deriving compare [@mode m]]

           val map : ('a1, 'a2) t -> f1:('a1 -> 'b1) -> f2:('a2 -> 'b2) -> ('b1, 'b2) t

           val to_stable_format : ('a1, 'a2) t @ m -> ('a1, 'a2) Stable_format.t @ m
           [@@mode m = (m, global)]

           val of_stable_format : ('a1, 'a2) Stable_format.t -> ('a1, 'a2) t
         end) : With_stable_witness.S2 [@mode m] with type ('a1, 'a2) t = ('a1, 'a2) M.t =
    struct
      include Of_stable_format2.V1 [@modality p] [@mode m] (Stable_format) (M)
      module Witness = Stable_witness.Of_serializable2 (Stable_format) (M)

      let stable_witness witness =
        Witness.of_serializable
          Stable_format.stable_witness
          M.of_stable_format
          M.to_stable_format
          witness
      ;;
    end

    module%template.portable
      [@modality p] V2
        (Stable_format : sig
           type ('a1, 'a2) t [@@deriving (bin_io [@mode m]), sexp, stable_witness]
         end)
        (M : sig
           type ('a1, 'a2) t [@@deriving compare [@mode m]]

           val map : ('a1, 'a2) t -> f1:('a1 -> 'b1) -> f2:('a2 -> 'b2) -> ('b1, 'b2) t

           val to_stable_format : ('a1, 'a2) t @ m -> ('a1, 'a2) Stable_format.t @ m
           [@@mode m = (m, global)]

           val of_stable_format : ('a1, 'a2) Stable_format.t -> ('a1, 'a2) t
           val caller_identity : Bin_shape.Uuid.t
         end) : With_stable_witness.S2 [@mode m] with type ('a1, 'a2) t = ('a1, 'a2) M.t =
    struct
      include Of_stable_format2.V2 [@modality p] [@mode m] (Stable_format) (M)
      module Witness = Stable_witness.Of_serializable2 (Stable_format) (M)

      let stable_witness witness =
        Witness.of_serializable
          Stable_format.stable_witness
          M.of_stable_format
          M.to_stable_format
          witness
      ;;
    end
  end

  module%template Of_stable_format3 = struct
    [@@@mode.default m = (global, local)]

    module%template.portable
      [@modality p] V1
        (Stable_format : sig
           type ('a1, 'a2, 'a3) t [@@deriving (bin_io [@mode m]), sexp, stable_witness]
         end)
        (M : sig
           type ('a1, 'a2, 'a3) t [@@deriving compare [@mode m]]

           val map
             :  ('a1, 'a2, 'a3) t
             -> f1:('a1 -> 'b1)
             -> f2:('a2 -> 'b2)
             -> f3:('a3 -> 'b3)
             -> ('b1, 'b2, 'b3) t

           val to_stable_format
             :  ('a1, 'a2, 'a3) t @ m
             -> ('a1, 'a2, 'a3) Stable_format.t @ m
           [@@mode m = (m, global)]

           val of_stable_format : ('a1, 'a2, 'a3) Stable_format.t -> ('a1, 'a2, 'a3) t
         end) :
      With_stable_witness.S3 [@mode m] with type ('a1, 'a2, 'a3) t = ('a1, 'a2, 'a3) M.t =
    struct
      include Of_stable_format3.V1 [@modality p] [@mode m] (Stable_format) (M)
      module Witness = Stable_witness.Of_serializable3 (Stable_format) (M)

      let stable_witness witness =
        Witness.of_serializable
          Stable_format.stable_witness
          M.of_stable_format
          M.to_stable_format
          witness
      ;;
    end

    module%template.portable
      [@modality p] V2
        (Stable_format : sig
           type ('a1, 'a2, 'a3) t [@@deriving (bin_io [@mode m]), sexp, stable_witness]
         end)
        (M : sig
           type ('a1, 'a2, 'a3) t [@@deriving compare [@mode m]]

           val map
             :  ('a1, 'a2, 'a3) t
             -> f1:('a1 -> 'b1)
             -> f2:('a2 -> 'b2)
             -> f3:('a3 -> 'b3)
             -> ('b1, 'b2, 'b3) t

           val to_stable_format
             :  ('a1, 'a2, 'a3) t @ m
             -> ('a1, 'a2, 'a3) Stable_format.t @ m
           [@@mode m = (m, global)]

           val of_stable_format : ('a1, 'a2, 'a3) Stable_format.t -> ('a1, 'a2, 'a3) t
           val caller_identity : Bin_shape.Uuid.t
         end) :
      With_stable_witness.S3 [@mode m] with type ('a1, 'a2, 'a3) t = ('a1, 'a2, 'a3) M.t =
    struct
      include Of_stable_format3.V2 [@modality p] [@mode m] (Stable_format) (M)
      module Witness = Stable_witness.Of_serializable3 (Stable_format) (M)

      let stable_witness witness =
        Witness.of_serializable
          Stable_format.stable_witness
          M.of_stable_format
          M.to_stable_format
          witness
      ;;
    end
  end
end
