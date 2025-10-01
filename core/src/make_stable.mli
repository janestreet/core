@@ portable

open! Import
open Stable_module_types

module%template Of_stable_format : sig
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
       end) : S0 [@mode m] [@modality p] with type t = M.t

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
       end) : S0 [@mode m] [@modality p] with type t = M.t
end

module%template Of_stable_format1 : sig
  [@@@mode.default m = (global, local)]

  module%template.portable V1
      (Stable_format : sig
         type 'a t [@@deriving (bin_io [@mode m]), sexp]
       end)
      (M : sig
         type 'a t [@@deriving compare [@mode m]]

         val map : 'a t -> f:('a -> 'b) -> 'b t

         val to_stable_format : 'a t @ m -> 'a Stable_format.t @ m
         [@@mode m = (m, global)]

         val of_stable_format : 'a Stable_format.t -> 'a t
       end) : S1 [@mode m] with type 'a t = 'a M.t

  module%template.portable V2
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
       end) : S1 [@mode m] with type 'a t = 'a M.t
end

module%template Of_stable_format2 : sig
  [@@@mode.default m = (global, local)]

  module%template.portable V1
      (Stable_format : sig
         type ('a1, 'a2) t [@@deriving (bin_io [@mode m]), sexp]
       end)
      (M : sig
         type ('a1, 'a2) t [@@deriving compare [@mode m]]

         val map : ('a1, 'a2) t -> f1:('a1 -> 'b1) -> f2:('a2 -> 'b2) -> ('b1, 'b2) t

         val to_stable_format : ('a1, 'a2) t @ m -> ('a1, 'a2) Stable_format.t @ m
         [@@mode m = (m, global)]

         val of_stable_format : ('a1, 'a2) Stable_format.t -> ('a1, 'a2) t
       end) : S2 [@mode m] with type ('a1, 'a2) t = ('a1, 'a2) M.t

  module%template.portable V2
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
       end) : S2 [@mode m] with type ('a1, 'a2) t = ('a1, 'a2) M.t
end

module%template Of_stable_format3 : sig
  [@@@mode.default m = (global, local)]

  module%template.portable V1
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
       end) : S3 [@mode m] with type ('a1, 'a2, 'a3) t = ('a1, 'a2, 'a3) M.t

  module%template.portable V2
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
       end) : S3 [@mode m] with type ('a1, 'a2, 'a3) t = ('a1, 'a2, 'a3) M.t
end

module With_stable_witness : sig
  module%template Of_stable_format : sig
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
         end) : With_stable_witness.S0 [@mode m] [@modality p] with type t = M.t

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
         end) : With_stable_witness.S0 [@mode m] [@modality p] with type t = M.t
  end

  module%template Of_stable_format1 : sig
    [@@@mode.default m = (global, local)]

    module%template.portable V1
        (Stable_format : sig
           type 'a t [@@deriving (bin_io [@mode m]), sexp, stable_witness]
         end)
        (M : sig
           type 'a t [@@deriving compare [@mode m]]

           val map : 'a t -> f:('a -> 'b) -> 'b t

           val to_stable_format : 'a t @ m -> 'a Stable_format.t @ m
           [@@mode m = (m, global)]

           val of_stable_format : 'a Stable_format.t -> 'a t
         end) : With_stable_witness.S1 [@mode m] with type 'a t = 'a M.t

    module%template.portable V2
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
         end) : With_stable_witness.S1 [@mode m] with type 'a t = 'a M.t
  end

  module%template Of_stable_format2 : sig
    [@@@mode.default m = (global, local)]

    module%template.portable V1
        (Stable_format : sig
           type ('a1, 'a2) t [@@deriving (bin_io [@mode m]), sexp, stable_witness]
         end)
        (M : sig
           type ('a1, 'a2) t [@@deriving compare [@mode m]]

           val map : ('a1, 'a2) t -> f1:('a1 -> 'b1) -> f2:('a2 -> 'b2) -> ('b1, 'b2) t

           val to_stable_format : ('a1, 'a2) t @ m -> ('a1, 'a2) Stable_format.t @ m
           [@@mode m = (m, global)]

           val of_stable_format : ('a1, 'a2) Stable_format.t -> ('a1, 'a2) t
         end) : With_stable_witness.S2 [@mode m] with type ('a1, 'a2) t = ('a1, 'a2) M.t

    module%template.portable V2
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
         end) : With_stable_witness.S2 [@mode m] with type ('a1, 'a2) t = ('a1, 'a2) M.t
  end

  module%template Of_stable_format3 : sig
    [@@@mode.default m = (global, local)]

    module%template.portable V1
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
      With_stable_witness.S3 [@mode m] with type ('a1, 'a2, 'a3) t = ('a1, 'a2, 'a3) M.t

    module%template.portable V2
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
      With_stable_witness.S3 [@mode m] with type ('a1, 'a2, 'a3) t = ('a1, 'a2, 'a3) M.t
  end
end
