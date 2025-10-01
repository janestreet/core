module Stable = struct
  module Global = struct
    module V1 = struct
      type 'a t = 'a Base.Modes.Global.t = { global_ global : 'a }
      [@@unboxed] [@@deriving quickcheck ~portable, stable_witness, typerep]

      let%template[@mode m = (global, local)] compare =
        (Base.Modes.Global.compare [@mode m])
      ;;

      let t_of_sexp = Base.Modes.Global.t_of_sexp
      let sexp_of_t = Base.Modes.Global.sexp_of_t
      let map t ~f = Base.Modes.Global.map t ~f

      (* Implement bin-io without adding a UUID. Wrapping with a modality should not
         change the bin-shape. *)
        include%template
          Binable0.Stable.Of_binable1.V1
            [@mode local]
            [@modality portable]
            [@alert "-legacy"]
            (struct
              type 'a t = 'a [@@deriving bin_io ~localize]
            end)
            (struct
              type 'a t = 'a Base.Modes.Global.t

              let[@mode m = (global, local)] to_binable = Base.Modes.Global.unwrap
              let of_binable = Base.Modes.Global.wrap
            end)
    end
  end
end

open! Base
include Modes

module Global = struct
  include Stable.Global.V1
  include Global
end
