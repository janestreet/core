module Stable = struct
  module Global = struct
    module V1 = struct
      type 'a t = 'a Base.Modes.Global.t = { global : 'a }
      [@@unboxed] [@@deriving quickcheck, stable_witness, typerep]

      let compare = Base.Modes.Global.compare
      let t_of_sexp = Base.Modes.Global.t_of_sexp
      let sexp_of_t = Base.Modes.Global.sexp_of_t
      let map t ~f = Base.Modes.Global.map t ~f

      (* Implement bin-io without adding a UUID. Wrapping with a modality should not
         change the bin-shape. *)
      include
        Binable0.Stable.Of_binable1.V1 [@alert "-legacy"]
          (struct
            type 'a t = 'a [@@deriving bin_io]
          end)
          (struct
            type 'a t = 'a Base.Modes.Global.t

            let to_binable = Base.Modes.Global.unwrap
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
