open! Base

module Global : sig
  include module type of struct
    include Modes.Global
  end

  include sig
      type 'a t [@@deriving bin_io, quickcheck, typerep]
    end
    with type 'a t := 'a t
end

include module type of struct
    include Modes
  end [@remove_aliases]
  with module Global := Global

module Stable : sig
  module Global : sig
    module V1 : sig
      type 'a t = 'a Global.t

      include Stable_module_types.With_stable_witness.S1 with type 'a t := 'a t
    end
  end
end
