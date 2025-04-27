(** This module extends {{!Base.Nativeint} [Base.Nativeint]}. *)

(** @inline *)
include module type of struct
  include Base.Nativeint
end

include
  Int_intf.Extension with type t := t and type comparator_witness := comparator_witness @@ 
portable

include sig @@ portable
    type t [@@deriving bin_io ~localize]
  end
  with type t := t
