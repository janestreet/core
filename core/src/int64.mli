(** This module extends {{!Base.Int64}[Base.Int64]}. *)

(** {2 Interface from Base} *)

(** @inline *)
include module type of struct
  include Base.Int64
end

(** {2 Extensions} *)

(** @inline *)
include
  Int_intf.Extension with type t := t and type comparator_witness := comparator_witness

include sig
  type nonrec t = t [@@deriving bin_io ~localize]
end
