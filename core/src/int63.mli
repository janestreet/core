(** This module extends {{!Base.Int63}[Base.Int63]}. *)

(** {2 Interface from Base} *)

(** @inline *)
include module type of struct
  include Base.Int63
end

(** {2 Extensions} *)

(** @inline *)
include
  Int_intf.Extension_with_stable
  with type t := t
   and type comparator_witness := comparator_witness
