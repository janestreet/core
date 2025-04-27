open! Import

include%template
  Hashtbl_intf.Hashtbl [@modality portable] with type ('a, 'b) t = ('a, 'b) Base.Hashtbl.t
(** @inline *)
