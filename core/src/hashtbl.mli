open! Import

include%template
  Hashtbl_intf.Hashtbl
  [@modality portable]
  with type ('a : value_or_null, 'b : value_or_null) t = ('a, 'b) Base.Hashtbl.t
(** @inline *)
