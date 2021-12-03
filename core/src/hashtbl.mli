open! Import

include Hashtbl_intf.Hashtbl with type ('a, 'b) t = ('a, 'b) Base.Hashtbl.t (** @inline *)
