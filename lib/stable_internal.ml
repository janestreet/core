include Bin_prot.Std
(* Constrain Sexplib.Std to remove the Hashtbl module (and some other modules we don't
   care about, e.g. Ratio), which would later conflict with Hashtbl in
   Stable_containers. *)
include (Sexplib.Std : sig
  val sexp_of_array : ('a -> Sexplib.Sexp.t) -> 'a array -> Sexplib.Sexp.t
  val array_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a array
  val sexp_of_bool : bool -> Sexplib.Sexp.t
  val bool_of_sexp : Sexplib.Sexp.t -> bool
  val sexp_of_char : char -> Sexplib.Sexp.t
  val char_of_sexp : Sexplib.Sexp.t -> char
  val sexp_of_exn : exn -> Sexplib.Sexp.t
  val sexp_of_float : float -> Sexplib.Sexp.t
  val float_of_sexp : Sexplib.Sexp.t -> float
  val sexp_of_int : int -> Sexplib.Sexp.t
  val int_of_sexp : Sexplib.Sexp.t -> int
  val sexp_of_int32 : int32 -> Sexplib.Sexp.t
  val int32_of_sexp : Sexplib.Sexp.t -> int32
  val sexp_of_int64 : int64 -> Sexplib.Sexp.t
  val int64_of_sexp : Sexplib.Sexp.t -> int64
  val sexp_of_list : ('a -> Sexplib.Sexp.t) -> 'a list -> Sexplib.Sexp.t
  val list_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a list
  val sexp_of_nativeint : nativeint -> Sexplib.Sexp.t
  val nativeint_of_sexp : Sexplib.Sexp.t -> nativeint
  val sexp_of_option : ('a -> Sexplib.Sexp.t) -> 'a option -> Sexplib.Sexp.t
  val option_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a option
  val sexp_of_string : string -> Sexplib.Sexp.t
  val string_of_sexp : Sexplib.Sexp.t -> string
  val ref_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a ref
  val sexp_of_ref : ('a -> Sexplib.Sexp.t) -> 'a ref -> Sexplib.Sexp.t
  val unit_of_sexp : Sexplib.Sexp.t -> unit
  val sexp_of_unit : unit -> Sexplib.Sexp.t
end)

type 'a sexp_option = 'a Std_internal.sexp_option with bin_io, compare

(* Hack, because Sexp isn't Binable *)
module Sexp = struct
  type t = Sexplib.Sexp.t = Atom of string | List of t list with bin_io, compare
  include (Sexplib.Sexp : module type of Sexplib.Sexp with type t := t)
end
