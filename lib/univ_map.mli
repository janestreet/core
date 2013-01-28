(** universal/heterogeneous maps *)
open Std_internal

type t with sexp_of

val empty : t
val is_empty : t -> bool

module Key : sig
  type 'a t with sexp_of
  (** [create name to_sexp] generates a fresh key.
      Note: If type ['a] doesn't support sexp conversion, then a good
            practice is to use [sexp_of_opaque] as the converter. *)
  val create : string -> ('a -> Sexp.t) -> 'a t
end

val set : t -> 'a Key.t -> 'a -> t
val mem : t -> 'a Key.t -> bool

val find     : t -> 'a Key.t -> 'a option
val find_exn : t -> 'a Key.t -> 'a

val add     : t -> 'a Key.t -> 'a -> [ `Ok of t | `Duplicate ]
val add_exn : t -> 'a Key.t -> 'a -> t

val change     : t -> 'a Key.t -> ('a option -> 'a option) -> t
val change_exn : t -> 'a Key.t -> ('a        -> 'a       ) -> t

(* keys with associated default values, so that [find] is no longer partial *)
module With_default : sig
  module Key : sig
    type 'a t
    val create : default:'a -> string -> ('a -> Sexp.t) -> 'a t
  end
  val set    : t -> 'a Key.t -> 'a -> t
  val find   : t -> 'a Key.t -> 'a
  val change : t -> 'a Key.t -> ('a -> 'a) -> t
end

(* keys that map to an accumulator value with an associated fold operation *)
module With_fold : sig
  module Key : sig
    type ('a, 'b) t
    val create : init:'b -> f:('b -> 'a -> 'b) -> string -> ('b -> Sexp.t) -> ('a, 'b) t
  end
  val set    : t -> ('a, 'b) Key.t -> 'b -> t (* reset the accumulator *)
  val find   : t -> ('a, 'b) Key.t -> 'b      (* the current accumulator *)
  val add    : t -> ('a, 'b) Key.t -> 'a -> t (* fold value into accumulator *)
  val change : t -> ('a, 'b) Key.t -> ('b -> 'b) -> t (* accumulator update *)
end

(* list-accumulating keys with a default value of the empty list *)
module Multi : sig
  module Key : sig
    type 'a t
    val create : string -> ('a -> Sexp.t) -> 'a t
  end
  val set    : t -> 'a Key.t -> 'a list -> t
  val find   : t -> 'a Key.t -> 'a list
  val add    : t -> 'a Key.t -> 'a -> t
  val change : t -> 'a Key.t -> ('a list -> 'a list) -> t
end

