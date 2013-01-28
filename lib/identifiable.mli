(** a signature for opaque identifier types. *)

module type S = sig
  type t
  include Stringable.S         with type t := t
  include Comparable.S_binable with type t := t
  include Hashable.S_binable   with type t := t
  include Sexpable.S           with type t := t
  include Binable.S            with type t := t
  val pp : Format.formatter -> t -> unit (* pretty print for top-level *)
end

(** [Of_stringable] and [Of_sexpable] each create an identifiable that uses string
    conversions for binable, sexpable, equality, hash, compare, and pp.  Should only be
    used for modules where to_string is a cheap operation or where performance is not
    critical.
*)
module Of_stringable (T : Stringable.S) : S with type t = T.t
module Of_sexpable (T : Sexpable.S) : S with type t = T.t
