(** Signatures and functors for making types that are used as identifiers. *)

open! Import

module type S_common = sig
  type t [@@deriving compare, hash, sexp_of]

  include Stringable.S with type t := t
  include Pretty_printer.S with type t := t
end

module type S_plain = sig
  include S_common
  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
end

module type S_not_binable = sig
  type t [@@deriving hash, sexp]

  include S_common with type t := t
  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module type S = sig
  type t [@@deriving bin_io, hash, sexp]

  include S_common with type t := t
  include Comparable.S_binable with type t := t
  include Hashable.S_binable with type t := t
end

module type S_sexp_grammar = sig
  type t [@@deriving sexp_grammar]

  include S with type t := t
end

module type Identifiable = sig
  module type S_common = S_common
  module type S_plain = S_plain
  module type S_not_binable = S_not_binable
  module type S = S
  module type S_sexp_grammar = S_sexp_grammar

  module Make_plain (M : sig
    type t [@@deriving compare, hash, sexp_of]

    include Stringable.S with type t := t

    (** for registering the pretty printer *)
    val module_name : string
  end) : S_plain with type t := M.t

  (** Used for making an Identifiable module. Here's an example:

      {[
        module Id = struct
          module T = struct
            type t = A | B [@@deriving bin_io, compare, hash, sexp]
            include Sexpable.To_stringable (struct type nonrec t = t [@@deriving sexp] end)
            let module_name = "My_library.Id"
          end
          include T
          include Identifiable.Make (T)
        end
      ]}
  *)
  module Make (M : sig
    type t [@@deriving bin_io, compare, hash, sexp]

    include Stringable.S with type t := t

    (** for registering the pretty printer *)
    val module_name : string
  end) : S with type t := M.t

  module Make_with_sexp_grammar (M : sig
    type t [@@deriving bin_io, compare, hash, sexp, sexp_grammar]

    include Stringable.S with type t := t

    (** for registering the pretty printer *)
    val module_name : string
  end) : S_sexp_grammar with type t := M.t

  module Make_and_derive_hash_fold_t (M : sig
    type t [@@deriving bin_io, compare, sexp]

    include Stringable.S with type t := t

    val hash : t -> int

    (** for registering the pretty printer *)
    val module_name : string
  end) : S with type t := M.t

  module Make_using_comparator (M : sig
    type t [@@deriving bin_io, compare, hash, sexp]

    include Comparator.S with type t := t
    include Stringable.S with type t := t

    val module_name : string
  end) : S with type t := M.t with type comparator_witness := M.comparator_witness

  module Make_plain_using_comparator (M : sig
    type t [@@deriving compare, hash, sexp_of]

    include Comparator.S with type t := t
    include Stringable.S with type t := t

    (** for registering the pretty printer *)
    val module_name : string
  end) : S_plain with type t := M.t with type comparator_witness := M.comparator_witness

  module Make_using_comparator_and_derive_hash_fold_t (M : sig
    type t [@@deriving bin_io, compare, sexp]

    include Comparator.S with type t := t
    include Stringable.S with type t := t

    val hash : t -> int
    val module_name : string
  end) : S with type t := M.t with type comparator_witness := M.comparator_witness

  module Extend (M : Base.Identifiable.S) (B : Binable0.S with type t = M.t) :
    S with type t := M.t with type comparator_witness := M.comparator_witness
end
