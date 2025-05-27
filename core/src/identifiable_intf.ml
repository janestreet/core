(** Signatures and functors for making types that are used as identifiers. *)

open! Import

module Definitions = struct
  [%%template
  [@@@mode.default m = (global, local)]

  module type S_common = sig
    type t [@@deriving (compare [@mode m]), hash, sexp_of]

    include Stringable.S with type t := t
    include Pretty_printer.S with type t := t
  end

  [@@@modality.default p = (portable, nonportable)]

  module type S_plain = sig
    include S_common [@mode m]
    include Comparable.S_plain [@modality p] [@mode m] with type t := t
    include Hashable.S_plain with type t := t
  end

  module type S_not_binable = sig
    type t [@@deriving hash, sexp]

    include S_common [@mode m] with type t := t
    include Comparable.S [@modality p] [@mode m] with type t := t
    include Hashable.S with type t := t
  end

  module type S = sig
    type t [@@deriving (bin_io [@mode m]), hash, sexp]

    include S_common [@mode m] with type t := t
    include Comparable.S_binable [@modality p] [@mode m] with type t := t
    include Hashable.S_binable with type t := t
  end

  module type S_sexp_grammar = sig
    type t [@@deriving sexp_grammar]

    include S [@mode m] [@modality p] with type t := t
  end]
end

module type%template Identifiable = sig @@ portable
  include module type of struct
    include Definitions
  end

  [@@@mode.default m = (global, local)]

  module%template.portable
    [@modality p] Make_plain (M : sig
      type t [@@deriving (compare [@mode m]), hash, sexp_of]

      include Stringable.S with type t := t

      (** for registering the pretty printer *)
      val module_name : string
    end) : S_plain [@modality p] [@mode m] with type t := M.t

  (** Used for making an Identifiable module. Here's an example:

      {[
        module Id = struct
          module T = struct
            type t =
              | A
              | B
            [@@deriving bin_io, compare, hash, sexp]

            include Sexpable.To_stringable (struct
                type nonrec t = t [@@deriving sexp]
              end)

            let module_name = "My_library.Id"
          end

          include T
          include Identifiable.Make (T)
        end
      ]} *)
  module%template.portable
    [@modality p] Make (M : sig
      type t [@@deriving (bin_io [@mode m]), (compare [@mode m]), hash, sexp]

      include Stringable.S with type t := t

      (** for registering the pretty printer *)
      val module_name : string
    end) : S [@modality p] [@mode m] with type t := M.t

  module%template.portable
    [@modality p] Make_with_sexp_grammar (M : sig
      type t
      [@@deriving (bin_io [@mode m]), (compare [@mode m]), hash, sexp, sexp_grammar]

      include Stringable.S with type t := t

      (** for registering the pretty printer *)
      val module_name : string
    end) : S_sexp_grammar [@modality p] [@mode m] with type t := M.t

  module%template.portable
    [@modality p] Make_and_derive_hash_fold_t (M : sig
      type t [@@deriving (bin_io [@mode m]), (compare [@mode m]), sexp]

      include Stringable.S with type t := t

      val hash : t -> int

      (** for registering the pretty printer *)
      val module_name : string
    end) : S [@modality p] [@mode m] with type t := M.t

  module%template.portable
    [@modality p] Make_using_comparator (M : sig
      type t [@@deriving (bin_io [@mode m]), (compare [@mode m]), hash, sexp]

      include Comparator.S [@modality p] with type t := t
      include Stringable.S with type t := t

      val module_name : string
    end) :
    S
    [@modality p] [@mode m]
    with type t := M.t
    with type comparator_witness := M.comparator_witness

  module%template.portable
    [@modality p] Make_plain_using_comparator (M : sig
      type t [@@deriving (compare [@mode m]), hash, sexp_of]

      include Comparator.S [@modality p] with type t := t
      include Stringable.S with type t := t

      (** for registering the pretty printer *)
      val module_name : string
    end) :
    S_plain
    [@modality p] [@mode m]
    with type t := M.t
    with type comparator_witness := M.comparator_witness

  module%template.portable
    [@modality p] Make_using_comparator_and_derive_hash_fold_t (M : sig
      type t [@@deriving (bin_io [@mode m]), (compare [@mode m]), sexp]

      include Comparator.S [@modality p] with type t := t
      include Stringable.S with type t := t

      val hash : t -> int
      val module_name : string
    end) :
    S
    [@modality p] [@mode m]
    with type t := M.t
    with type comparator_witness := M.comparator_witness

  module%template.portable
    [@modality p] Extend
      (M : Base.Identifiable.S
    [@mode m] [@modality p])
      (B : Binable0.S [@mode m] with type t = M.t) :
    S
    [@modality p] [@mode m]
    with type t := M.t
    with type comparator_witness := M.comparator_witness
end
