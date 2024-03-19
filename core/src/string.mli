(** This module extends {{!Base.String}[Base.String]}. *)

(** @inline *)
include module type of struct
  include Base.String
end

type t = string [@@deriving bin_io ~localize, typerep]

(** [Caseless] compares and hashes strings ignoring case, so that for example
    [Caseless.equal "OCaml" "ocaml"] and [Caseless.("apple" < "Banana")] are [true], and
    [Caseless.Map], [Caseless.Table] lookup and [Caseless.Set] membership is
    case-insensitive.

    [Caseless] also provides case-insensitive [is_suffix] and [is_prefix] functions, so
    that for example [Caseless.is_suffix "OCaml" ~suffix:"AmL"] and [Caseless.is_prefix
    "OCaml" ~prefix:"oc"] are [true]. *)
module Caseless : sig
  include module type of struct
    include Caseless
  end

  type nonrec t = t [@@deriving bin_io ~localize, hash, sexp]

  include Comparable.S_binable with type t := t
  include Hashable.S_binable with type t := t
end

(** [slice t start stop] returns a new string including elements [t.(start)] through
    [t.(stop-1)], normalized Python-style with the exception that [stop = 0] is treated as
    [stop = length t]. *)
val slice : t -> int -> int -> t

(** [nget s i] gets the char at normalized position [i] in [s]. *)
val nget : t -> int -> char

(** [take_while s ~f] returns the longest prefix of [s] satisfying [for_all prefix ~f]
    (See [lstrip] to drop such a prefix) *)
val take_while : t -> f:(char -> bool) -> t

(** [rtake_while s ~f] returns the longest suffix of [s] satisfying [for_all suffix ~f]
    (See [rstrip] to drop such a suffix) *)
val rtake_while : t -> f:(char -> bool) -> t

include Hexdump.S with type t := t
include Identifiable.S with type t := t and type comparator_witness := comparator_witness
include Diffable.S_atomic with type t := t
include Quickcheckable.S with type t := t

(** Like [quickcheck_generator], but without empty strings. *)
val gen_nonempty : t Quickcheck.Generator.t

(** Like [quickcheck_generator], but generate strings with the given distribution of
    characters. *)
val gen' : char Quickcheck.Generator.t -> t Quickcheck.Generator.t

(** Like [gen'], but without empty strings. *)
val gen_nonempty' : char Quickcheck.Generator.t -> t Quickcheck.Generator.t

(** Like [gen'], but generate strings with the given length. *)
val gen_with_length : int -> char Quickcheck.Generator.t -> t Quickcheck.Generator.t

module type Utf = sig
  include Utf

  include
    Identifiable.S with type t := t and type comparator_witness := comparator_witness

  include Quickcheckable.S with type t := t
end

(** Iterface for Unicode encodings, specialized for string representation. *)
module type Utf_as_string = Utf with type t = private string

module Utf8 :
  Utf with type t = Utf8.t and type comparator_witness = Utf8.comparator_witness

module Utf16le :
  Utf with type t = Utf16le.t and type comparator_witness = Utf16le.comparator_witness

module Utf16be :
  Utf with type t = Utf16be.t and type comparator_witness = Utf16be.comparator_witness

module Utf32le :
  Utf
    with type t = Base.String.Utf32le.t
     and type comparator_witness = Base.String.Utf32le.comparator_witness

module Utf32be :
  Utf
    with type t = Base.String.Utf32be.t
     and type comparator_witness = Base.String.Utf32be.comparator_witness

(** Note that [string] is already stable by itself, since as a primitive type it is an
    integral part of the sexp / bin_io protocol. [String.Stable] exists only to introduce
    [String.Stable.Set], [String.Stable.Map], [String.Stable.Table], and provide interface
    uniformity with other stable types. *)
module Stable : sig
  module type Identifiable_without_binio := sig
    type t [@@deriving equal, hash, sexp_grammar]
    type comparator_witness

    include Base.Stringable.S with type t := t

    include
      Stable_comparable.With_stable_witness.V1
        with type t := t
        with type comparator_witness := comparator_witness

    include Hashable.Stable.V1.With_stable_witness.S with type key := t
  end

  module V1 : sig
    type nonrec t = t [@@deriving bin_io ~localize, diff ~extra_derive:[ sexp ]]

    include
      Identifiable_without_binio
        with type t := t
         and type comparator_witness = comparator_witness
  end

  module Utf8 : sig
    module V1 : sig
      type t = Utf8.t [@@deriving bin_io]

      include
        Identifiable_without_binio
          with type t := t
           and type comparator_witness = Utf8.comparator_witness
    end
  end

  module Utf16le : sig
    module V1 : sig
      type t = Utf16le.t [@@deriving bin_io]

      include
        Identifiable_without_binio
          with type t := t
           and type comparator_witness = Utf16le.comparator_witness
    end
  end

  module Utf16be : sig
    module V1 : sig
      type t = Utf16be.t [@@deriving bin_io]

      include
        Identifiable_without_binio
          with type t := t
           and type comparator_witness = Utf16be.comparator_witness
    end
  end

  module Utf32le : sig
    module V1 : sig
      type t = Utf32le.t [@@deriving bin_io]

      include
        Identifiable_without_binio
          with type t := t
           and type comparator_witness = Utf32le.comparator_witness
    end
  end

  module Utf32be : sig
    module V1 : sig
      type t = Utf32be.t [@@deriving bin_io]

      include
        Identifiable_without_binio
          with type t := t
           and type comparator_witness = Utf32be.comparator_witness
    end
  end
end
