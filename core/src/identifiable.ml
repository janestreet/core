open! Import
include Identifiable_intf
module Binable = Binable0

module Make_plain (T : sig
  type t [@@deriving compare, hash, sexp_of]

  include Stringable.S with type t := t

  val module_name : string
end) =
struct
  include T
  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)
  include Pretty_printer.Register (T)
end

module Make (T : sig
  type t [@@deriving bin_io, compare, hash, sexp]

  include Stringable.S with type t := t

  val module_name : string
end) =
struct
  include T
  include Comparable.Make_binable (T)
  include Hashable.Make_binable (T)
  include Pretty_printer.Register (T)
end

module Make_with_sexp_grammar (T : sig
  type t [@@deriving bin_io, compare, hash, sexp, sexp_grammar]

  include Stringable.S with type t := t

  val module_name : string
end) =
struct
  include T
  include Make (T)
end

module Make_and_derive_hash_fold_t (T : sig
  type t [@@deriving bin_io, compare, sexp]

  include Stringable.S with type t := t

  val hash : t -> int
  val module_name : string
end) =
Make (struct
  include T

  let hash_fold_t state t = hash_fold_int state (hash t)
end)

module Make_using_comparator (T : sig
  type t [@@deriving bin_io, compare, hash, sexp]

  include Comparator.S with type t := t
  include Stringable.S with type t := t

  val module_name : string
end) =
struct
  include T
  include Comparable.Make_binable_using_comparator (T)
  include Hashable.Make_binable (T)
  include Pretty_printer.Register (T)
end

module Make_plain_using_comparator (T : sig
  type t [@@deriving compare, hash, sexp_of]

  include Comparator.S with type t := t
  include Stringable.S with type t := t

  val module_name : string
end) =
struct
  include T
  include Comparable.Make_plain_using_comparator (T)
  include Hashable.Make_plain (T)
  include Pretty_printer.Register (T)
end

module Make_using_comparator_and_derive_hash_fold_t (T : sig
  type t [@@deriving bin_io, compare, sexp]

  include Comparator.S with type t := t
  include Stringable.S with type t := t

  val hash : t -> int
  val module_name : string
end) =
Make_using_comparator (struct
  include T

  let hash_fold_t state t = hash_fold_int state (hash t)
end)

module Extend (M : Base.Identifiable.S) (B : Binable0.S with type t = M.t) = struct
  module T = struct
    include M
    include (B : Binable.S with type t := t)
  end

  include T
  include Comparable.Extend_binable (M) (T)

  include Hashable.Make_binable_with_hashable (struct
    module Key = T

    let hashable = M.hashable
  end)
end
