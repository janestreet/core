open! Import
include Identifiable_intf.Definitions
module Binable = Binable0

[%%template
[@@@mode.default m = (global, local)]
[@@@modality.default p = (portable, nonportable)]

module Make_plain (T : sig
  @@ p
    type t [@@deriving (compare [@mode m]), hash, sexp_of]

    include Stringable.S with type t := t

    val module_name : string
  end) =
struct
  include T
  include Comparable.Make_plain [@mode m] [@modality p] (T)
  include Hashable.Make_plain [@modality p] (T)
  include Pretty_printer.Register [@modality p] (T)
end

module Make (T : sig
  @@ p
    type t [@@deriving (bin_io [@mode m]), (compare [@mode m]), hash, sexp]

    include Stringable.S with type t := t

    val module_name : string
  end) =
struct
  include T
  include Comparable.Make_binable [@mode m] [@modality p] (T)
  include Hashable.Make_binable [@modality p] (T)
  include Pretty_printer.Register [@modality p] (T)
end

module Make_with_sexp_grammar (T : sig
  @@ p
    type t [@@deriving (bin_io [@mode m]), (compare [@mode m]), hash, sexp, sexp_grammar]

    include Stringable.S with type t := t

    val module_name : string
  end) =
struct
  include T
  include Make [@mode m] [@modality p] (T)
end

module Make_and_derive_hash_fold_t (T : sig
  @@ p
    type t [@@deriving (bin_io [@mode m]), (compare [@mode m]), sexp]

    include Stringable.S with type t := t

    val hash : t -> int
    val module_name : string
  end) =
Make [@mode m] [@modality p] (struct
    include T

    let hash_fold_t state t = hash_fold_int state (hash t)
  end)

module Make_using_comparator (T : sig
  @@ p
    type t [@@deriving (bin_io [@mode m]), (compare [@mode m]), hash, sexp]

    include Comparator.S [@modality p] with type t := t
    include Stringable.S with type t := t

    val module_name : string
  end) =
struct
  include T
  include Comparable.Make_binable_using_comparator [@mode m] [@modality p] (T)
  include Hashable.Make_binable [@modality p] (T)
  include Pretty_printer.Register [@modality p] (T)
end

module Make_plain_using_comparator (T : sig
  @@ p
    type t [@@deriving (compare [@mode m]), hash, sexp_of]

    include Comparator.S [@modality p] with type t := t
    include Stringable.S with type t := t

    val module_name : string
  end) =
struct
  include T
  include Comparable.Make_plain_using_comparator [@mode m] [@modality p] (T)
  include Hashable.Make_plain [@modality p] (T)
  include Pretty_printer.Register [@modality p] (T)
end

module Make_using_comparator_and_derive_hash_fold_t (T : sig
  @@ p
    type t [@@deriving (bin_io [@mode m]), (compare [@mode m]), sexp]

    include Comparator.S [@modality p] with type t := t
    include Stringable.S with type t := t

    val hash : t -> int
    val module_name : string
  end) =
Make_using_comparator [@mode m] [@modality p] (struct
    include T

    let hash_fold_t state t = hash_fold_int state (hash t)
  end)

module Extend
    (M : sig
     @@ p
       include Base.Identifiable.S [@mode m] [@modality p]
     end)
    (B : sig
     @@ p
       include Binable0.S [@mode m] with type t = M.t
     end) =
struct
  module T = struct
    include M

    include (
      B :
      sig
      @@ p
        include Binable.S [@mode m] with type t := t
      end)
  end

  include T
  include Comparable.Extend_binable [@mode m] [@modality p] (M) (T)

  include Hashable.Make_binable_with_hashable [@modality p] (struct
      module Key = T

      let hashable = M.hashable
    end)
end]
