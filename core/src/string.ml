open! Import
include Base.String

(* These two are needed because [include Identifiable.Extend] (present later in the file)
   binds new [Map] and [Set] modules. *)

module Stable = struct
  module V1 = struct
    module T = struct
      include Base.String

      type t = string [@@deriving bin_io ~localize, stable_witness]
    end

    include T

    let to_string = Fn.id
    let of_string = Fn.id

    include Comparable.Stable.V1.With_stable_witness.Make (T)
    include Hashable.Stable.V1.With_stable_witness.Make (T)
    include Diffable.Atomic.Make (T)
  end

  module Make_utf (Utf : sig
    type t [@@deriving sexp_grammar]

    include Base.Identifiable.S with type t := t

    val caller_identity : Bin_shape.Uuid.t
  end) =
  struct
    module V1 = struct
      module T = struct
        include Utf

        include
          Binable0.Of_binable_with_uuid
            (struct
              type t = string [@@deriving bin_io]
            end)
            (struct
              type t = Utf.t

              let of_binable = Utf.of_string
              let to_binable = Utf.to_string
              let caller_identity = Utf.caller_identity
            end)

        let stable_witness : t Stable_witness.t = Stable_witness.assert_stable
      end

      include T
      include Comparable.Stable.V1.With_stable_witness.Make (T)
      include Hashable.Stable.V1.With_stable_witness.Make (T)
    end
  end

  module Utf8 = Make_utf (struct
    include Utf8

    let caller_identity =
      Bin_prot.Shape.Uuid.of_string "5bc29e13-1c6f-4b6d-b431-3befb256ebda"
    ;;
  end)

  module Utf16le = Make_utf (struct
    include Utf16le

    let caller_identity =
      Bin_prot.Shape.Uuid.of_string "7a4f8cac-8fff-11ee-bd11-aaa233d0b6a7"
    ;;
  end)

  module Utf16be = Make_utf (struct
    include Utf16be

    let caller_identity =
      Bin_prot.Shape.Uuid.of_string "7c3a50ce-8fff-11ee-94c6-aaa233d0b6a7"
    ;;
  end)

  module Utf32le = Make_utf (struct
    include Utf32le

    let caller_identity =
      Bin_prot.Shape.Uuid.of_string "961d2214-9252-11ee-9f77-aaa233d0b6a7"
    ;;
  end)

  module Utf32be = Make_utf (struct
    include Utf32be

    let caller_identity =
      Bin_prot.Shape.Uuid.of_string "9fcbae5c-9252-11ee-9f34-aaa233d0b6a7"
    ;;
  end)
end

module Caseless = struct
  module T = struct
    include Caseless

    type t = string [@@deriving bin_io ~localize]
  end

  include T
  include Comparable.Make_binable_using_comparator (T)
  include Hashable.Make_binable (T)
end

type t = string [@@deriving bin_io ~localize, typerep]

include
  Identifiable.Extend
    (struct
      include Base.String

      let hashable = Stable.V1.hashable
    end)
    (struct
      type t = string [@@deriving bin_io]
    end)

include Comparable.Validate (Base.String)

include Diffable.Atomic.Make (struct
  type nonrec t = t [@@deriving sexp, bin_io, equal]
end)

include Hexdump.Of_indexable (struct
  type t = string

  let length = length
  let get = get
end)

let quickcheck_generator = Base_quickcheck.Generator.string
let quickcheck_observer = Base_quickcheck.Observer.string
let quickcheck_shrinker = Base_quickcheck.Shrinker.string
let gen_nonempty = Base_quickcheck.Generator.string_non_empty
let gen' = Base_quickcheck.Generator.string_of
let gen_nonempty' = Base_quickcheck.Generator.string_non_empty_of

let gen_with_length length chars =
  Base_quickcheck.Generator.string_with_length_of chars ~length
;;

let take_while t ~f =
  match lfindi t ~f:(fun _ elt -> not (f elt)) with
  | None -> t
  | Some i -> sub t ~pos:0 ~len:i
;;

let rtake_while t ~f =
  match rfindi t ~f:(fun _ elt -> not (f elt)) with
  | None -> t
  | Some i -> sub t ~pos:(i + 1) ~len:(length t - i - 1)
;;

(** See {!Array.normalize} for the following 4 functions. *)
let normalize t i = Ordered_collection_common.normalize ~length_fun:length t i

let slice t start stop =
  Ordered_collection_common.slice ~length_fun:length ~sub_fun:sub t start stop
;;

let nget x i =
  let module String = Base.String in
  x.[normalize x i]
;;

module type Utf = sig
  include Utf

  include
    Identifiable.S with type t := t and type comparator_witness := comparator_witness

  include Quickcheckable.S with type t := t
end

module type Utf_as_string = Utf with type t = private string

module Extend_utf (Utf : Base.String.Utf) (B : Binable0.S with type t = Utf.t) :
  Utf with type t = Utf.t and type comparator_witness = Utf.comparator_witness = struct
  include Utf
  include B
  include Identifiable.Extend (Utf) (B)

  let quickcheck_observer = Base_quickcheck.Observer.(unmap string ~f:Utf.to_string)

  let quickcheck_shrinker =
    let open Base_quickcheck in
    Shrinker.map
      [%quickcheck.shrinker: Uchar.t list]
      ~f:Utf.of_list
      ~f_inverse:Utf.to_list
  ;;

  let quickcheck_generator =
    let open Base_quickcheck in
    Generator.list Uchar.quickcheck_generator |> Generator.map ~f:Utf.of_list
  ;;
end

module Utf8 = Extend_utf (Utf8) (Stable.Utf8.V1)
module Utf16le = Extend_utf (Utf16le) (Stable.Utf16le.V1)
module Utf16be = Extend_utf (Utf16be) (Stable.Utf16be.V1)
module Utf32le = Extend_utf (Utf32le) (Stable.Utf32le.V1)
module Utf32be = Extend_utf (Utf32be) (Stable.Utf32be.V1)
