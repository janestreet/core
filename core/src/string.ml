open! Import
include Base.String

(* These two are needed because [include Identifiable.Extend] (present later in the file)
   binds new [Map] and [Set] modules. *)

module Stable = struct
  module V1 = struct
    module T = struct
      include Base.String

      type t = string [@@deriving bin_io, stable_witness]
    end

    include T

    let to_string = Fn.id
    let of_string = Fn.id

    include Comparable.Stable.V1.With_stable_witness.Make (T)
    include Hashable.Stable.V1.With_stable_witness.Make (T)
  end
end

module Caseless = struct
  module T = struct
    include Caseless

    type t = string [@@deriving bin_io]
  end

  include T
  include Comparable.Make_binable_using_comparator (T)
  include Hashable.Make_binable (T)
end

type t = string [@@deriving typerep]

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
