open! Import
include Hash_set_intf
include Base.Hash_set

module type S_plain = S_plain with type 'a hash_set := 'a t
module type S = S with type 'a hash_set := 'a t
module type S_binable = S_binable with type 'a hash_set := 'a t
module type S_stable = S_stable with type 'a hash_set := 'a t
module type Elt_plain = Hashtbl.Key_plain
module type Elt = Hashtbl.Key
module type Elt_binable = Hashtbl.Key_binable
module type Elt_stable = Hashtbl.Key_stable

module Make_plain_with_hashable (T : sig
  module Elt : Elt_plain

  val hashable : Elt.t Hashtbl.Hashable.t
end) =
struct
  type elt = T.Elt.t
  type nonrec t = elt t

  let equal = equal

  include Creators (struct
    type 'a t = T.Elt.t

    let hashable = T.hashable
  end)

  let sexp_of_t t = Poly.sexp_of_t T.Elt.sexp_of_t t

  module Provide_of_sexp
    (X : sig
      type t [@@deriving of_sexp]
    end
    with type t := elt) =
  struct
    let t_of_sexp sexp = t_of_sexp X.t_of_sexp sexp
  end

  module Provide_bin_io
    (X : sig
      type t [@@deriving bin_io]
    end
    with type t := elt) =
  Bin_prot.Utils.Make_iterable_binable (struct
    module Elt = struct
      include T.Elt
      include X
    end

    type nonrec t = t
    type el = Elt.t [@@deriving bin_io]

    let _ = bin_el

    let caller_identity =
      Bin_prot.Shape.Uuid.of_string "ad381672-4992-11e6-9e36-b76dc8cd466f"
    ;;

    let module_name = Some "Core.Hash_set"
    let length = length
    let iter = iter

    let init ~len ~next =
      let t = create ~size:len () in
      for _i = 0 to len - 1 do
        let v = next () in
        add t v
      done;
      t
    ;;
  end)

  module Provide_stable_witness
    (X : sig
      type t [@@deriving stable_witness]
    end
    with type t := elt) =
  struct
    (* The binary representation of hash_set is used in the stable modules below, so it's
       assumed to be stable (if the elt is stable) . *)
    let stable_witness : t Stable_witness.t =
      let (_ : elt Stable_witness.t) = X.stable_witness in
      Stable_witness.assert_stable
    ;;
  end
end

module Make_with_hashable (T : sig
  module Elt : Elt

  val hashable : Elt.t Hashtbl.Hashable.t
end) =
struct
  include Make_plain_with_hashable (T)
  include Provide_of_sexp (T.Elt)
end

module Make_binable_with_hashable (T : sig
  module Elt : Elt_binable

  val hashable : Elt.t Hashtbl.Hashable.t
end) =
struct
  include Make_with_hashable (T)
  include Provide_bin_io (T.Elt)
end

module Make_stable_with_hashable (T : sig
  module Elt : Elt_stable

  val hashable : Elt.t Hashtbl.Hashable.t
end) =
struct
  include Make_binable_with_hashable (T)
  include Provide_stable_witness (T.Elt)
end

module Make_plain (Elt : Elt_plain) = Make_plain_with_hashable (struct
  module Elt = Elt

  let hashable = Hashtbl.Hashable.of_key (module Elt)
end)

module Make (Elt : Elt) = struct
  include Make_plain (Elt)
  include Provide_of_sexp (Elt)
end

module Make_binable (Elt : Elt_binable) = struct
  include Make (Elt)
  include Provide_bin_io (Elt)
end

module Make_stable (Elt : Elt_stable) = struct
  include Make_binable (Elt)
  include Provide_stable_witness (Elt)
end

module Using_hashable = struct
  type 'a elt = 'a

  let create ?growth_allowed ?size ~hashable () =
    create ?growth_allowed ?size (Base.Hashable.to_key hashable)
  ;;

  let of_list ?growth_allowed ?size ~hashable l =
    of_list ?growth_allowed ?size (Base.Hashable.to_key hashable) l
  ;;
end

let hashable = Private.hashable
let create ?growth_allowed ?size m = create ?growth_allowed ?size m

let quickcheck_generator_m__t (type key) (module Key : M_quickcheck with type t = key) =
  [%quickcheck.generator: Key.t List0.t]
  |> Quickcheck.Generator.map ~f:(of_list (module Key))
;;

let quickcheck_observer_m__t (type key) (module Key : M_quickcheck with type t = key) =
  [%quickcheck.observer: Key.t List0.t] |> Quickcheck.Observer.unmap ~f:to_list
;;

let quickcheck_shrinker_m__t (type key) (module Key : M_quickcheck with type t = key) =
  [%quickcheck.shrinker: Key.t List0.t]
  |> Quickcheck.Shrinker.map ~f:(of_list (module Key)) ~f_inverse:to_list
;;
