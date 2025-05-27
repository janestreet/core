open! Import
include Hash_set_intf
include Base.Hash_set

module type S_plain = S_plain with type 'a hash_set := 'a t
module type S = S with type 'a hash_set := 'a t
module type S_binable = S_binable with type 'a hash_set := 'a t
module type S_stable = S_stable with type 'a hash_set := 'a t

[%%template
[@@@mode.default m = (local, global)]

module type Elt_plain = Hashtbl.Key_plain [@mode m]
module type Elt = Hashtbl.Key [@mode m]
module type Elt_binable = Hashtbl.Key_binable [@mode m]
module type Elt_stable = Hashtbl.Key_stable [@mode m]]

module%template.portable
  [@modality p] Make_plain_with_hashable (T : sig
    module Elt : Elt_plain

    val hashable : Elt.t Hashtbl.Hashable.t
  end) =
struct
  type elt = T.Elt.t
  type nonrec t = elt t

  let%template equal = (equal [@mode m]) [@@mode m = (local, global)]

  include Creators [@modality p] (struct
      type 'a t = T.Elt.t

      let hashable = T.hashable
    end)

  let sexp_of_t t = Poly.sexp_of_t T.Elt.sexp_of_t t
end

module%template.portable Provide_of_sexp (Elt : Base.Hashtbl.M_of_sexp) = struct
  let t_of_sexp sexp = Base.Hash_set.m__t_of_sexp (module Elt) sexp
end

module%template.portable
  [@modality p] Provide_bin_io (Elt : sig
    type t [@@deriving bin_io]

    include Elt_plain with type t := t
  end) =
Bin_prot.Utils.Make_iterable_binable [@modality p] (struct
    type nonrec t = Elt.t t
    type el = Elt.t [@@deriving bin_io]

    let _ = bin_el

    let caller_identity =
      Bin_prot.Shape.Uuid.of_string "ad381672-4992-11e6-9e36-b76dc8cd466f"
    ;;

    let module_name = Some "Core.Hash_set"
    let length = length
    let iter = iter

    let init ~len ~next =
      let t = create (module Elt) ~size:len in
      for _i = 0 to len - 1 do
        let v = next () in
        add t v
      done;
      t
    ;;
  end)

module Provide_stable_witness (Elt : sig
    type t [@@deriving stable_witness]
  end) =
struct
  (* The binary representation of hash_set is used in the stable modules below, so it's
       assumed to be stable (if the elt is stable) . *)
  let stable_witness : Elt.t t Stable_witness.t =
    let (_ : Elt.t Stable_witness.t) = Elt.stable_witness in
    Stable_witness.assert_stable
  ;;
end

module%template.portable
  [@modality p] Make_with_hashable (T : sig
    module Elt : Elt

    val hashable : Elt.t Hashtbl.Hashable.t
  end) =
struct
  include Make_plain_with_hashable [@modality p] (T)
  include Provide_of_sexp [@modality p] (T.Elt)
end

module%template.portable
  [@modality p] Make_binable_with_hashable (T : sig
    module Elt : Elt_binable

    val hashable : Elt.t Hashtbl.Hashable.t
  end) =
struct
  include Make_with_hashable [@modality p] (T)
  include Provide_bin_io [@modality p] (T.Elt)
end

module%template.portable
  [@modality p] Make_stable_with_hashable (T : sig
    module Elt : Elt_stable

    val hashable : Elt.t Hashtbl.Hashable.t
  end) =
struct
  include Make_binable_with_hashable [@modality p] (T)
  include Provide_stable_witness (T.Elt)
end

module%template.portable [@modality p] Make_plain (Elt : Elt_plain) =
Make_plain_with_hashable [@modality p] (struct
    module Elt = Elt

    let hashable = (Hashtbl.Hashable.of_key [@modality p]) (module Elt)
  end)

module%template.portable [@modality p] Make (Elt : Elt) = struct
  include Make_plain [@modality p] (Elt)
  include Provide_of_sexp [@modality p] (Elt)
end

module%template.portable [@modality p] Make_binable (Elt : Elt_binable) = struct
  include Make [@modality p] (Elt)
  include Provide_bin_io [@modality p] (Elt)
end

module%template.portable [@modality p] Make_stable (Elt : Elt_stable) = struct
  include Make_binable [@modality p] (Elt)
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
  |> Base_quickcheck.Generator.map ~f:(fun list -> of_list (module Key) list)
;;

let quickcheck_observer_m__t (type key) (module Key : M_quickcheck with type t = key) =
  [%quickcheck.observer: Key.t List0.t] |> Base_quickcheck.Observer.unmap ~f:to_list
;;

let quickcheck_shrinker_m__t (type key) (module Key : M_quickcheck with type t = key) =
  [%quickcheck.shrinker: Key.t List0.t]
  |> Base_quickcheck.Shrinker.map
       ~f:(fun list -> of_list (module Key) list)
       ~f_inverse:to_list
;;
