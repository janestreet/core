open! Import
open Hashtbl_intf
module Hashable = Hashtbl_intf.Hashable
module Merge_into_action = Hashtbl_intf.Merge_into_action
module List = List0

let failwiths = Error.failwiths

module Creators = Hashtbl.Creators

include (
  Hashtbl :
    sig
      type ('a, 'b) t = ('a, 'b) Hashtbl.t [@@deriving sexp_of]

      include Base.Hashtbl.S_without_submodules with type ('a, 'b) t := ('a, 'b) t
    end)

let validate ~name f t = Validate.alist ~name f (to_alist t)

module Using_hashable = struct
  type nonrec ('a, 'b) t = ('a, 'b) t [@@deriving sexp_of]

  let create ?growth_allowed ?size ~hashable () =
    create ?growth_allowed ?size (Base.Hashable.to_key hashable)
  ;;

  let of_alist ?growth_allowed ?size ~hashable l =
    of_alist ?growth_allowed ?size (Base.Hashable.to_key hashable) l
  ;;

  let of_alist_report_all_dups ?growth_allowed ?size ~hashable l =
    of_alist_report_all_dups ?growth_allowed ?size (Base.Hashable.to_key hashable) l
  ;;

  let of_alist_or_error ?growth_allowed ?size ~hashable l =
    of_alist_or_error ?growth_allowed ?size (Base.Hashable.to_key hashable) l
  ;;

  let of_alist_exn ?growth_allowed ?size ~hashable l =
    of_alist_exn ?growth_allowed ?size (Base.Hashable.to_key hashable) l
  ;;

  let of_alist_multi ?growth_allowed ?size ~hashable l =
    of_alist_multi ?growth_allowed ?size (Base.Hashable.to_key hashable) l
  ;;

  let create_mapped ?growth_allowed ?size ~hashable ~get_key ~get_data l =
    create_mapped
      ?growth_allowed
      ?size
      (Base.Hashable.to_key hashable)
      ~get_key
      ~get_data
      l
  ;;

  let create_with_key ?growth_allowed ?size ~hashable ~get_key l =
    create_with_key ?growth_allowed ?size (Base.Hashable.to_key hashable) ~get_key l
  ;;

  let create_with_key_or_error ?growth_allowed ?size ~hashable ~get_key l =
    create_with_key_or_error
      ?growth_allowed
      ?size
      (Base.Hashable.to_key hashable)
      ~get_key
      l
  ;;

  let create_with_key_exn ?growth_allowed ?size ~hashable ~get_key l =
    create_with_key_exn ?growth_allowed ?size (Base.Hashable.to_key hashable) ~get_key l
  ;;

  let group ?growth_allowed ?size ~hashable ~get_key ~get_data ~combine l =
    group
      ?growth_allowed
      ?size
      (Base.Hashable.to_key hashable)
      ~get_key
      ~get_data
      ~combine
      l
  ;;
end

module type S_plain = S_plain with type ('a, 'b) hashtbl = ('a, 'b) t
module type S = S with type ('a, 'b) hashtbl = ('a, 'b) t
module type S_binable = S_binable with type ('a, 'b) hashtbl = ('a, 'b) t
module type S_stable = S_stable with type ('a, 'b) hashtbl = ('a, 'b) t
module type Key_plain = Key_plain
module type Key = Key
module type Key_binable = Key_binable
module type Key_stable = Key_stable

module Poly = struct
  include Hashtbl.Poly

  let validate = validate

  include Bin_prot.Utils.Make_iterable_binable2 (struct
    type nonrec ('a, 'b) t = ('a, 'b) t
    type ('a, 'b) el = 'a * 'b [@@deriving bin_io]

    let caller_identity =
      Bin_prot.Shape.Uuid.of_string "8f3e445c-4992-11e6-a279-3703be311e7b"
    ;;

    let module_name = Some "Core.Hashtbl"
    let length = length
    let iter t ~f = iteri t ~f:(fun ~key ~data -> f (key, data))

    let init ~len ~next =
      let t = create ~size:len () in
      for _i = 0 to len - 1 do
        let key, data = next () in
        match find t key with
        | None -> set t ~key ~data
        | Some _ -> failwith "Core_hashtbl.bin_read_t_: duplicate key"
      done;
      t
    ;;
  end)
end

module Make_plain_with_hashable (T : sig
  module Key : Key_plain

  val hashable : Key.t Hashable.t
end) =
struct
  let hashable = T.hashable

  type key = T.Key.t
  type ('a, 'b) hashtbl = ('a, 'b) t
  type 'a t = (T.Key.t, 'a) hashtbl
  type 'a key_ = T.Key.t

  include Creators (struct
    type 'a t = T.Key.t

    let hashable = hashable
  end)

  include (
    Hashtbl :
      sig
        include Invariant.S2 with type ('a, 'b) t := ('a, 'b) hashtbl
      end)

  let equal = Hashtbl.equal
  let invariant invariant_key t = invariant ignore invariant_key t
  let sexp_of_t sexp_of_v t = Poly.sexp_of_t T.Key.sexp_of_t sexp_of_v t

  module Provide_of_sexp
    (Key : sig
      type t [@@deriving of_sexp]
    end
    with type t := key) =
  struct
    let t_of_sexp v_of_sexp sexp = t_of_sexp Key.t_of_sexp v_of_sexp sexp
  end

  module Provide_bin_io
    (Key' : sig
      type t [@@deriving bin_io]
    end
    with type t := key) =
  Bin_prot.Utils.Make_iterable_binable1 (struct
    module Key = struct
      include T.Key
      include Key'
    end

    type nonrec 'a t = 'a t
    type 'a el = Key.t * 'a [@@deriving bin_io]

    let caller_identity =
      Bin_prot.Shape.Uuid.of_string "8fabab0a-4992-11e6-8cca-9ba2c4686d9e"
    ;;

    let module_name = Some "Core.Hashtbl"
    let length = length
    let iter t ~f = iteri t ~f:(fun ~key ~data -> f (key, data))

    let init ~len ~next =
      let t = create ~size:len () in
      for _i = 0 to len - 1 do
        let key, data = next () in
        match find t key with
        | None -> set t ~key ~data
        | Some _ ->
          failwiths
            ~here:[%here]
            "Hashtbl.bin_read_t: duplicate key"
            key
            [%sexp_of: Key.t]
      done;
      t
    ;;
  end)

  module Provide_stable_witness
    (Key' : sig
      type t [@@deriving stable_witness]
    end
    with type t := key) =
  struct
    (* The binary representation of hashtbl is relied on by stable modules
       (e.g. Hashtable.Stable) and is therefore assumed to be stable.  So, if the key and
       data can provide a stable witnesses, then we can safely the hashtbl is also
       stable. *)
    let stable_witness (type data) (_data_stable_witness : data Stable_witness.t)
      : data t Stable_witness.t
      =
      let (_ : key Stable_witness.t) = Key'.stable_witness in
      Stable_witness.assert_stable
    ;;
  end
end

module Make_with_hashable (T : sig
  module Key : Key

  val hashable : Key.t Hashable.t
end) =
struct
  include Make_plain_with_hashable (T)
  include Provide_of_sexp (T.Key)
end

module Make_binable_with_hashable (T : sig
  module Key : Key_binable

  val hashable : Key.t Hashable.t
end) =
struct
  include Make_with_hashable (T)
  include Provide_bin_io (T.Key)
end

module Make_stable_with_hashable (T : sig
  module Key : Key_stable

  val hashable : Key.t Hashable.t
end) =
struct
  include Make_binable_with_hashable (T)
  include Provide_stable_witness (T.Key)
end

module Make_plain (Key : Key_plain) = Make_plain_with_hashable (struct
  module Key = Key

  let hashable =
    { Hashable.hash = Key.hash; compare = Key.compare; sexp_of_t = Key.sexp_of_t }
  ;;
end)

module Make (Key : Key) = struct
  include Make_plain (Key)
  include Provide_of_sexp (Key)
end

module Make_binable (Key : Key_binable) = struct
  include Make (Key)
  include Provide_bin_io (Key)
end

module Make_stable (Key : Key_stable) = struct
  include Make_binable (Key)
  include Provide_stable_witness (Key)
end

module M = Hashtbl.M

module type For_deriving = For_deriving

module For_deriving : For_deriving with type ('a, 'b) t := ('a, 'b) t = struct
  include (Hashtbl : Hashtbl.For_deriving with type ('a, 'b) t := ('a, 'b) t)

  module type M_quickcheck = M_quickcheck

  let of_alist_option m alist = Result.ok (of_alist_or_error m alist)

  let quickcheck_generator_m__t
    (type key)
    (module Key : M_quickcheck with type t = key)
    quickcheck_generator_data
    =
    [%quickcheck.generator: (Key.t * data) List.t]
    |> Quickcheck.Generator.filter_map ~f:(of_alist_option (module Key))
  ;;

  let quickcheck_observer_m__t
    (type key)
    (module Key : M_quickcheck with type t = key)
    quickcheck_observer_data
    =
    [%quickcheck.observer: (Key.t * data) List.t] |> Quickcheck.Observer.unmap ~f:to_alist
  ;;

  let quickcheck_shrinker_m__t
    (type key)
    (module Key : M_quickcheck with type t = key)
    quickcheck_shrinker_data
    =
    [%quickcheck.shrinker: (Key.t * data) List.t]
    |> Quickcheck.Shrinker.filter_map
         ~f:(of_alist_option (module Key))
         ~f_inverse:to_alist
  ;;
end

include For_deriving

let hashable = Hashtbl.Private.hashable
