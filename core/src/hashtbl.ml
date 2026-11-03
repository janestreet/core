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
    type (!'a, !'b) t = ('a, 'b) Hashtbl.t [@@deriving sexp_of]

    module type Non_value = Base.Hashtbl.Non_value

    include Base.Hashtbl.S_without_submodules with type ('a, 'b) t := ('a, 'b) t
    include Hashtbl_equality with type ('k, 'v) t := ('k, 'v) t
    include Non_value with type ('k, 'v) t := ('k, 'v) t
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

      let[@inline always] iter t ~f =
        iteri t ~f:(fun ~key ~data -> f (key, data)) [@nontail]
      ;;

      let init ~len ~next =
        let t = create ~size:len () in
        for _i = 0 to len - 1 do
          let key, data = next () in
          match add t ~key ~data with
          | `Ok -> ()
          | `Duplicate -> failwith "Core_hashtbl.bin_read_t_: duplicate key"
        done;
        t
      ;;
    end)
end

module
  [@inline]
  Provide_bin_io (Key : sig
    type t [@@deriving bin_io]

    include Key_plain with type t := t
  end) =
Bin_prot.Utils.Make_iterable_binable1 [@inlined hint] (struct
    module Key = Key

    type nonrec 'v t = (Key.t, 'v) t
    type 'v el = Key.t * 'v

    (* You may be tempted to replace these with
       [type 'v el = Key.t * 'v [@@deriving bin_io]], but this generates a new
       [Bin_shape.t], which is both allocating and side-effectful, and difficult for the
       compiler to eliminate in [bin_size_m__t] etc. *)

    let bin_size_el (type v) (bin_size_v : v Bin_prot.Size.sizer) = [%bin_size: Key.t * v]

    let bin_write_el (type v) (bin_write_v : v Bin_prot.Write.writer) =
      [%bin_write: Key.t * v]
    ;;

    let bin_read_el (type v) (bin_read_v : v Bin_prot.Read.reader) =
      [%bin_read: Key.t * v]
    ;;

    let bin_shape_el bin_shape_v = [%bin_shape: Key.t * v]

    let caller_identity =
      Bin_prot.Shape.Uuid.of_string "8fabab0a-4992-11e6-8cca-9ba2c4686d9e"
    ;;

    let module_name = Some "Core.Hashtbl"
    let length = length

    let[@inline always] iter t ~f =
      iteri t ~f:(fun ~key ~data -> f (key, data)) [@nontail]
    ;;

    let init ~len ~next =
      let t = create ~size:len (module Key) in
      for _i = 0 to len - 1 do
        let key, data = next () in
        match add t ~key ~data with
        | `Ok -> ()
        | `Duplicate ->
          failwiths "Hashtbl.bin_read_t: duplicate key" key [%sexp_of: Key.t]
      done;
      t
    ;;
  end)

module Make_plain_with_hashable (T : sig
    module Key : sig
      type t

      include Key_plain with type t := t
    end

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
end

module Provide_of_sexp (Key : Base.Hashtbl.M_of_sexp) = struct
  let t_of_sexp v_of_sexp sexp = Base.Hashtbl.m__t_of_sexp (module Key) v_of_sexp sexp
end

module Provide_stable_witness (Key : sig
    type t [@@deriving stable_witness]
  end) =
struct
  (* The binary representation of hashtbl is relied on by stable modules (e.g.
     Hashtable.Stable) and is therefore assumed to be stable. So, if the key and data can
     provide a stable witnesses, then we can safely the hashtbl is also stable. *)
  let stable_witness (type data) (_data_stable_witness : data Stable_witness.t)
    : (Key.t, data) t Stable_witness.t
    =
    let (_ : Key.t Stable_witness.t) = Key.stable_witness in
    Stable_witness.assert_stable
  ;;
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

module Make_plain (Key : sig
    type t

    include Key_plain with type t := t
  end) =
Make_plain_with_hashable (struct
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
module type For_deriving_stable = For_deriving_stable

module For_deriving : sig
  include For_deriving with type ('a, 'b) t := ('a, 'b) t
end = struct
  include (
    Hashtbl :
    sig
      include Hashtbl.For_deriving with type ('a, 'b) t := ('a, 'b) t
    end)

  module type M_quickcheck = M_quickcheck

  let of_alist_option m alist = Result.ok (of_alist_or_error m alist)

  let quickcheck_generator_m__t
    (type key)
    (module Key : M_quickcheck with type t = key)
    quickcheck_generator_data
    =
    [%quickcheck.generator: (Key.t * data) List.t]
    |> Base_quickcheck.Generator.filter_map ~f:(of_alist_option (module Key))
  ;;

  let quickcheck_observer_m__t
    (type key)
    (module Key : M_quickcheck with type t = key)
    quickcheck_observer_data
    =
    [%quickcheck.observer: (Key.t * data) List.t]
    |> Base_quickcheck.Observer.unmap ~f:to_alist
  ;;

  let quickcheck_shrinker_m__t
    (type key)
    (module Key : M_quickcheck with type t = key)
    quickcheck_shrinker_data
    =
    [%quickcheck.shrinker: (Key.t * data) List.t]
    |> Base_quickcheck.Shrinker.filter_map
         ~f:(of_alist_option (module Key))
         ~f_inverse:to_alist
  ;;

  let[@inline] bin_shape_m__t (type t) (module Key : Key_binable with type t = t) =
    let module M = Provide_bin_io (Key) in
    M.bin_shape_t
  ;;

  let[@inline] bin_size_m__t (type t) (module Key : Key_binable with type t = t) =
    let module M = Provide_bin_io (Key) in
    M.bin_size_t
  ;;

  let[@inline] bin_write_m__t (type t) (module Key : Key_binable with type t = t) =
    let module M = Provide_bin_io (Key) in
    M.bin_write_t
  ;;

  let[@inline] bin_read_m__t (type t) (module Key : Key_binable with type t = t) =
    let module M = Provide_bin_io (Key) in
    M.bin_read_t
  ;;

  let[@inline] __bin_read_m__t__ (type t) (module Key : Key_binable with type t = t) =
    let module M = Provide_bin_io (Key) in
    M.__bin_read_t__
  ;;

  type binio =
    { bin_shape_m__t :
        'a. (module Key_binable with type t = 'a) -> Bin_shape.t -> Bin_shape.t
    ; bin_size_m__t :
        'a 'b.
        (module Key_binable with type t = 'a) -> ('b, ('a, 'b) t) Bin_prot.Size.sizer1
    ; bin_write_m__t :
        'a 'b.
        (module Key_binable with type t = 'a) -> ('b, ('a, 'b) t) Bin_prot.Write.writer1
    ; bin_read_m__t :
        'a 'b.
        (module Key_binable with type t = 'a) -> ('b, ('a, 'b) t) Bin_prot.Read.reader1
    ; __bin_read_m__t__ :
        'a 'b.
        (module Key_binable with type t = 'a)
        -> ('b, ('a, 'b) t) Bin_prot.Read.vtag_reader1
    }

  let binio =
    { bin_shape_m__t; bin_size_m__t; bin_write_m__t; bin_read_m__t; __bin_read_m__t__ }
    |> Portability_hacks.magic_portable__needs_portable_functors
  ;;

  let { bin_shape_m__t; bin_size_m__t; bin_write_m__t; bin_read_m__t; __bin_read_m__t__ } =
    binio
  ;;
end

module For_deriving_stable = struct
  let stable_witness_m__t (type t) (module Key : Stable_witness.S with type t = t) =
    let module M = Provide_stable_witness (Key) in
    M.stable_witness
  ;;
end

include For_deriving

module Stable = struct
  module V1 = struct
    type nonrec ('key, 'data) t = ('key, 'data) Poly.t [@@deriving bin_io, sexp]

    module M = Hashtbl.M
    include For_deriving
    include For_deriving_stable

    let stable_witness
      (type key data)
      (key_stable_witness : key Stable_witness.t)
      (data_stable_witness : data Stable_witness.t)
      : (key, data) t Stable_witness.t
      =
      stable_witness_m__t
        (module struct
          type t = key

          let stable_witness = key_stable_witness
        end)
        data_stable_witness
    ;;
  end
end

let hashable = Hashtbl.Private.hashable
