open! Import
open Hashtbl_intf
module Hashable = Hashtbl_intf.Hashable
module Merge_into_action = Hashtbl_intf.Merge_into_action
module List = List0

let failwiths = Error.failwiths

module%template.portable [@modality p] Creators = Hashtbl.Creators [@modality p]

include (
  Hashtbl :
  sig
  @@ portable
    type ('a, 'b) t = ('a, 'b) Hashtbl.t [@@deriving sexp_of]

    module type Non_value = Base.Hashtbl.Non_value

    include Non_value with type ('k, 'v) t := ('k, 'v) t
    include Base.Hashtbl.S_without_submodules with type ('a, 'b) t := ('a, 'b) t
    include Hashtbl_equality with type ('k, 'v) t := ('k, 'v) t
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

[%%template
[@@@mode.default m = (local, global)]

module type Key_plain = Key_plain [@mode m]
module type Key = Key [@mode m]
module type Key_binable = Key_binable [@mode m]
module type Key_stable = Key_stable [@mode m]]

module Poly = struct
  include Hashtbl.Poly

  let validate = validate

  include%template Bin_prot.Utils.Make_iterable_binable2 [@modality portable] (struct
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
          match find t key with
          | None -> set t ~key ~data
          | Some _ -> failwith "Core_hashtbl.bin_read_t_: duplicate key"
        done;
        t
      ;;
    end)
end

module%template.portable
  [@modality p] Provide_bin_io (Key : sig
    type t [@@deriving bin_io]

    include Key_plain with type t := t
  end) =
Bin_prot.Utils.Make_iterable_binable1 [@modality p] (struct
    module Key = Key

    type nonrec 'v t = (Key.t, 'v) t
    type 'v el = Key.t * 'v [@@deriving bin_io]

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
        match find t key with
        | None -> set t ~key ~data
        | Some _ -> failwiths "Hashtbl.bin_read_t: duplicate key" key [%sexp_of: Key.t]
      done;
      t
    ;;
  end)

module%template.portable
  [@modality p] Make_plain_with_hashable (T : sig
    module Key : Key_plain

    val hashable : Key.t Hashable.t
  end) =
struct
  let hashable = T.hashable

  type key = T.Key.t
  type ('a, 'b) hashtbl = ('a, 'b) t
  type 'a t = (T.Key.t, 'a) hashtbl
  type 'a key_ = T.Key.t

  include Creators [@modality p] (struct
      type 'a t = T.Key.t

      let hashable = hashable
    end)

  include (
    Hashtbl :
    sig
    @@ portable
      include Invariant.S2 with type ('a, 'b) t := ('a, 'b) hashtbl
    end)

  let%template equal = (Hashtbl.equal [@mode m]) [@@mode m = (local, global)]
  let invariant invariant_key t = invariant ignore invariant_key t
  let sexp_of_t sexp_of_v t = Poly.sexp_of_t T.Key.sexp_of_t sexp_of_v t
end

module%template.portable Provide_of_sexp (Key : Base.Hashtbl.M_of_sexp) = struct
  let t_of_sexp v_of_sexp sexp = Base.Hashtbl.m__t_of_sexp (module Key) v_of_sexp sexp
end

module Provide_stable_witness (Key : sig
    type t [@@deriving stable_witness]
  end) =
struct
  (* The binary representation of hashtbl is relied on by stable modules
       (e.g. Hashtable.Stable) and is therefore assumed to be stable.  So, if the key and
       data can provide a stable witnesses, then we can safely the hashtbl is also
       stable. *)
  let stable_witness (type data) (_data_stable_witness : data Stable_witness.t)
    : (Key.t, data) t Stable_witness.t
    =
    let (_ : Key.t Stable_witness.t) = Key.stable_witness in
    Stable_witness.assert_stable
  ;;
end

module%template.portable
  [@modality p] Make_with_hashable (T : sig
    module Key : Key

    val hashable : Key.t Hashable.t
  end) =
struct
  include Make_plain_with_hashable [@modality p] (T)
  include Provide_of_sexp [@modality p] (T.Key)
end

module%template.portable
  [@modality p] Make_binable_with_hashable (T : sig
    module Key : Key_binable

    val hashable : Key.t Hashable.t
  end) =
struct
  include Make_with_hashable [@modality p] (T)
  include Provide_bin_io [@modality p] (T.Key)
end

module%template.portable
  [@modality p] Make_stable_with_hashable (T : sig
    module Key : Key_stable

    val hashable : Key.t Hashable.t
  end) =
struct
  include Make_binable_with_hashable [@modality p] (T)
  include Provide_stable_witness (T.Key)
end

module%template.portable [@modality p] Make_plain (Key : Key_plain) =
Make_plain_with_hashable [@modality p] (struct
    module Key = Key

    let hashable =
      { Hashable.hash = Key.hash; compare = Key.compare; sexp_of_t = Key.sexp_of_t }
    ;;
  end)

module%template.portable [@modality p] Make (Key : Key) = struct
  include Make_plain [@modality p] (Key)
  include Provide_of_sexp [@modality p] (Key)
end

module%template.portable [@modality p] Make_binable (Key : Key_binable) = struct
  include Make [@modality p] (Key)
  include Provide_bin_io [@modality p] (Key)
end

module%template.portable [@modality p] Make_stable (Key : Key_stable) = struct
  include Make_binable [@modality p] (Key)
  include Provide_stable_witness (Key)
end

module M = Hashtbl.M

module type For_deriving = For_deriving

module For_deriving : sig @@ portable
  include For_deriving with type ('a, 'b) t := ('a, 'b) t
end = struct
  include (
    Hashtbl :
    sig
    @@ portable
      include Hashtbl.For_deriving with type ('a, 'b) t := ('a, 'b) t
    end)

  module type%template M_quickcheck = M_quickcheck [@mode m] [@@mode m = (local, global)]

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

  let bin_shape_m__t (type t) (module Key : Key_binable with type t = t) =
    let module M = Provide_bin_io (Key) in
    M.bin_shape_t
  ;;

  let bin_size_m__t (type t) (module Key : Key_binable with type t = t) =
    let module M = Provide_bin_io (Key) in
    M.bin_size_t
  ;;

  let bin_write_m__t (type t) (module Key : Key_binable with type t = t) =
    let module M = Provide_bin_io (Key) in
    M.bin_write_t
  ;;

  let bin_read_m__t (type t) (module Key : Key_binable with type t = t) =
    let module M = Provide_bin_io (Key) in
    M.bin_read_t
  ;;

  let __bin_read_m__t__ (type t) (module Key : Key_binable with type t = t) =
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

include For_deriving

let hashable = Hashtbl.Private.hashable
