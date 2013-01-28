


open Core.Std

  hash: 'k -> int;
  compare: 'k -> 'k -> int;
}

external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"

let poly = {
  hash = (fun z -> hash_param 10 100 z);
  compare = Pervasives.compare
}

type params = {
  load_factor: int;
  grow: bool;
  initial_size: int;
} with sexp

let default_params = {
  load_factor = 2;
  grow = true;
  initial_size = 1;
}

let size n = { default_params with initial_size = n }

module type Basic = sig
  type ('k, 'v) t

  val create : ?params:params -> 'k hashable -> ('k, 'v) t

  val invariant : ('k, 'v) t -> unit
  val hashable : ('k, 'v) t -> 'k hashable
  val set_params : (_, _) t -> params -> unit
  val get_params : (_, _) t -> params

  val find : ('k, 'v) t -> 'k -> 'v option
  val fold : ('k, 'v) t -> init:'c -> f:(key:'k -> data:'v -> 'c -> 'c) -> 'c
  val length : ('k, 'v) t -> int
  val mem : ('k, 'v) t -> 'k -> bool
  val clear : ('k, 'v) t -> unit
  val remove : ('k, 'v) t -> 'k -> unit
  val add : ('k, 'v) t -> key:'k -> data:'v -> unit
end

module type S = sig
  include Basic

  include Core.Std.Sexpable.S2 with type ('k, 'v) sexpable = ('k, 'v) t

  module Specialize (Key: sig
    type t
    include Core.Std.Sexpable with type t := t

    val hash : t -> int
    val compare : t -> t -> int
  end) : sig

    val hashable : Key.t hashable

    module Table : sig
      type ('k, 'v) z = ('k, 'v) t
      type 'v t = (Key.t, 'v) z

      include Core.Std.Sexpable.S1 with type 'v sexpable = 'v t
    end
  end

  val copy : ('k, 'v) t -> ('k, 'v) t
  val iter : ('k, 'v) t -> f:(key:'k -> data:'v -> unit) -> unit
  val map : ('k, 'v) t -> f:('v -> 'c) -> ('k, 'c) t
  val mapi : ('k, 'v) t -> f:(key:'k -> data:'v -> 'c) -> ('k, 'c) t
  val filter_map : ('k, 'v) t -> f:('v -> 'c option) -> ('k, 'c) t
  val filter_mapi : ('k, 'v) t -> f:(key:'k -> data:'v -> 'c option) -> ('k, 'c) t

  val find_default : ('k, 'v) t -> 'k -> default:(unit -> 'v) -> 'v
  val find_exn : ('k, 'v) t -> 'k -> 'v
  val iter_vals : ('k, 'v) t -> f:('v -> unit) -> unit

  val of_alist : ?params:params
    -> 'k hashable
    -> ('k * 'v) list
    -> [ `Ok of ('k, 'v) t | `Duplicate_key of 'k ]

  val of_alist_exn : 'k hashable
    -> ('k * 'v) list -> ('k, 'v) t

  val to_alist : ('k, 'v) t -> ('k * 'v) list

  val merge : ?params:params
    -> f:(key:'k -> 'a option -> 'b option -> 'c option)
    -> ('k, 'a) t -> ('k, 'b) t -> ('k, 'c) t

  val keys : ('k, 'v) t -> 'k list
  val data : ('k, 'v) t -> 'v list

  val filter_inplace : ('k, 'v) t -> f:('v -> bool) -> unit
  val filteri_inplace : ('k, 'v) t -> f:('k -> 'v -> bool) -> unit

  val equal : ('k, 'v) t
    -> ('k, 'v) t
    -> ('v -> 'v -> bool)
    -> bool

  val add_to_groups : ('k, 'v) t
    -> get_key:('r -> 'k)
    -> get_data:('r -> 'v)
    -> combine:('v -> 'v -> 'v)
    -> rows:'r list
    -> unit

  val group : ?params:params
    -> hashable:'k hashable
    -> get_key:('r -> 'k)
    -> get_data:('r -> 'v)
    -> combine:('v -> 'v -> 'v)
    -> 'r list
    -> ('k, 'v) t

  val create_mapped : ?params:params
    -> hashable:'k hashable
    -> get_key:('r -> 'k)
    -> get_data:('r -> 'v)
    -> 'r list
    -> ('k, 'v) t

  val create_with_key : ?params:params
    -> hashable:'k hashable
    -> get_key:('v -> 'k)
    -> 'v list
    -> ('k, 'v) t
end

module Make (Basic : Basic) : S with type ('k, 'v) t = ('k, 'v) Basic.t = struct
  include Basic


  let find_exn t id =
    match find t id with
    | None -> raise Not_found
    | Some x -> x

  let mapi t ~f =
    let bindings =
      fold t ~init:[] ~f:(fun ~key ~data bindings ->
        (key, f ~key ~data) :: bindings)
    in
    let new_t = create ~params:(get_params t) (hashable t) in
    List.iter bindings ~f:(fun (key,data) -> add new_t ~key ~data);
    new_t

  let iter t ~f =
    fold t ~init:() ~f:(fun ~key ~data () -> f ~key ~data)

  let map t ~f = mapi t ~f:(fun ~key:_ ~data -> f data)

  let copy t = map t ~f:ident

  let filter_mapi t ~f =
    let bindings =
      fold t ~init:[] ~f:(fun ~key ~data bindings -> match f ~key ~data with
      | Some new_data -> (key,new_data) :: bindings
      | None -> bindings)
    in
    let new_t = create ~params:(get_params t) (hashable t) in
    List.iter bindings ~f:(fun (key,data) -> add new_t ~key ~data);
    new_t

  let filter_map t ~f = filter_mapi t ~f:(fun ~key:_ ~data -> f data)

  let remove_all = remove

  let find_default t id ~default =
    match find t id with
    | Some x -> x
    | None ->
        let default = default () in
        add t ~key:id ~data:default;
        default

  let iter_vals t ~f = iter t ~f:(fun ~key:_ ~data -> f data)

  let of_alist ?params hashable lst =
    let t = create ?params hashable in
    let res = ref (`Ok t) in
    List.iter lst ~f:(fun (k, v) ->
      match mem t k with
      | true -> res := `Duplicate_key k
      | false -> add t ~key:k ~data:v);
    !res

  let of_alist_exn hashable lst =
    match of_alist hashable lst with
    | `Ok v -> v
    | `Duplicate_key _k -> failwith "Hashtbl.of_alist_exn: duplicate key"

  let to_alist t =
    fold ~f:(fun ~key ~data list -> (key, data)::list) ~init:[] t

  let keys t = fold t ~init:[] ~f:(fun ~key ~data:_ acc -> key :: acc)

  let data t = fold ~f:(fun ~key:_ ~data list -> data::list) ~init:[] t

  let add_to_groups groups ~get_key ~get_data ~combine ~rows =
    List.iter rows ~f:(fun row ->
      let key = get_key row in
      let data = get_data row in
      let data =
        match find groups key with
        | None -> data
        | Some old -> combine old data
      in
      add groups ~key ~data)
  ;;

  let group ?params ~hashable ~get_key ~get_data ~combine rows =
    let res = create ?params hashable in
    add_to_groups res ~get_key ~get_data ~combine ~rows;
    res
  ;;

  let create_mapped ?params ~hashable ~get_key ~get_data rows =
    let res = create ?params hashable in
    List.iter rows ~f:(fun r ->
      let key = get_key r in
      let data = get_data r in
      add res ~key ~data);
    res
  ;;

  let create_with_key ?params ~hashable ~get_key rows =
    create_mapped ?params ~hashable ~get_key ~get_data:(fun x -> x) rows
  ;;

  let merge ?params ~f t1 t2 =
    let t = create ?params (hashable t1) in
    let unique_keys = create ?params (hashable t1) in
    let record_key ~key ~data:_ = add unique_keys ~key ~data:() in
    iter t1 ~f:record_key;
    iter t2 ~f:record_key;
    iter unique_keys ~f:(fun ~key ~data:_ ->
                           match f ~key (find t1 key) (find t2 key) with
                           | Some data -> add t ~key ~data
                           | None -> ());
    t

  let filteri_inplace t ~f =
    let to_remove =
      fold t ~init:[] ~f:(fun ~key ~data ac ->
        if f key data then ac else key :: ac)
    in
    List.iter to_remove ~f:(fun key -> remove t key);
  ;;

  let filter_inplace t ~f =
    filteri_inplace t ~f:(fun _ data -> f data)
  ;;

  let sexp_of_t sexp_of_k sexp_of_d t =
    let coll ~key:k ~data:v acc = Sexp.List [sexp_of_k k; sexp_of_d v] :: acc in
    Sexp.List (fold ~f:coll t ~init:[])

  let t_of_sexp k_of_sexp d_of_sexp sexp =
    match sexp with
    | Sexp.List sexps ->
        let t = create poly in
        List.iter sexps ~f:(function
          | Sexp.List [k_sexp; v_sexp] ->
              add t ~key:(k_of_sexp k_sexp) ~data:(d_of_sexp v_sexp)
          | Sexp.List _ | Sexp.Atom _ ->
              Sexplib.Conv.of_sexp_error "Hashtbl.t_of_sexp: tuple list needed" sexp);
        t
    | Sexp.Atom _ ->
        Sexplib.Conv.of_sexp_error
          "Hashtbl.t_of_sexp: found atom where list was expected" sexp

  exception Not_equal
  let equal t t' equal =
    try
      iter t ~f:(fun ~key ~data ->
        match find t' key with
        | None -> raise Not_equal
        | Some data' -> if not (equal data data') then raise Not_equal);
      true
    with Not_equal -> false
  ;;

  module Specialize (Key: sig
    type t
    include Sexpable with type t := t

    val hash : t -> int
    val compare : t -> t -> int
  end) = struct

    let hashable = {
      hash = Key.hash;
      compare = Key.compare
    }

    module Table = struct
      type ('k, 'v) z = ('k, 'v) t
      type 'v t = (Key.t, 'v) z
      let sexp_of_t sexp_of_d t = sexp_of_t Key.sexp_of_t sexp_of_d t

      let t_of_sexp d_of_sexp sexp =
        match sexp with
        | Sexp.List sexps ->
            let t = create hashable in
            List.iter sexps ~f:(function
              | Sexp.List [k_sexp; v_sexp] ->
                  add t ~key:(Key.t_of_sexp k_sexp) ~data:(d_of_sexp v_sexp)
              | Sexp.List _ | Sexp.Atom _ ->
                  Sexplib.Conv.of_sexp_error "Hashtbl.t_of_sexp: tuple list needed" sexp);
            t
        | Sexp.Atom _ ->
            Sexplib.Conv.of_sexp_error
              "Hashtbl.t_of_sexp: found atom where list was expected" sexp
    end
  end
end
