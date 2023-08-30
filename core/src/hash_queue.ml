open! Import
open Hash_queue_intf

module type Key = Key
module type S_backend = S_backend

module Make_backend (Table : Hashtbl_intf.Hashtbl) : S_backend = struct
  module type Backend =
    S1
      with type 'key create_arg := 'key Hashtbl.Hashable.t
      with type 'key create_key := 'key

  module Backend : Backend = struct
    module Key_value = struct
      module T = struct
        type ('key, 'value) t =
          { key : 'key
          ; mutable value : 'value
          }
      end

      include T

      let key t = t.key
      let value t = t.value

      let sexp_of_t sexp_of_key sexp_of_data { key; value } =
        [%sexp_of: key * data] (key, value)
      ;;
    end

    open Key_value.T
    module Elt = Doubly_linked.Elt

    type ('key, 'data) t =
      { mutable num_readers : int
      ; queue : ('key, 'data) Key_value.t Doubly_linked.t
      ; table : ('key, ('key, 'data) Key_value.t Elt.t) Table.t
      }

    let sexp_of_t sexp_of_key sexp_of_data t =
      [%sexp_of: (key, data) Key_value.t Doubly_linked.t] t.queue
    ;;

    let invariant t =
      assert (Doubly_linked.length t.queue = Table.length t.table);
      (* Look at each element in the queue, checking:
       *   - every element in the queue is in the hash table
       *   - there are no duplicate keys
       *)
      let keys = Table.create ~size:(Table.length t.table) (Table.hashable_s t.table) in
      Doubly_linked.iter t.queue ~f:(fun kv ->
        let key = kv.key in
        match Table.find t.table key with
        | None -> assert false
        | Some _ ->
          assert (not (Table.mem keys key));
          Table.set keys ~key ~data:())
    ;;

    let create ?(growth_allowed = true) ?(size = 16) hashable =
      { num_readers = 0
      ; queue = Doubly_linked.create ()
      ; table = Table.create ~growth_allowed ~size (Table.Hashable.to_key hashable)
      }
    ;;

    let read t f =
      t.num_readers <- t.num_readers + 1;
      Exn.protect ~f ~finally:(fun () -> t.num_readers <- t.num_readers - 1) [@nontail]
    ;;

    let ensure_can_modify t =
      if t.num_readers > 0
      then failwith "It is an error to modify a Hash_queue.t while iterating over it."
    ;;

    let clear t =
      ensure_can_modify t;
      Doubly_linked.clear t.queue;
      Table.clear t.table
    ;;

    let length t = Table.length t.table
    let is_empty t = length t = 0

    let lookup t k =
      match Table.find t.table k with
      | None -> None
      | Some elt -> Some (Elt.value elt).value
    ;;

    let lookup_exn t k = (Elt.value (Table.find_exn t.table k)).value
    let mem t k = Table.mem t.table k

    (* Note that this is the tail-recursive Core_list.map *)
    let to_list t = List.map (Doubly_linked.to_list t.queue) ~f:Key_value.value
    let to_array t = Array.map (Doubly_linked.to_array t.queue) ~f:Key_value.value

    let for_all t ~f =
      read t (fun () ->
        Doubly_linked.for_all t.queue ~f:(fun kv -> f kv.value) [@nontail]) [@nontail]
    ;;

    let exists t ~f =
      read t (fun () -> Doubly_linked.exists t.queue ~f:(fun kv -> f kv.value) [@nontail]) 
      [@nontail]
    ;;

    let find_map t ~f =
      read t (fun () ->
        Doubly_linked.find_map t.queue ~f:(fun kv -> f kv.value) [@nontail]) [@nontail]
    ;;

    let find t ~f =
      read t (fun () ->
        Option.map
          (Doubly_linked.find t.queue ~f:(fun kv -> f kv.value))
          ~f:Key_value.value) [@nontail]
    ;;

    let enqueue_unchecked t back_or_front key value =
      let contents = { Key_value.key; value } in
      let elt =
        match back_or_front with
        | `back -> Doubly_linked.insert_last t.queue contents
        | `front -> Doubly_linked.insert_first t.queue contents
      in
      Table.set t.table ~key ~data:elt
    ;;

    let enqueue t back_or_front key value =
      ensure_can_modify t;
      if Table.mem t.table key
      then `Key_already_present
      else (
        enqueue_unchecked t back_or_front key value;
        `Ok)
    ;;

    let enqueue_back t = enqueue t `back
    let enqueue_front t = enqueue t `front

    let raise_enqueue_duplicate_key t key =
      raise_s
        [%message
          "Hash_queue.enqueue_exn: duplicate key"
            ~_:(Table.sexp_of_key t.table key : Sexp.t)]
    ;;

    let enqueue_exn t back_or_front key value =
      match enqueue t back_or_front key value with
      | `Key_already_present -> raise_enqueue_duplicate_key t key
      | `Ok -> ()
    ;;

    let enqueue_back_exn t = enqueue_exn t `back
    let enqueue_front_exn t = enqueue_exn t `front

    (* Performance hack: we implement this version separately to avoid allocation from the
       option. *)
    let lookup_and_move_to_back_exn t key =
      ensure_can_modify t;
      let elt = Table.find_exn t.table key in
      Doubly_linked.move_to_back t.queue elt;
      Key_value.value (Elt.value elt)
    ;;

    let lookup_and_move_to_back t key =
      let open Option.Let_syntax in
      ensure_can_modify t;
      let%map elt = Table.find t.table key in
      Doubly_linked.move_to_back t.queue elt;
      Key_value.value (Elt.value elt)
    ;;

    let lookup_and_move_to_front_exn t key =
      ensure_can_modify t;
      let elt = Table.find_exn t.table key in
      Doubly_linked.move_to_front t.queue elt;
      Key_value.value (Elt.value elt)
    ;;

    let lookup_and_move_to_front t key =
      let open Option.Let_syntax in
      ensure_can_modify t;
      let%map elt = Table.find t.table key in
      Doubly_linked.move_to_front t.queue elt;
      Key_value.value (Elt.value elt)
    ;;

    let dequeue_with_key t back_or_front =
      ensure_can_modify t;
      let maybe_kv =
        match back_or_front with
        | `back -> Doubly_linked.remove_last t.queue
        | `front -> Doubly_linked.remove_first t.queue
      in
      match maybe_kv with
      | None -> None
      | Some kv ->
        Table.remove t.table kv.key;
        Some (kv.key, kv.value)
    ;;

    let raise_dequeue_with_key_empty () =
      raise_s [%message "Hash_queue.dequeue_with_key: empty queue"]
    ;;

    let dequeue_with_key_exn t back_or_front =
      match dequeue_with_key t back_or_front with
      | None -> raise_dequeue_with_key_empty ()
      | Some (k, v) -> k, v
    ;;

    let dequeue_back_with_key t = dequeue_with_key t `back
    let dequeue_back_with_key_exn t = dequeue_with_key_exn t `back
    let dequeue_front_with_key t = dequeue_with_key t `front
    let dequeue_front_with_key_exn t = dequeue_with_key_exn t `front

    let dequeue t back_or_front =
      match dequeue_with_key t back_or_front with
      | None -> None
      | Some (_, v) -> Some v
    ;;

    let dequeue_back t = dequeue t `back
    let dequeue_front t = dequeue t `front

    let last_with_key t =
      match Doubly_linked.last t.queue with
      | None -> None
      | Some { key; value } -> Some (key, value)
    ;;

    let last t =
      match Doubly_linked.last t.queue with
      | None -> None
      | Some kv -> Some kv.value
    ;;

    let first_with_key t =
      match Doubly_linked.first t.queue with
      | None -> None
      | Some { key; value } -> Some (key, value)
    ;;

    let first t =
      match Doubly_linked.first t.queue with
      | None -> None
      | Some kv -> Some kv.value
    ;;

    let raise_dequeue_empty () = raise_s [%message "Hash_queue.dequeue_exn: empty queue"]

    let dequeue_exn t back_or_front =
      match dequeue t back_or_front with
      | None -> raise_dequeue_empty ()
      | Some v -> v
    ;;

    let dequeue_back_exn t = dequeue_exn t `back
    let dequeue_front_exn t = dequeue_exn t `front

    let keys t =
      (* Return the keys in the order of the queue. *)
      List.map (Doubly_linked.to_list t.queue) ~f:Key_value.key
    ;;

    let to_alist t =
      List.map (Doubly_linked.to_list t.queue) ~f:(fun kv -> kv.key, kv.value)
    ;;

    let iteri t ~f =
      read t (fun () ->
        Doubly_linked.iter t.queue ~f:(fun kv -> f ~key:kv.key ~data:kv.value) [@nontail]) 
      [@nontail]
    ;;

    let iter t ~f = iteri t ~f:(fun ~key:_ ~data -> f data) [@nontail]

    let foldi t ~init ~f =
      read t (fun () ->
        Doubly_linked.fold t.queue ~init ~f:(fun ac kv -> f ac ~key:kv.key ~data:kv.value) 
        [@nontail]) [@nontail]
    ;;

    let fold t ~init ~f = foldi t ~init ~f:(fun ac ~key:_ ~data -> f ac data) [@nontail]
    let count t ~f = Container.count ~fold t ~f
    let sum m t ~f = Container.sum m ~fold t ~f
    let min_elt t ~compare = Container.min_elt ~fold t ~compare
    let max_elt t ~compare = Container.max_elt ~fold t ~compare
    let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t
    let fold_until t ~init ~f ~finish = Container.fold_until ~fold ~init ~f t ~finish

    let dequeue_all t ~f =
      let rec loop () =
        match dequeue_front t with
        | None -> ()
        | Some v ->
          f v;
          loop ()
      in
      loop () [@nontail]
    ;;

    let remove t k =
      ensure_can_modify t;
      match Table.find_and_remove t.table k with
      | None -> `No_such_key
      | Some elt ->
        Doubly_linked.remove t.queue elt;
        `Ok
    ;;

    let raise_remove_unknown_key t key =
      raise_s
        [%message
          "Hash_queue.remove_exn: unknown key" ~_:(Table.sexp_of_key t.table key : Sexp.t)]
    ;;

    let remove_exn t k =
      ensure_can_modify t;
      match remove t k with
      | `No_such_key -> raise_remove_unknown_key t k
      | `Ok -> ()
    ;;

    let lookup_and_remove t k =
      ensure_can_modify t;
      match Table.find_and_remove t.table k with
      | None -> None
      | Some elt ->
        Doubly_linked.remove t.queue elt;
        Some (Elt.value elt).value
    ;;

    let replace t k v =
      ensure_can_modify t;
      match Table.find t.table k with
      | None -> `No_such_key
      | Some elt ->
        (Elt.value elt).value <- v;
        `Ok
    ;;

    let replace_or_enqueue t back_or_front key value =
      ensure_can_modify t;
      match Table.find t.table key with
      | None -> enqueue_unchecked t back_or_front key value
      | Some elt -> (Elt.value elt).value <- value
    ;;

    let replace_or_enqueue_front t key value = replace_or_enqueue t `front key value
    let replace_or_enqueue_back t key value = replace_or_enqueue t `back key value

    let raise_replace_unknown_key t key =
      raise_s
        [%message
          "Hash_queue.replace_exn: unknown key"
            ~_:(Table.sexp_of_key t.table key : Sexp.t)]
    ;;

    let replace_exn t k v =
      ensure_can_modify t;
      match replace t k v with
      | `No_such_key -> raise_replace_unknown_key t k
      | `Ok -> ()
    ;;

    let drop ?(n = 1) t back_or_front =
      if n >= length t
      then clear t
      else
        for _ = 1 to n do
          ignore (dequeue_with_key t back_or_front : _ option)
        done
    ;;

    let drop_back ?n t = drop ?n t `back
    let drop_front ?n t = drop ?n t `front

    let copy t =
      let copied = create ~size:(length t) (Table.hashable t.table) in
      iteri t ~f:(fun ~key ~data -> enqueue_back_exn copied key data);
      copied
    ;;
  end

  module type S = S0 with type ('key, 'data) hash_queue := ('key, 'data) Backend.t

  module Make_with_hashable (T : sig
    module Key : Key

    val hashable : Key.t Hashtbl.Hashable.t
  end) : S with type key = T.Key.t = struct
    include (Backend : Backend with type ('k, 'd) t := ('k, 'd) Backend.t)

    type key = T.Key.t
    type 'data t = (T.Key.t, 'data) Backend.t [@@deriving sexp_of]

    let hashable = T.hashable
    let create ?growth_allowed ?size () = create ?growth_allowed ?size hashable
  end

  module Make (Key : Key) : S with type key = Key.t = Make_with_hashable (struct
    module Key = Key

    let hashable = Table.Hashable.of_key (module Key)
  end)

  include Backend
end

include Make_backend (Hashtbl)
