open! Import


type ('a, 'b) t =
  { entry_by_key                             : ('a, 'b Weak_pointer.t) Hashtbl.t
  ; keys_with_unused_data                    : 'a Thread_safe_queue.t
  ; mutable thread_safe_run_when_unused_data : unit -> unit
  }
[@@deriving sexp_of]

module Using_hashable = struct
  let create ?growth_allowed ?size hashable =
    { entry_by_key                     = Hashtbl.Using_hashable.create ~hashable
                                           ?growth_allowed ?size ()
    ; keys_with_unused_data            = Thread_safe_queue.create ()
    ; thread_safe_run_when_unused_data = ignore
    }
  ;;
end

let create ?growth_allowed ?size m =
  Using_hashable.create ?growth_allowed ?size (Hashtbl.Hashable.of_key m)
;;

let set_run_when_unused_data t ~thread_safe_f =
  t.thread_safe_run_when_unused_data <- thread_safe_f;
;;

let remove t key = Hashtbl.remove t.entry_by_key key

(* In order for a call to [reclaim_space_for_keys_with_unused_data] to reclaim a key that
   was previously finalized, the weak pointer must have been cleared.  This relies on the
   fact that the OCaml garbage collector clears weaks and then runs finalizers. *)
let reclaim_space_for_keys_with_unused_data t =
  while Thread_safe_queue.length t.keys_with_unused_data > 0 do
    let key = Thread_safe_queue.dequeue_exn t.keys_with_unused_data in
    match Hashtbl.find t.entry_by_key key with
    | None -> ()
    | Some entry -> if Weak_pointer.is_none entry then remove t key
  done;
;;

let get_entry t key =
  Hashtbl.find_or_add t.entry_by_key key
    ~default:(fun () -> Weak_pointer.create ());
;;

let mem t key =
  match Hashtbl.find t.entry_by_key key with
  | None -> false
  | Some entry -> Weak_pointer.is_some entry
;;

let key_is_using_space t key = Hashtbl.mem t.entry_by_key key

let set_data t key entry data =
  Weak_pointer.set entry data;
  Gc.Expert.add_finalizer_last data (fun () ->
    Thread_safe_queue.enqueue t.keys_with_unused_data key;
    t.thread_safe_run_when_unused_data ());
;;

let replace t ~key ~data = set_data t key (get_entry t key) data

let add_exn t ~key ~data =
  let entry = get_entry t key in
  if Weak_pointer.is_some entry
  then failwiths "Weak_hashtbl.add_exn of key in use" t [%sexp_of: (_, _) t];
  set_data t key entry data;
;;

let find t key =
  match Hashtbl.find t.entry_by_key key with
  | None -> None
  | Some entry -> Weak_pointer.get entry
;;

let find_or_add t key ~default =
  let entry = get_entry t key in
  match Weak_pointer.get entry with
  | Some v -> v
  | None ->
    let data = default () in
    set_data t key entry data;
    data
;;
