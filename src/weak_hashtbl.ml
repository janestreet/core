open Core_kernel.Std


module Entry = struct

  type 'a t = 'a Weak.t [@@deriving sexp_of]

  let create () = Weak.create ~len:1

  let data t = Weak.get t 0

  let set_data t data = Weak.set t 0 (Some data)

  let is_in_use t = Weak.is_some t 0

end

type ('a, 'b) t =
  { entry_by_key                             : ('a, 'b Entry.t) Hashtbl.t
  ; keys_with_unused_data                    : 'a Thread_safe_queue.t
  ; mutable thread_safe_run_when_unused_data : unit -> unit
  }
[@@deriving sexp_of]

let create ?growth_allowed ?size hashable =
  { entry_by_key                     = Hashtbl.create ~hashable ?growth_allowed ?size ()
  ; keys_with_unused_data            = Thread_safe_queue.create ()
  ; thread_safe_run_when_unused_data = ignore
  }
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
    | Some entry -> if not (Entry.is_in_use entry) then remove t key
  done;
;;

let get_entry t key = Hashtbl.find_or_add t.entry_by_key key ~default:Entry.create

let mem t key =
  match Hashtbl.find t.entry_by_key key with
  | None -> false
  | Some entry -> Entry.is_in_use entry
;;

let key_is_using_space t key = Hashtbl.mem t.entry_by_key key

let set_data t key entry data =
  Entry.set_data entry data;
  Gc.Expert.add_finalizer data (fun _ ->
    Thread_safe_queue.enqueue t.keys_with_unused_data key;
    t.thread_safe_run_when_unused_data ());
;;

let replace t ~key ~data = set_data t key (get_entry t key) data

let add_exn t ~key ~data =
  let entry = get_entry t key in
  if Entry.is_in_use entry
  then failwiths "Weak_hashtbl.add_exn of key in use" t [%sexp_of: (_, _) t];
  set_data t key entry data;
;;

let find t key =
  match Hashtbl.find t.entry_by_key key with
  | None -> None
  | Some entry -> Entry.data entry
;;

let find_or_add t key ~default =
  let entry = get_entry t key in
  match Entry.data entry with
  | Some v -> v
  | None ->
    let data = default () in
    set_data t key entry data;
    data
;;
