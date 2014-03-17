
open Core_kernel.Std
open Std_internal


module Entry = struct

  type 'a t = 'a Weak.t with sexp_of

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
with sexp_of

let create hashable =
  { entry_by_key                     = Hashtbl.create ~hashable ()
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
  then failwiths "Weak_hashtbl.add_exn of key in use" t <:sexp_of< (_, _) t >>;
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

TEST_UNIT =
  let module M = struct
    type t =
      { foo : int
      ; bar : int
      ; baz : string
      }
  end
  in
  let open M in
  let block foo = Heap_block.create_exn ({ foo; bar = 0; baz = "hello" }, 0) in
  let tbl = create Int.hashable in
  let stabilize () =
    Gc.full_major ();
    reclaim_space_for_keys_with_unused_data tbl;
  in
  let add k b = ignore (find_or_add tbl k ~default:(fun () -> !b)) in
  (* We put the blocks in refs and manually blackhole them, so that the unit test will
     pass with the bytecode compiler. *)
  let b1 = ref (block 1) in
  let b2 = ref (block 2) in
  let b3 = ref (block 3) in
  let b4 = ref (block 4) in
  let blackhole b = b := block 0 in
  let k1 = 1 in
  let k2 = 2 in
  let k3 = 3 in
  add k1 b1;
  add k2 b2;
  add k3 b3;
  (* Checking [is_absent k] is stronger than checking that [is_none (find tbl k)].  We
     want to make sure that a key has been removed from the table, and in particular rule
     out the case where the key is in the table but the corresponding weak is none. *)
  let is_absent k = not (key_is_using_space tbl k) in
  let is_block k b =
    match find tbl k with
    | None -> false
    | Some v -> phys_equal v b
  in
  assert (is_block k1 !b1);
  assert (is_block k2 !b2);
  assert (is_block k3 !b3);
  blackhole b1;
  stabilize ();
  assert (is_absent k1);
  assert (is_block k2 !b2);
  assert (is_block k3 !b3);
  blackhole b2;
  stabilize ();
  assert (is_absent k1);
  assert (is_absent k2);
  assert (is_block k3 !b3);
  replace tbl ~key:k3 ~data:!b4;
  blackhole b3;
  stabilize ();
  assert (is_block k3 !b4);
  blackhole b4;
  stabilize ();
  assert (is_absent k3);
;;
