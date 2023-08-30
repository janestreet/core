(* This program benchmarks all functions exported by [Hashtbl.Make (...)].  For functions
   that look up / add / remove a single key, we attempt to construct a benchmark that uses
   one hash table, and touches a random key each time.  For operations that behave
   differently when the key is present vs absent, we try to maintain a 50/50 chance on
   average that the operation hits a key each time.  For operations that do not modify the
   key set, we just pick keys out of twice the hash table's initial set.  For operations
   that add or remove keys, we arrange to have a ~50% chance to add or remove on each try.
   For monotonic operations, we either pair them with another operation, or we construct a
   fresh hash table and benchmark repeated application of the operation.

   It's worth noting that the functorization in these tests (which is an artifact of
   trying to test the same things with different implementations, load, etc.) does not
   influence the output of the tests too much. Here is comparison of the int table
   constructed in these tests vs direct use of [Int.Table]:

   ┌──────────────────────────────────────────────────────────────────┬──────────┐
   │ Name                                                             │ Time/Run │
   ├──────────────────────────────────────────────────────────────────┼──────────┤
   │ Hashtbl.Make(Int): find_exn + <rand key>:1                       │  36.30ns │
   │ Hashtbl.Make(Int): find_exn + <rand key>:100                     │  36.76ns │
   │ Hashtbl.Make(Int): find_exn + <rand key>:10000                   │  41.14ns │
   │ Hashtbl.Make(Int): find_exn + <rand key> [using Int.Table]:1     │  34.05ns │
   │ Hashtbl.Make(Int): find_exn + <rand key> [using Int.Table]:100   │  35.02ns │
   │ Hashtbl.Make(Int): find_exn + <rand key> [using Int.Table]:10000 │  36.43ns │
   └──────────────────────────────────────────────────────────────────┴──────────┘

   Similarly testing with hastable data that are immediate vs non-immediate had no
   appreciable effect on the test results.
*)
open Core
open Core_bench

module type Config = sig
  val sizes : int list
  val regex : Re2.t option
end

module type Impl = sig
  include Hashtbl_intf.Hashtbl

  val module_name : string
end

module type Key = sig
  include Hashtbl.Key

  val module_name : string
  val of_int_exn : int -> t
end

module Benchmarks (Config : Config) (Impl : Impl) (Key : Key) : sig
  val benchmarks : Bench.Test.t list
end = struct
  module Table = Impl.Make (Key)

  module Example = struct
    let data size = List.init size ~f:Fn.id
    let keys size = List.init size ~f:Key.of_int_exn
    let key_array size = keys size |> Array.of_list
    let alist size = List.zip_exn (keys size) (data size)
    let t size = Table.of_alist_exn (alist size)
    let t_multi size = Table.of_alist_multi (alist size)
    let sexp size = Table.sexp_of_t sexp_of_int (t size)

    type random =
      { state : Random.State.t
      ; size : int
      ; keys : Key.t array
      }

    let random size =
      let state = Random.State.make [||] in
      let keys = key_array (size * 2) in
      { state; size; keys }
    ;;

    let random_index { state; size; _ } mode =
      match mode with
      | `present -> Random.State.int state size
      | `absent -> Random.State.int state size + size
      | `either -> Random.State.int state (size * 2)
    ;;

    let random_data r mode = random_index r mode
    let random_key r mode = random_index r mode |> Array.get r.keys
  end

  module For_bench = struct
    let queue = Queue.create ()
    let all () = Queue.to_list queue
    let name str = sprintf "%s.Make(%s): %s" Impl.module_name Key.module_name str

    (* we require functions to return [unit] so benchmarks aren't accidentally only
       partially applying some function *)
    let enqueue str (f : int -> (unit -> unit) Staged.t) =
      let name = name str in
      let args = Config.sizes in
      match Config.regex with
      | Some regex when not (Re2.matches regex name) -> ()
      | _ -> Queue.enqueue queue (Bench.Test.create_indexed ~name ~args f)
    ;;
  end

  let ( !! ) = For_bench.enqueue

  (* helpful for users to subtract these times from other benchmarks that use them *)
  let () =
    ( !! ) "<rand key>" (fun size ->
      let r = Example.random size in
      stage (fun () -> ignore (Example.random_key r `either : Key.t)))
  ;;

  let () =
    ( !! ) "<rand data>" (fun size ->
      let r = Example.random size in
      stage (fun () -> ignore (Example.random_data r `either : int)))
  ;;

  module _ : sig
    include Hashtbl_intf.S
    include Hashtbl_intf.Accessors with type ('k, 'v) t := 'v t and type 'k key := key
  end = struct
    type key = Table.key
    type ('k, 'v) hashtbl = ('k, 'v) Table.hashtbl
    type 'v t = 'v Table.t
    type ('k, 'v) t_ = ('k, 'v) Table.t_
    type 'k key_ = 'k Table.key_

    module Provide_of_sexp = Table.Provide_of_sexp
    module Provide_bin_io = Table.Provide_bin_io

    (* benchmarks begin here *)

    let sexp_of_t = Table.sexp_of_t

    let () =
      ( !! ) "sexp_of_t" (fun size ->
        let t = Example.t size in
        stage (fun () -> ignore (sexp_of_t sexp_of_int t : Sexp.t)))
    ;;

    let t_of_sexp = Table.t_of_sexp

    let () =
      ( !! ) "t_of_sexp" (fun size ->
        let sexp = Example.sexp size in
        stage (fun () -> ignore (t_of_sexp int_of_sexp sexp : int t)))
    ;;

    let invariant = Table.invariant

    let () =
      ( !! ) "invariant" (fun size ->
        let t = Example.t size in
        stage (fun () -> invariant ignore t))
    ;;

    let create = Table.create

    let () =
      ( !! ) "create" (fun size ->
        let size = Some size in
        stage (fun () -> ignore (create ?size () : int t)))
    ;;

    let of_alist = Table.of_alist

    let () =
      ( !! ) "of_alist [no dups]" (fun size ->
        let alist = Example.alist size in
        stage (fun () ->
          ignore (of_alist alist : [ `Ok of int t | `Duplicate_key of key ])))
    ;;

    let of_alist_exn = Table.of_alist_exn

    let () =
      ( !! ) "of_alist_exn [no dups]" (fun size ->
        let alist = Example.alist size in
        stage (fun () -> ignore (of_alist_exn alist : int t)))
    ;;

    let of_alist_or_error = Table.of_alist_or_error

    let () =
      ( !! ) "of_alist_or_error [no dups]" (fun size ->
        let alist = Example.alist size in
        stage (fun () -> ignore (of_alist_or_error alist : int t Or_error.t)))
    ;;

    let of_alist_report_all_dups = Table.of_alist_report_all_dups

    let () =
      ( !! ) "of_alist_report_all_dups [no dups]" (fun size ->
        let alist = Example.alist size in
        stage (fun () ->
          ignore
            (of_alist_report_all_dups alist
              : [ `Ok of int t | `Duplicate_keys of key list ])))
    ;;

    let of_alist_multi = Table.of_alist_multi

    let () =
      ( !! ) "of_alist_multi [no dups]" (fun size ->
        let alist = Example.alist size in
        stage (fun () -> ignore (of_alist_multi alist : int list t)))
    ;;

    let create_mapped = Table.create_mapped

    let () =
      ( !! ) "create_mapped [no dups]" (fun size ->
        let alist = Example.alist size in
        stage (fun () ->
          ignore
            (create_mapped alist ~get_key:fst ~get_data:snd
              : [ `Ok of int t | `Duplicate_keys of key list ])))
    ;;

    let create_with_key = Table.create_with_key

    let () =
      ( !! ) "create_with_key [no dups]" (fun size ->
        let data = Example.data size in
        let key_array = Example.key_array size in
        stage (fun () ->
          ignore
            (create_with_key data ~get_key:(Array.get key_array)
              : [ `Ok of int t | `Duplicate_keys of key list ])))
    ;;

    let create_with_key_exn = Table.create_with_key_exn

    let () =
      ( !! ) "create_with_key_exn [no dups]" (fun size ->
        let data = Example.data size in
        let key_array = Example.key_array size in
        stage (fun () ->
          ignore (create_with_key_exn data ~get_key:(Array.get key_array) : int t)))
    ;;

    let create_with_key_or_error = Table.create_with_key_or_error

    let () =
      ( !! ) "create_with_key_or_error [no dups]" (fun size ->
        let data = Example.data size in
        let key_array = Example.key_array size in
        stage (fun () ->
          ignore
            (create_with_key_or_error data ~get_key:(Array.get key_array)
              : int t Or_error.t)))
    ;;

    let group = Table.group

    let () =
      ( !! ) "group [no dups]" (fun size ->
        let alist = Example.alist size in
        stage (fun () ->
          ignore (group alist ~get_key:fst ~get_data:snd ~combine:( + ) : int t)))
    ;;

    let sexp_of_key = Impl.sexp_of_key

    let () =
      ( !! ) "sexp_of_key + <rand key>" (fun size ->
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () -> ignore (sexp_of_key t (Example.random_key r `present) : Sexp.t)))
    ;;

    let choose = Impl.choose

    let () =
      ( !! ) "choose" (fun size ->
        let t = Example.t size in
        stage (fun () -> ignore (choose t : (key * int) option)))
    ;;

    let choose_randomly = Impl.choose_randomly

    let () =
      ( !! ) "choose_randomly" (fun size ->
        let t = Example.t size in
        stage (fun () -> ignore (choose t : (key * int) option)))
    ;;

    let choose_and_remove_to_clear_table t ~choose =
      let rec loop () =
        match choose t with
        | Some (key, (_ : int)) ->
          Impl.remove t key;
          loop ()
        | None -> ()
      in
      loop ()
    ;;

    let () =
      ( !! ) "copy + choose and remove to clear table" (fun size ->
        let t = Example.t size in
        stage (fun () ->
          let t = Impl.copy t in
          choose_and_remove_to_clear_table t ~choose))
    ;;

    let () =
      ( !! ) "copy + choose_randomly and remove to clear table" (fun size ->
        let t = Example.t size in
        stage (fun () ->
          let t = Impl.copy t in
          choose_and_remove_to_clear_table t ~choose:choose_randomly))
    ;;

    let choose_exn = Impl.choose_exn

    let () =
      ( !! ) "choose_exn" (fun size ->
        let t = Example.t size in
        stage (fun () -> ignore (choose_exn t : key * int)))
    ;;

    let choose_randomly_exn = Impl.choose_randomly_exn

    let () =
      ( !! ) "choose_randomly_exn" (fun size ->
        let t = Example.t size in
        stage (fun () -> ignore (choose_randomly_exn t : key * int)))
    ;;

    let copy = Impl.copy

    let () =
      ( !! ) "copy" (fun size ->
        let t = Example.t size in
        stage (fun () -> ignore (copy t : int t)))
    ;;

    let clear = Impl.clear

    let () =
      ( !! ) "clear + copy" (fun size ->
        let t = Example.t size in
        stage (fun () -> clear (copy t)))
    ;;

    let fold = Impl.fold

    let () =
      ( !! ) "fold" (fun size ->
        let f ~key:_ ~data:_ _ = () in
        let t = Example.t size in
        stage (fun () -> fold t ~init:() ~f))
    ;;

    let iter = Impl.iter

    let () =
      ( !! ) "iter" (fun size ->
        let t = Example.t size in
        stage (fun () -> iter t ~f:ignore))
    ;;

    let iter_keys = Impl.iter_keys

    let () =
      ( !! ) "iter_keys" (fun size ->
        let t = Example.t size in
        stage (fun () -> iter_keys t ~f:ignore))
    ;;

    let iteri = Impl.iteri

    let () =
      ( !! ) "iteri" (fun size ->
        let f ~key:_ ~data:_ = () in
        let t = Example.t size in
        stage (fun () -> iteri t ~f))
    ;;

    let exists = Impl.exists

    let () =
      ( !! ) "exists [true]" (fun size ->
        let f _ = true in
        let t = Example.t size in
        stage (fun () -> ignore (exists t ~f : bool)))
    ;;

    let () =
      ( !! ) "exists [false]" (fun size ->
        let f _ = false in
        let t = Example.t size in
        stage (fun () -> ignore (exists t ~f : bool)))
    ;;

    let () =
      ( !! ) "exists + <rand data>" (fun size ->
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          let v = Example.random_data r `present in
          ignore (exists t ~f:(fun data -> data = v) : bool)))
    ;;

    let existsi = Impl.existsi

    let () =
      ( !! ) "existsi [true]" (fun size ->
        let f ~key:_ ~data:_ = true in
        let t = Example.t size in
        stage (fun () -> ignore (existsi t ~f : bool)))
    ;;

    let () =
      ( !! ) "existsi [false]" (fun size ->
        let f ~key:_ ~data:_ = false in
        let t = Example.t size in
        stage (fun () -> ignore (existsi t ~f : bool)))
    ;;

    let () =
      ( !! ) "existsi + <rand key>" (fun size ->
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          let k = Example.random_key r `present in
          ignore (existsi t ~f:(fun ~key ~data:_ -> Key.compare key k = 0) : bool)))
    ;;

    let for_all = Impl.for_all

    let () =
      ( !! ) "for_all [true]" (fun size ->
        let f _ = true in
        let t = Example.t size in
        stage (fun () -> ignore (for_all t ~f : bool)))
    ;;

    let () =
      ( !! ) "for_all [false]" (fun size ->
        let f _ = false in
        let t = Example.t size in
        stage (fun () -> ignore (for_all t ~f : bool)))
    ;;

    let () =
      ( !! ) "for_all + <rand data>" (fun size ->
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          let v = Example.random_data r `present in
          ignore (for_all t ~f:(fun data -> data <> v) : bool)))
    ;;

    let for_alli = Impl.for_alli

    let () =
      ( !! ) "for_alli [true]" (fun size ->
        let f ~key:_ ~data:_ = true in
        let t = Example.t size in
        stage (fun () -> ignore (for_alli t ~f : bool)))
    ;;

    let () =
      ( !! ) "for_alli [false]" (fun size ->
        let f ~key:_ ~data:_ = false in
        let t = Example.t size in
        stage (fun () -> ignore (for_alli t ~f : bool)))
    ;;

    let () =
      ( !! ) "for_alli + <rand key>" (fun size ->
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          let k = Example.random_key r `present in
          ignore (for_alli t ~f:(fun ~key ~data:_ -> Key.compare key k <> 0) : bool)))
    ;;

    let count = Impl.count

    let () =
      ( !! ) "count [true]" (fun size ->
        let f _ = true in
        let t = Example.t size in
        stage (fun () -> ignore (count t ~f : int)))
    ;;

    let () =
      ( !! ) "count [false]" (fun size ->
        let f _ = false in
        let t = Example.t size in
        stage (fun () -> ignore (count t ~f : int)))
    ;;

    let counti = Impl.counti

    let () =
      ( !! ) "counti [true]" (fun size ->
        let f ~key:_ ~data:_ = true in
        let t = Example.t size in
        stage (fun () -> ignore (counti t ~f : int)))
    ;;

    let () =
      ( !! ) "counti [false]" (fun size ->
        let f ~key:_ ~data:_ = false in
        let t = Example.t size in
        stage (fun () -> ignore (counti t ~f : int)))
    ;;

    let is_empty = Impl.is_empty

    let () =
      ( !! ) "is_empty" (fun size ->
        let t = Example.t size in
        stage (fun () -> ignore (is_empty t : bool)))
    ;;

    let length = Impl.length

    let () =
      ( !! ) "length" (fun size ->
        let t = Example.t size in
        stage (fun () -> ignore (length t : int)))
    ;;

    let capacity = Impl.capacity

    let () =
      ( !! ) "capacity" (fun size ->
        let t = Example.t size in
        stage (fun () -> ignore (capacity t : int)))
    ;;

    let keys = Impl.keys

    let () =
      ( !! ) "keys" (fun size ->
        let t = Example.t size in
        stage (fun () -> ignore (keys t : Key.t list)))
    ;;

    let data = Impl.data

    let () =
      ( !! ) "data" (fun size ->
        let t = Example.t size in
        stage (fun () -> ignore (data t : int list)))
    ;;

    let mem = Impl.mem

    let () =
      ( !! ) "mem + <rand key> [true]" (fun size ->
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () -> ignore (Impl.mem t (Example.random_key r `present) : bool)))
    ;;

    let () =
      ( !! ) "mem + <rand key> [false]" (fun size ->
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () -> ignore (Impl.mem t (Example.random_key r `absent) : bool)))
    ;;

    let remove = Impl.remove

    let () =
      ( !! ) "copy + remove [all]" (fun size ->
        let t = Example.t size in
        let keys = keys t in
        stage (fun () ->
          let t = copy t in
          List.iter keys ~f:(fun key -> remove t key)))
    ;;

    let add_exn = Impl.add_exn

    let () =
      let ( !!! ) ~capacity ~resize =
        let name =
          sprintf
            "create [%s capacity, %s resize] + add_exn [N+1]"
            (if capacity then "w/" else "no")
            (if resize then "w/" else "no")
        in
        ( !! ) name (fun size ->
          let alist = Example.alist (size + 1) in
          let growth_allowed = if resize then None else Some true in
          let size = if capacity then None else Some size in
          stage (fun () ->
            let t = create ?size ?growth_allowed () in
            List.iter alist ~f:(fun (key, data) -> add_exn t ~key ~data)))
      in
      ( !!! ) ~capacity:true ~resize:true;
      ( !!! ) ~capacity:true ~resize:false;
      ( !!! ) ~capacity:false ~resize:true;
      ( !!! ) ~capacity:false ~resize:false
    ;;

    let () =
      ( !! ) "add_exn or remove + mem + <rand key>" (fun size ->
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          let key = Example.random_key r `either in
          if mem t key then remove t key else add_exn t ~key ~data:0))
    ;;

    let set = Impl.set

    let () =
      ( !! ) "set or remove + mem + <rand key>" (fun size ->
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          let key = Example.random_key r `either in
          if mem t key then remove t key else set t ~key ~data:0))
    ;;

    let () =
      ( !! ) "set + <rand key> + remove + <rand key>" (fun size ->
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          set t ~key:(Example.random_key r `either) ~data:0;
          remove t (Example.random_key r `either)))
    ;;

    let add = Impl.add

    let () =
      ( !! ) "add or remove + mem + <rand key>" (fun size ->
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          let key = Example.random_key r `either in
          if mem t key
          then remove t key
          else ignore (add t ~key ~data:0 : [ `Ok | `Duplicate ])))
    ;;

    let () =
      ( !! ) "add + <rand key> + remove + <rand key>" (fun size ->
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          ignore
            (add t ~key:(Example.random_key r `either) ~data:0 : [ `Ok | `Duplicate ]);
          remove t (Example.random_key r `either)))
    ;;

    let add_multi = Impl.add_multi
    let remove_multi = Impl.remove_multi

    let () =
      ( !! ) "add_multi + <rand key> + remove_multi + <rand key>" (fun size ->
        let r = Example.random size in
        let t = Example.t_multi size in
        stage (fun () ->
          add_multi t ~key:(Example.random_key r `either) ~data:0;
          remove_multi t (Example.random_key r `either)))
    ;;

    let change = Impl.change

    let () =
      ( !! ) "change + <rand key> + <rand data>" (fun size ->
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          let key = Example.random_key r `either in
          let data = Example.random_data r `either in
          change t key ~f:(fun _ -> if data < size then Some data else None)))
    ;;

    let update = Impl.update
    let update_and_return = Impl.update_and_return

    (* [update] is in terms of [update_and_return] so there's no need for a separate
       benchmark *)

    let () =
      ( !! ) "update + <rand key> + remove + <rand key>" (fun size ->
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          update t (Example.random_key r `either) ~f:(function
            | None -> 0
            | Some n -> n + 1);
          remove t (Example.random_key r `either)))
    ;;

    let map = Impl.map

    let () =
      ( !! ) "map" (fun size ->
        let f data = data + 1 in
        let t = Example.t size in
        stage (fun () -> ignore (map t ~f : int t)))
    ;;

    let mapi = Impl.mapi

    let () =
      ( !! ) "mapi" (fun size ->
        let f ~key:_ ~data = data + 1 in
        let t = Example.t size in
        stage (fun () -> ignore (mapi t ~f : int t)))
    ;;

    let filter = Impl.filter

    let () =
      ( !! ) "filter [halve]" (fun size ->
        let f data = data % 2 = 0 in
        let t = Example.t size in
        stage (fun () -> ignore (filter t ~f : int t)))
    ;;

    let filteri = Impl.filteri

    let () =
      ( !! ) "filteri [halve]" (fun size ->
        let f ~key:_ ~data = data % 2 = 0 in
        let t = Example.t size in
        stage (fun () -> ignore (filteri t ~f : int t)))
    ;;

    let filter_keys = Impl.filter_keys

    let () =
      ( !! ) "filter_keys [halve]" (fun size ->
        let r = Example.random size in
        let f _ = Random.State.bool r.state in
        let t = Example.t size in
        stage (fun () -> ignore (filter_keys t ~f : int t)))
    ;;

    let filter_map = Impl.filter_map

    let () =
      ( !! ) "filter_map [halve]" (fun size ->
        let f data = if data % 2 = 0 then Some (data + 1) else None in
        let t = Example.t size in
        stage (fun () -> ignore (filter_map t ~f : int t)))
    ;;

    let filter_mapi = Impl.filter_mapi

    let () =
      ( !! ) "filter_mapi [halve]" (fun size ->
        let f ~key:_ ~data = if data % 2 = 0 then Some (data + 1) else None in
        let t = Example.t size in
        stage (fun () -> ignore (filter_mapi t ~f : int t)))
    ;;

    let partition_tf = Impl.partition_tf

    let () =
      ( !! ) "partition_tf [halve]" (fun size ->
        let f data = data % 2 = 0 in
        let t = Example.t size in
        stage (fun () -> ignore (partition_tf t ~f : int t * int t)))
    ;;

    let partitioni_tf = Impl.partitioni_tf

    let () =
      ( !! ) "partitioni_tf [halve]" (fun size ->
        let f ~key:_ ~data = data % 2 = 0 in
        let t = Example.t size in
        stage (fun () -> ignore (partitioni_tf t ~f : int t * int t)))
    ;;

    let partition_map = Impl.partition_map

    let () =
      ( !! ) "partition_map [halve]" (fun size ->
        let f data = if data % 2 = 0 then First (data + 1) else Second (data - 1) in
        let t = Example.t size in
        stage (fun () -> ignore (partition_map t ~f : int t * int t)))
    ;;

    let partition_mapi = Impl.partition_mapi

    let () =
      ( !! ) "partition_mapi [halve]" (fun size ->
        let f ~key:_ ~data =
          if data % 2 = 0 then First (data + 1) else Second (data - 1)
        in
        let t = Example.t size in
        stage (fun () -> ignore (partition_mapi t ~f : int t * int t)))
    ;;

    let find_or_add = Impl.find_or_add
    let find_and_remove = Impl.find_and_remove

    let () =
      ( !! ) "find_or_add + <rand key> + find_and_remove + <rand key>" (fun size ->
        let default () = 0 in
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          ignore (find_or_add t (Example.random_key r `either) ~default : int);
          ignore (find_and_remove t (Example.random_key r `either) : int option)))
    ;;

    let findi_or_add = Impl.findi_or_add

    let () =
      ( !! ) "findi_or_add + <rand key> + find_and_remove + <rand key>" (fun size ->
        let default _key = 0 in
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          ignore (findi_or_add t (Example.random_key r `either) ~default : int);
          ignore (find_and_remove t (Example.random_key r `either) : int option)))
    ;;

    let find_exn = Impl.find_exn

    let () =
      ( !! ) "find_exn + <rand key>" (fun size ->
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () -> ignore (find_exn t (Example.random_key r `present) : int)))
    ;;

    let find = Impl.find

    let () =
      ( !! ) "find + <rand key>" (fun size ->
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () -> ignore (find t (Example.random_key r `either) : int option)))
    ;;

    let find_multi = Impl.find_multi

    let () =
      ( !! ) "find_multi + <rand key>" (fun size ->
        let r = Example.random size in
        let t = Example.t_multi size in
        stage (fun () -> ignore (find_multi t (Example.random_key r `either) : int list)))
    ;;

    let find_and_call = Impl.find_and_call

    let () =
      ( !! ) "find_and_call + <rand key>" (fun size ->
        let if_not_found _ = 0 in
        let if_found data = data in
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          let key = Example.random_key r `either in
          ignore (find_and_call t key ~if_found ~if_not_found : int)))
    ;;

    let findi_and_call = Impl.findi_and_call

    let () =
      ( !! ) "findi_and_call + <rand key>" (fun size ->
        let if_not_found _ = 0 in
        let if_found ~key:_ ~data = data in
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          let key = Example.random_key r `either in
          ignore (findi_and_call t key ~if_found ~if_not_found : int)))
    ;;

    let find_and_call1 = Impl.find_and_call1

    let () =
      ( !! ) "find_and_call1 + <rand key>" (fun size ->
        let if_not_found _ () = 0 in
        let if_found data () = data in
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          let key = Example.random_key r `either in
          ignore (find_and_call1 t key ~a:() ~if_found ~if_not_found : int)))
    ;;

    let findi_and_call1 = Impl.findi_and_call1

    let () =
      ( !! ) "findi_and_call1 + <rand key>" (fun size ->
        let if_not_found _ () = 0 in
        let if_found ~key:_ ~data () = data in
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          let key = Example.random_key r `either in
          ignore (findi_and_call1 t key ~a:() ~if_found ~if_not_found : int)))
    ;;

    let find_and_call2 = Impl.find_and_call2

    let () =
      ( !! ) "find_and_call2 + <rand key>" (fun size ->
        let if_not_found _ () () = 0 in
        let if_found data () () = data in
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          let key = Example.random_key r `either in
          ignore (find_and_call2 t key ~a:() ~b:() ~if_found ~if_not_found : int)))
    ;;

    let findi_and_call2 = Impl.findi_and_call2

    let () =
      ( !! ) "findi_and_call2 + <rand key>" (fun size ->
        let if_not_found _ () () = 0 in
        let if_found ~key:_ ~data () () = data in
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () ->
          let key = Example.random_key r `either in
          ignore (findi_and_call2 t key ~a:() ~b:() ~if_found ~if_not_found : int)))
    ;;

    let merge = Impl.merge

    let () =
      let bench desc merge_fun =
        ( !! ) (sprintf "merge [%s]" desc) (fun size ->
          let t = Example.t (size * 2) in
          (* [t1] and [t2] both have [size] keys, and half of their keys overlap. *)
          let t1 = filter t ~f:(fun data -> data % 2 = 0) in
          let t2 = filter t ~f:(fun data -> data < size) in
          stage (fun () -> ignore (merge t1 t2 ~f:merge_fun : int t)))
      in
      bench "drop" (fun ~key:_ _ -> None);
      bench "keep" (fun ~key:_ -> function
        | `Left data | `Right data -> Some data
        | `Both (left, right) -> Some (left + right))
    ;;

    let merge_into = Impl.merge_into

    let () =
      let bench desc merge_fun =
        ( !! ) (sprintf "copy + merge_into [%s]" desc) (fun size ->
          let t = Example.t (size * 2) in
          (* [t1] and [t2] both have [size] keys, and half of their keys overlap. *)
          let t1 = filter t ~f:(fun data -> data % 2 = 0) in
          let t2 = filter t ~f:(fun data -> data < size) in
          stage (fun () -> merge_into ~dst:(copy t1) ~src:t2 ~f:merge_fun))
      in
      bench "drop" (fun ~key:_ _ _ -> Remove);
      bench "keep" (fun ~key:_ x _ -> Set_to x)
    ;;

    let filter_inplace = Impl.filter_inplace

    let () =
      ( !! ) "copy + filter_inplace [halve]" (fun size ->
        let f data = data % 2 = 0 in
        let t = Example.t size in
        stage (fun () -> filter_inplace (copy t) ~f))
    ;;

    let filteri_inplace = Impl.filteri_inplace

    let () =
      ( !! ) "copy + filteri_inplace [halve]" (fun size ->
        let f ~key:_ ~data = data % 2 = 0 in
        let t = Example.t size in
        stage (fun () -> filteri_inplace (copy t) ~f))
    ;;

    let filter_keys_inplace = Impl.filter_keys_inplace

    let () =
      ( !! ) "copy + filter_keys_inplace [true]" (fun size ->
        let f _ = true in
        let t = Example.t size in
        stage (fun () -> filter_keys_inplace (copy t) ~f))
    ;;

    let () =
      ( !! ) "copy + filter_keys_inplace [false]" (fun size ->
        let f _ = false in
        let t = Example.t size in
        stage (fun () -> filter_keys_inplace (copy t) ~f))
    ;;

    let map_inplace = Impl.map_inplace

    let () =
      ( !! ) "copy + map_inplace" (fun size ->
        let f data = data + 1 in
        let t = Example.t size in
        stage (fun () -> map_inplace (copy t) ~f))
    ;;

    let mapi_inplace = Impl.mapi_inplace

    let () =
      ( !! ) "copy + mapi_inplace" (fun size ->
        let f ~key:_ ~data = data + 1 in
        let t = Example.t size in
        stage (fun () -> mapi_inplace (copy t) ~f))
    ;;

    let filter_map_inplace = Impl.filter_map_inplace

    let () =
      ( !! ) "copy + filter_map_inplace [halve]" (fun size ->
        let f data = if data % 2 = 0 then Some (data + 1) else None in
        let t = Example.t size in
        stage (fun () -> filter_map_inplace (copy t) ~f))
    ;;

    let filter_mapi_inplace = Impl.filter_mapi_inplace

    let () =
      ( !! ) "copy + filter_mapi_inplace [halve]" (fun size ->
        let f ~key:_ ~data = if data % 2 = 0 then Some (data + 1) else None in
        let t = Example.t size in
        stage (fun () -> filter_mapi_inplace (copy t) ~f))
    ;;

    let equal = Impl.equal

    let () =
      ( !! ) "equal [same]" (fun size ->
        let t1 = Example.t size in
        let t2 = copy t1 in
        stage (fun () -> ignore (equal Int.equal t1 t2 : bool)))
    ;;

    let similar = Impl.similar

    let () =
      ( !! ) "similar [same]" (fun size ->
        let t1 = Example.t size in
        let t2 = copy t1 in
        stage (fun () -> ignore (similar Int.equal t1 t2 : bool)))
    ;;

    let to_alist = Impl.to_alist

    let () =
      ( !! ) "to_alist" (fun size ->
        let t = Example.t size in
        stage (fun () -> ignore (to_alist t : (Key.t * int) list)))
    ;;

    let validate = Impl.validate

    let () =
      ( !! ) "validate [pass]" (fun size ->
        let check _ = Validate.pass in
        let name _ = "name" in
        let t = Example.t size in
        stage (fun () -> ignore (validate check t ~name : Validate.t)))
    ;;

    let incr = Impl.incr

    let () =
      ( !! ) "incr + <rand key> [existing]" (fun size ->
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () -> incr t (Example.random_key r `present)))
    ;;

    let decr = Impl.decr

    let () =
      ( !! ) "decr + <rand key> [existing]" (fun size ->
        let r = Example.random size in
        let t = Example.t size in
        stage (fun () -> decr t (Example.random_key r `present)))
    ;;

    (* no benchmarks for non-functions *)

    let hashable = Table.hashable
  end

  let benchmarks = For_bench.all ()
end

module Int_key = struct
  include Int

  let hash s = [%hash: int] s
  let module_name = "Int"
end

module Int_key_with_collisions = struct
  include Int

  let module_name = "Int_sqr"

  (* This hash function causes irregular numbers of collisions because squaring does not
     map uniformly onto modulo space, which I think is more like what a "bad" hash
     function would cause. *)
  let hash x = x * x
end

module String_key = struct
  include String

  let hash = [%hash: string]
  let module_name = "String"
  let of_int_exn i = sprintf "%016x" i
end

(* There is no specific reason why we chose this record structure. It would be good to
   test a mix of immediate components, non-immediate components, and recursive components.
   So we threw a few such things into this record structure.  *)
module Compound_key = struct
  type t =
    { int : int
    ; string : string
    ; tuple : bool option * char
    }
  [@@deriving sexp, compare, hash]

  let of_int_exn int =
    let option =
      match int % 3 with
      | 0 -> None
      | 1 -> Some true
      | 2 -> Some false
      | _ -> assert false
    in
    let char = Char.of_int_exn (int % 256) in
    { int; string = String_key.of_int_exn int; tuple = option, char }
  ;;

  let module_name = "Compound"
end

module Bench_impl (Config : Config) (Impl : Impl) = struct
  module M1 = Benchmarks (Config) (Impl) (Int_key)
  module M2 = Benchmarks (Config) (Impl) (Int_key_with_collisions)
  module M3 = Benchmarks (Config) (Impl) (String_key)
  module M4 = Benchmarks (Config) (Impl) (Compound_key)

  let benchmarks = M1.benchmarks @ M2.benchmarks @ M3.benchmarks @ M4.benchmarks
end

module Hash_impl : Impl = struct
  include Core.Hashtbl

  let module_name = "Hashtbl"
end

module Pool_impl : Impl = struct
  include Pooled_hashtbl

  let module_name = "Pooled_hashtbl"
end

module Bench_hashtbl (Config : Config) = Bench_impl (Config) (Hash_impl)
module Bench_pooled (Config : Config) = Bench_impl (Config) (Pool_impl)

let benchmarks ~regex ~sizes =
  let module Config = struct
    let regex = regex
    let sizes = sizes
  end
  in
  let module H = Bench_hashtbl (Config) in
  let module P = Bench_pooled (Config) in
  H.benchmarks @ P.benchmarks
  |> List.sort ~compare:(fun test1 test2 ->
       String.compare (Bench.Test.name test1) (Bench.Test.name test2))
;;

module Top_level = struct
  let command =
    let open Command.Let_syntax in
    let open Command.Spec in
    let regex = Arg_type.create Re2.create_exn in
    Bench.make_command_ext
      ~summary:"Benchmarks for hash tables."
      [%map_open
        let regex =
          flag
            "-matching"
            (optional regex)
            ~doc:"REGEX Select tests matching given regex."
        and sizes =
          flag
            "-sizes"
            (optional_with_default
               [ 1; 32; 1024 ]
               (Arg_type.comma_separated int ~allow_empty:true))
            ~doc:"INT,... Use hash tables of specified sizes."
        and list =
          flag "-list" no_arg ~doc:" List benchmark names; do not run benchmarks."
        in
        fun (analysis_configs, display_config, mode) ->
          let tests = benchmarks ~regex ~sizes in
          if list
          then (
            printf
              "There are %s benchmarks:\n"
              (Int.to_string_hum (List.length tests * List.length sizes));
            List.iter tests ~f:(fun test ->
              List.iter sizes ~f:(fun size ->
                printf "%s:%d\n" (Bench.Test.name test) size)))
          else (
            match mode with
            | `From_file _ ->
              failwith
                "This executable is for running benchmarks, not analyzing saved \
                 measurements."
            | `Run (save_to_file, run_config) ->
              Bench.bench
                ~run_config
                ~analysis_configs
                ~display_config
                ?save_to_file
                tests)]
  ;;
end

let () = Command_unix.run Top_level.command
