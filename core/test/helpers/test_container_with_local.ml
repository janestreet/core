open! Core
open Base_quickcheck
open Expect_test_helpers_core
include Test_container_with_local_intf.Definitions

module Either = struct
  type ('a, 'b) t = ('a, 'b) Either.t =
    | First of 'a
    | Second of 'b
  [@@deriving compare, equal, globalize, quickcheck, sexp_of]
end

module Result = struct
  type ('a, 'b) t = ('a, 'b) Result.t =
    | Ok of 'a
    | Error of 'b
  [@@deriving compare, equal, globalize, quickcheck, sexp_of]
end

module Continue_or_stop = struct
  type ('a, 'b) t = ('a, 'b) Continue_or_stop.t =
    | Continue of 'a
    | Stop of 'b
  [@@deriving compare, equal, globalize, quickcheck, sexp_of]
end

let test
  (type input output)
  ~(here : [%call_pos])
  (module Input : Input with type t = input)
  (module Output : Output with type t = output)
  ?cr
  ?(noalloc : (Input.t -> local_ Output.t) option)
  (f_local : local_ Input.t -> local_ Output.t)
  (f_global : Input.t -> Output.t)
  =
  quickcheck_m ~here ?cr (module Input) ~f:(fun input ->
    require_equal
      ~here
      ?cr
      (module Output)
      (Output.globalize (local_ f_local input))
      (f_global input);
    let f_noalloc = Option.value noalloc ~default:(fun x -> exclave_ f_local x) in
    require_no_allocation ~here (fun () ->
      ignore (Sys.opaque_identity (local_ f_noalloc input) : Output.t)))
;;

let test_indexed_container_with_creators
  ~(here : [%call_pos])
  ?cr
  (module M : Indexed_container_with_creators)
  =
  let test a b c d ~noalloc = test ~here ?cr a b c d ~noalloc in
  let module _ : Container_with_local.Generic_indexed_with_creators = struct
    type ('a, _, _) t = 'a M.t
    type 'a elt = 'a M.elt
    type ('a, _, _) concat = 'a M.concat

    let length =
      print_endline "Container_with_local: length";
      test
        (module struct
          type t = int M.t [@@deriving quickcheck, sexp_of]
        end)
        (module Int)
        M.Local.length
        M.Global.length
        ~noalloc:(fun t -> M.Local.length t);
      M.Local.length

    and is_empty =
      print_endline "Container_with_local: is_empty";
      test
        (module struct
          type t = int M.t [@@deriving quickcheck, sexp_of]
        end)
        (module Bool)
        M.Local.is_empty
        M.Global.is_empty
        ~noalloc:(fun t -> M.Local.is_empty t);
      M.Local.is_empty

    and mem =
      print_endline "Container_with_local: mem";
      test
        (module struct
          type t = int M.t * int M.elt [@@deriving quickcheck, sexp_of]
        end)
        (module Bool)
        (fun (t, elt) -> exclave_ M.Local.mem t elt ~equal:[%equal_local: int M.elt])
        (fun (t, elt) -> M.Global.mem t elt ~equal:[%equal: int M.elt])
        ~noalloc:(fun (t, elt) -> exclave_
          M.Local.mem t elt ~equal:[%equal_local: int M.elt]);
      M.Local.mem

    and iter =
      print_endline "Container_with_local: iter";
      test
        (module struct
          type t = int M.t [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.elt list [@@deriving equal, globalize, sexp_of]
        end)
        (fun t ->
          let q = Queue.create () in
          M.Local.iter t ~f:(fun b -> Queue.enqueue q ([%globalize: int M.elt] b));
          Queue.to_list q)
        (fun t ->
          let q = Queue.create () in
          M.Global.iter t ~f:(fun b -> Queue.enqueue q b);
          Queue.to_list q)
        ~noalloc:(fun t ->
          M.Local.iter t ~f:(fun b -> ignore (Sys.opaque_identity b : int M.elt));
          []);
      M.Local.iter

    and fold =
      print_endline "Container_with_local: fold";
      test
        (module struct
          type t = int M.t * int * (int -> int M.elt -> int)
          [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, init, f) -> exclave_
          M.Local.fold t ~init ~f:(fun acc elt -> (f acc) ([%globalize: int M.elt] elt)))
        (fun (t, init, f) -> M.Global.fold t ~init ~f)
        ~noalloc:(fun (t, _, _) -> M.Local.fold t ~init:0 ~f:(fun n _ -> n) [@nontail]);
      M.Local.fold

    and fold_result =
      print_endline "Container_with_local: fold_result";
      test
        (module struct
          type t = int M.t * int * (int -> int M.elt -> (int, int) Result.t)
          [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = (int, int) Result.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, init, f) -> exclave_
          M.Local.fold_result t ~init ~f:(fun acc elt ->
            (f acc) ([%globalize: int M.elt] elt)))
        (fun (t, init, f) -> M.Global.fold_result t ~init ~f)
        ~noalloc:(fun (t, _, _) -> exclave_
          M.Local.fold_result t ~init:0 ~f:(fun acc _ -> exclave_ Ok acc));
      M.Local.fold_result

    and fold_until =
      print_endline "Container_with_local: fold_until";
      test
        (module struct
          type t =
            int M.t
            * int
            * (int -> int M.elt -> (int, int) Continue_or_stop.t)
            * (int -> int)
          [@@deriving quickcheck, sexp_of]
        end)
        (module Int)
        (fun (t, init, f, finish) -> exclave_
          M.Local.fold_until
            t
            ~init
            ~f:(fun acc elt -> (f acc) ([%globalize: int M.elt] elt))
            ~finish:(fun acc -> finish acc))
        (fun (t, init, f, finish) -> M.Global.fold_until t ~init ~f ~finish)
        ~noalloc:(fun (t, _, _, _) -> exclave_
          M.Local.fold_until t ~init:0 ~finish:Fn.id ~f:(fun acc _ -> exclave_
            Continue acc));
      M.Local.fold_until

    and exists =
      print_endline "Container_with_local: exists";
      test
        (module struct
          type t = int M.t * (int M.elt -> bool) [@@deriving quickcheck, sexp_of]
        end)
        (module Bool)
        (fun (t, f) -> exclave_
          M.Local.exists t ~f:(fun elt -> f ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.exists t ~f)
        ~noalloc:(fun (t, _) -> exclave_ M.Local.exists t ~f:(fun _ -> false));
      M.Local.exists

    and for_all =
      print_endline "Container_with_local: for_all";
      test
        (module struct
          type t = int M.t * (int M.elt -> bool) [@@deriving quickcheck, sexp_of]
        end)
        (module Bool)
        (fun (t, f) -> exclave_
          M.Local.for_all t ~f:(fun elt -> f ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.for_all t ~f)
        ~noalloc:(fun (t, _) -> exclave_ M.Local.for_all t ~f:(fun _ -> true));
      M.Local.for_all

    and count =
      print_endline "Container_with_local: count";
      test
        (module struct
          type t = int M.t * (int M.elt -> bool) [@@deriving quickcheck, sexp_of]
        end)
        (module Int)
        (fun (t, f) -> exclave_
          M.Local.count t ~f:(fun elt -> f ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.count t ~f)
        ~noalloc:(fun (t, _) -> exclave_ M.Local.count t ~f:(fun _ -> true));
      M.Local.count

    and sum =
      print_endline "Container_with_local: sum";
      test
        (module struct
          type t = int M.t * (int M.elt -> int) [@@deriving quickcheck, sexp_of]
        end)
        (module Int)
        (fun (t, f) -> exclave_
          M.Local.sum (module Int) t ~f:(fun elt -> f ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.sum (module Int) t ~f)
        ~noalloc:
          (let m = (module Int : Container_with_local.Summable with type t = int) in
           fun (t, _) -> M.Local.sum m t ~f:(fun _ -> 1));
      M.Local.sum

    and find =
      print_endline "Container_with_local: find";
      test
        (module struct
          type t = int M.t * (int M.elt -> bool) [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.elt option [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.find t ~f:(fun elt -> f ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.find t ~f)
        ~noalloc:(fun (t, _) -> exclave_ M.Local.find t ~f:(fun _ -> true));
      M.Local.find

    and find_map =
      print_endline "Container_with_local: find_map";
      test
        (module struct
          type t = int M.t * (int M.elt -> int option) [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int option [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.find_map t ~f:(fun elt -> f ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.find_map t ~f)
        ~noalloc:(fun (t, _) -> exclave_ M.Local.find_map t ~f:(fun _ -> exclave_ Some 0));
      M.Local.find_map

    and to_list =
      print_endline "Container_with_local: to_list";
      test
        (module struct
          type t = int M.t [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.elt list [@@deriving equal, globalize, sexp_of]
        end)
        M.Local.to_list
        M.Global.to_list
        ~noalloc:(fun t -> exclave_ M.Local.to_list t);
      M.Local.to_list

    and min_elt =
      print_endline "Container_with_local: min_elt";
      test
        (module struct
          type t = int M.t [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.elt option [@@deriving equal, globalize, sexp_of]
        end)
        (fun t -> exclave_ M.Local.min_elt t ~compare:[%compare_local: int M.elt])
        (fun t -> M.Global.min_elt t ~compare:[%compare: int M.elt])
        ~noalloc:(fun t -> exclave_
          M.Local.min_elt t ~compare:[%compare_local: int M.elt]);
      M.Local.min_elt

    and max_elt =
      print_endline "Container_with_local: max_elt";
      test
        (module struct
          type t = int M.t [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.elt option [@@deriving equal, globalize, sexp_of]
        end)
        (fun t -> exclave_ M.Local.max_elt t ~compare:[%compare_local: int M.elt])
        (fun t -> M.Global.max_elt t ~compare:[%compare: int M.elt])
        ~noalloc:(fun t -> exclave_
          M.Local.max_elt t ~compare:[%compare_local: int M.elt]);
      M.Local.max_elt

    and of_list =
      print_endline "Container_with_local: of_list";
      test
        (module struct
          type t = int M.elt list [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t [@@deriving equal, globalize, sexp_of]
        end)
        M.Local.of_list
        M.Global.of_list
        ~noalloc:(fun l -> exclave_ M.Local.of_list l);
      M.Local.of_list

    and append =
      print_endline "Container_with_local: append";
      test
        (module struct
          type t = int M.t * int M.t [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (a, b) -> exclave_ M.Local.append a b)
        (fun (a, b) -> M.Global.append a b)
        ~noalloc:(fun (a, b) -> exclave_ M.Local.append a b);
      M.Local.append

    and concat =
      print_endline "Container_with_local: concat";
      test
        (module struct
          type t = int M.t M.concat [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t [@@deriving equal, globalize, sexp_of]
        end)
        M.Local.concat
        M.Global.concat
        ~noalloc:(fun tt -> exclave_ M.Local.concat tt);
      M.Local.concat

    and map =
      print_endline "Container_with_local: map";
      test
        (module struct
          type t = int M.t * (int M.elt -> int M.elt) [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.map t ~f:(fun elt -> f ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.map t ~f)
        ~noalloc:(fun (t, _) -> exclave_ M.Local.map t ~f:Fn.id);
      M.Local.map

    and map_to_global =
      print_endline "Container_with_local: map_to_global";
      test
        (module struct
          type t = int M.t * (int M.elt -> int M.elt) [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.map_to_global t ~f:(fun elt -> f ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.map t ~f)
          (* [map_to_global] inherently allocates, so we provide a trivial function for
             the noalloc test. *)
        ~noalloc:(fun (t, _) -> t);
      M.Local.map_to_global

    and map_of_global =
      print_endline "Container_with_local: map_of_global";
      test
        (module struct
          type t = int M.t * (int M.elt -> int M.elt) [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.map_of_global
            ([%globalize: int M.t] t)
            ~f:(fun elt -> f ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.map t ~f)
        ~noalloc:(fun (t, _) -> exclave_ M.Local.map_of_global t ~f:Fn.id);
      M.Local.map_of_global

    and filter =
      print_endline "Container_with_local: filter";
      test
        (module struct
          type t = int M.t * (int M.elt -> bool) [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.filter t ~f:(fun elt -> f ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.filter t ~f)
        ~noalloc:(fun (t, _) -> exclave_ M.Local.filter t ~f:(fun _ -> true));
      M.Local.filter

    and filter_map =
      print_endline "Container_with_local: filter_map";
      test
        (module struct
          type t = int M.t * (int M.elt -> int M.elt option)
          [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.filter_map t ~f:(fun elt -> f ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.filter_map t ~f)
        ~noalloc:(fun (t, _) -> exclave_
          M.Local.filter_map t ~f:(fun elt -> exclave_ Some elt));
      M.Local.filter_map

    and concat_map =
      print_endline "Container_with_local: concat_map";
      test
        (module struct
          type t = int M.t * (int M.elt -> int M.t) [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.concat_map t ~f:(fun elt -> f ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.concat_map t ~f)
        ~noalloc:(fun (t, _) -> exclave_
          M.Local.concat_map t ~f:(fun elt -> exclave_ M.Local.of_list [ elt ]));
      M.Local.concat_map

    and partition_tf =
      print_endline "Container_with_local: partition_tf";
      test
        (module struct
          type t = int M.t * (int M.elt -> bool) [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t * int M.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.partition_tf t ~f:(fun elt -> f ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.partition_tf t ~f)
        ~noalloc:(fun (t, _) -> exclave_
          let local_ flag = ref false in
          M.Local.partition_tf t ~f:(fun _ ->
            flag := not !flag;
            !flag));
      M.Local.partition_tf

    and partition_map =
      print_endline "Container_with_local: partition_map";
      test
        (module struct
          type t = int M.t * (int M.elt -> (int M.elt, int M.elt) Either.t)
          [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t * int M.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.partition_map t ~f:(fun elt -> f ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.partition_map t ~f)
        ~noalloc:(fun (t, _) -> exclave_
          let local_ flag = ref false in
          M.Local.partition_map t ~f:(fun elt -> exclave_
            flag := not !flag;
            if !flag then First elt else Second elt));
      M.Local.partition_map

    and foldi =
      print_endline "Container_with_local: foldi";
      test
        (module struct
          type t = int M.t [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = (int * int M.elt) list [@@deriving equal, globalize, sexp_of]
        end)
        (fun t -> exclave_
          M.Local.foldi t ~init:[] ~f:(fun i acc elt -> exclave_ (i, elt) :: acc))
        (fun t -> M.Global.foldi t ~init:[] ~f:(fun i acc elt -> (i, elt) :: acc))
        ~noalloc:(fun t -> exclave_
          M.Local.foldi t ~init:[] ~f:(fun i acc elt -> exclave_ (i, elt) :: acc));
      M.Local.foldi

    and iteri =
      print_endline "Container_with_local: iteri";
      test
        (module struct
          type t = int M.t [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = (int * int M.elt) list [@@deriving equal, globalize, sexp_of]
        end)
        (fun t ->
          let q = Queue.create () in
          M.Local.iteri t ~f:(fun i b -> Queue.enqueue q (i, [%globalize: int M.elt] b));
          Queue.to_list q)
        (fun t ->
          let q = Queue.create () in
          M.Global.iteri t ~f:(fun i b -> Queue.enqueue q (i, b));
          Queue.to_list q)
        ~noalloc:(fun t ->
          M.Local.iteri t ~f:(fun _ _ -> ());
          []);
      M.Local.iteri

    and existsi =
      print_endline "Container_with_local: existsi";
      test
        (module struct
          type t = int M.t * (int -> int M.elt -> bool) [@@deriving quickcheck, sexp_of]
        end)
        (module Bool)
        (fun (t, f) -> exclave_
          M.Local.existsi t ~f:(fun i elt -> (f i) ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.existsi t ~f)
        ~noalloc:(fun (t, _) -> exclave_ M.Local.existsi t ~f:(fun _ _ -> false));
      M.Local.existsi

    and for_alli =
      print_endline "Container_with_local: for_alli";
      test
        (module struct
          type t = int M.t * (int -> int M.elt -> bool) [@@deriving quickcheck, sexp_of]
        end)
        (module Bool)
        (fun (t, f) -> exclave_
          M.Local.for_alli t ~f:(fun i elt -> (f i) ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.for_alli t ~f)
        ~noalloc:(fun (t, _) -> exclave_ M.Local.for_alli t ~f:(fun _ _ -> true));
      M.Local.for_alli

    and counti =
      print_endline "Container_with_local: counti";
      test
        (module struct
          type t = int M.t * (int -> int M.elt -> bool) [@@deriving quickcheck, sexp_of]
        end)
        (module Int)
        (fun (t, f) -> exclave_
          M.Local.counti t ~f:(fun i elt -> (f i) ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.counti t ~f)
        ~noalloc:(fun (t, _) -> M.Local.counti t ~f:(fun _ _ -> true));
      M.Local.counti

    and findi =
      print_endline "Container_with_local: findi";
      test
        (module struct
          type t = int M.t * (int -> int M.elt -> bool) [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = (int * int M.elt) option [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.findi t ~f:(fun i elt -> (f i) ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.findi t ~f)
        ~noalloc:(fun (t, _) -> exclave_ M.Local.findi t ~f:(fun _ _ -> true));
      M.Local.findi

    and find_mapi =
      print_endline "Container_with_local: find_mapi";
      test
        (module struct
          type t = int M.t * (int -> int M.elt -> int option)
          [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int option [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.find_mapi t ~f:(fun i elt -> (f i) ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.find_mapi t ~f)
        ~noalloc:(fun (t, _) -> exclave_
          M.Local.find_mapi t ~f:(fun _ _ -> exclave_ Some 0));
      M.Local.find_mapi

    and init =
      print_endline "Container_with_local: init";
      test
        (module struct
          type t = int M.elt array [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun array -> exclave_
          M.Local.init (Array.length array) ~f:(fun i -> Array.get array i))
        (fun array -> M.Global.init (Array.length array) ~f:(fun i -> Array.get array i))
        ~noalloc:(fun array -> exclave_
          M.Local.init (Array.length array) ~f:(fun i -> Array.get array i));
      M.Local.init

    and mapi =
      print_endline "Container_with_local: mapi";
      test
        (module struct
          type t = int M.t * (int -> int M.elt -> int M.elt)
          [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.mapi t ~f:(fun i elt -> (f i) ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.mapi t ~f)
        ~noalloc:(fun (t, _) -> exclave_ M.Local.mapi t ~f:(fun _ elt -> elt));
      M.Local.mapi

    and mapi_to_global =
      print_endline "Container_with_local: mapi_to_global";
      test
        (module struct
          type t = int M.t * (int -> int M.elt -> int M.elt)
          [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.mapi_to_global t ~f:(fun i elt -> (f i) ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.mapi t ~f)
          (* [mapi_to_global] inherently allocates, so we pass a trivial noalloc test. *)
        ~noalloc:(fun (t, _) -> t);
      M.Local.mapi_to_global

    and mapi_of_global =
      print_endline "Container_with_local: mapi_of_global";
      test
        (module struct
          type t = int M.t * (int -> int M.elt -> int M.elt)
          [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.mapi_of_global
            ([%globalize: int M.t] t)
            ~f:(fun i elt -> (f i) ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.mapi t ~f)
        ~noalloc:(fun (t, _) -> exclave_ M.Local.mapi_of_global t ~f:(fun _ elt -> elt));
      M.Local.mapi_of_global

    and filteri =
      print_endline "Container_with_local: filteri";
      test
        (module struct
          type t = int M.t * (int -> int M.elt -> bool) [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.filteri t ~f:(fun i elt -> (f i) ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.filteri t ~f)
        ~noalloc:(fun (t, _) -> exclave_ M.Local.filteri t ~f:(fun _ _ -> true));
      M.Local.filteri

    and filter_mapi =
      print_endline "Container_with_local: filter_mapi";
      test
        (module struct
          type t = int M.t * (int -> int M.elt -> int M.elt option)
          [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.filter_mapi t ~f:(fun i elt -> (f i) ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.filter_mapi t ~f)
        ~noalloc:(fun (t, _) -> exclave_
          M.Local.filter_mapi t ~f:(fun _ elt -> exclave_ Some elt));
      M.Local.filter_mapi

    and concat_mapi =
      print_endline "Container_with_local: concat_mapi";
      test
        (module struct
          type t = int M.t * (int -> int M.elt -> int M.t)
          [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.concat_mapi t ~f:(fun i elt -> (f i) ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.concat_mapi t ~f)
        ~noalloc:(fun (t, _) -> exclave_
          M.Local.concat_mapi t ~f:(fun _ _ -> exclave_ M.Local.of_list []));
      M.Local.concat_mapi

    and partitioni_tf =
      print_endline "Container_with_local: partitioni_tf";
      test
        (module struct
          type t = int M.t * (int -> int M.elt -> bool) [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t * int M.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.partitioni_tf t ~f:(fun i elt -> (f i) ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.partitioni_tf t ~f)
        ~noalloc:(fun (t, _) -> exclave_
          let local_ flag = ref false in
          M.Local.partitioni_tf t ~f:(fun _ _ ->
            flag := not !flag;
            !flag));
      M.Local.partitioni_tf

    and partition_mapi =
      print_endline "Container_with_local: partition_mapi";
      test
        (module struct
          type t = int M.t * (int -> int M.elt -> (int M.elt, int M.elt) Either.t)
          [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = int M.t * int M.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, f) -> exclave_
          M.Local.partition_mapi t ~f:(fun i elt -> (f i) ([%globalize: int M.elt] elt)))
        (fun (t, f) -> M.Global.partition_mapi t ~f)
        ~noalloc:(fun (t, _) -> exclave_
          let local_ flag = ref false in
          M.Local.partition_mapi t ~f:(fun _ elt -> exclave_
            flag := not !flag;
            if !flag then First elt else Second elt));
      M.Local.partition_mapi
    ;;
  end
  in
  ()
;;
