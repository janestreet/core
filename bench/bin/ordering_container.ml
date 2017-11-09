open Core
open Core_bench.Std

module Int = struct
  let name = "int"

  include Int

  let random () = Random.int 1_000_000
end

module String_list = struct
  let name = "string-list"

  module T = struct
    type t = string list [@@deriving sexp, bin_io, compare, hash]
  end

  include T
  include Comparable.Make(T)
  include Hashable.Make(T)

  let random_char () = Random.int 127 |> Char.of_int_exn

  let random () = List.init 5 ~f:(fun _ -> String.init 5 ~f:(fun _ -> random_char ()))
end

module type Key = sig
  type t
  include Hashable.S with type t := t
  include Comparable.S with type t := t
  val name : string
  val random : unit -> t
end

module Mock_heap = struct
  let create ~cmp:_ () = ()
  let add_removable _ _ = ()
  let update _ _ _ = ()
end

let gen_test_add_and_update ~create ~random ~add ~update n =
  let sample  = Array.init n ~f:(fun _ -> random ()) in
  let sample2 = Array.init n ~f:(fun _ -> random ()) in
  fun () ->
    let container = create () in
    Array.iter sample ~f:(fun x -> add container x);
    Array.iteri sample2 ~f:(fun i x -> update container sample.(i) x)

let gen_test_update ~create ~random ~add ~update n =
  let sample  = Array.init n ~f:(fun _ -> random ()) in
  let container = create () in
  Array.iter sample ~f:(fun x -> add container x);
  fun () ->
    for _ = 0 to n/10 do
      let i = Random.int n in
      let a = random () in
      update container sample.(i) a;
      sample.(i) <- a
    done

let gen_tests m n =
  let module Key = (val m : Key) in
  let test_set =
    let random = Key.random in
    let create () = ref Key.Set.empty in
    let add c x = c := Set.add !c x in
    let update c x y =
      c := Set.add (Set.remove !c x) y
    in
    [ Bench.Test.create ~name:(Key.name ^ "-set-add&update")
        (gen_test_add_and_update ~create ~random ~add ~update n)
    ; Bench.Test.create ~name:(Key.name ^ "-set-update")
        (gen_test_update ~create ~random ~add ~update n)
    ]
  in
  let test_mock_heap =
    let module Heap = Mock_heap in
    let random = Key.random in
    let create () = Heap.create ~cmp:Key.compare (), Key.Table.create () in
    let add (heap, elts) x =
      let elt = Heap.add_removable heap x in
      Hashtbl.set elts ~key:x ~data:elt
    in
    let update (heap, elts) x y =
      match Hashtbl.find elts x with
      | None -> ()
      | Some elt ->
        let elt = Heap.update heap elt y in
        Hashtbl.set elts ~key:y ~data:elt
    in
    [ Bench.Test.create ~name:(Key.name ^ "-mockheap-add&update")
        (gen_test_add_and_update ~create ~random ~add ~update n)
    ; Bench.Test.create ~name:(Key.name ^ "-mockheap-update")
        (gen_test_update ~create ~random ~add ~update n)
    ]
  in
  let test_heap =
    let random = Key.random in
    let create () = Heap.create ~cmp:Key.compare (), Key.Table.create () in
    let add (heap, elts) x =
      let elt = Heap.add_removable heap x in
      Hashtbl.set elts ~key:x ~data:elt
    in
    let update (heap, elts) x y =
      match Hashtbl.find elts x with
      | None -> ()
      | Some elt ->
        let elt = Heap.update heap elt y in
        Hashtbl.set elts ~key:y ~data:elt
    in
    [ Bench.Test.create ~name:(Key.name ^ "-heap-add&update")
        (gen_test_add_and_update ~create ~random ~add ~update n)
    ; Bench.Test.create ~name:(Key.name ^ "-heap-update")
        (gen_test_update ~create ~random ~add ~update n)
    ]
  in
  List.concat [ test_set; test_heap; test_mock_heap ]

let () =
  Random.self_init ();
  let n = 100_000 in
  [ gen_tests (module Int : Key) n
  ; gen_tests (module String_list : Key) n
  ]
  |> List.concat
  |> Bench.bench
