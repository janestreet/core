open OUnit;;
open Core

module Hash_heap = Hash_heap.Make(Int)

let s = [1;2;3;4;5;6;7;8;9;10;11]

let make () =
  let h = Hash_heap.create Int.compare in
  List.iter s ~f:(fun i ->
    match Hash_heap.push h ~key:i ~data:i with
    | `Ok -> ()
    | `Key_already_present -> assert false);
  h

let z = string_of_int

let test =
  "hash_heap" >:::
  [ "create, and push" >:: (fun () -> ignore (make ()));
    "find" >:: (fun () ->
      let t = make () in
      List.iter s ~f:(fun i ->
        z i @? (Hash_heap.find t i = Some i)));
    "pop and top" >:: (fun () ->
      let t = make () in
      List.iter s ~f:(fun i ->
        ("top" ^ z i) @? (Hash_heap.top t = Some i);
        ("pop" ^ z i) @? (Hash_heap.pop t = Some i);
        ("topafter" ^ z i) @? (Hash_heap.top t <> Some i);
        ("findable" ^ z i) @? (Hash_heap.find t i = None)));
    "mem" >:: (fun () ->
      let t = make () in
      "all" @? (List.for_all s ~f:(Hash_heap.mem t)));
    "find_pop" >:: (fun () ->
      let t = make () in
      List.iter s ~f:(fun i ->
        ("lookup" ^ z i) @? (Hash_heap.find_pop t i = Some i);
        ("remove" ^ z i) @? (not (Hash_heap.mem t i))));
    "pop_if" >:: (fun () ->
      let t = make () in
      "no" @? (Hash_heap.pop_if t (fun i -> i < 0) = None);
      "still-there" @? (List.for_all s ~f:(Hash_heap.mem t));
      "yes" @? (Hash_heap.pop_if t (fun _ -> true) = Some 1);
      "gone-top" @? (Hash_heap.top t <> Some 1);
      "gone-mem" @? (not (Hash_heap.mem t 1)));
    "iteri" >:: (fun () ->
      let t = make () in
      "match" @?
      (Set.compare_direct (Set.Poly.of_list s)
         (let s = ref Set.Poly.empty in
          Hash_heap.iteri t ~f:(fun ~key ~data ->
            s := Set.add !s key;
            assert (key = data));
          !s)
       = 0));
    "remove" >:: (fun () ->
      let t = make () in
      Hash_heap.remove t 2;
      Hash_heap.remove t 1;
      "1gone-mem" @? (not (Hash_heap.mem t 1));
      "2gone-mem" @? (not (Hash_heap.mem t 2));
      "1gone-top" @? (Hash_heap.top t <> Some 1);
      "2gone-top" @? (Hash_heap.top t <> Some 2));
    "replace" >:: (fun () ->
      let t = make () in
      Hash_heap.replace t ~key:1 ~data:12;
      "present" @? (Hash_heap.find t 1 = Some 12);
      "top" @? (Hash_heap.top t = Some 2));
  ]
