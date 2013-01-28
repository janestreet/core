open OUnit;;
open Core.Std

let test_data = [("a",1);("b",2);("c",3)]

let string_of_alist a =
  let sexp_of_tuple t = Tuple2.sexp_of_t String.sexp_of_t Int.sexp_of_t t in
  let sexp = List.sexp_of_t sexp_of_tuple a in Sexp.to_string_hum sexp

let (empty_hash:(string,int) Hashtbl.t) = Hashtbl.Poly.create () ~size:10
let test_hash = begin
  let h = Hashtbl.Poly.create () ~size:10 in
  List.iter test_data ~f:(fun (k,v) ->
    Hashtbl.replace h ~key:k ~data:v
  );
  h
end

(* This is a very strong notion of equality on hash tables *)
let equal t t' equal_data =
  let subtable t t' =
    try
      List.for_all (Hashtbl.keys t) ~f:(fun key ->
        equal_data (Hashtbl.find_exn t key) (Hashtbl.find_exn t' key))
    with
    | Invalid_argument _ -> false
  in
  subtable t t' && subtable t' t

let test =
  "Table" >:::
    [ "find" >::
        (fun () ->
          "no_exception" @?
         let found = Hashtbl.find test_hash "a" in
         let not_found = Hashtbl.find test_hash "A" in
         Hashtbl.invariant test_hash;
         match found,not_found with
         | Some _, None -> true
         | _ -> false
        );
      "add" >:: (fun () ->
        "no_exception" @?
        let our_hash = Hashtbl.copy test_hash in
        let duplicate = Hashtbl.add our_hash ~key:"a" ~data:4 in
        let no_duplicate = Hashtbl.add our_hash ~key:"d" ~data:5 in
        assert (Hashtbl.find our_hash "a" = Some 1);
        assert (Hashtbl.find our_hash "d" = Some 5);
        Hashtbl.invariant our_hash;
        match duplicate, no_duplicate with
        | `Duplicate, `Ok -> true
        | _ -> false
      );
      "iter_vals" >::
        (fun () ->
          let predicted = List.sort ~cmp:Int.descending (
            List.map test_data ~f:(fun (_,v) -> v))
          in
          let found = ref [] in
          Hashtbl.iter_vals test_hash ~f:(fun v -> found := v :: !found);
          (sprintf "all_vals: Expected: %s\nFound: %s"
             (List.to_string ~f:Int.to_string predicted)
             (List.to_string ~f:Int.to_string !found))
          @? ( !found = predicted )
        );
      "of_alist" >::
        (fun () ->
          "size" @?
            (let predicted = List.length test_data in
             let found = Hashtbl.length (Hashtbl.Poly.of_alist_exn test_data) in
             predicted = found);
          "right keys" @?
            (let predicted = List.map test_data ~f:(fun (k,_) -> k) in
             let found = Hashtbl.keys (Hashtbl.Poly.of_alist_exn test_data) in
             let sp = List.sort ~cmp:ascending predicted in
             let sf = List.sort ~cmp:ascending found in
             sp = sf)
        );
      "keys" >::
        (fun () ->
          "size and right keys" @?
            (let predicted = List.map test_data ~f:(fun (k,_) -> k) in
             let found = Hashtbl.keys test_hash in
             let sp = List.sort ~cmp:ascending predicted in
             let sf = List.sort ~cmp:ascending found in
             sp = sf)
        );
      "data" >::
        (fun () ->
          "size and right data" @?
            (let predicted = List.map test_data ~f:(fun (_,v) -> v) in
             let found = Hashtbl.data test_hash in
             let sp = List.sort ~cmp:ascending predicted in
             let sf = List.sort ~cmp:ascending found in
             sp = sf)
        );

      "map" >:: (fun () ->
        let add1 x = x + 1 in
        let predicted_data =
          List.sort ~cmp:ascending (List.map test_data ~f:(fun (k,v) -> (k,add1 v)))
        in
        let found = Hashtbl.map test_hash ~f:add1 in
        let found_alist = List.sort ~cmp:ascending (Hashtbl.to_alist found) in
        "size" @? ( List.length test_data = Hashtbl.length found );
        let title =
          sprintf "right_data:\nExpected: %s\nFound: %s"
            (string_of_alist predicted_data)
            (string_of_alist found_alist)
        in
        title @? (predicted_data = found_alist));

      "filter_map" >:: (fun () ->
        begin
          let to_string h = Sexp.to_string_hum (
            Hashtbl.Poly.sexp_of_t String.sexp_of_t Int.sexp_of_t h)
          in
          let f x = Some x in
          let result = Hashtbl.filter_map test_hash ~f in
          (sprintf "Result is identical: Expected: %s\nFound: %s"
             (to_string test_hash)
             (to_string result))
          @? ( equal test_hash result Int.(=) )
        end;
        let is_even x = x mod 2 = 0 in
        let add1_to_even x = if is_even x then Some (x + 1) else None in
        let predicted_data = List.filter_map test_data ~f:(fun (k,v) ->
          if is_even v then Some (k, v+1) else None)
        in
        let found = Hashtbl.filter_map test_hash ~f:add1_to_even in
        let found_alist = List.sort ~cmp:ascending (Hashtbl.to_alist found) in
        "size and right data" @? (
          List.length predicted_data = Hashtbl.length found
          && predicted_data = found_alist));
      "insert-find-remove" >:: (fun () ->
        let t = Hashtbl.Poly.create () ~size:1 in
        let inserted = ref [] in
        Random.self_init ();
        let verify_inserted t =
          let missing =
            List.fold !inserted ~init:[] ~f:(fun acc (key, data) ->
              match Hashtbl.find t key with
              | None -> `Missing key :: acc
              | Some d ->
                if data = d then acc
                else `Wrong_data (key, data) :: acc)
          in
          match missing with
          | [] -> ()
          | l ->
            List.iter l ~f:(function
              | `Missing k -> Printf.eprintf "missing key:%d\n" k
              | `Wrong_data (k, d) ->
                Printf.eprintf "wrong data key:%d data:%d\n" k d);
            Printf.eprintf "missing %d of %d\n" (List.length l)
              (List.length !inserted);
            failwith "some inserts are missing"
        in
        let rec loop i t =
          if i < 2000 then begin
            let k = Random.int 10_000 in
            inserted := List.Assoc.add (List.Assoc.remove !inserted k) k i;
            Hashtbl.replace t ~key:k ~data:i;
            Hashtbl.invariant t;
            verify_inserted t;
            loop (i + 1) t
          end
        in
        loop 0 t;
        List.iter !inserted ~f:(fun (x, _) ->
          Hashtbl.remove t x;
          Hashtbl.invariant t;
          begin match Hashtbl.find t x with
          | None -> ()
          | Some _ -> failwith (sprintf "present after removal: %d" x)
          end;
          inserted := List.Assoc.remove !inserted x;
          verify_inserted t));
      "clear" >:: (fun () ->
        let printer = Int.to_string in
        let t = Hashtbl.Poly.create () ~size:1 in
        let l = List.range 0 100 in
        let verify_present l = List.for_all l ~f:(Hashtbl.mem t) in
        let verify_not_present l =
          List.for_all l ~f:(fun i -> not (Hashtbl.mem t i))
        in
        List.iter l ~f:(fun i -> Hashtbl.replace t ~key:i ~data:(i * i));
        List.iter l ~f:(fun i -> Hashtbl.replace t ~key:i ~data:(i * i));
        assert_equal ~printer 100 (Hashtbl.length t);
        assert_bool "Should contain all list elements" (verify_present l);
        Hashtbl.clear t;
        Hashtbl.invariant t;
        assert_equal ~printer 0 (Hashtbl.length t);
        assert_bool "Should be empty" (verify_not_present l);
        let l = List.take l 42 in
        List.iter l ~f:(fun i -> Hashtbl.replace t ~key:i ~data:(i * i));
        assert_equal ~printer 42 (Hashtbl.length t);
        assert_bool "Should contain all 42 elements" (verify_present l);
        Hashtbl.invariant t);
      "mem" >:: (fun () ->
        let t = Hashtbl.Poly.create () ~size:1 in
        Hashtbl.invariant t;
        assert_bool "should not have Fred" (not (Hashtbl.mem t "Fred"));
        Hashtbl.invariant t;
        Hashtbl.replace t ~key:"Fred" ~data:"Wilma";
        Hashtbl.invariant t;
        assert_bool "should have Fred" (Hashtbl.mem t "Fred");
        Hashtbl.invariant t;
        Hashtbl.remove t "Fred";
        Hashtbl.invariant t;
        assert_bool "should not have Fred anymore" (not (Hashtbl.mem t "Fred"));
        Hashtbl.invariant t);
    ]
