open OUnit;;
open Core

let test =
  "hash_queue" >:::
  [
    "basic" >:: (fun () ->
      let module Hq_arg = struct
        include String
        let hash (x : t) = Hashtbl.hash x
        let sexp_of_t = Sexplib.Conv.sexp_of_string
      end in
      let module Hq = Hash_queue.Make (Hq_arg) in
      let hq = Hq.create () in
      let inv () = Hq.invariant hq in

      (* tests over empty queue *)
      inv ();
      assert (Hq.is_empty hq);
      assert (Hq.dequeue hq = None);
      assert (try ignore (Hq.dequeue_exn hq); false with _ -> true);
      assert (Hq.dequeue_with_key hq = None);
      assert (try ignore (Hq.dequeue_with_key_exn hq); false with _ -> true);
      Hq.dequeue_all hq ~f:(fun _ -> assert false);
      assert (Hq.remove hq "foobar" = `No_such_key);
      assert (try ignore (Hq.remove_exn hq "foobar"); false with | _ -> true);
      assert (Hq.replace hq "foobar" 0 = `No_such_key);
      assert (try ignore (Hq.replace_exn hq "foobar" 0); false with
        | _ -> true);
      assert
        ([] = Hq.foldi hq ~init:[] ~f:(fun ac ~key:_ ~data:_ -> () :: ac));
      assert ([] = Hq.fold hq ~init:[] ~f:(fun ac _ -> () :: ac));
      Hq.iteri hq ~f:(fun ~key:_ ~data:_ -> assert false);

      (* test with 10 elems *)
      let n = 10 in
      for i = 1 to n do
        assert (Hq.enqueue hq (string_of_int i) i = `Ok);
        inv ();
      done;
      assert (Hq.length hq = n);
      assert
        (List.rev
           (Hq.foldi hq ~init:[] ~f:(fun ac ~key ~data -> (key, data) :: ac))
         = List.init n ~f:(fun i -> let i = i + 1 in (string_of_int i, i)));
      assert
        (List.rev (Hq.fold hq ~init:[] ~f:(fun ac data -> data :: ac))
         = List.init n ~f:(fun i -> i + 1));
      Hq.iteri hq ~f:(fun ~key ~data -> assert (key = string_of_int data));

      (* test removing the first element from the queue *)
      let sum = ref 0 in
      Hq.iter hq ~f:(fun x -> sum := !sum + x);
      assert (!sum = (n * (n + 1) / 2));
      assert (Hq.mem hq "1");
      ignore (Hq.dequeue hq);
      inv ();
      assert (not (Hq.mem hq "1"));
      assert (Hq.length hq = n - 1);

      (* remove the last *)
      assert (Hq.remove hq (string_of_int n) = `Ok);
      (* double remove *)
      assert (Hq.remove hq (string_of_int n) = `No_such_key);
      inv ();
      assert (Hq.length hq = n - 2);

      (* remove everything *)
      let num = ref 0 in
      Hq.dequeue_all hq ~f:(fun _ -> num := !num + 1);
      inv ();
      assert (!num = n - 2);
      assert (Hq.is_empty hq);
      inv ();
      Hq.clear hq;
      assert (Hq.is_empty hq);

      (* add 100 *)
      for i = 1 to 100 do
        assert (Hq.enqueue hq (string_of_int i) i = `Ok);
      done;
      (* double booking *)
      assert (Hq.enqueue hq "42" 42 = `Key_already_present);
      assert (try
                Hq.enqueue_exn hq "42" 42; false
              with
              | _ -> true);
      assert (Hq.replace hq "1" 42 = `Ok);
      assert (Hq.lookup hq "1" = Some 42);
      assert (Hq.lookup_exn hq "1" = 42);
      assert (Hq.dequeue_with_key hq = Some ("1", 42));
      assert (Hq.replace hq "1" 42 = `No_such_key);
      assert (try
                Hq.replace_exn hq "1" 42; false
              with
              | _ -> true);
      assert (Hq.lookup hq "1" = None);
      assert (try ignore (Hq.lookup_exn hq "1"); false with
        | Not_found_s _ | Caml.Not_found -> true | _ -> false);

      Hq.clear hq;
      assert (Hq.is_empty hq);

      let add i = Hq.enqueue_exn hq (Int.to_string i) i in
      List.iter [1; 2; 3] ~f:add;
      assert (["1"; "2"; "3" ] = Hq.keys hq);
      begin
        try Hq.iter hq ~f:(fun _ -> add 13); assert false
        with _ -> ();
      end;
      begin
        try Hq.iter hq ~f:(fun _ -> ignore (Hq.remove hq "foo")); assert false
        with _ -> ();
      end;
      [%test_result: int] (Hq.lookup_and_move_to_back_exn hq "2") ~expect:2;
      [%test_result: string * int] (Hq.dequeue_with_key_exn hq) ~expect:("1",1);
      [%test_result: string * int] (Hq.dequeue_with_key_exn hq) ~expect:("3",3);
      [%test_result: string * int] (Hq.dequeue_with_key_exn hq) ~expect:("2",2);
      [%test_result: (string * int) option] (Hq.dequeue_with_key hq) ~expect:None;
    )
  ]
