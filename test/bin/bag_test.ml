open OUnit;;
open Core

let test =
  "bag" >:::
  [ "foo" >::
    (fun () ->
       "create" @? begin
         let b = Bag.create () in
         Bag.invariant ignore b;
         assert (Bag.is_empty b);
         assert (0 = Bag.length b);
         true;
       end;
       "add1" @? begin
         let b = Bag.create () in
         let _e1 = Bag.add b 1 in
         Bag.invariant ignore b;
         assert (1 = Bag.length b);
         assert (not (Bag.is_empty b));
         true;
       end;
       "add2" @? begin
         let b = Bag.create () in
         let _e1 = Bag.add b 1 in
         let _e2 = Bag.add b 2 in
         Bag.invariant ignore b;
         assert (2 = Bag.length b);
         assert (not (Bag.is_empty b));
         true
       end;
       "remove" @? begin
         let b = Bag.create () in
         ignore (Bag.remove b (Bag.add b 1));
         Bag.invariant ignore b;
         assert (Bag.is_empty b);
         true;
       end;
       "remove2" @? begin
         let b = Bag.create () in
         let e1 = Bag.add b 1 in
         let _e2 = Bag.add b 2 in
         ignore (Bag.remove b e1);
         Bag.invariant ignore b;
         assert (1 = Bag.length b);
         true;
       end;
       "add100" @? begin
         let b = Bag.create () in
         let n = 20 in
         for i = 1 to n do
           let _e = Bag.add b i in
           Bag.invariant ignore b;
         done;
         assert (Bag.length b = n);
         for _ = 1 to n do
           Bag.invariant ignore b;
           match Bag.remove_one b with
           | None -> assert false
           | Some _ -> ()
         done;
         assert (Bag.is_empty b);
         Bag.invariant ignore b;
         true
       end;
       "container" @? begin
         let b = Bag.create () in
         let n = 20 in
         for i = 1 to n do
           ignore (Bag.add b i);
         done;
         assert (n = Bag.fold b ~init:0 ~f:(fun n _ -> n + 1));
         for i = 1 to n do
           assert (Bag.exists b ~f:(fun i' -> i = i'));
         done;
         Bag.iter b ~f:(fun i -> assert (1 <= i && i <= n));
         assert (Bag.for_all b ~f:(fun i -> 1 <= i && i <= n));
         true;
       end;
    )
  ]
