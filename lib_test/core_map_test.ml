open OUnit;;
open Core.Std

module StringMap = Map.Make (String)

let m1 = StringMap.of_alist_exn ["a",1; "b",2; "c",3; "d",4]
let m2 = StringMap.of_alist_exn ["a",1; "c",-3; "d",4; "e",5]

let test =
  "core_fmap" >:::
    [
      "merge1" >::
        (fun () ->
           let f ~key:_ = function
             | `Left _ | `Right _ -> None
             | `Both (x, y) -> Some (x+y)
           in
           "eq1" @? StringMap.equal (=) (StringMap.merge ~f m1 m2)
             (StringMap.of_alist_exn ["a",2;"c",0;"d",8;]);
           "eq2" @? StringMap.equal (=) (StringMap.merge ~f m2 m1)
             (StringMap.of_alist_exn ["a",2;"c",0;"d",8;]);
        );
      "merge2" >::
        (fun () ->
           let f ~key:_ = function
             | `Left x -> Some x
             | `Right _ -> None
             | `Both (x, y) -> Some (x+y)
           in
           "eq" @? StringMap.equal (=) (StringMap.merge ~f m1 m2)
             (StringMap.of_alist_exn ["a",2;"b",2;"c",0;"d",8;])
        );
      "merge3" >::
        (fun () ->
           let f ~key:_ = function
             | `Left x | `Right x -> Some x
             | `Both (x, y) -> Some (x+y)
           in
           "eq1" @? StringMap.equal (=) (StringMap.merge ~f m1 m2)
             (StringMap.of_alist_exn ["a",2;"b",2;"c",0;"d",8;"e",5]);
           "eq2" @? StringMap.equal (=) (StringMap.merge ~f m2 m1)
             (StringMap.of_alist_exn ["a",2;"b",2;"c",0;"d",8;"e",5])
        );
      "merge3" >::
        (fun () ->
           let f ~key:_ = function
             | `Left x | `Right x -> Some x
             | `Both (x, y) -> Some (x+y)
           in
           "eq1" @? StringMap.equal (=) (StringMap.merge ~f m1 StringMap.empty) m1;
           "eq2" @? StringMap.equal (=) (StringMap.merge ~f StringMap.empty m1) m1;
        );
      "sexp" >::
        (fun () ->
          let s = "((a 1) (b 2) (c 3) (d 4))" in
          let m1' = StringMap.t_of_sexp int_of_sexp (Sexp.of_string s) in
          "of_sexp1" @? (StringMap.equal (=) m1' m1);
          let s_dup = "((a 1) (b 2) (a 3) (d 4))" in
          let s_dup = Sexp.of_string s_dup in
          try ignore (StringMap.t_of_sexp int_of_sexp s_dup); assert false with _ -> ()
        );
      "of_alist" >::
        (fun () ->
          let a = [("a", 1); ("b", 2); ("c", 3); ("d", 4)] in
          let m =
            match StringMap.of_alist a with `Ok x -> x | `Duplicate_key _ -> failwith "argh"
          in
          "1" @? (StringMap.find_exn m "a" = 1 && StringMap.find_exn m "d" = 4);
          let a_dup = [("a", 1); ("b", 2); ("c", 3);  ("b", 4);  ("e", 5)] in
          "2" @?
            (match StringMap.of_alist a_dup with `Ok _ -> false | `Duplicate_key x -> x = "b");
          "3" @?
            ((List.sort ~cmp:ascending
                (StringMap.to_alist (StringMap.of_alist_exn a)))
              = List.sort ~cmp:ascending a);
          try ignore (StringMap.of_alist_exn a_dup); assert false with _ -> ()
        );
      "for_all/exists" >:: (fun () ->
        let m = StringMap.of_alist_exn ["a",1;"b",2;"c",3;"d",4] in
        "1" @? (StringMap.for_all ~f:(fun x -> x > 0) m);
        "2" @? (not (StringMap.for_all ~f:(fun x -> x % 2 = 0) m));
        "3" @? (StringMap.exists ~f:(fun x -> x % 2 = 0) m);
        "4" @? (not (StringMap.exists ~f:(fun x -> x < 0) m));
        "short circuit forall" @? (
          let sum = ref 0 in
          ignore (StringMap.for_all m ~f:(fun x -> sum := !sum + x; x <> 1));
          !sum = 1
        );
        "short circuit exists" @? (
          let sum = ref 0 in
          ignore (StringMap.exists m ~f:(fun x -> sum := !sum + x; x = 1));
          !sum = 1
        );
      );
    ]
