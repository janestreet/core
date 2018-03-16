open OUnit;;
open Core

module Map = Map.Poly

let m1 = Map.of_alist_exn ["a",1; "b",2; "c",3; "d",4]
let m2 = Map.of_alist_exn ["a",1; "c",-3; "d",4; "e",5]

let test =
  "pMap" >:::
  [ "merge1" >::
    (fun () ->
       let f ~key:_ = function
         | `Left _ | `Right _ -> None
         | `Both (x, y) -> Some (x+y)
       in
       "eq1" @? Map.equal (=) (Map.merge ~f m1 m2)
                  (Map.of_alist_exn ["a",2;"c",0;"d",8;]);
       "eq2" @? Map.equal (=) (Map.merge ~f m2 m1)
                  (Map.of_alist_exn ["a",2;"c",0;"d",8;]);
    );
    "merge2" >::
    (fun () ->
       let f ~key:_ = function
         | `Left x -> Some x
         | `Right _ -> None
         | `Both (x, y) -> Some (x+y)
       in
       "eq" @? Map.equal (=) (Map.merge ~f m1 m2)
                 (Map.of_alist_exn ["a",2;"b",2;"c",0;"d",8;])
    );
    "merge3" >::
    (fun () ->
       let f ~key:_ = function
         | `Left x | `Right x -> Some x
         | `Both (x, y) -> Some (x+y)
       in
       "eq1" @? Map.equal (=) (Map.merge ~f m1 m2)
                  (Map.of_alist_exn ["a",2;"b",2;"c",0;"d",8;"e",5]);
       "eq2" @? Map.equal (=) (Map.merge ~f m2 m1)
                  (Map.of_alist_exn ["a",2;"b",2;"c",0;"d",8;"e",5])
    );
    "merge3" >::
    (fun () ->
       let f ~key:_ = function
         | `Left x | `Right x -> Some x
         | `Both (x, y) -> Some (x+y)
       in
       "eq1" @? Map.equal (=) (Map.merge ~f m1 Map.empty) m1;
       "eq2" @? Map.equal (=) (Map.merge ~f Map.empty m1) m1;
    );
    "sexp" >::
    (fun () ->
       let s = "((a 1) (b 2) (c 3) (d 4))" in
       let m1' = Map.t_of_sexp string_of_sexp int_of_sexp (Sexp.of_string s) in
       "of_sexp1" @? (Map.equal (=) m1' m1);
       let s_dup = "((a 1) (b 2) (a 3) (d 4))" in
       let s_dup = Sexp.of_string s_dup in
       try ignore (Map.t_of_sexp string_of_sexp int_of_sexp s_dup); assert false
       with _ -> ()
    );
    "of_alist" >::
    (fun () ->
       let a = ["a",1;"b",2;"c",3;"d",4] in
       let m =
         match Map.of_alist a with `Ok x -> x | `Duplicate_key _ -> failwith "argh"
       in
       "1" @? (Map.find_exn m "a" = 1 && Map.find_exn m "d" = 4);
       let a_dup = ["a",1;"b",2;"c",3;"b",4; "e", 5] in
       "2" @?
       (match Map.of_alist a_dup with `Ok _ -> false | `Duplicate_key x -> x = "b");
       "3" @?
       (Map.to_alist (Map.of_alist_exn a)
        = List.sort ~compare:Poly.ascending a);
       (try ignore (Map.of_alist_exn a_dup); assert false with _ -> ())
    );
    "for_all/exists" >:: (fun () ->
      let m = Map.of_alist_exn ["a",1;"b",2;"c",3;"d",4] in
      "1" @? (Map.for_all ~f:(fun x -> x > 0) m);
      "2" @? (not (Map.for_all ~f:(fun x -> x % 2 = 0) m));
      "3" @? (Map.exists ~f:(fun x -> x % 2 = 0) m);
      "4" @? (not (Map.exists ~f:(fun x -> x < 0) m));
      "short circuit forall" @? (
        let sum = ref 0 in
        ignore (Map.for_all m ~f:(fun x -> sum := !sum + x; x <> 1));
        !sum = 1
      );
      "short circuit exists" @? (
        let sum = ref 0 in
        ignore (Map.exists m ~f:(fun x -> sum := !sum + x; x = 1));
        !sum = 1
      );
    );
  ]
