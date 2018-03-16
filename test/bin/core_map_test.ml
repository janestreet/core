open OUnit;;
open Core

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
    "bin_io" >::
    (fun () ->
       let max_n = 20 in
       let bstr = Bigstring.create (2 * max_n + 1) in
       for n = 0 to max_n do
         let m1 = Int.Map.of_alist_exn (List.init n ~f:(fun x -> x, succ x)) in
         let pos = Int.Map.bin_write_t Int.bin_write_t bstr ~pos:0 m1 in
         "pos" @? (pos = 2 * n + 1);
         let pos_ref = ref 0 in
         let m2 = Int.Map.bin_read_t Int.bin_read_t bstr ~pos_ref in
         "pos_ref" @? (!pos_ref = 2 * n + 1);
         "equal" @? (Int.Map.equal Int.equal m1 m2);
         if n >= 2
         then begin
           bstr.{1} <- 'x';
           bstr.{3} <- 'x';
           pos_ref := 0;
           let dup_check =
             try
               ignore(Int.Map.bin_read_t Int.bin_read_t bstr ~pos_ref : Int.t Int.Map.t);
               false
             with Failure _ -> !pos_ref = 2 * n + 1
           in
           "dup_check" @? dup_check
         end
       done;
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
       ((List.sort ~compare:Poly.ascending
           (StringMap.to_alist (StringMap.of_alist_exn a)))
        = List.sort ~compare:Poly.ascending a);
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
