open OUnit;;
open Core
open Array

let ar1 = [|1;2;3;4;5;6;7;8;9;10|]

let ( =|= ) list array = list = Array.to_list array

let test =
  "core_array" >:::
  [ "slice" >::
    (fun () ->
       "all" @? (slice ar1 0 0 = ar1);
       "ordinary" @? (slice ar1 1 3 = [|2;3|]);
       "neg1" @? (slice ar1 0 (-1) = [|1;2;3;4;5;6;7;8;9|]);
       "neg2" @? (slice ar1 (-1) 0 = [|10|]);
       "neg3" @? (slice ar1 (-5) (-4) = [|6|];)
    );
    "nget" >::
    (fun () ->
       "neg" @? (nget ar1 (-3) = 8);
       "pos" @? (nget ar1 3 = ar1.(3));
       "invalid" @?
       (try ignore (nget ar1 (-100)); false
        with Invalid_argument _ -> true | _ -> false)
    );
    "filter_opt" >::
    (fun () ->
       "none" @? (filter_opt [|None;None;None|] = [||]);
       "single" @? (filter_opt [|None;Some 3;None|] = [|3|]);
       "singlef" @? (filter_opt [|None;Some 3.;None|] = [|3.|]);
       "double" @? (filter_opt [|None; Some 3; Some 4|] = [|3;4|]);
    );
    "swap" >::
    (fun () ->
       let array = [|0; 1; 2; 3|] in
       "same" @? (swap array 0 0; array = [|0; 1; 2; 3|]);
       "different" @? (swap array 0 3; array = [|3; 1; 2; 0|]);
    );
    "exists" >::
    (fun () ->
       let list =
         List.init ~f:(fun _ -> Random.int 1000) 1000
       in
       let array = Array.of_list list in
       "list1" @? (List.exists ~f:((=) 1) list = Array.exists ~f:((=) 1) array);
       "list2" @? (List.exists ~f:((=) 2) list = Array.exists ~f:((=) 2) array);
       "list3" @? (List.exists ~f:((=) 3) list = Array.exists ~f:((=) 3) array);
       "list4" @? (List.exists ~f:((=) 4) list = Array.exists ~f:((=) 4) array);
    );
    "for_all" >::
    (fun () ->
       let list = Quickcheck_deprecated.lg (fun () -> Random.int 1000) ~size_gen:(fun _ -> 1000) () in
       let array = Array.of_list list in
       "list1" @? (List.for_all ~f:((<>) 1) list = Array.for_all ~f:((<>) 1) array);
       "list2" @? (List.for_all ~f:((<>) 2) list = Array.for_all ~f:((<>) 2) array);
       "list3" @? (List.for_all ~f:((<>) 3) list = Array.for_all ~f:((<>) 3) array);
       "list4" @? (List.for_all ~f:((<>) 4) list = Array.for_all ~f:((<>) 4) array);
    );
    "mem" >::
    (fun () ->
       let list = Quickcheck_deprecated.lg (fun () -> Random.int 1000) ~size_gen:(fun _ -> 1000) () in
       let array = Array.of_list list in
       let equal = Core.Int.equal in
       "list1" @? (List.mem list 1 ~equal = Array.mem array 1 ~equal);
       "list2" @? (List.mem list 2 ~equal = Array.mem array 2 ~equal);
       "list3" @? (List.mem list 3 ~equal = Array.mem array 3 ~equal);
       "list4" @? (List.mem list 4 ~equal = Array.mem array 4 ~equal);
    );
    "rev" >::
    (fun () ->
       let ordered_list = List.init 100 ~f:(fun i -> i) in
       let empty_list = [] in
       let one_list = [0] in
       "ordered" @?
       (let ordered_array = Array.of_list ordered_list in
        Array.rev_inplace ordered_array;
        List.rev ordered_list =|= ordered_array);
       "empty" @?
       (let empty_array = Array.of_list empty_list in
        Array.rev_inplace empty_array;
        List.rev empty_list =|= empty_array);
       "one" @?
       (let one_array = Array.of_list one_list in
        Array.rev_inplace one_array;
        List.rev one_list =|= one_array);
    );
    "map_inplace" >::
    (fun () ->
       let random_list =
         Quickcheck_deprecated.lg (fun () -> Random.int 1000) ~size_gen:(fun _ -> 1000) ()
       in
       let empty_list = [] in
       let one_list = [0] in
       let f i = i * i in
       "random" @?
       (let random_array = Array.of_list random_list in
        Array.map_inplace ~f random_array;
        List.map ~f random_list =|= random_array);
       "empty" @?
       (let empty_array = Array.of_list empty_list in
        Array.map_inplace ~f empty_array;
        List.map ~f empty_list =|= empty_array);
       "one" @?
       (let one_array = Array.of_list one_list in
        Array.map_inplace ~f one_array;
        List.map ~f one_list =|= one_array);
    );
    "cartesian_product" >::
    (fun () ->
       "empty1" @? is_empty (cartesian_product [||] [||]);
       "empty2" @? is_empty (cartesian_product [||] [|13|]);
       "empty3" @? is_empty (cartesian_product [|13|] [||]);
       "simple" @?
       (cartesian_product [|1; 2; 3;|] [|"a"; "b";|]
        = [|(1, "a"); (1, "b"); (2, "a"); (2, "b"); (3, "a"); (3, "b");|]));
  ]
