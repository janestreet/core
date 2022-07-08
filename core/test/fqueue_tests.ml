open Core

let%test_unit "Fqueue round trip via list" =
  Quickcheck.test
    (List.quickcheck_generator Int.quickcheck_generator)
    ~sexp_of:[%sexp_of: int list]
    ~f:(fun a ->
      let b = Fqueue.of_list a in
      let c = Fqueue.to_list b in
      let d = Fqueue.of_list c in
      [%test_result: int list] ~expect:a c;
      [%test_result: int Fqueue.t] ~expect:b d)
    ~examples:[ []; [ 1 ]; [ 1; 2 ]; [ 1; 2; 3 ] ]
;;
