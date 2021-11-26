open! Core
open! Import
open! Char

let%test_module "Caseless Comparable" =
  (module struct
    let%test _ =
      Int.equal (Base.Map.find_exn (Caseless.Map.of_alist_exn [ 'a', 4; 'b', 5 ]) 'A') 4
    ;;

    let%test _ = Base.Set.mem (Caseless.Set.of_list [ 'a'; 'b' ]) 'A'
    let%test _ = Int.equal (Base.Set.length (Caseless.Set.of_list [ 'a'; 'A' ])) 1
  end)
;;

let%test_module "Caseless Hash" =
  (module struct
    let%test _ = Int.equal (Caseless.hash 'a') (Caseless.hash 'A')
  end)
;;
