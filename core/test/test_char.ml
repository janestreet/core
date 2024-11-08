open! Core
open! Import
open! Char

module%test [@name "Caseless Comparable"] _ = struct
  let%test _ =
    Int.equal (Base.Map.find_exn (Caseless.Map.of_alist_exn [ 'a', 4; 'b', 5 ]) 'A') 4
  ;;

  let%test _ = Base.Set.mem (Caseless.Set.of_list [ 'a'; 'b' ]) 'A'
  let%test _ = Int.equal (Base.Set.length (Caseless.Set.of_list [ 'a'; 'A' ])) 1
end

module%test [@name "Caseless Hash"] _ = struct
  let%test _ = Int.equal (Caseless.hash 'a') (Caseless.hash 'A')
end

let%expect_test "of_string" =
  require_equal (module Char) (Char.of_string "c") 'c';
  require_does_raise (fun () -> Char.of_string "");
  [%expect {| (Failure "Char.of_string: \"\"") |}];
  require_does_raise (fun () -> Char.of_string "too long");
  [%expect {| (Failure "Char.of_string: \"too long\"") |}]
;;
