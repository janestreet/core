open! Core
open Or_error
open Expect_test_helpers_core

let%test_unit "[error_s] produces a value with the expected [sexp_of_t]" =
  let sexp = [%sexp "foo"] in
  match [%sexp (error_s sexp : _ t)] with
  | List [ Atom "Error"; sexp2 ] -> assert (phys_equal sexp sexp2)
  | _ -> assert false
;;

let%test _ = Result.is_error (filter_ok_at_least_one [])

let%test_unit _ =
  for i = 1 to 10 do
    assert (
      [%compare.equal: unit list t]
        (filter_ok_at_least_one (List.init i ~f:(fun _ -> Ok ())))
        (Ok (List.init i ~f:(fun _ -> ()))))
  done
;;

let%test _ =
  let a = Error.of_string "a"
  and b = Error.of_string "b" in
  match filter_ok_at_least_one [ Ok 1; Error a; Ok 2; Error b ] with
  | Ok x -> [%equal: int list] x [ 1; 2 ]
  | Error _ -> false
;;

let%test _ =
  let a = Error.of_string "a"
  and b = Error.of_string "b" in
  match filter_ok_at_least_one [ Error a; Error b ] with
  | Ok _ -> false
  | Error e ->
    String.( = ) (Error.to_string_hum e) (Error.to_string_hum (Error.of_list [ a; b ]))
;;

let%test _ = Result.is_error (find_ok [])

let%test _ =
  let a = Error.of_string "a"
  and b = Error.of_string "b" in
  match find_ok [ Error a; Ok 1; Error b ] with
  | Ok x -> x = 1
  | Error _ -> false
;;

let%test _ =
  let a = Error.of_string "a"
  and b = Error.of_string "b" in
  match find_ok [ Error a; Error b; Ok 2; Ok 3 ] with
  | Ok x -> x = 2
  | Error _ -> false
;;

let%test _ =
  let a = Error.of_string "a"
  and b = Error.of_string "b" in
  match find_ok [ Error a; Error b ] with
  | Ok _ -> false
  | Error e ->
    String.( = ) (Error.to_string_hum e) (Error.to_string_hum (Error.of_list [ a; b ]))
;;

let%test _ = Result.is_error (find_map_ok ~f:(fun _ -> assert false) [])

let%expect_test _ =
  require_does_raise [%here] (fun () ->
    ignore (find_map_ok ~f:(fun _ -> failwith "abc") [ 1 ] : _ t));
  [%expect {| (Failure abc) |}];
  Ok ()
;;

let%test _ =
  let a = Error.of_string "a"
  and b = Error.of_string "b" in
  match find_map_ok ~f:Fn.id [ Error a; Ok 1; Error b ] with
  | Ok x -> x = 1
  | Error _ -> false
;;

let%test _ =
  let a = Error.of_string "a"
  and b = Error.of_string "b" in
  match find_map_ok ~f:Fn.id [ Error a; Error b; Ok 2; Ok 3 ] with
  | Ok x -> x = 2
  | Error _ -> false
;;

let%test _ =
  let a = Error.of_string "a"
  and b = Error.of_string "b" in
  match find_map_ok ~f:Fn.id [ Error a; Error b ] with
  | Ok _ -> false
  | Error e ->
    String.( = ) (Error.to_string_hum e) (Error.to_string_hum (Error.of_list [ a; b ]))
;;

let%expect_test "[Expect_test_config]" =
  let open Let_syntax in
  let hearts = "hearts" in
  let%bind map =
    String.Map.of_alist_or_error [ hearts, 1; "diamonds", 2; "clubs", 3; "spades", 4 ]
  in
  print_s [%sexp (map : int String.Map.t)];
  [%expect
    {|
    ((clubs    3)
     (diamonds 2)
     (hearts   1)
     (spades   4))
    |}];
  let%bind hearts_value = Map.find_or_error map hearts in
  print_s [%sexp (hearts_value : int)];
  [%expect {| 1 |}];
  Ok ()
;;

module Expect_test_config = Expect_test_config_with_unit_expect [@alert "-deprecated"]

let%expect_test "[Expect_test_config_with_unit_expect]" =
  let open Let_syntax in
  let hearts = "hearts" in
  let%bind map =
    String.Map.of_alist_or_error [ hearts, 1; "diamonds", 2; "clubs", 3; "spades", 4 ]
  in
  print_s [%sexp (map : int String.Map.t)];
  [%expect
    {|
    ((clubs    3)
     (diamonds 2)
     (hearts   1)
     (spades   4))
    |}];
  let%bind hearts_value = Map.find_or_error map hearts in
  print_s [%sexp (hearts_value : int)];
  [%expect {| 1 |}];
  return ()
;;
