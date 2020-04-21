open! Core
open! Expect_test_helpers_core
open! Uuid
open! Uuid_unix

let%test_unit "nil is valid" = Private.(is_valid_exn nil)

module Test = struct
  let test_size = 100_000

  let no_collisions l =
    let rec loop set l =
      match l with
      | [] -> true
      | t :: rest -> if Set.mem set t then false else loop (Set.add set t) rest
    in
    loop Set.empty l
  ;;

  let generate (n : int) =
    let rec loop acc n =
      if Int.( = ) n 0 then acc else loop (create () :: acc) (n - 1)
    in
    loop [] n
  ;;

  let thread_test () =
    let res1 = ref [] in
    let res2 = ref [] in
    let thread1 =
      Thread.create
        (fun () -> res1 := generate test_size)
        ~on_uncaught_exn:`Print_to_stderr
        ()
    in
    let thread2 =
      Thread.create
        (fun () -> res2 := generate test_size)
        ~on_uncaught_exn:`Print_to_stderr
        ()
    in
    Thread.join thread1;
    Thread.join thread2;
    no_collisions (List.rev_append !res1 !res2)
  ;;

  let%test _ = no_collisions (generate test_size)
  let%test _ = thread_test ()

  let%expect_test "UUIDs are shown as [nil] in tests" =
    print_endline (to_string_hum (create ()));
    [%expect {| 00000000-0000-0000-0000-000000000000 |}];
    print_s [%sexp (create () : t)];
    [%expect {| 00000000-0000-0000-0000-000000000000 |}]
  ;;
end

let%expect_test "[Unstable.t_of_sexp] validates its input" =
  require_does_raise [%here] (fun () -> [%of_sexp: Unstable.t] [%sexp "not a uuid"]);
  [%expect
    {|
    (Of_sexp_error "not a uuid: not a valid UUID" (invalid_sexp "not a uuid")) |}]
;;

let%expect_test "[Stable.V1.t_of_sexp] does not validate its input" =
  require_does_not_raise [%here] (fun () ->
    ignore ([%of_sexp: Stable.V1.t] [%sexp "not a uuid"] : t));
  [%expect {| |}]
;;

let%expect_test "[Unstable.sexp_of_t] on a valid input" =
  require_does_not_raise [%here] (fun () ->
    ignore ([%of_sexp: Unstable.t] [%sexp (create () : t)] : t));
  [%expect {| |}]
;;

let%expect_test "[Stable.V1.sexp_of_t] on a valid input" =
  require_does_not_raise [%here] (fun () ->
    ignore ([%of_sexp: Stable.V1.t] [%sexp (create () : t)] : t));
  [%expect {| |}]
;;
