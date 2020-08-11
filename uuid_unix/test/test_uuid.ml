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
    let rec loop acc n = if Int.( = ) n 0 then acc else loop (create () :: acc) (n - 1) in
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

  let%expect_test "UUIDs are hidden in tests" =
    print_s [%sexp (create () : t)];
    [%expect {| <uuid-omitted-in-test> |}]
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

let%expect_test "[Unstable.t_of_sexp] on a valid input" =
  require_does_not_raise [%here] (fun () ->
    ignore ([%of_sexp: Unstable.t] [%sexp "1f7f8c2e-d297-11ea-aafd-aa0000ef6338"] : t));
  [%expect {| |}]
;;

let%expect_test "[Stable.V1.t_of_sexp] on a valid input" =
  require_does_not_raise [%here] (fun () ->
    ignore ([%of_sexp: Stable.V1.t] [%sexp "1f7f8c2e-d297-11ea-aafd-aa0000ef6338"] : t));
  [%expect {| |}]
;;

let roundtrips (module M : Stable_without_comparator with type t = t) =
  Quickcheck.test [%quickcheck.generator: t] ~f:(fun t ->
    let sexp_roundtrip = t |> M.sexp_of_t |> M.t_of_sexp in
    [%test_result: t] sexp_roundtrip ~expect:t;
    let bin_prot_roundtrip =
      t |> Binable.to_string (module M) |> Binable.of_string (module M)
    in
    [%test_result: t] bin_prot_roundtrip ~expect:t)
;;

let%test_unit "[Unstable] round-trips" = roundtrips (module Unstable)
let%test_unit "[Stable.V1] round-trips" = roundtrips (module Stable.V1)
