open! Core
open! Async
open! Import
open! Thread

let%expect_test "[sexp_of_t]" =
  let t = create ignore () in
  print_s [%sexp (t : t)];
  (* We expect thread id [2] to be deterministically printed because we inline test each
     file on its own, and there is no top-level effect that will create a thread prior to
     this. *)
  let%bind () = [%expect {|
    (thread (id 2)) |}] in
  join t;
  print_s [%sexp (t : t)];
  let%bind () = [%expect {|
    (thread (id 2)) |}] in
  return ();
;;
