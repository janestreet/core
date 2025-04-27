open! Core
open! Import

let%expect_test "[raise_s sexp] raises an exn whose [sexp_of_t] is [sexp]" =
  let sexp = [%sexp "foo"] in
  match Error.raise_s sexp with
  | (_ : Nothing.t) -> .
  | exception exn -> require (phys_equal [%sexp (exn : exn)] sexp)
;;

let%expect_test "[failwiths] handles [Lexing.dummy_pos]" =
  (* Location information should be excluded when [here] is [Lexing.dummy_pos], which is
     the default value of [here] in the external version of Base for [%call_pos]
     arguments *)
  require_does_raise ~hide_positions:true (fun () ->
    Error.failwiths "hello world" 13 sexp_of_int);
  [%expect {| ("hello world" 13 lib/core/test/test_error.ml:LINE:COL) |}];
  require_does_raise ~hide_positions:true (fun () ->
    Error.failwiths ~here:Lexing.dummy_pos "hello world" 13 sexp_of_int);
  [%expect {| ("hello world" 13) |}]
;;

(* Since t_of_sexp and sexp_of_t are a little bespoke and are used by our
   shrinker, we check that shrinking doesn't actually blow things up. *)
module%test [@name "shrinking"] _ = struct
  let shrink = Base_quickcheck.Shrinker.shrink Error.quickcheck_shrinker

  (* Our first claim is that errors built from Error.of_string are not shrinkable. *)
  let%test_unit "shrinking from of_string" =
    let config = { Base_quickcheck.Test.default_config with test_count = 100 } in
    Base_quickcheck.Test.run_exn
      ~config
      ~f:(fun string ->
        let t = Error.of_string string in
        let shrunk = shrink t in
        assert (Sequence.is_empty shrunk))
      (module String)
  ;;

  (* Our second claim is that starting from a small sexp, we can only shrink a
     few times before not being able to shrink farther.  We check this on a
     particular sexp where we believe that it is only possible to shrink twice. *)
  let%expect_test "shrinking from sexp" =
    let t = Error.t_of_sexp (Sexp.of_string "(foo bar)") in
    let shrunk1 = shrink t in
    let shrunk2 = Sequence.bind shrunk1 ~f:shrink in
    let shrunk3 = Sequence.bind shrunk2 ~f:shrink in
    print_s [%message "shrunk twice" (shrunk2 : Error.t Sequence.t)];
    assert (Sequence.is_empty shrunk3);
    [%expect {| ("shrunk twice" (shrunk2 (() bar () foo))) |}]
  ;;
end
