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
     the default value of [here] in the external version of Base for [%call_pos] arguments *)
  require_does_raise ~hide_positions:true (fun () ->
    Error.failwiths "hello world" 13 sexp_of_int);
  [%expect {| ("hello world" 13 lib/core/test/test_error.ml:LINE:COL) |}];
  require_does_raise ~hide_positions:true (fun () ->
    Error.failwiths ~here:Lexing.dummy_pos "hello world" 13 sexp_of_int);
  [%expect {| ("hello world" 13) |}]
;;

(* Since t_of_sexp and sexp_of_t are a little bespoke and are used by our shrinker, we
   check that shrinking doesn't actually blow things up. *)
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

  (* Our second claim is that starting from a small sexp, we can only shrink a few times
     before not being able to shrink farther. We check this on a particular sexp where we
     believe that it is only possible to shrink twice. *)
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

module%test Portable = struct
  (* [test] checks that [A] and [B] are wire-compatible:
     - They share the same bin_digest, which on its own gives us some confidence the
       serializers/deserializers are compatible.
     - The [A.t] values round-trip through the bin-io serializers and deserializers. More
       specifically, both occurrences of "bytes" in this diagram are the same:
       [A.t -> bytes -> B.t -> bytes]
  *)
  let test
    (type a b)
    (module A : Stable_without_comparator with type t = a)
    (module B : Stable_without_comparator with type t = b)
    examples_a
    ~expect
    =
    Expect_test_helpers_core.print_and_check_stable_type (module A) examples_a;
    expect ();
    let examples_b =
      List.map examples_a ~f:(fun a ->
        Binable.of_string (module B) (Binable.to_string (module A) a))
    in
    Expect_test_helpers_core.print_and_check_stable_type (module B) examples_b;
    expect ()
  ;;

  let%expect_test "error -> portable" =
    let examples =
      [ Error.of_string "hello"
      ; Error.create_s [%sexp "world"]
      ; Error.of_list [ Error.of_string "hello"; Error.create_s [%sexp "world"] ]
      ; Error.of_exn Stdlib.Not_found
      ]
    in
    test
      (module Error.Stable.V1)
      (module Error.Stable.Portable.V1)
      examples
      ~expect:(fun () ->
        [%expect
          {|
          (bin_shape_digest 832b40ae394f2851da8ba67b3339b429)
          ((sexp   hello)
           (bin_io "\000\005hello"))
          ((sexp   world)
           (bin_io "\000\005world"))
          ((sexp (hello world)) (bin_io "\001\002\000\005hello\000\005world"))
          ((sexp   Not_found)
           (bin_io "\000\tNot_found"))
          |}]);
    test
      (module Error.Stable.V2)
      (module Error.Stable.Portable.V2)
      examples
      ~expect:(fun () ->
        [%expect
          {|
          (bin_shape_digest 52966f4a49a77bfdff668e9cc61511b3)
          ((sexp   hello)
           (bin_io "\001\005hello"))
          ((sexp   world)
           (bin_io "\003\000\005world"))
          ((sexp (hello world)) (bin_io "\007\000\002\001\005hello\003\000\005world"))
          ((sexp   Not_found)
           (bin_io "\002\000\tNot_found"))
          |}]);
    (* Note: Testing Unstable Modules

       In principle, we need not guarantee wire-compatibility of the unstable bin_io
       conversion functions on [Error] and [Error.Portable]: they're unstable! In
       practice, though, we'd rather allow users to swap between the modules with little
       effort, at least until/unless a newer stable version gets added, so we witness
       their compatibility with a test. *)
    test (module Error) (module Error.Portable) examples ~expect:(fun () ->
      [%expect
        {|
        (bin_shape_digest 52966f4a49a77bfdff668e9cc61511b3)
        ((sexp   hello)
         (bin_io "\001\005hello"))
        ((sexp   world)
         (bin_io "\003\000\005world"))
        ((sexp (hello world)) (bin_io "\007\000\002\001\005hello\003\000\005world"))
        ((sexp   Not_found)
         (bin_io "\002\000\tNot_found"))
        |}])
  ;;

  let%expect_test "portable -> error" =
    let examples =
      [ Error.Portable.of_string "hello"
      ; Error.Portable.create_s [%sexp "world"]
      ; Error.Portable.of_list
          [ Error.Portable.of_string "hello"; Error.Portable.create_s [%sexp "world"] ]
      ]
    in
    test
      (module Error.Stable.Portable.V1)
      (module Error.Stable.V1)
      examples
      ~expect:(fun () ->
        [%expect
          {|
          (bin_shape_digest 832b40ae394f2851da8ba67b3339b429)
          ((sexp   hello)
           (bin_io "\000\005hello"))
          ((sexp   world)
           (bin_io "\000\005world"))
          ((sexp (hello world)) (bin_io "\001\002\000\005hello\000\005world"))
          |}]);
    test
      (module Error.Stable.Portable.V2)
      (module Error.Stable.V2)
      examples
      ~expect:(fun () ->
        [%expect
          {|
          (bin_shape_digest 52966f4a49a77bfdff668e9cc61511b3)
          ((sexp   hello)
           (bin_io "\001\005hello"))
          ((sexp   world)
           (bin_io "\003\000\005world"))
          ((sexp (hello world)) (bin_io "\007\000\002\001\005hello\003\000\005world"))
          |}]);
    (* See the above "Note: Testing Unstable Modules". *)
    test (module Error.Portable) (module Error) examples ~expect:(fun () ->
      [%expect
        {|
        (bin_shape_digest 52966f4a49a77bfdff668e9cc61511b3)
        ((sexp   hello)
         (bin_io "\001\005hello"))
        ((sexp   world)
         (bin_io "\003\000\005world"))
        ((sexp (hello world)) (bin_io "\007\000\002\001\005hello\003\000\005world"))
        |}])
  ;;
end
