open! Core
open Expect_test_helpers_core

(* Tests to make sure the implementation of values hasn't changed and that witnesses are
   provided correctly.  *)

(* Tests are bound to the value returned by Immediate.of_typerep, as the test cases are
   designed along the value returned and so that they must be revised if changes are
   made. *)
let%test_module _ =
  (module struct
    type t =
      | Always
      | Sometimes
      | Never
      | Unknown

    (* Where appropriate, unit is used as a representative of something that will always be
       immediate and int ref is used as a representative of something that will always be
       boxed, no matter the actual representation. *)
    let imm_t = typerep_of_unit
    let box_t = typerep_of_ref typerep_of_int
    let is_imm v = Obj.is_int (Obj.repr v)

    (* When [expected] and the actual result is [Sometimes], at least one immediate and one
       non-immediate value must be supplied. It requires at least one value in any case.  *)
    let check expected typerep list =
      if List.length list = 0 then failwith "Must provide at least one test case.";
      let actual =
        ( Type_immediacy.dest (Type_immediacy.of_typerep typerep)
        , Type_immediacy.Always.of_typerep typerep
        , Type_immediacy.Sometimes.of_typerep typerep
        , Type_immediacy.Never.of_typerep typerep )
      in
      let has_imm = List.exists ~f:is_imm list in
      let has_boxed = List.exists ~f:(fun x -> not (is_imm x)) list in
      match has_imm, has_boxed, expected, actual with
      | true, false, Always, (Type_immediacy.Always _, Some _, None, None) -> true
      | true, true, Sometimes, (Type_immediacy.Sometimes _, None, Some _, None) -> true
      | false, true, Never, (Type_immediacy.Never _, None, None, Some _) -> true
      | _, _, Unknown, (Type_immediacy.Unknown, None, None, None) -> true
      | _, _, _, _ -> false
    ;;

    module T : sig
      val check_a : 'a Typerep.t -> 'a list -> bool
      val check_s : 'a Typerep.t -> 'a list -> bool
      val check_n : 'a Typerep.t -> 'a list -> bool
      val check_u : 'a Typerep.t -> 'a list -> bool
    end = struct
      let check_a typerep list = check Always typerep list
      let check_s typerep list = check Sometimes typerep list
      let check_n typerep list = check Never typerep list
      let check_u typerep list = check Unknown typerep list
    end

    include T

    let sexp_of_dest dest =
      match (dest : _ Type_immediacy.dest) with
      | Always _ -> [%sexp "Always"]
      | Never _ -> [%sexp "Never"]
      | Sometimes _ -> [%sexp "Sometimes"]
      | Unknown -> [%sexp "Unknown"]
    ;;

    let require_self_consistent ?cr typerep list =
      let type_immediacy = Type_immediacy.dest (Type_immediacy.of_typerep typerep) in
      let has_imm = List.exists ~f:is_imm list in
      let has_boxed = List.exists ~f:(fun x -> not (is_imm x)) list in
      match has_imm, has_boxed, type_immediacy with
      | true, false, Always _ -> ()
      | false, true, Never _ -> ()
      | true, true, Sometimes _ -> ()
      | _, _, Unknown -> ()
      | _, _, _ ->
        Expect_test_helpers_base.print_cr
          ?cr
          [%here]
          [%message
            "The immediacy of the values did not match the Type_immediacy"
              (type_immediacy : dest)
              (has_imm : bool)
              (has_boxed : bool)]
    ;;

    let%test _ =
      let module M = struct
        type t = int [@@deriving typerep]
      end
      in
      check_a M.typerep_of_t [ 0; 1 ]
    ;;

    let%test (_ [@tags "no-js"]) =
      let module M = struct
        type t = int32 [@@deriving typerep]
      end
      in
      check_n
        M.typerep_of_t
        [ Int32.zero; Int32.one; Int32.minus_one; Int32.of_int_exn 32580 ]
    ;;

    let%test _ =
      let module M = struct
        type t = int64 [@@deriving typerep]
      end
      in
      check_n M.typerep_of_t [ Int64.zero ]
    ;;

    let%test (_ [@tags "no-js"]) =
      let module M = struct
        type t = nativeint [@@deriving typerep]
      end
      in
      check_n M.typerep_of_t [ Nativeint.zero ]
    ;;

    let%test _ =
      let module M = struct
        type t = char [@@deriving typerep]
      end
      in
      check_a M.typerep_of_t [ 'a' ]
    ;;

    let%test (_ [@tags "no-js"]) =
      let module M = struct
        type t = float [@@deriving typerep]
      end
      in
      check_n
        M.typerep_of_t
        [ 1.1
        ; 0.0
        ; -3.3
        ; Float.nan
        ; Float.infinity
        ; Float.neg_infinity
        ; Float.max_value
        ; Float.min_value
        ]
    ;;

    let%test _ =
      let module M = struct
        type t = string [@@deriving typerep]
      end
      in
      check_n M.typerep_of_t [ ""; "Hello world!" ]
    ;;

    let%test _ =
      let module M = struct
        type t = bool [@@deriving typerep]
      end
      in
      check_a M.typerep_of_t [ true; false ]
    ;;

    let%test _ =
      let module M = struct
        type t = unit [@@deriving typerep]
      end
      in
      check_a M.typerep_of_t [ () ]
    ;;

    let%test _ =
      let module M = struct
        (* The generated typrep in the case of parametric types have this signature:
           val M.typerep_of_t : 'a Typerep.t -> 'a t Typerep.t *)
        type 'a t = 'a option [@@deriving typerep]
      end
      in
      check_s (M.typerep_of_t imm_t) [ None; Some () ]
      && check_s (M.typerep_of_t box_t) [ None; Some (ref 1) ]
    ;;

    let%test _ =
      let module M = struct
        type 'a t = 'a list [@@deriving typerep]
      end
      in
      check_s (M.typerep_of_t imm_t) [ []; [ () ] ]
      && check_s (M.typerep_of_t box_t) [ []; [ ref 1 ] ]
    ;;

    let static_empty_array = [||]

    let%test _ =
      let module M = struct
        type 'a t = 'a array [@@deriving typerep]
      end
      in
      check_n (M.typerep_of_t imm_t) [ static_empty_array; [||]; [| (); () |] ]
    ;;

    let%test _ =
      let module M = struct
        type 'a t = 'a ref [@@deriving typerep]
      end
      in
      check_n (M.typerep_of_t imm_t) [ ref () ]
    ;;

    let%test _ =
      let module M = struct
        type 'a t = 'a lazy_t [@@deriving typerep]
      end
      in
      check_s (M.typerep_of_t typerep_of_int) [ lazy 1; lazy (1 + 2) ]
      && check_n (M.typerep_of_t typerep_of_float) [ lazy 0.0; lazy (1.0 +. -3.3) ]
    ;;

    let require_unboxed ?cr =
      Expect_test_helpers_base.require
        ?cr
        ~if_false_then_print_s:
          (lazy
            [%message
              "Unboxed container types should have the immediacy of their contained type"])
        [%here]
    ;;

    let require_boxed ?cr =
      Expect_test_helpers_base.require
        ?cr
        ~if_false_then_print_s:
          (lazy [%message "Boxed container types should never be immediate"])
        [%here]
    ;;

    let require_maybe_boxed ?cr =
      Expect_test_helpers_base.require
        ?cr
        ~if_false_then_print_s:
          (lazy
            [%message
              "Unboxing may be the default depending on compiler settings. Type \
               immediacy for unboxable types that are not explicitly unboxed or boxed \
               should be unknown."])
        [%here]
    ;;

    let%expect_test _ =
      let module M = struct
        type t = { foo : unit } [@@deriving typerep]
      end
      in
      require_self_consistent M.typerep_of_t [ { M.foo = () } ];
      check_u M.typerep_of_t [ { M.foo = () } ] |> require_maybe_boxed
    ;;

    let%expect_test _ =
      let module M = struct
        type t = { foo : unit } [@@boxed] [@@deriving typerep]
      end
      in
      require_self_consistent M.typerep_of_t [ { M.foo = () } ];
      check_n M.typerep_of_t [ { M.foo = () } ] |> require_boxed ~cr:CR_someday;
      check_u M.typerep_of_t [ { M.foo = () } ] |> require_maybe_boxed;
      [%expect {|
        "Boxed container types should never be immediate"
        |}]
    ;;

    let%expect_test _ =
      let module M = struct
        type t = { foo : unit } [@@unboxed] [@@deriving typerep]
      end
      in
      require_self_consistent M.typerep_of_t [ { M.foo = () } ];
      check_a M.typerep_of_t [ { M.foo = () } ] |> require_unboxed ~cr:CR_someday;
      check_u M.typerep_of_t [ { M.foo = () } ] |> require_maybe_boxed;
      [%expect
        {|
        "Unboxed container types should have the immediacy of their contained type"
        |}]
    ;;

    let%expect_test _ =
      let module M = struct
        type t = { foo : string } [@@unboxed] [@@deriving typerep]
      end
      in
      require_self_consistent M.typerep_of_t [ { M.foo = "foo" } ];
      check_n M.typerep_of_t [ { M.foo = "foo" } ] |> require_unboxed
    ;;

    let%test _ =
      let module M = struct
        type t =
          { foo : unit
          ; bar : unit
          }
        [@@deriving typerep]
      end
      in
      check_n M.typerep_of_t [ { M.foo = (); bar = () } ]
    ;;

    let%test _ =
      let module M = struct
        type ('a, 'b) t = 'a * 'b [@@deriving typerep]
      end
      in
      check_n (M.typerep_of_t imm_t imm_t) [ (), () ]
    ;;

    let%test _ =
      let module M = struct
        type ('a, 'b, 'c) t = 'a * 'b * 'c [@@deriving typerep]
      end
      in
      check_n (M.typerep_of_t imm_t imm_t imm_t) [ (), (), () ]
    ;;

    let%test _ =
      let module M = struct
        type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd [@@deriving typerep]
      end
      in
      check_n (M.typerep_of_t imm_t imm_t imm_t imm_t) [ (), (), (), () ]
    ;;

    let%test _ =
      let module M = struct
        type ('a, 'b, 'c, 'd, 'e) t = 'a * 'b * 'c * 'd * 'e [@@deriving typerep]
      end
      in
      check_n (M.typerep_of_t imm_t imm_t imm_t imm_t imm_t) [ (), (), (), (), () ]
    ;;

    let%test _ =
      let module M = struct
        type t =
          | Foo
          | Bar of unit
        [@@deriving typerep]
      end
      in
      check_s M.typerep_of_t [ M.Foo; M.Bar () ]
    ;;

    let%test _ =
      let module M = struct
        type t =
          | Foo
          | Bar
          | Baz
        [@@deriving typerep]
      end
      in
      check_a M.typerep_of_t [ M.Foo; M.Bar; M.Baz ]
    ;;

    let%test _ =
      let module M = struct
        type t =
          | Foo of unit
          | Bar of unit
          | Baz of unit
        [@@deriving typerep]
      end
      in
      check_n M.typerep_of_t [ M.Foo (); M.Bar (); M.Baz () ]
    ;;

    let%expect_test _ =
      let module M = struct
        type t = Foo of unit [@@deriving typerep]
      end
      in
      require_self_consistent M.typerep_of_t [ M.Foo () ];
      check_u M.typerep_of_t [ M.Foo () ] |> require_maybe_boxed
    ;;

    let%expect_test _ =
      let module M = struct
        type t = Foo of unit [@@boxed] [@@deriving typerep]
      end
      in
      require_self_consistent M.typerep_of_t [ M.Foo () ];
      check_n M.typerep_of_t [ M.Foo () ] |> require_boxed ~cr:CR_someday;
      check_u M.typerep_of_t [ M.Foo () ] |> require_maybe_boxed;
      [%expect {|
        "Boxed container types should never be immediate"
        |}]
    ;;

    let%expect_test _ =
      let module M = struct
        type t = Foo of unit [@@unboxed] [@@deriving typerep]
      end
      in
      require_self_consistent M.typerep_of_t [ M.Foo () ];
      check_a M.typerep_of_t [ M.Foo () ] |> require_unboxed ~cr:CR_someday;
      check_u M.typerep_of_t [ M.Foo () ] |> require_maybe_boxed;
      [%expect
        {|
        "Unboxed container types should have the immediacy of their contained type"
        |}]
    ;;

    let%expect_test _ =
      let module M = struct
        type t = Foo of string [@@unboxed] [@@deriving typerep]
      end
      in
      require_self_consistent M.typerep_of_t [ M.Foo "foo" ];
      check_n M.typerep_of_t [ M.Foo "foo" ] |> require_unboxed
    ;;

    let%test _ =
      let module M = struct
        type t =
          [ `Foo
          | `Bar of unit
          ]
        [@@deriving typerep]
      end
      in
      check_s M.typerep_of_t [ `Foo; `Bar () ]
    ;;

    let%test _ =
      let module M = struct
        type t =
          [ `Foo
          | `Bar
          | `Baz
          ]
        [@@deriving typerep]
      end
      in
      check_a M.typerep_of_t [ `Foo; `Bar; `Baz ]
    ;;

    let%test _ =
      let module M = struct
        type t =
          [ `Foo of unit
          | `Bar of unit
          | `Baz of unit
          ]
        [@@deriving typerep]
      end
      in
      check_n M.typerep_of_t [ `Foo (); `Bar (); `Baz () ]
    ;;

    let%expect_test _ =
      let module M = struct
        type t = [ `Foo of unit ] [@@deriving typerep]
      end
      in
      require_self_consistent M.typerep_of_t [ `Foo () ];
      check_n M.typerep_of_t [ `Foo () ] |> require_boxed
    ;;

    let%test _ =
      let module M = struct
        type 'a t =
          | Nil
          | Cons of 'a * 'a t
        [@@deriving typerep]
      end
      in
      check_s (M.typerep_of_t typerep_of_int) [ M.Nil; M.Cons (1, M.Cons (2, M.Nil)) ]
    ;;

    let%test _ =
      let module M = struct
        type 'a t =
          | A
          | B of 'a t lazy_t
          | C of 'a * 'a t
        [@@deriving typerep]
      end
      in
      check_s (M.typerep_of_t typerep_of_int) [ M.A; M.B (lazy M.A); M.C (1, M.A) ]
    ;;

    let%test _ =
      let module M = struct
        type 'a t =
          | A
          | B of ('a t * 'a t)
          | C of 'a
        [@@deriving typerep]
      end
      in
      check_s (M.typerep_of_t typerep_of_int) [ M.A; M.B (M.A, M.C 1) ]
    ;;

    let%test _ =
      let module M = struct
        type t = | [@@deriving typerep]
      end
      in
      let (_ : _ Type_immediacy.Always.t) =
        Type_immediacy.Always.of_typerep_exn [%here] M.typerep_of_t
      in
      true
    ;;

    (* Test the [For_all_parameters] generic witness functors. *)
    let%test _ =
      let module M = struct
        type 'a t =
          | A
          | B
          | C
        [@@deriving typerep]
      end
      in
      let module _ = Type_immediacy.Always.For_all_parameters_S1 (M) in
      true
    ;;

    let%test _ =
      let module M = struct
        type 'a t = 'a option lazy_t [@@deriving typerep]
      end
      in
      let module _ = Type_immediacy.Sometimes.For_all_parameters_S1 (M) in
      true
    ;;

    let%test _ =
      let module M = struct
        type 'a t = 'a array lazy_t [@@deriving typerep]
      end
      in
      let module _ = Type_immediacy.Never.For_all_parameters_S1 (M) in
      true
    ;;

    let%expect_test _ =
      let module M = struct
        type 'a t = 'a lazy_t [@@deriving typerep]
      end
      in
      require_does_raise [%here] (fun () ->
        let module _ = Type_immediacy.Sometimes.For_all_parameters_S1 (M) in
        ());
      [%expect
        {|
        (Failure
         "type type_immediacy_witness_unit_tests.ml.t is not independent of its arguments")
        |}];
      require_does_raise [%here] (fun () ->
        let module _ = Type_immediacy.Never.For_all_parameters_S1 (M) in
        ());
      [%expect
        {|
        (Failure
         "type type_immediacy_witness_unit_tests.ml.t is not independent of its arguments")
        |}];
      require_does_raise [%here] (fun () ->
        let module _ = Type_immediacy.Always.For_all_parameters_S1 (M) in
        ());
      [%expect
        {|
        (Failure
         "type type_immediacy_witness_unit_tests.ml.t is not independent of its arguments")
        |}]
    ;;

    let%test _ =
      let module M = struct
        type 'a t = | [@@deriving typerep]
      end
      in
      let module _ = Type_immediacy.Always.For_all_parameters_S1 (M) in
      true
    ;;
  end)
;;
