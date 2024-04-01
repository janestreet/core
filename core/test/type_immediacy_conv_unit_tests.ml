(* Tests checking the correctness of conversions between values and ints. *)

open! Core

let%test_module _ =
  (module struct
    module Typerep_obj = Typerep_lib.Std.Typerep_obj

    type 'a t =
      { int_as_value : int -> 'a option
      ; int_as_value_exn : int -> 'a
      ; int_is_value : int -> bool
      ; value_as_int : 'a -> int option
      ; value_as_int_exn : 'a -> int
      ; value_is_int : 'a -> bool
      }

    let get_conv typerep_of_t =
      let immediacy = Type_immediacy.of_typerep typerep_of_t in
      let int_as_value i = Type_immediacy.int_as_value immediacy i in
      let int_as_value_exn i = Type_immediacy.int_as_value_exn immediacy i in
      let int_is_value i = Type_immediacy.int_is_value immediacy i in
      let value_as_int a = Type_immediacy.value_as_int immediacy a in
      let value_as_int_exn a = Type_immediacy.value_as_int_exn immediacy a in
      let value_is_int a = Type_immediacy.value_is_int immediacy a in
      { int_as_value
      ; int_as_value_exn
      ; int_is_value
      ; value_as_int
      ; value_as_int_exn
      ; value_is_int
      }
    ;;

    (*In the following, the three cases for values and two for ints that are discuessed are
      described in [./type_immediacy.mli].

      We merge case 1) and I), as they are really the same, and provide the following 4
      test cases:

      | Convertible of 'a * int (* Corresponds to cases 1) and I) *)

      For [Convertible (v,i)], we test the conversion invariants mentioned in 1), i.e. we
      a) check that [v] is immediate, and that it's underlying [int] representation is [i]
      b) check that [v] converts to [i], using [value_is_int], and [value_as_int(_exn)?]
      c) check that [i] converts to [v], using [int_is_value], and [int_as_value(_exn)?]

      | Val_does_not_convert of 'a (* Corresponds to case 3) *)

      For [Val_does_not_convert v], we test that
      a) [v] is boxed
      b) the conversion functions return the appropriate results as described in 3)

      | Int_does_not_convert of 'a (* Corresponds to case II) *)

      For [Int_does_not_convert i], we test that the conversion functions return the
      appropriate results as described in II)

      | Boxed_value_converts of ('a lazy_t * 'a * int) (* Corresponds to case 2) *)

      Since case 2) is only relevant if the toplevel type is [lazy_t], we have specialized
      this case.

      For [Boxed_value_converts (lazy_v,v,i)], we test that

      a) [lazy_v] is boxed.
      b) [lazy_v] converts to [i] when the conversion functions are called.
      c) [Lazy.force lazy_v == Lazy.force v]
      d) If we test [Convertible (v,i)], the test passes.
      (We also implicitly have the property that [Lazy.force lazy_v] is immediate and
      internally represented by [i], which follows from b), c) and d) above. )

      The following functions provide tests for the four cases.
    *)

    let is_immediate (type a) (v : a) : bool = Obj.is_int (Obj.repr v)

    (* Raises an exception if [v] is not immediate, just as a sanity check. *)
    let to_int (type a) (v : a) : int =
      if is_immediate v
      then (Obj.magic (v : a) : int)
      else failwith "Can't convert a boxed value to an int."
    ;;

    (* Does not take a list but a single argument only. *)
    let test_convertible_single (type a) (conv : a t) ((v, i) : a * int) : bool =
      to_int v = i
      (* Contains check for v being immediate. *)
      && conv.value_is_int v
      && conv.int_is_value i
      && i = conv.value_as_int_exn v
      && phys_equal v (conv.int_as_value_exn i)
      && (match conv.value_as_int v with
          | Some i' -> i = i'
          | None -> false)
      &&
      match conv.int_as_value i with
      | Some v' -> phys_equal v v'
      | None -> false
    ;;

    let test_convertibles (type a) (conv : a t) (test_cases : (a * int) list) : bool =
      List.for_all test_cases ~f:(test_convertible_single conv)
    ;;

    let test_inconvertible_values (type a) (conv : a t) (test_cases : a list) : bool =
      List.for_all test_cases ~f:(fun v ->
        (not (is_immediate v))
        && (not (conv.value_is_int v))
        && (match conv.value_as_int v with
            | Some _ -> false
            | None -> true)
        && Exn.does_raise (fun () -> conv.value_as_int_exn v))
    ;;

    let test_inconvertible_ints (type a) (conv : a t) (test_cases : int list) : bool =
      List.for_all test_cases ~f:(fun i ->
        (not (conv.int_is_value i))
        && (match conv.int_as_value i with
            | Some _ -> false
            | None -> true)
        && Exn.does_raise (fun () -> conv.int_as_value_exn i))
    ;;

    (* Recurring conversion patterns: *)
    let no_int_but_zero_converts = [ 1; 2; 3; -1; Int.max_value; Int.min_value ]
    let no_int_converts = 0 :: no_int_but_zero_converts

    module type S = sig
      type t [@@deriving typerep]
    end

    let is_never (type a) (module M : S with type t = a) examples =
      let conv = get_conv M.typerep_of_t in
      test_inconvertible_values conv examples
      && test_inconvertible_ints conv no_int_converts
    ;;

    let%test (_ [@tags "no-js"]) =
      is_never
        (module struct
          type t = int32 [@@deriving typerep]
        end)
        [ Int32.zero
        ; Int32.one
        ; Int32.minus_one
        ; Int32.max_value
        ; Int32.min_value
        ; Int32.of_int_exn 328121
        ]
    ;;

    let%test _ =
      is_never
        (module struct
          type t = int64 [@@deriving typerep]
        end)
        [ Int64.zero; Int64.one; Int64.minus_one; Int64.max_value; Int64.min_value ]
    ;;

    let%test (_ [@tags "no-js"]) =
      is_never
        (module struct
          type t = nativeint [@@deriving typerep]
        end)
        [ Nativeint.zero
        ; Nativeint.one
        ; Nativeint.minus_one
        ; Nativeint.max_value
        ; Nativeint.min_value
        ]
    ;;

    let%test _ =
      let module M = struct
        type t = char [@@deriving typerep]
      end
      in
      let conv = get_conv M.typerep_of_t in
      test_convertibles
        conv
        [ 'a', 97
        ; 'A', 65
        ; '0', 48
        ; '9', 57
        ; '\t', 9
        ; '\n', 10
        ; Char.of_int_exn 0, 0
        ; Char.of_int_exn 127, 127
        ]
      && test_inconvertible_ints conv [ 256; -1; 500; Int.max_value; Int.min_value ]
    ;;

    let%test (_ [@tags "no-js"]) =
      is_never
        (module struct
          type t = float [@@deriving typerep]
        end)
        [ 1.1; 0.0; -3.3; Float.nan; Float.infinity; Float.neg_infinity ]
    ;;

    let%test _ =
      is_never
        (module struct
          type t = string [@@deriving typerep]
        end)
        [ ""; "Hello world!" ]
    ;;

    let%test _ =
      let module M = struct
        type t = bool [@@deriving typerep]
      end
      in
      let conv = get_conv M.typerep_of_t in
      test_convertibles conv [ true, 1; false, 0 ]
      && test_inconvertible_ints conv [ 2; 3; -1; -2; 50; Int.max_value; Int.min_value ]
    ;;

    let%test _ =
      let module M = struct
        type t = unit [@@deriving typerep]
      end
      in
      let conv = get_conv M.typerep_of_t in
      test_convertibles conv [ (), 0 ]
      && test_inconvertible_ints conv no_int_but_zero_converts
    ;;

    let%test _ =
      let module M = struct
        type t = unit option [@@deriving typerep]
      end
      in
      let conv = get_conv M.typerep_of_t in
      test_convertibles conv [ None, 0 ]
      && test_inconvertible_ints conv no_int_but_zero_converts
      && test_inconvertible_values conv [ Some () ]
    ;;

    let%test _ =
      let module M = struct
        type t = int ref option [@@deriving typerep]
      end
      in
      let conv = get_conv M.typerep_of_t in
      test_convertibles conv [ None, 0 ]
      && test_inconvertible_ints conv no_int_but_zero_converts
      && test_inconvertible_values conv [ Some (ref 0); Some (ref 1) ]
    ;;

    let%test _ =
      let module M = struct
        type t = int ref list [@@deriving typerep]
      end
      in
      let conv = get_conv M.typerep_of_t in
      test_convertibles conv [ [], 0 ]
      && test_inconvertible_ints conv no_int_but_zero_converts
      && test_inconvertible_values conv [ [ ref 0 ]; [ ref 1 ] ]
    ;;

    let%test _ =
      let module M = struct
        type t = unit list [@@deriving typerep]
      end
      in
      let conv = get_conv M.typerep_of_t in
      test_convertibles conv [ [], 0 ]
      && test_inconvertible_ints conv no_int_but_zero_converts
      && test_inconvertible_values conv [ [ () ] ]
    ;;

    let%test _ =
      is_never
        (module struct
          type t = int ref array [@@deriving typerep]
        end)
        [ [||]; [| ref 1 |] ]
    ;;

    let%test _ =
      is_never
        (module struct
          type t = unit array [@@deriving typerep]
        end)
        [ [||]; [| () |]; [| (); () |] ]
    ;;

    let%test _ =
      is_never
        (module struct
          type t = unit ref [@@deriving typerep]
        end)
        [ ref () ]
    ;;

    let%test _ =
      is_never
        (module struct
          type t = int ref [@@deriving typerep]
        end)
        [ ref 1; ref 2; ref (-1) ]
    ;;

    let%test_module _ =
      (module struct
        type 'a t = 'a lazy_t [@@deriving typerep]

        let%test _ =
          (* type t = unit lazy_t *)
          let conv = get_conv (typerep_of_t typerep_of_unit) in
          test_inconvertible_values conv [ lazy ((fun () -> ()) ()) ]
          && test_convertibles conv [ Lazy.from_val (), 0 ]
          && test_inconvertible_ints conv no_int_but_zero_converts
        ;;

        let%test _ =
          (* type t = int lazy_t *)
          let conv = get_conv (typerep_of_t typerep_of_int) in
          test_convertibles
            conv
            [ lazy 0, 0; lazy (-1), -1; Lazy.from_val Int.max_value, Int.max_value ]
          && test_inconvertible_values conv [ lazy (1 + 2); lazy (List.length []) ]
        ;;

        (* depending on when the Gc runs, some lazy after being forced might be
           replaced by their immediate value.  *)

        let%test _ =
          (* type t = int list lazy_t *)
          let conv = get_conv (typerep_of_t (typerep_of_list typerep_of_int)) in
          (* Define some lazy lists. *)
          let l1 = lazy (List.filter [ 1; 2; 3; 4 ] ~f:(fun _ -> false)) in
          let l2 = lazy (List.filter [] ~f:(fun _ -> true)) in
          let l3 = lazy (List.filter [ 1 ] ~f:(fun _ -> true)) in
          let empty = Lazy.from_val [] in
          test_convertibles conv [ empty, 0 ]
          && test_inconvertible_values conv [ Lazy.from_val [ 1 ]; l1; l2; l3 ]
          && test_inconvertible_ints conv no_int_but_zero_converts
        ;;

        let%test _ =
          let conv = get_conv (typerep_of_t (typerep_of_t typerep_of_int)) in
          let l1 = lazy (lazy (1 + 2)) in
          test_convertibles conv [ Lazy.from_val (Lazy.from_val 3), 3 ]
          && test_inconvertible_values conv [ l1 ]
        ;;
      end)
    ;;

    let%test _ =
      let module M = struct
        type t = { foo : unit } [@@deriving typerep]
      end
      in
      is_never (module M) [ { M.foo = () } ]
    ;;

    let%expect_test _ =
      let module M = struct
        type t = { foo : unit } [@@unboxed] [@@deriving typerep]
      end
      in
      let conv = get_conv M.typerep_of_t in
      Expect_test_helpers_core.require
        [%here]
        ~cr:CR_someday
        (test_convertibles conv [ { M.foo = () }, 0 ])
        ~if_false_then_print_s:(lazy [%message "zero should convert"]);
      Expect_test_helpers_core.require
        [%here]
        (test_inconvertible_ints conv no_int_but_zero_converts)
        ~if_false_then_print_s:(lazy [%message "non-zero should not convert"]);
      [%expect {|
        "zero should convert"
        |}]
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
      is_never (module M) [ { M.foo = (); bar = () } ]
    ;;

    let%test _ =
      let module M = struct
        type t = { foo : string } [@@unboxed] [@@deriving typerep]
      end
      in
      is_never (module M) [ { M.foo = "foo" } ]
    ;;

    let%test _ =
      is_never
        (module struct
          type t = unit * unit [@@deriving typerep]
        end)
        [ (), () ]
    ;;

    let%test _ =
      is_never
        (module struct
          type t = unit * unit * unit [@@deriving typerep]
        end)
        [ (), (), () ]
    ;;

    let%test _ =
      is_never
        (module struct
          type t = unit * unit * unit * unit [@@deriving typerep]
        end)
        [ (), (), (), () ]
    ;;

    let%test _ =
      is_never
        (module struct
          type t = unit * unit * unit * unit * unit [@@deriving typerep]
        end)
        [ (), (), (), (), () ]
    ;;

    let%expect_test _ =
      let module M = struct
        type t = Foo of unit [@@unboxed] [@@deriving typerep]
      end
      in
      let conv = get_conv M.typerep_of_t in
      Expect_test_helpers_core.require
        [%here]
        ~cr:CR_someday
        (test_convertibles conv [ M.Foo (), 0 ])
        ~if_false_then_print_s:(lazy [%message "zero should convert"]);
      Expect_test_helpers_core.require
        [%here]
        (test_inconvertible_ints conv no_int_but_zero_converts)
        ~if_false_then_print_s:(lazy [%message "non-zero should not convert"]);
      [%expect {|
        "zero should convert"
        |}]
    ;;

    let%test _ =
      let module M = struct
        type t =
          | Foo
          | Bar of unit
        [@@deriving typerep]
      end
      in
      let conv = get_conv M.typerep_of_t in
      test_convertibles conv [ M.Foo, 0 ]
      && test_inconvertible_values conv [ M.Bar () ]
      && test_inconvertible_ints conv no_int_but_zero_converts
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
      let conv = get_conv M.typerep_of_t in
      test_convertibles conv [ M.Foo, 0; M.Bar, 1; M.Baz, 2 ]
      && test_inconvertible_ints
           conv
           [ 3; 4; 5; 500; 232; -1; -2; Int.max_value; Int.min_value ]
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
      is_never (module M) [ M.Foo (); M.Bar (); M.Baz () ]
    ;;

    let filter_variants variants list =
      let variants =
        List.filter_map variants ~f:(fun v ->
          if Obj.is_int (Obj.repr v) then Some (Obj.magic (v : [> ]) : int) else None)
      in
      List.filter list ~f:(fun int ->
        List.for_all variants ~f:(fun variant -> variant <> int))
    ;;

    let%test _ =
      let module M = struct
        type t =
          [ `Foo
          | `Bar of unit
          ]
        [@@deriving typerep, enumerate]
      end
      in
      let conv = get_conv M.typerep_of_t in
      test_convertibles conv [ `Foo, Typerep_obj.repr_of_poly_variant `Foo ]
      && test_inconvertible_values conv [ `Bar () ]
      && test_inconvertible_ints
           conv
           (filter_variants
              M.all
              [ 0
              ; 1
              ; 2
              ; 3
              ; -1
              ; -2
              ; 563
              ; Int.max_value
              ; Int.min_value
              ; Typerep_obj.repr_of_poly_variant `Bar
              ])
    ;;

    let%test _ =
      let module M = struct
        type t =
          [ `Foo
          | `Bar
          | `Baz
          ]
        [@@deriving typerep, enumerate]
      end
      in
      let conv = get_conv M.typerep_of_t in
      test_convertibles
        conv
        [ `Foo, Typerep_obj.repr_of_poly_variant `Foo
        ; `Bar, Typerep_obj.repr_of_poly_variant `Bar
        ; `Baz, Typerep_obj.repr_of_poly_variant `Baz
        ]
      && test_inconvertible_ints
           conv
           (filter_variants
              M.all
              [ 0; 1; 2; 3; -1; -2; 563; Int.max_value; Int.min_value ])
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
      let conv = get_conv M.typerep_of_t in
      test_inconvertible_values conv [ `Foo (); `Bar (); `Baz () ]
      && test_inconvertible_ints
           conv
           ([ Typerep_obj.repr_of_poly_variant `Foo
            ; Typerep_obj.repr_of_poly_variant `Bar
            ; Typerep_obj.repr_of_poly_variant `Baz
            ]
            @ no_int_converts)
    ;;

    let%test _ =
      let module M = struct
        type 'a t =
          | Nil
          | Cons of 'a * 'a t
        [@@deriving typerep]
      end
      in
      let conv = get_conv (M.typerep_of_t typerep_of_unit) in
      test_convertibles conv [ M.Nil, 0 ]
      && test_inconvertible_values
           conv
           [ M.Cons ((), M.Nil); M.Cons ((), M.Cons ((), M.Nil)) ]
      && test_inconvertible_ints conv no_int_but_zero_converts
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
      let conv = get_conv (M.typerep_of_t typerep_of_int) in
      test_convertibles conv [ M.A, 0 ]
      && test_inconvertible_values conv [ M.B (lazy M.A); M.C (1, M.A) ]
      && test_inconvertible_ints conv no_int_but_zero_converts
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
      let conv = get_conv (M.typerep_of_t typerep_of_unit) in
      test_convertibles conv [ M.A, 0 ]
      && test_inconvertible_values conv [ M.B (M.A, M.A); M.C () ]
      && test_inconvertible_ints conv no_int_but_zero_converts
    ;;

    let%test _ =
      let module M = struct
        type t = | [@@deriving typerep]
      end
      in
      let conv = get_conv M.typerep_of_t in
      test_inconvertible_ints conv no_int_converts
    ;;

    let%test _ =
      let module M = struct
        type 'a t = | [@@deriving typerep]
      end
      in
      let conv = get_conv (M.typerep_of_t typerep_of_unit) in
      test_inconvertible_ints conv no_int_converts
    ;;
  end)
;;
