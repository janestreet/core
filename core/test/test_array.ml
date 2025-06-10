open! Core
open Expect_test_helpers_core
open Array

let ar1 = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]

let%expect_test "slice" =
  let test start stop = print_s [%sexp (slice ar1 start stop : int array)] in
  test 0 0;
  [%expect {| (1 2 3 4 5 6 7 8 9 10) |}];
  test 1 3;
  [%expect {| (2 3) |}];
  test 0 (-1);
  [%expect {| (1 2 3 4 5 6 7 8 9) |}];
  test (-1) 0;
  [%expect {| (10) |}];
  test (-5) (-4);
  [%expect {| (6) |}]
;;

module%test [@name "nget"] _ = struct
  let%expect_test "neg" = require_equal (module Base.Int) (nget ar1 (-3)) 8
  let%expect_test "pos" = require_equal (module Base.Int) (nget ar1 3) ar1.(3)

  let%expect_test "invalid" =
    require_does_raise (fun () : int -> nget ar1 (-100));
    [%expect {| (Invalid_argument "index out of bounds") |}]
  ;;
end

module%test Array_tests = struct
  module Generator = Base_quickcheck.Generator
  module Int = Core.Int
  module Float = Core.Float

  module%template
    [@kind
      k
      = ( bits32
        , bits64
        , word
        , float64
        , value & value
        , (bits64 & bits64) mod external_
        , (bits64 & value) mod external_ )] Test
      (E : sig
         type t : value [@@deriving compare, equal, quickcheck, sexp_of]
         type unboxed : k [@@deriving equal]

         val create_uninitialized : len:int -> unboxed array

         include
           Unboxed_test_harness.Boxable
           [@kind k]
           with type t := unboxed
            and type boxed := t
       end)
      (Arg : sig
         val flambda2 : bool
         val filenames_to_suppress_in_backtraces : string list
       end) =
  struct
    module Harness =
      Unboxed_test_harness.Make [@kind k]
        (struct
          module Boxed = E
          include Boxed

          type t = Boxed.unboxed
        end)
        (Arg)

    let require_equal = Harness.require_equal

    let unbox_array arr =
      let arr' = E.create_uninitialized ~len:(Array.length arr) in
      Array.iteri arr ~f:(fun i a -> Array.set arr' i (E.unbox a));
      arr'
    ;;

    let box_array (arr : E.unboxed array) =
      Array.init (Array.length arr) ~f:(fun i -> E.box (Array.get arr i))
    ;;

    let sexp_of_uarray (arr : E.unboxed array) = [%sexp_of: E.t array] (box_array arr)

    let check_int32_array_against_int32_u_array confirm_test arr_i arr_iu =
      force confirm_test;
      [%equal: E.t array] arr_i (box_array arr_iu)
    ;;

    module Array_pair = struct
      type t =
        { arr_i : E.t array
        ; arr_iu : (E.unboxed array[@sexp.opaque])
        }
      [@@deriving sexp_of]

      let quickcheck_generator =
        let%map.Generator arr = [%generator: E.t array] in
        { arr_i = arr; arr_iu = unbox_array arr }
      ;;

      (* To avoid breaking invariants, don't shrink *)
      let quickcheck_shrinker : t Quickcheck.Shrinker.t = Quickcheck.Shrinker.empty ()
    end

    module In_bounds_array_pair = struct
      type t = int * Array_pair.t [@@deriving sexp_of]

      (* Only generate in-bounds array accesses. *)
      let quickcheck_generator =
        let%bind.Generator arr_pair, len =
          [%generator: Array_pair.t]
          |> Generator.filter_map ~f:(fun arr_pair ->
            let len = Array.length arr_pair.arr_i in
            Option.some_if (len > 0) (arr_pair, len))
        in
        let%map.Generator i = Generator.int_inclusive 0 (len - 1) in
        i, arr_pair
      ;;

      (* To avoid breaking invariants, don't shrink *)
      let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
    end

    let () =
      let confirm_test = lazy (print_endline "testing [sexp_of]") in
      quickcheck_m (module Array_pair) ~f:(fun { arr_i; arr_iu } ->
        force confirm_test;
        require_equal
          ~is_zero_alloc_with_flambda2:false
          [%here]
          (module Sexp)
          (fun () -> Array.sexp_of_t E.sexp_of_t arr_i)
          (fun () -> sexp_of_uarray arr_iu))
    ;;

    let () =
      let confirm_test = lazy (print_endline "testing [length]") in
      quickcheck_m (module Array_pair) ~f:(fun { arr_i; arr_iu } ->
        force confirm_test;
        require_equal
          [%here]
          (module Int)
          (fun () -> Array.length arr_i)
          (fun () -> Array.length arr_iu))
    ;;

    let () =
      let confirm_test = lazy (print_endline "testing [get]") in
      Harness.test_X_to_W
        [%here]
        (fun (i, { Array_pair.arr_i; _ }) ->
          force confirm_test;
          Array.get arr_i i)
        (fun (i, { Array_pair.arr_iu; _ }) -> Array.get arr_iu i)
        (module struct
          type t = int * Array_pair.t
          [@@deriving sexp_of, quickcheck ~generator ~shrinker]
        end)
    ;;

    let () =
      let confirm_test = lazy (print_endline "testing [unsafe_get]") in
      quickcheck_m (module In_bounds_array_pair) ~f:(fun (i, { Array_pair.arr_iu; _ }) ->
        force confirm_test;
        Harness.require_equal_unboxed
          [%here]
          (fun () -> Array.get arr_iu i)
          (fun () -> Array.unsafe_get arr_iu i))
    ;;

    let () =
      let confirm_test = lazy (print_endline "testing [set]") in
      Harness.test_W_X_with_check
        [%here]
        (fun val_ (i, { Array_pair.arr_i; _ }) ->
          Array.set arr_i i val_;
          arr_i)
        (fun val_ (i, { Array_pair.arr_iu; _ }) ->
          Array.set arr_iu i val_;
          arr_iu)
        [%eta2 check_int32_array_against_int32_u_array confirm_test]
        (module struct
          type t = int * Array_pair.t
          [@@deriving sexp_of, quickcheck ~generator ~shrinker]
        end)
        ~sexp_of_output1:[%sexp_of: E.t array]
        ~sexp_of_output2:sexp_of_uarray
    ;;

    let () =
      let confirm_test = lazy (print_endline "testing [unsafe_set]") in
      let open struct
        (* Invariant: the two arrays are equal copies. *)
        type t =
          { i : int
          ; val_ : E.t
          ; for_safe : (E.unboxed Array.t[@sexp.opaque])
          ; for_unsafe : (E.unboxed Array.t[@sexp.opaque])
          }
        [@@deriving sexp_of]
      end in
      quickcheck_m
        (module struct
          type nonrec t = t [@@deriving sexp_of]

          let quickcheck_generator =
            let%map.Generator i, { arr_iu; arr_i } = [%generator: In_bounds_array_pair.t]
            and val_ = [%generator: E.t] in
            { i; val_; for_safe = arr_iu; for_unsafe = unbox_array arr_i }
          ;;

          (* To avoid breaking invariants, don't shrink *)
          let quickcheck_shrinker : t Quickcheck.Shrinker.t = Quickcheck.Shrinker.empty ()
        end)
        ~f:(fun { i; val_; for_safe; for_unsafe } ->
          force confirm_test;
          let val_ = E.unbox val_ in
          require_equal
            [%here]
            (module struct
              type t = E.unboxed Array.t

              let sexp_of_t = sexp_of_uarray
              let equal t1 t2 = [%equal: E.t array] (box_array t1) (box_array t2)
            end)
            (fun () ->
              Array.set for_safe i val_;
              for_safe)
            (fun () ->
              Array.unsafe_set for_unsafe i val_;
              for_safe))
    ;;

    let () =
      let confirm_test = lazy (print_endline "testing [magic_create_uninitialized]") in
      quickcheck_m
        (module struct
          type t = E.t array [@@deriving sexp_of, quickcheck]
        end)
        ~f:(fun arr ->
          Expect_test_helpers_core.require_does_not_raise (fun () ->
            let len = Array.length arr in
            let arr_u = E.create_uninitialized ~len in
            Array.iteri arr ~f:(fun i val_ ->
              force confirm_test;
              (* Check that we can index into the array *)
              let _ : E.unboxed = Array.get arr_u i in
              (* Check that write/read round-trips *)
              Array.set arr_u i (E.unbox val_);
              Harness.require_equal_wrapped
                [%here]
                (fun () -> val_)
                (fun () -> Array.get arr_u i))))
    ;;
  end

  module _ = struct
    module Test_int32 = Test [@kind bits32] (struct
        include Int32_u
        include Int32

        type unboxed = Int32_u.t [@@deriving equal]

        let create_uninitialized = Core.Array.magic_create_uninitialized
      end)

    let%expect_test "int32#" =
      let module _ =
        Test_int32 (struct
          let flambda2 = false
          let filenames_to_suppress_in_backtraces = [ "int32.ml"; "int32_u.ml" ]
        end)
      in
      [%expect
        {|
        testing [sexp_of]
        testing [length]
        testing [get]
        testing [unsafe_get]
        testing [set]
        testing [unsafe_set]
        testing [magic_create_uninitialized]
        |}]
    ;;

    let%expect_test ("int32# (alloc)" [@tags "fast-flambda2"]) =
      let module _ =
        Test_int32 (struct
          let flambda2 = true
          let filenames_to_suppress_in_backtraces = [ "int32.ml"; "int32_u.ml" ]
        end)
      in
      [%expect
        {|
        testing [sexp_of]
        testing [length]
        testing [get]
        testing [unsafe_get]
        testing [set]
        testing [unsafe_set]
        testing [magic_create_uninitialized]
        |}]
    ;;

    module Test_int64 = Test [@kind bits64] (struct
        include Int64_u
        include Int64

        type unboxed = Int64_u.t [@@deriving equal]

        let create_uninitialized = Core.Array.magic_create_uninitialized
      end)

    let%expect_test "int64#" =
      let module _ =
        Test_int64 (struct
          let flambda2 = false
          let filenames_to_suppress_in_backtraces = [ "int64.ml"; "int64_u.ml" ]
        end)
      in
      [%expect
        {|
        testing [sexp_of]
        testing [length]
        testing [get]
        testing [unsafe_get]
        testing [set]
        testing [unsafe_set]
        testing [magic_create_uninitialized]
        |}]
    ;;

    let%expect_test ("int64# (alloc)" [@tags "fast-flambda2"]) =
      let module _ =
        Test_int64 (struct
          let flambda2 = true
          let filenames_to_suppress_in_backtraces = [ "int64.ml"; "int64_u.ml" ]
        end)
      in
      [%expect
        {|
        testing [sexp_of]
        testing [length]
        testing [get]
        testing [unsafe_get]
        testing [set]
        testing [unsafe_set]
        testing [magic_create_uninitialized]
        |}]
    ;;

    module Test_nativeint = Test [@kind word] (struct
        include Nativeint_u
        include Nativeint

        type unboxed = Nativeint_u.t [@@deriving equal]

        let create_uninitialized = Core.Array.magic_create_uninitialized
      end)

    let%expect_test "nativeint#" =
      let module _ =
        Test_nativeint (struct
          let flambda2 = false
          let filenames_to_suppress_in_backtraces = [ "nativeint.ml"; "nativeint_u.ml" ]
        end)
      in
      [%expect
        {|
        testing [sexp_of]
        testing [length]
        testing [get]
        testing [unsafe_get]
        testing [set]
        testing [unsafe_set]
        testing [magic_create_uninitialized]
        |}]
    ;;

    let%expect_test ("nativeint# (alloc)" [@tags "fast-flambda2"]) =
      let module _ =
        Test_nativeint (struct
          let flambda2 = true
          let filenames_to_suppress_in_backtraces = [ "nativeint.ml"; "nativeint_u.ml" ]
        end)
      in
      [%expect
        {|
        testing [sexp_of]
        testing [length]
        testing [get]
        testing [unsafe_get]
        testing [set]
        testing [unsafe_set]
        testing [magic_create_uninitialized]
        |}]
    ;;

    module Test_float = Test [@kind float64] (struct
        include Float_u
        include Float

        type unboxed = Float_u.t

        let create_uninitialized = Core.Array.magic_create_uninitialized
        let equal_unboxed = [%compare.equal: Float_u.t]
        let equal = [%compare.equal: Float.t]
      end)

    let%expect_test "float#" =
      let module _ =
        Test_float (struct
          let flambda2 = false
          let filenames_to_suppress_in_backtraces = [ "float.ml"; "float_u.ml" ]
        end)
      in
      [%expect
        {|
        testing [sexp_of]
        testing [length]
        testing [get]
        testing [unsafe_get]
        testing [set]
        testing [unsafe_set]
        testing [magic_create_uninitialized]
        |}]
    ;;

    let%expect_test ("float# (alloc)" [@tags "fast-flambda2"]) =
      let module _ =
        Test_float (struct
          let flambda2 = true
          let filenames_to_suppress_in_backtraces = [ "float.ml"; "float_u.ml" ]
        end)
      in
      [%expect
        {|
        testing [sexp_of]
        testing [length]
        testing [get]
        testing [unsafe_get]
        testing [set]
        testing [unsafe_set]
        testing [magic_create_uninitialized]
        |}]
    ;;

    module Test_int64_u_pair = Test [@kind (bits64 & bits64) mod external_] (struct
        type t = int64 * int64 [@@deriving compare, equal, quickcheck, sexp_of]
        type unboxed = #(int64# * int64#)

        let create_uninitialized : len:int -> unboxed array =
          Core.Array.magic_create_uninitialized
        ;;

        let equal_unboxed #(x1, y1) #(x2, y2) =
          [%equal: Int64_u.t] x1 x2 && [%equal: Int64_u.t] y1 y2
        ;;

        let box #(x, y) = Int64_u.to_int64 x, Int64_u.to_int64 y
        let unbox (x, y) = #(Int64_u.of_int64 x, Int64_u.of_int64 y)
      end)

    let%expect_test "#(int64# * int64#)" =
      let module _ =
        Test_int64_u_pair (struct
          let flambda2 = false
          let filenames_to_suppress_in_backtraces = [ "test_array.ml" ]
        end)
      in
      [%expect
        {|
        testing [sexp_of]
        testing [length]
        testing [get]
        testing [unsafe_get]
        testing [set]
        testing [unsafe_set]
        testing [magic_create_uninitialized]
        |}]
    ;;

    let%expect_test ("#(int64# * int64#) (alloc)" [@tags "fast-flambda2"]) =
      let module _ =
        Test_int64_u_pair (struct
          let flambda2 = true
          let filenames_to_suppress_in_backtraces = [ "test_array.ml" ]
        end)
      in
      [%expect
        {|
        testing [sexp_of]
        testing [length]
        testing [get]
        testing [unsafe_get]
        testing [set]
        testing [unsafe_set]
        testing [magic_create_uninitialized]
        |}]
    ;;
  end

  module Test_int_pair = Test [@kind value & value] (struct
      type t = int * int [@@deriving compare, equal, quickcheck, sexp_of]
      type unboxed = #(int * int)

      (* We can't actually make uninitialized [int]s, so initialize with 0 *)
      let create_uninitialized ~len = Array.create ~len #(0, 0)

      let equal_unboxed #(x1, y1) #(x2, y2) =
        [%equal: Int.t] x1 x2 && [%equal: Int.t] y1 y2
      ;;

      let box #(x, y) = x, y
      let unbox (x, y) = #(x, y)
    end)

  let%expect_test "#(int * int)" =
    let module _ =
      Test_int_pair (struct
        let flambda2 = false
        let filenames_to_suppress_in_backtraces = [ "test_array.ml" ]
      end)
    in
    [%expect
      {|
      testing [sexp_of]
      testing [length]
      testing [get]
      testing [unsafe_get]
      testing [set]
      testing [unsafe_set]
      testing [magic_create_uninitialized]
      |}]
  ;;

  let%expect_test ("#(int * int) (alloc)" [@tags "fast-flambda2"]) =
    let module _ =
      Test_int_pair (struct
        let flambda2 = true
        let filenames_to_suppress_in_backtraces = [ "test_array.ml" ]
      end)
    in
    [%expect
      {|
      testing [sexp_of]
      testing [length]
      testing [get]
      testing [unsafe_get]
      testing [set]
      testing [unsafe_set]
      testing [magic_create_uninitialized]
      |}]
  ;;

  module Test_int64_u_int_pair = Test [@kind (bits64 & value) mod external_] (struct
      type t = int64 * int [@@deriving compare, equal, quickcheck, sexp_of]
      type unboxed = #(int64# * int)

      (* We can't actually make uninitialized [int]s, so initialize with 0 *)
      let create_uninitialized ~len = Array.create ~len #(#0L, 0)

      let equal_unboxed #(x1, y1) #(x2, y2) =
        [%equal: Int64_u.t] x1 x2 && [%equal: Int.t] y1 y2
      ;;

      let box #(x, y) = Int64_u.to_int64 x, y
      let unbox (x, y) = #(Int64_u.of_int64 x, y)
    end)

  let%expect_test "#(int64# * int)" =
    let module _ =
      Test_int64_u_int_pair (struct
        let flambda2 = false
        let filenames_to_suppress_in_backtraces = [ "test_array.ml" ]
      end)
    in
    [%expect
      {|
      testing [sexp_of]
      testing [length]
      testing [get]
      testing [unsafe_get]
      testing [set]
      testing [unsafe_set]
      testing [magic_create_uninitialized]
      |}]
  ;;

  let%expect_test ("#(int64# * int) (alloc)" [@tags "fast-flambda2"]) =
    let module _ =
      Test_int64_u_int_pair (struct
        let flambda2 = true
        let filenames_to_suppress_in_backtraces = [ "test_array.ml" ]
      end)
    in
    [%expect
      {|
      testing [sexp_of]
      testing [length]
      testing [get]
      testing [unsafe_get]
      testing [set]
      testing [unsafe_set]
      testing [magic_create_uninitialized]
      |}]
  ;;
end
