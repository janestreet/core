open! Core
open Expect_test_helpers_core
module Sexpable = Sexpable.Stable
module Binable = Binable.Stable

let%test_module _ =
  (module struct
    let old_style = !Sexp.of_int_style
    let () = Sexp.of_int_style := `No_underscores

    let int_tests =
      [ ~-1, "-1", "\002-1"; 0, "0", "\0010"; 1, "1", "\0011" ]
      @
      if Sys.word_size_in_bits = 64
      then
        [ Int.min_value, "-4611686018427387904", "\020-4611686018427387904"
        ; Int.min_value / 2, "-2305843009213693952", "\020-2305843009213693952"
        ; Int.max_value / 2, "2305843009213693951", "\0192305843009213693951"
        ; Int.max_value, "4611686018427387903", "\0194611686018427387903"
        ]
      else []
    ;;

    (* S0 *)
    module _ = Stable_unit_test.Make (struct
      module T = struct
        type t = int [@@deriving compare]

        include
          Sexpable.Of_sexpable.V1
            (String)
            (struct
              type t = int

              let to_sexpable = string_of_int
              let of_sexpable = int_of_string
            end)

        include
          Binable.Of_binable.V1 [@alert "-legacy"]
            (String)
            (struct
              type t = int

              let to_binable = string_of_int
              let of_binable = int_of_string
            end)
      end

      type t = T.t [@@deriving bin_io, compare, sexp]

      let equal a b = Int.( = ) 0 ([%compare: t] a b)
      let tests = int_tests
    end)

    module _ = Stable_unit_test.Make (struct
      module T = struct
        type 'a t = 'a option [@@deriving compare]

        include
          Sexpable.Of_sexpable1.V1
            (List)
            (struct
              type 'a t = 'a option

              let to_sexpable = Option.to_list
              let of_sexpable = List.hd
            end)

        include
          Binable.Of_binable1.V1 [@alert "-legacy"]
            (List)
            (struct
              type 'a t = 'a option

              let to_binable = Option.to_list
              let of_binable = List.hd
            end)
      end

      type t = int T.t [@@deriving bin_io, compare, sexp]

      let equal a b = Int.( = ) 0 ([%compare: t] a b)

      let tests =
        [ None, "()", "\000"
        ; Some 1, "(1)", "\001\001"
        ; Some 1_000, "(1000)", "\001\254\232\003"
        ; Some ~-1, "(-1)", "\001\255\255"
        ]
      ;;
    end)

    module _ = Stable_unit_test.Make (struct
      module T = struct
        type ('a, 'b) t = ('a, 'b) Either.Stable.V1.t [@@deriving compare]

        module Format = struct
          type ('a, 'b) t =
            | Left of 'a
            | Right of 'b
          [@@deriving bin_io, sexp]

          let of_t = function
            | First x -> Left x
            | Second x -> Right x
          ;;

          let to_t = function
            | Left x -> First x
            | Right x -> Second x
          ;;
        end

        include
          Sexpable.Of_sexpable2.V1
            (Format)
            (struct
              type nonrec ('a, 'b) t = ('a, 'b) t

              let to_sexpable = Format.of_t
              let of_sexpable = Format.to_t
            end)

        include
          Binable.Of_binable2.V1 [@alert "-legacy"]
            (Format)
            (struct
              type nonrec ('a, 'b) t = ('a, 'b) t

              let to_binable = Format.of_t
              let of_binable = Format.to_t
            end)
      end

      type t = (int, string) T.t [@@deriving bin_io, compare, sexp]

      let equal a b = Int.( = ) 0 ([%compare: t] a b)

      let tests =
        [ First 1, "(Left 1)", "\000\001"
        ; First 0, "(Left 0)", "\000\000"
        ; Second "", "(Right \"\")", "\001\000"
        ; Second "second", "(Right second)", "\001\006second"
        ]
        @
        if Sys.word_size_in_bits = 64
        then
          [ ( First Int.min_value
            , "(Left -4611686018427387904)"
            , "\000\252\000\000\000\000\000\000\000\192" )
          ]
        else []
      ;;
    end)

    let%test_module "Of_stringable" =
      (module struct
        module M = struct
          type t = int [@@deriving quickcheck]

          include Sexpable.Of_stringable.V1 (Int)
          include Binable.Of_stringable.V1 [@alert "-legacy"] (Int)
        end

        include Stable_unit_test.Make (struct
          include M

          let equal = Poly.( = )
          let tests = int_tests
        end)

        let%expect_test "validate sexp grammar" =
          require_ok [%here] (Sexp_grammar_validation.validate_grammar (module M));
          [%expect {| String |}]
        ;;
      end)
    ;;

    module _ = struct
      module T = struct
        type t = int [@@deriving bin_io, sexp]
      end

      include Sexpable.To_stringable.V1 (T)

      let%test_unit _ =
        List.iter int_tests ~f:(fun (t, s, _) ->
          [%test_eq: int] t (of_string s);
          [%test_eq: string] (to_string t) s)
      ;;
    end

    let () = Sexp.of_int_style := old_style
  end)
;;
