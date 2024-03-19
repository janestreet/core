open! Core
open! Expect_test_helpers_core

let%expect_test "[sexp_of_int] respects [sexp_of_int_style]" =
  let sexp_of_int = Core.Core_stable.sexp_of_int in
  let r = Int_conversions.sexp_of_int_style in
  let old = !r in
  r := `Underscores;
  print_s [%sexp (1234 : int)];
  [%expect {| 1_234 |}];
  r := `No_underscores;
  print_s [%sexp (1234 : int)];
  [%expect {| 1234 |}];
  r := old
;;

let%expect_test "older [int_of_sexp] supports both [sexp_of_int_style]s" =
  let sexp_of_int = Core.Core_stable.sexp_of_int in
  let int_of_sexp = Sexplib.Std.int_of_sexp in
  let print () = print_s [%sexp (int_of_sexp (sexp_of_int 1234) : int)] in
  let r = Int_conversions.sexp_of_int_style in
  let old = !r in
  r := `Underscores;
  print ();
  [%expect {| 1_234 |}];
  r := `No_underscores;
  print ();
  [%expect {| 1234 |}];
  r := old
;;

module _ = struct
  let%test_module "Hashtbl.V1" =
    (module Stable_unit_test.Make_unordered_container (struct
      module Hashable = Core_stable.Hashable.V1.Make (Int)
      module Table = Hashable.Table

      type t = string Table.t [@@deriving sexp, bin_io]

      let equal t1 t2 = Int.Table.equal String.equal t1 t2
      let triple_table = Int.Table.of_alist_exn ~size:16 [ 1, "foo"; 2, "bar"; 3, "baz" ]
      let single_table = Int.Table.of_alist_exn [ 0, "foo" ]

      module Test = Stable_unit_test.Unordered_container_test

      let tests =
        [ ( triple_table
          , { Test.sexps = [ "(1 foo)"; "(2 bar)"; "(3 baz)" ]
            ; bin_io_header = "\003"
            ; bin_io_elements = [ "\001\003foo"; "\002\003bar"; "\003\003baz" ]
            } )
        ; ( Int.Table.create ()
          , { Test.sexps = []; bin_io_header = "\000"; bin_io_elements = [] } )
        ; ( single_table
          , { Test.sexps = [ "(0 foo)" ]
            ; bin_io_header = "\001"
            ; bin_io_elements = [ "\000\003foo" ]
            } )
        ]
      ;;
    end))
  ;;
end

let%test_module "Hash_set.V1" =
  (module Stable_unit_test.Make_unordered_container (struct
    module Hashable = Core_stable.Hashable.V1.Make (Int)
    include Hashable.Hash_set

    let equal = Hash_set.equal
    let int_list = List.init 10 ~f:Fn.id
    let ten_set = Int.Hash_set.of_list ~size:32 int_list
    let single_set = Int.Hash_set.of_list [ 0 ]

    module Test = Stable_unit_test.Unordered_container_test

    let tests =
      [ ( ten_set
        , { Test.sexps = List.init 10 ~f:Int.to_string
          ; bin_io_header = "\010"
          ; bin_io_elements =
              List.init 10 ~f:(fun n -> Char.to_string (Char.of_int_exn n))
          } )
      ; ( Int.Hash_set.create ()
        , { Test.sexps = []; bin_io_header = "\000"; bin_io_elements = [] } )
      ; ( single_set
        , { Test.sexps = [ "0" ]; bin_io_header = "\001"; bin_io_elements = [ "\000" ] } )
      ]
    ;;
  end))
;;

module _ = struct
  module type F = functor (Key : Stable) -> sig
    type 'a t = (Key.t, 'a, Key.comparator_witness) Map.t
    [@@deriving bin_io, compare, sexp]
  end

  module Test (F : F) = Stable_unit_test.Make (struct
    include F (Int)

    type nonrec t = string t [@@deriving sexp, bin_io]

    let equal = Map.equal String.equal

    let tests =
      [ ( Int.Map.of_alist_exn [ 1, "foo"; 2, "bar"; 3, "baz" ]
        , "((1 foo) (2 bar) (3 baz))"
        , "\003\001\003foo\002\003bar\003\003baz" )
      ; Int.Map.empty, "()", "\000"
      ; Int.Map.singleton 0 "foo", "((0 foo))", "\001\000\003foo"
      ]
    ;;
  end)

  let%test_module "Map.V1" = (module Test (Map.Stable.V1.Make))

  let%test_module "Map.V1.M" =
    (module struct
      module F (Key : Stable) = struct
        type 'a t = 'a Map.Stable.V1.M(Key).t [@@deriving bin_io, compare, sexp]
      end

      include Test (F)
    end)
  ;;
end

module _ = struct
  module type F = functor (Elt : Stable) -> sig
    type t = (Elt.t, Elt.comparator_witness) Set.t [@@deriving sexp, bin_io, compare]
  end

  module Test (F : F) = Stable_unit_test.Make (struct
    include F (Int)

    let equal = Set.equal

    let tests =
      [ ( Int.Set.of_list (List.init 10 ~f:Fn.id)
        , "(0 1 2 3 4 5 6 7 8 9)"
        , "\010\000\001\002\003\004\005\006\007\008\009" )
      ; Int.Set.empty, "()", "\000"
      ; Int.Set.singleton 0, "(0)", "\001\000"
      ]
    ;;
  end)

  let%test_module "Set.V1" = (module Test (Set.Stable.V1.Make))

  let%test_module "Set.V1.M" =
    (module struct
      module F (Elt : Stable) = struct
        type t = Set.Stable.V1.M(Elt).t [@@deriving bin_io, compare, sexp]
      end

      include Test (F)
    end)
  ;;
end
