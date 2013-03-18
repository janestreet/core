open Std
open Stable_internal

module Hashtbl = struct
  module V1 (Elt : Hashtbl.Key_binable) : sig
    type 'a t = (Elt.t, 'a) Hashtbl.t with sexp, bin_io
  end = Hashtbl.Make_binable (Elt)

  TEST_MODULE "Hashtbl.V1" = Stable_unit_test.Make_unordered_container (struct
    module Table = V1 (Int)
    type t = string Table.t with sexp, bin_io

    let equal t1 t2 = Int.Table.equal t1 t2 String.equal

    let triple_table =
      Int.Table.of_alist_exn ~size:16 [ 1, "foo"; 2, "bar"; 3, "baz" ]

    let single_table = Int.Table.of_alist_exn [ 0, "foo" ]

    module Test = Stable_unit_test_intf.Unordered_container_test

    let tests =
      [ triple_table, {Test.
          sexps = ["(1 foo)"; "(2 bar)"; "(3 baz)"];
          bin_io_header = "\003";
          bin_io_elements = ["\001\003foo"; "\002\003bar"; "\003\003baz"];
        };
        Int.Table.create (), {Test.
          sexps = []; bin_io_header = "\000"; bin_io_elements = [];
        };
        single_table, {Test.
          sexps = ["(0 foo)"]; bin_io_header = "\001"; bin_io_elements = ["\000\003foo"];
        };
      ]
  end)
end

module Hash_set = struct
  module V1 (Elt : Hash_set.Elt_binable) : sig
    type t = Elt.t Hash_set.t with sexp, bin_io
  end = Hash_set.Make_binable (Elt)

  TEST_MODULE "Hash_set.V1" = Stable_unit_test.Make_unordered_container (struct
    include V1 (Int)

    let equal = Hash_set.equal

    let int_list = List.init 10 ~f:Fn.id

    let ten_set = Int.Hash_set.of_list ~size:32 int_list

    let single_set = Int.Hash_set.of_list [0]

    module Test = Stable_unit_test_intf.Unordered_container_test

    let tests =
      [ ten_set, {Test.
          sexps = List.init 10 ~f:Int.to_string;
          bin_io_header = "\010";
          bin_io_elements = List.init 10 ~f:(fun n -> Char.to_string (Char.of_int_exn n));
        };
        Int.Hash_set.create (), {Test.
          sexps = []; bin_io_header = "\000"; bin_io_elements = [];
        };
        single_set, {Test.
          sexps = ["0"]; bin_io_header = "\001"; bin_io_elements = ["\000"];
        };
      ]
  end)
end

module Map = struct
  module V1 (Key : Comparator.S_binable) : sig
    type 'a t = (Key.t, 'a, Key.comparator) Map.t with sexp, bin_io
  end = Map.Make_binable_using_comparator (Key)

  TEST_MODULE "Map.V1" = Stable_unit_test.Make (struct
    module Map = V1 (Int)
    type t = string Map.t with sexp, bin_io

    let equal = Int.Map.equal String.equal

    let tests =
      [ Int.Map.of_alist_exn [ 1, "foo"; 2, "bar"; 3, "baz" ],
        "((1 foo) (2 bar) (3 baz))", "\003\001\003foo\002\003bar\003\003baz";
        Int.Map.empty, "()", "\000";
        Int.Map.singleton 0 "foo", "((0 foo))", "\001\000\003foo";
      ]
  end)
end

module Set = struct
  module V1 (Elt : Comparator.S_binable) : sig
    type t = (Elt.t, Elt.comparator) Set.t with sexp, bin_io
  end = Set.Make_binable_using_comparator (Elt)

  TEST_MODULE "Set.V1" = Stable_unit_test.Make (struct
    include V1 (Int)

    let equal = Set.equal

    let tests =
      [ Int.Set.of_list (List.init 10 ~f:Fn.id),
        "(0 1 2 3 4 5 6 7 8 9)",
        "\010\000\001\002\003\004\005\006\007\008\009";
        Int.Set.empty, "()", "\000";
        Int.Set.singleton 0, "(0)", "\001\000";
      ]
  end)
end
