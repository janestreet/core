open Std
open Stable_internal

let hash_version () =
  if Sys.ocaml_version < "4" then
    `Old_hash
  else
    `New_hash

module Hashtbl = struct
  module V1 (Elt : Hashtbl.Key_binable) : sig
    type 'a t = (Elt.t, 'a) Hashtbl.t with sexp, bin_io
  end = Hashtbl.Make_binable (Elt)

  TEST_MODULE "Hashtbl.V1" = Stable_unit_test.Make (struct
    module Table = V1 (Int)
    type t = string Table.t with sexp, bin_io

    let equal t1 t2 = Int.Table.equal t1 t2 String.equal

    let triple_table =
      Int.Table.of_alist_exn ~size:10 [ 1, "foo"; 2, "bar"; 3, "baz" ]

    let single_table = Int.Table.of_alist_exn [ 0, "foo" ]

    let tests =
      (* The first test is dependent on the implementation of ocaml's hash function, the
         initial size of the table being greater than any of the ints placed in it, and
         some implementation details of Hashtbl.sexp_of_t.  Hence, this test might fail if
         some of those implementation details change, even though the serialization would
         still be recognized. *)
      [ (match hash_version () with
         | `Old_hash ->
           triple_table, "((3 baz) (2 bar) (1 foo))",
           "\003\001\003foo\002\003bar\003\003baz"
         | `New_hash ->
           triple_table, "((3 baz) (1 foo) (2 bar))",
           "\003\002\003bar\001\003foo\003\003baz");
        Int.Table.create (), "()", "\000";
        single_table, "((0 foo))", "\001\000\003foo";
      ]
  end)
end

module Hash_set = struct
  module V1 (Elt : Hash_set.Elt_binable) : sig
    type t = Elt.t Hash_set.t with sexp, bin_io
  end = Hash_set.Make_binable (Elt)

  TEST_MODULE "Hash_set.V1" = Stable_unit_test.Make (struct
    include V1 (Int)

    let equal = Hash_set.equal

    let int_list = List.init 10 ~f:Fn.id
    let char_list = List.map ~f:Char.of_int_exn int_list

    let ten_set = Int.Hash_set.of_list ~size:20 int_list

    let single_set = Int.Hash_set.of_list [0]

    let tests =
      (* See comment in Hashtbl.V1's test module. *)
      [ (match hash_version () with
         | `Old_hash ->
           ten_set, "(9 8 7 6 5 4 3 2 1 0)",
           String.of_char_list ('\010' :: char_list)
         | `New_hash ->
           ten_set, "(9 4 1 0 8 3 7 6 5 2)",
           String.of_char_list
             (List.map ~f:Char.of_int_exn
                [10;  2;5;6;7;3;8;0;1;4;9]));
        Int.Hash_set.create (), "()", "\000";
        single_set, "(0)", "\001\000";
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
