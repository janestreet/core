open Std
open Stable_internal

(* These tests are brittle, in that they rely on the order in which pairs are serialized,
   which can certainly change with implementations. It was decided it would be left this
   way to encourage implementors to consider backwards compatibility.

   I adjusted all sizes to either be the defaults or a power of 2, so that some hashtbl
   features could be rolled back without adjusting this test.

   The alternate tests for ocaml 3 vs. 4 (due to the poly hash function change) have been
   removed, as we are committed to ocaml 4 at this point. *)

module Hashtbl = struct
  module V1 (Elt : Hashtbl.Key_binable) : sig
    type 'a t = (Elt.t, 'a) Hashtbl.t with sexp, bin_io
  end = Hashtbl.Make_binable (Elt)

  TEST_MODULE "Hashtbl.V1" = Stable_unit_test.Make (struct
    module Table = V1 (Int)
    type t = string Table.t with sexp, bin_io

    let equal t1 t2 = Int.Table.equal t1 t2 String.equal

    let triple_table =
      Int.Table.of_alist_exn ~size:16 [ 1, "foo"; 2, "bar"; 3, "baz" ]

    let single_table = Int.Table.of_alist_exn [ 0, "foo" ]

    let tests =
      (* The first test is dependent on the implementation of ocaml's hash function, the
         initial size of the table being greater than any of the ints placed in it, and
         some implementation details of Hashtbl.sexp_of_t.  Hence, this test might fail if
         some of those implementation details change, even though the serialization would
         still be recognized. *)
      [ triple_table, "((1 foo) (3 baz) (2 bar))",
          "\003\002\003bar\003\003baz\001\003foo";
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

    let ten_set = Int.Hash_set.of_list ~size:32 int_list

    let single_set = Int.Hash_set.of_list [0]

    let tests =
      (* See comment in Hashtbl.V1's test module. *)
      [ ten_set, "(1 5 4 3 6 0 9 7 8 2)",
          "\n\002\b\007\t\000\006\003\004\005\001";
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
