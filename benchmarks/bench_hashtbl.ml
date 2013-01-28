open Core.Std

module Bench = Core_extended.Bench
module Test = Bench.Test

let gen_test_int tbl n =
  for i = 0 to n - 1 do
    Hashtbl.replace tbl ~key:i ~data:i
  done

let int_table n = (); fun () ->
  gen_test_int (Int.Table.create ~size:(2*n) ()) n

let make_int1_table n = ();
  let module I = Hashable.Make(Int) in
  fun () -> gen_test_int (I.Table.create ~size:(2*n) ()) n

let make_int2_table n = ();
  let module I = Hashable.Make(struct
    include Int
    let hash = Hashtbl.hash
  end)
  in
  fun () -> gen_test_int (I.Table.create ~size:(2*n) ()) n

let poly_int_table n = (); fun () ->
  gen_test_int (Hashtbl.Poly.create ~size:(2*n) ()) n

let strings = Array.init 1_000_000 ~f:(fun i -> sprintf "%8d" i)

let gen_test_string tbl n =
  for i = 0 to n - 1 do
    Hashtbl.replace tbl ~key:strings.(i) ~data:i
  done

let string_table n = (); fun () ->
  gen_test_string (String.Table.create ~size:(2*n) ()) n

let poly_string_table n = (); fun () ->
  gen_test_string (Hashtbl.Poly.create ~size:(2*n) ()) n

let hashtbl_hash n = (); fun () ->
  for i = 0 to n - 1 do
    let (_ : int) = Hashtbl.hash i in ()
  done

external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"

let inlined_hash n = (); fun () ->
  for i = 0 to n - 1 do
    let (_ : int) = hash_param 10 100 i in ()
  done

let not_inlined_hash n = (); fun () ->
  let hash = if Time.now () > Time.now () then Hashtbl.hash else Hashtbl.hash in
  for i = 0 to n - 1 do
    let (_ : int) = hash i in ()
  done

let () =
  let n = 1_000_000 in
  Bench.bench
    [ Test.create ~name:"Int"               (int_table n)
    ; Test.create ~name:"Make-Int1"         (make_int1_table n)
    ; Test.create ~name:"Make-Int2"         (make_int2_table n)
    ; Test.create ~name:"Poly-Int"          (poly_int_table n)
    ; Test.create ~name:"String"            (string_table n)
    ; Test.create ~name:"Poly-String"       (poly_string_table n)

    ; Test.create ~name:"hashtbl_hash"      (hashtbl_hash n)
    ; Test.create ~name:"hash_param"        (inlined_hash n)
    ; Test.create ~name:"not_inlined_hash"  (not_inlined_hash n)
    ]
