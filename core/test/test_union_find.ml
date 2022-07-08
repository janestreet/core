open! Core
open! Import
open! Union_find
open! Union_find.Private

(* invariant checking wrapper functions *)

let create x =
  let t = create x in
  assert (is_compressed t);
  t
;;

let union t1 t2 =
  union t1 t2;
  invariant ignore t1;
  invariant ignore t2;
  assert (is_compressed t1 || is_compressed t2)
;;

let get t =
  let x = get t in
  assert (is_compressed t);
  x
;;

let set t x =
  set t x;
  assert (is_compressed t)
;;

let same_class t1 t2 =
  let b = same_class t1 t2 in
  assert (is_compressed t1);
  assert (is_compressed t2);
  b
;;

let%test_unit "union" =
  let a = create 1 in
  let b = create 2 in
  assert (same_class a a);
  assert (same_class b b);
  assert (not (same_class a b));
  union a b;
  assert (same_class a b);
  let c = create 3 in
  assert (not (same_class a c));
  assert (not (same_class b c));
  union b c;
  assert (same_class a c);
  assert (same_class b c);
  let d = create 1 in
  let e = create 2 in
  let f = create 3 in
  union d e;
  union d f;
  assert (same_class d e);
  assert (same_class d f);
  assert (same_class e f)
;;

let%test_unit "union" =
  let a = create 1 in
  let b = create 2 in
  union a b;
  let c = create 1 in
  let d = create 2 in
  union c d;
  union b d;
  assert (same_class a c)
;;

let%test_unit "set/get" =
  let a = create 1 in
  let b = create 2 in
  assert (get a = 1);
  assert (get b = 2);
  union a b;
  assert (get a = 1 || get a = 2);
  set a 3;
  assert (get a = 3);
  assert (get b = 3)
;;

let%test_unit "compressed" =
  let n = 1000 in
  let ts = List.init n ~f:create in
  let t =
    List.reduce_exn ts ~f:(fun a b ->
      union a b;
      b)
  in
  let max_rank = List.fold ts ~init:0 ~f:(fun acc t -> max acc (rank t)) in
  assert (max_rank = 1);
  set t 42;
  assert (List.for_all ts ~f:(fun t' -> same_class t t' && get t' = 42))
;;

let%test_unit "balanced" =
  let log2 n = int_of_float (Float.round_up (log (float_of_int n) /. log 2.)) in
  let n = 1000 in
  let ts = Array.init n ~f:create in
  let rec sub i j =
    if i = j
    then ts.(i)
    else (
      let k = (i + j) / 2 in
      let a = sub i k in
      if k + 1 > j
      then a
      else (
        let b = sub (k + 1) j in
        union a b;
        a))
  in
  let t = sub 0 (pred n) in
  Array.iter ts ~f:(invariant ignore);
  assert (Array.exists ts ~f:(fun t -> not (is_compressed t)));
  let max_rank = Array.fold ts ~init:0 ~f:(fun acc t -> max acc (rank t)) in
  assert (max_rank <= log2 n);
  set t 42;
  assert (Array.for_all ts ~f:(fun t' -> same_class t t' && get t' = 42));
  assert (Array.for_all ts ~f:is_compressed)
;;

let%expect_test "[get] of a root does not allocate" =
  let t = create () in
  require_no_allocation [%here] (fun () -> get t)
;;
