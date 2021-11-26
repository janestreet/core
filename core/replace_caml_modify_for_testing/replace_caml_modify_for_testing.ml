external count : unit -> int = "replace_caml_modify_for_testing_count" [@@noalloc]
external reset : unit -> unit = "replace_caml_modify_for_testing_reset" [@@noalloc]

let%test_unit _ =
  let x = Array.make (32 * 1024) [ Random.int 10 ] in
  let v = [ Random.int 10 ] in
  let n = count () in
  x.(0) <- v;
  assert (count () = n + 1);
  let x = Array.make (32 * 1024) 0 in
  let n = count () in
  x.(0) <- 2;
  assert (count () = n)
;;
