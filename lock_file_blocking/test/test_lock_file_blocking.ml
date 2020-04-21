open! Core
open! Lock_file_blocking

let%test_module _ = (module struct
  let lock_file = Filename.temp_file "lock_file" "unit_test"
  let () = Unix.unlink lock_file
  let%test _ = create lock_file
  let%test _ = not (create lock_file)
  let%test _ = is_locked lock_file

  let nolock_file = Filename.temp_file "nolock_file" "unit_test"
  let () =
    Unix.unlink nolock_file;
    (* Create an empty file. *)
    Unix.close (Unix.openfile nolock_file ~mode:[Unix.O_CREAT; Unix.O_WRONLY])
  let%test _ =
    (* Check that file exists. *)
    try ignore (Unix.stat nolock_file); true
    with Unix.Unix_error (ENOENT, _, _) -> false
  let%test _ = not (is_locked nolock_file)
end)

let%test_module "[Nfs]" = (module struct
  open! Nfs

  let create_bool path = match create path with Ok () -> true | Error _ -> false
  let path = Filename.temp_file "lock_file" "unit_test"
  let () = Unix.unlink path
  let%test _ = create_bool path
  let%test _ = not (create_bool path)
  let () = unlock_exn path
  let%test _ = create_bool path
  let () = unlock_exn path
end)

open Expect_test_helpers_core

let%expect_test "Symlink lock protocol" =
  let lock_path = Filename.temp_file "lock_file" "unit_test" in
  let () = Unix.unlink lock_path in
  let lock_expect_success ~metadata =
    match Symlink.lock_exn ~lock_path ~metadata with
    | `We_took_it lock -> lock
    | `Somebody_else_took_it _ ->
      failwith "BUG: failed to take the lock when we should have succeeded"
  in
  let lock_expect_failure ~metadata =
    match Symlink.lock_exn ~lock_path ~metadata with
    | `We_took_it _lock ->
      failwith "BUG: took the lock when we shouldn't have"
    | `Somebody_else_took_it metadata ->
      print_s [%sexp "Somebody else took it", { metadata : string Or_error.t }]
  in
  let lock = lock_expect_success ~metadata:"hello world" in
  lock_expect_failure ~metadata:"goodbye world";
  [%expect {|("Somebody else took it" ((metadata (Ok "hello world"))))|}];
  let () = Symlink.unlock_exn lock in
  let lock = lock_expect_success ~metadata:"hi" in
  lock_expect_failure ~metadata:"hi";
  [%expect {|("Somebody else took it" ((metadata (Ok hi))))|}];
  Symlink.unlock_exn lock
