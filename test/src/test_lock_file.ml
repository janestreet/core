open! Core
open! Lock_file

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

