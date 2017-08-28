open Core_kernel.Std

external digest_fd : int -> string = "core_md5_fd"

include Md5

let digest_fd_blocking fd =
  of_binary_exn (digest_fd (Core_unix.File_descr.to_int fd))

let digest_file_blocking path =
  let fd = Core_unix.openfile path ~mode:[O_RDONLY] in
  match digest_fd_blocking fd with
  | res -> Core_unix.close fd; res
  | exception e -> Core_unix.close fd; raise e

let%test_unit _ =
  let cwd = Sys.getcwd () in
  let file = Filename.concat cwd (Filename.basename [%here].pos_fname) in
  let our_digest = digest_file_blocking file in
  let actual_digest = Md5.digest_file_blocking_without_releasing_runtime_lock file in
  [%test_result: Md5.t] our_digest ~expect:actual_digest;
  [%test_pred: Md5.t Or_error.t] Result.is_error (
    Or_error.try_with (fun () -> digest_file_blocking cwd))
