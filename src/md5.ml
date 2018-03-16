open Core_kernel

external digest_fd : Core_unix.File_descr.t -> string = "core_md5_fd"

include Md5

let digest_fd_blocking fd = of_binary_exn (digest_fd fd)

let digest_file_blocking path =
  Exn.protectx
    (Core_unix.openfile path ~mode:[O_RDONLY])
    ~f:digest_fd_blocking
    ~finally:Core_unix.close

let%test_unit _ =
  let cwd = Sys.getcwd () in
  let file = Filename.concat cwd (Filename.basename [%here].pos_fname) in
  let our_digest = digest_file_blocking file in
  let actual_digest = Md5.digest_file_blocking_without_releasing_runtime_lock file in
  [%test_result: Md5.t] our_digest ~expect:actual_digest;
  [%test_pred: Md5.t Or_error.t] Result.is_error (
    Or_error.try_with (fun () -> digest_file_blocking cwd))
