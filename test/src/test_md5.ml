open Core
open Expect_test_helpers_kernel

open Md5

let%test_unit _ =
  let cwd = Sys.getcwd () in
  let file = Filename.concat cwd (Filename.basename [%here].pos_fname) in
  let our_digest = digest_file_blocking file in
  let actual_digest = Md5.digest_file_blocking_without_releasing_runtime_lock file in
  [%test_result: Md5.t] our_digest ~expect:actual_digest;
  [%test_pred: Md5.t Or_error.t] Result.is_error (
    Or_error.try_with (fun () -> digest_file_blocking cwd))

let tests buf =
  let test (type t) (t : t) (module M : Binable.S with type t = t) =
    require_equal [%here] (module Md5)
      (digest_bin_prot M.bin_writer_t t)
      (Bigbuffer.add_bin_prot buf M.bin_writer_t t;
       let md5 = Bigbuffer.md5 buf in
       Bigbuffer.clear buf;
       md5)
  in
  test ()   (module Unit);
  test 1337 (module Int);
  test [1; 2; 3; 4] (module struct
    type t = int list [@@deriving bin_io]
  end)
;;

let%expect_test "Bigbuffer.add_bin_prot + Bigbuffer.md5" =
  tests (Bigbuffer.create 1024)
;;

let%expect_test "buffer resizing" =
  tests (Bigbuffer.create 1)
;;
