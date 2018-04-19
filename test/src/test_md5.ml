open Core
open Expect_test_helpers_kernel

open Md5

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
