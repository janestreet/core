open Core
open Expect_test_helpers_core
open Md5

let%expect_test "digest_bin_prot" =
  let test (type t) (t : t) (module M : Binable.S with type t = t) =
    print_s [%sexp (digest_bin_prot M.bin_writer_t t : Md5.t)]
  in
  test () (module Unit);
  test 1337 (module Int);
  [%expect
    {|
    93b885adfe0da089cdf634904fd59f71
    22eaa1d1a43daf2abf8bb3f7b8d0128c
    |}]
;;

let%test_module "Md5.As_binary_string.V1" =
  (module Stable_unit_test.Make (struct
    include As_binary_string.Stable.V1

    let equal = [%compare.equal: t]

    let tests =
      [ ( digest_string ""
        , {|"\212\029\140\217\143\000\178\004\233\128\t\152\236\248B~"|}
        , "\016\212\029\140\217\143\000\178\004\233\128\t\152\236\248B~" )
      ; ( digest_string "x"
        , {|"\157\212\228a&\140\1284\245\200VN\021\\g\166"|}
        , "\016\157\212\228a&\140\1284\245\200VN\021\\g\166" )
      ]
    ;;
  end))
;;

let%test_module "Md5.V1" =
  (module Stable_unit_test.Make (struct
    include Stable.V1

    let equal = [%compare.equal: t]

    let tests =
      [ ( digest_string ""
        , "d41d8cd98f00b204e9800998ecf8427e"
        , "\212\029\140\217\143\000\178\004\233\128\t\152\236\248B~" )
      ; ( digest_string "x"
        , "9dd4e461268c8034f5c8564e155c67a6"
        , "\157\212\228a&\140\1284\245\200VN\021\\g\166" )
      ]
    ;;
  end))
;;

let%test_unit "Md5.digest_file_blocking" =
  let cwd = Stdlib.Sys.getcwd () in
  let file = Filename.concat cwd (Filename.basename [%here].pos_fname) in
  let our_digest = digest_file_blocking file in
  let actual_digest = Md5.of_binary_exn (Stdlib.Digest.file file) in
  [%test_result: Md5.t] our_digest ~expect:actual_digest;
  assert (Exn.does_raise (fun () -> digest_file_blocking cwd))
;;

let%test_unit "Md5.digest_subbigstring" =
  let bigstr = Bigstring.of_string "abcd" in
  let d1 = Md5.digest_subbigstring bigstr ~pos:1 ~len:2 in
  let d2 = Md5.digest_string "bc" in
  [%test_result: Md5.t] d1 ~expect:d2
;;
