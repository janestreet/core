open! Core
open Poly
open! Bigstring
open Expect_test_helpers_core

let%test_module "concat" =
  (module struct
    let test ?sep list =
      [%test_result: t]
        (concat ?sep:(Option.map sep ~f:of_string) (List.map list ~f:of_string))
        ~expect:(of_string (String.concat ?sep list))
    ;;

    let%test_unit _ = test []
    let%test_unit _ = test [ "" ]
    let%test_unit _ = test [ "foo" ]
    let%test_unit _ = test [ "foo"; "bar" ]
    let%test_unit _ = test [ "foo"; "bar"; "baz" ]
    let%test_unit _ = test ~sep:"," []
    let%test_unit _ = test ~sep:"," [ "" ]
    let%test_unit _ = test ~sep:"," [ "foo" ]
    let%test_unit _ = test ~sep:"," [ "foo"; "bar" ]
    let%test_unit _ = test ~sep:"," [ "foo"; "bar"; "baz" ]
    let%test_unit _ = test ~sep:",.?" [ "Strings"; "of"; "different"; "lengths." ]
  end)
;;

let%test_unit (_ [@tags "64-bits-only"]) =
  let check s = [%test_eq: int] ([%hash: t_frozen] (of_string s)) ([%hash: string] s) in
  List.iter
    ~f:check
    [ ""
    ; "a"
    ; "ab"
    ; "abc"
    ; "abcd"
    ; "string hashing for bigstrings is the same as for standard strings"
    ]
;;

let%test_unit _ =
  let check s =
    [%test_eq: int]
      (Hash.run [%hash_fold: t_frozen] (of_string s))
      (Hash.run [%hash_fold: string] s)
  in
  List.iter
    ~f:check
    [ ""
    ; "a"
    ; "ab"
    ; "abc"
    ; "abcd"
    ; "string hashing for bigstrings is the same as for standard strings"
    ]
;;

let%test_module "comparison" =
  (module struct
    let sign n = if n < 0 then ~-1 else if n > 0 then 1 else 0

    let check t1 t2 int =
      let bool =
        match int with
        | 0 -> true
        | _ -> false
      in
      [%test_result: int] (sign (compare t1 t2)) ~expect:int;
      [%test_result: bool] (equal t1 t2) ~expect:bool
    ;;

    let%test_unit _ =
      let t = of_string "cat" in
      check t t 0
    ;;

    let%test_unit _ = check (of_string "cat") (of_string "cat") 0
    let%test_unit _ = check (of_string "cat") (of_string "cab") 1
    let%test_unit _ = check (of_string "cat") (of_string "caz") ~-1
    let%test_unit _ = check (of_string "cat") (of_string "c") 1
    let%test_unit _ = check (of_string "c") (of_string "cat") ~-1
    let%test_unit _ = check (of_string "cat") (of_string "dog") ~-1
    let%test_unit _ = check (of_string "dog") (of_string "cat") 1
  end)
;;

let%test_module _ =
  (module struct
    let make_t ~size input =
      (* We hardcode the size here to catch problems if [Bin_prot.Utils.size_header_length]
         ever changes. *)
      let t = create (String.length input + 8) in
      ignore (Bin_prot.Write.bin_write_int_64bit t ~pos:0 size : int);
      List.iteri (String.to_list input) ~f:(fun i c -> set t (i + 8) c);
      t
    ;;

    let test (type a) ~size input ?pos ?len reader sexp_of_a compare_a ~expect =
      let result =
        match read_bin_prot_verbose_errors (make_t ~size input) ?pos ?len reader with
        | `Ok (x, _bytes_read) -> `Ok x
        | `Not_enough_data -> `Not_enough_data
        | `Invalid_data _ -> `Invalid_data
      in
      [%test_result: [ `Ok of a | `Not_enough_data | `Invalid_data ]] result ~expect
    ;;

    let test_int ?pos ?len ~size input ~expect =
      test ~size input ?pos ?len Int.bin_reader_t Int.sexp_of_t Int.compare ~expect
    ;;

    let test_string ?pos ?len ~size input ~expect =
      test
        ~size
        input
        ?pos
        ?len
        String.bin_reader_t
        String.sexp_of_t
        String.compare
        ~expect
    ;;

    (* Keep in mind that the string bin-prot representation is itself prefixed with a
       length, so strings under the length-prefixed bin-prot protocol end up with two
       lengths at the front. *)
    let%test_unit _ = test_int ~size:1 "\042" ~expect:(`Ok 42)
    let%test_unit _ = test_int ~size:1 "\042suffix" ~expect:(`Ok 42)
    let%test_unit _ = test_string ~size:4 "\003foo" ~expect:(`Ok "foo")
    let%test_unit _ = test_string ~size:4 "\003foo" ~len:12 ~expect:(`Ok "foo")

    let%test "pos <> 0" =
      let t = "prefix" ^ to_string (make_t ~size:4 "\003foo") ^ "suffix" |> of_string in
      read_bin_prot_verbose_errors t ~pos:6 String.bin_reader_t = `Ok ("foo", 18)
    ;;

    let%test_unit "negative size" = test_string ~size:(-1) "\003foo" ~expect:`Invalid_data
    let%test_unit "wrong size" = test_string ~size:3 "\003foo" ~expect:`Invalid_data
    let%test_unit "bad bin-prot" = test_string ~size:4 "\007foo" ~expect:`Invalid_data

    let%test_unit "len too short" =
      test_string ~size:4 "\003foo" ~len:3 ~expect:`Not_enough_data
    ;;

    let%test "no header" =
      let t = of_string "\003foo" in
      read_bin_prot_verbose_errors t String.bin_reader_t = `Not_enough_data
    ;;
  end)
;;

let%test_module _ =
  (module struct
    let test ?pos writer v ~expect =
      let size = writer.Bin_prot.Type_class.size v + 8 in
      let t = create size in
      ignore (write_bin_prot t ?pos writer v : int);
      [%test_result: string] (to_string t) ~expect
    ;;

    let%test_unit _ =
      test String.bin_writer_t "foo" ~expect:"\004\000\000\000\000\000\000\000\003foo"
    ;;

    let%test_unit _ =
      test Int.bin_writer_t 123 ~expect:"\001\000\000\000\000\000\000\000\123"
    ;;

    let%test_unit _ =
      test
        (Or_error.bin_writer_t Unit.bin_writer_t)
        (Or_error.error_string "test")
        ~expect:"\007\000\000\000\000\000\000\000\001\001\004test"
    ;;
  end)
;;

let%test_unit _ =
  List.iter
    [ 0, 0; 1, 1; 0x7fff, 32767; 0xffff, -1; 0x8000, -32768 ]
    ~f:(fun (i, expect) ->
      assert (i >= 0);
      [%test_result: int] ~expect (Private.sign_extend_16 i))
;;

let%test_module "memcmp" =
  (module struct
    let empty = Bigstring.create 0
    let b1 = Bigstring.of_string "abcd"
    let b2 = Bigstring.of_string "jkcd"

    let%expect_test "out of bounds raises" =
      require_does_raise [%here] (fun () ->
        Bigstring.memcmp empty ~pos1:1 b1 ~pos2:1 ~len:0);
      [%expect {| (Invalid_argument "pos + len past end: 1 + 0 > 0") |}];
      require_does_raise [%here] (fun () ->
        Bigstring.memcmp empty ~pos1:1 b1 ~pos2:1 ~len:1);
      [%expect {| (Invalid_argument "pos + len past end: 1 + 1 > 0") |}];
      require_does_raise [%here] (fun () -> Bigstring.memcmp b1 ~pos1:0 b1 ~pos2:0 ~len:5);
      [%expect {| (Invalid_argument "pos + len past end: 0 + 5 > 4") |}];
      require_does_raise [%here] (fun () -> Bigstring.memcmp b1 ~pos1:0 b1 ~pos2:5 ~len:0);
      [%expect {| (Invalid_argument "pos + len past end: 5 + 0 > 4") |}];
      require_does_raise [%here] (fun () -> Bigstring.memcmp b1 ~pos1:0 b1 ~pos2:4 ~len:2);
      [%expect {| (Invalid_argument "pos + len past end: 4 + 2 > 4") |}]
    ;;

    let%test_unit "identical buffers are equal" =
      assert (Bigstring.memcmp empty ~pos1:0 empty ~pos2:0 ~len:0 = 0);
      assert (Bigstring.memcmp b1 ~pos1:0 b1 ~pos2:0 ~len:(Bigstring.length b1) = 0)
    ;;

    let%test_unit "prefix of identical buffers are equal" =
      assert (Bigstring.memcmp b1 ~pos1:0 b1 ~pos2:0 ~len:(Bigstring.length b1 - 1) = 0)
    ;;

    let%test_unit "suffix of identical buffers are equal" =
      assert (Bigstring.memcmp b1 ~pos1:1 b1 ~pos2:1 ~len:(Bigstring.length b1 - 1) = 0)
    ;;

    let%test_unit "memcmp agrees with compare" =
      assert (
        Bigstring.memcmp b1 ~pos1:0 b2 ~pos2:0 ~len:(Bigstring.length b1)
        = Bigstring.compare b1 b2);
      assert (
        Bigstring.memcmp b2 ~pos1:0 b1 ~pos2:0 ~len:(Bigstring.length b1)
        = Bigstring.compare b2 b1)
    ;;
  end)
;;

let%test_module "memset" =
  (module struct
    let empty = Bigstring.create 0

    let%test_unit "out of bounds raises" =
      let b1 = Bigstring.of_string "abcd" in
      require_does_raise [%here] (fun () -> Bigstring.memset empty ~pos:0 ~len:1 'a');
      require_does_raise [%here] (fun () -> Bigstring.memset b1 ~pos:1 ~len:4 'a');
      require_does_raise [%here] (fun () -> Bigstring.memset b1 ~pos:8 ~len:0 'a')
    ;;

    let%test_unit "total memset works" =
      let b1 = Bigstring.of_string "abcd" in
      let b2 = Bigstring.of_string "AAAA" in
      Bigstring.memset b1 ~pos:0 ~len:(Bigstring.length b1) 'A';
      assert (Bigstring.memcmp b1 ~pos1:0 b2 ~pos2:0 ~len:4 = 0)
    ;;

    let%test_unit "partial memset works" =
      let b1 = Bigstring.of_string "abcd" in
      let b2 = Bigstring.of_string "aAAd" in
      Bigstring.memset b1 ~pos:1 ~len:2 'A';
      assert (Bigstring.memcmp b1 ~pos1:0 b2 ~pos2:0 ~len:4 = 0)
    ;;
  end)
;;

let%test_module "stability" =
  (module struct
    let%expect_test "t" =
      print_endline [%bin_digest: Bigstring.Stable.V1.t];
      [%expect "e2d261c6c291b94bf6aa68ec2b08cb00"]
    ;;

    (* Should we assert these are the same?  *)
    let%expect_test "t_frozen" =
      print_endline [%bin_digest: Bigstring.Stable.V1.t_frozen];
      [%expect "e2d261c6c291b94bf6aa68ec2b08cb00"]
    ;;
  end)
;;
