open Core

let generic_mk_set_and_print_for_test ~to_string ~set ~get buf =
  Staged.stage (fun ~test_name ~pos n ->
    try
      set buf ~pos n;
      printf "%s: %s\n" test_name (to_string (get buf ~pos))
    with
    | e -> printf !"%s: %{Exn}\n" test_name e)
;;

let mk_set_and_print_for_test = generic_mk_set_and_print_for_test ~to_string:string_of_int

let generic_test_get ~to_string get buf ~pos =
  try printf "%s\n" (to_string (get buf ~pos)) with
  | e -> printf !"%{Exn}\n" e
;;

let test_get = generic_test_get ~to_string:Int.to_string

let%expect_test "set_int8_exn" =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let set_and_print =
    mk_set_and_print_for_test ~set:Bigstring.set_int8_exn ~get:Bigstring.get_int8 buf
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok pos" ~pos:1 127;
  [%expect {| ok pos: 127 |}];
  set_and_print ~test_name:"ok neg" ~pos:1 (-128);
  [%expect {| ok neg: -128 |}];
  set_and_print ~test_name:"too large" ~pos:1 128;
  [%expect
    {|
    too large: (Invalid_argument
      "Bigstring.set_int8_exn: 128 is not a valid (signed) 8-bit integer")
    |}];
  set_and_print ~test_name:"too small" ~pos:1 (-129);
  [%expect
    {|
    too small: (Invalid_argument
      "Bigstring.set_int8_exn: -129 is not a valid (signed) 8-bit integer")
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:16 64;
  [%expect {| out of bounds: (Invalid_argument "index out of bounds") |}]
;;

let%expect_test "get_int8 oob" =
  let buf = Bigstring.init 1 ~f:(fun _ -> ' ') in
  test_get Bigstring.get_int8 buf ~pos:1;
  [%expect {| (Invalid_argument "index out of bounds") |}]
;;

let%expect_test "set_uint8_exn" =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 15 in
  let set_and_print =
    mk_set_and_print_for_test ~set:Bigstring.set_uint8_exn ~get:Bigstring.get_uint8 buf
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok" ~pos 0xFF;
  [%expect {| ok: 255 |}];
  set_and_print ~test_name:"too large" ~pos 0x100;
  [%expect
    {|
    too large: (Invalid_argument
      "Bigstring.set_uint8_exn: 256 is not a valid unsigned 8-bit integer")
    |}];
  set_and_print ~test_name:"too small" ~pos (-1);
  [%expect
    {|
    too small: (Invalid_argument
      "Bigstring.set_uint8_exn: -1 is not a valid unsigned 8-bit integer")
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64;
  [%expect {| out of bounds: (Invalid_argument "index out of bounds") |}]
;;

let%expect_test "get_int16_le oob" =
  let buf = Bigstring.init 2 ~f:(fun _ -> ' ') in
  test_get Bigstring.get_int16_le buf ~pos:1;
  [%expect {| (Invalid_argument "Bigstring.get_16: length(bstr) < pos + len") |}]
;;

let%expect_test "get_int16_be oob" =
  let buf = Bigstring.init 2 ~f:(fun _ -> ' ') in
  test_get Bigstring.get_int16_be buf ~pos:1;
  [%expect {| (Invalid_argument "Bigstring.get_16: length(bstr) < pos + len") |}]
;;

let%expect_test "set_int16_le_exn" =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 14 in
  let set_and_print =
    mk_set_and_print_for_test
      ~set:Bigstring.set_int16_le_exn
      ~get:Bigstring.get_int16_le
      buf
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok pos" ~pos 32767;
  [%expect {| ok pos: 32767 |}];
  printf "@pos:     0x%x\n" (Bigstring.get_uint8 buf ~pos);
  printf "@pos + 1: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 1));
  [%expect {|
    @pos:     0xff
    @pos + 1: 0x7f
    |}];
  set_and_print ~test_name:"ok neg" ~pos (-32768);
  [%expect {| ok neg: -32768 |}];
  set_and_print ~test_name:"too large" ~pos 32768;
  [%expect
    {|
    too large: (Invalid_argument
      "Bigstring.write_int16: 32768 is not a valid (signed) 16-bit integer")
    |}];
  set_and_print ~test_name:"too small" ~pos (-32769);
  [%expect
    {|
    too small: (Invalid_argument
      "Bigstring.write_int16: -32769 is not a valid (signed) 16-bit integer")
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_16: length(bstr) < pos + len") |}]
;;

let%expect_test "set_int16_be_exn" =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 14 in
  let set_and_print =
    mk_set_and_print_for_test
      ~set:Bigstring.set_int16_be_exn
      ~get:Bigstring.get_int16_be
      buf
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok pos" ~pos 32767;
  [%expect {| ok pos: 32767 |}];
  printf "@pos:     0x%x\n" (Bigstring.get_uint8 buf ~pos);
  printf "@pos + 1: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 1));
  [%expect {|
    @pos:     0x7f
    @pos + 1: 0xff
    |}];
  set_and_print ~test_name:"ok neg" ~pos (-32768);
  [%expect {| ok neg: -32768 |}];
  set_and_print ~test_name:"too large" ~pos 32768;
  [%expect
    {|
    too large: (Invalid_argument
      "Bigstring.write_int16: 32768 is not a valid (signed) 16-bit integer")
    |}];
  set_and_print ~test_name:"too small" ~pos (-32769);
  [%expect
    {|
    too small: (Invalid_argument
      "Bigstring.write_int16: -32769 is not a valid (signed) 16-bit integer")
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_16: length(bstr) < pos + len") |}]
;;

let%expect_test "set_uint16_le_exn" =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 14 in
  let set_and_print =
    mk_set_and_print_for_test
      ~set:Bigstring.set_uint16_le_exn
      ~get:Bigstring.get_uint16_le
      buf
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok" ~pos 0xFFFF;
  [%expect {| ok: 65535 |}];
  set_and_print ~test_name:"endianness check" ~pos 51966;
  printf "@pos:     0x%x\n" (Bigstring.get_uint8 buf ~pos);
  printf "@pos + 1: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 1));
  [%expect {|
    endianness check: 51966
    @pos:     0xfe
    @pos + 1: 0xca
    |}];
  set_and_print ~test_name:"too large" ~pos 0x1_0000;
  [%expect
    {|
    too large: (Invalid_argument
      "Bigstring.write_uint16: 65536 is not a valid unsigned 16-bit integer")
    |}];
  set_and_print ~test_name:"too small" ~pos (-1);
  [%expect
    {|
    too small: (Invalid_argument
      "Bigstring.write_uint16: -1 is not a valid unsigned 16-bit integer")
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_16: length(bstr) < pos + len") |}]
;;

let%expect_test "set_uint16_be_exn" =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 14 in
  let set_and_print =
    mk_set_and_print_for_test
      ~set:Bigstring.set_uint16_be_exn
      ~get:Bigstring.get_uint16_be
      buf
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok" ~pos 0xFFFF;
  [%expect {| ok: 65535 |}];
  set_and_print ~test_name:"endianness check" ~pos 51966;
  printf "@pos:     0x%x\n" (Bigstring.get_uint8 buf ~pos);
  printf "@pos + 1: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 1));
  [%expect {|
    endianness check: 51966
    @pos:     0xca
    @pos + 1: 0xfe
    |}];
  set_and_print ~test_name:"too large" ~pos 0x1_0000;
  [%expect
    {|
    too large: (Invalid_argument
      "Bigstring.write_uint16: 65536 is not a valid unsigned 16-bit integer")
    |}];
  set_and_print ~test_name:"too small" ~pos (-1);
  [%expect
    {|
    too small: (Invalid_argument
      "Bigstring.write_uint16: -1 is not a valid unsigned 16-bit integer")
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_16: length(bstr) < pos + len") |}]
;;

let%expect_test "get_uint16_le oob" =
  let buf = Bigstring.init 2 ~f:(fun _ -> ' ') in
  test_get Bigstring.get_uint16_le buf ~pos:1;
  [%expect {| (Invalid_argument "Bigstring.get_16: length(bstr) < pos + len") |}]
;;

let%expect_test "get_uint16_be oob" =
  let buf = Bigstring.init 2 ~f:(fun _ -> ' ') in
  test_get Bigstring.get_uint16_be buf ~pos:1;
  [%expect {| (Invalid_argument "Bigstring.get_16: length(bstr) < pos + len") |}]
;;

(* There's a slightly different code path here on 32-bit, so let's test it *)
let%expect_test "set_int32_le_exn 32-bit" =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 12 in
  let set_and_print =
    mk_set_and_print_for_test
      ~set:Bigstring.set_int32_le_exn
      ~get:Bigstring.get_int32_le
      buf
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok pos" ~pos 1073741823;
  [%expect {| ok pos: 1073741823 |}];
  set_and_print ~test_name:"ok neg" ~pos (-1073741824);
  [%expect {| ok neg: -1073741824 |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_32: length(bstr) < pos + len") |}]
;;

let%expect_test "set_int32_be_exn 32-bit" =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 12 in
  let set_and_print =
    mk_set_and_print_for_test
      ~set:Bigstring.set_int32_be_exn
      ~get:Bigstring.get_int32_be
      buf
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok pos" ~pos 1073741823;
  [%expect {| ok pos: 1073741823 |}];
  set_and_print ~test_name:"ok neg" ~pos (-1073741824);
  [%expect {| ok neg: -1073741824 |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_32: length(bstr) < pos + len") |}]
;;

let%expect_test ("set_int32_le_exn" [@tags "64-bits-only"]) =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 12 in
  let set_and_print =
    mk_set_and_print_for_test
      ~set:Bigstring.set_int32_le_exn
      ~get:Bigstring.get_int32_le
      buf
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok pos" ~pos ((1 lsl 31) - 1);
  [%expect {| ok pos: 2147483647 |}];
  set_and_print ~test_name:"ok neg" ~pos (-1 lsl 31);
  [%expect {| ok neg: -2147483648 |}];
  set_and_print ~test_name:"endianness check" ~pos 0x1AFEDEAD;
  printf "@pos:     0x%x\n" (Bigstring.get_uint8 buf ~pos);
  printf "@pos + 1: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 1));
  printf "@pos + 2: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 2));
  printf "@pos + 3: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 3));
  [%expect
    {|
    endianness check: 452910765
    @pos:     0xad
    @pos + 1: 0xde
    @pos + 2: 0xfe
    @pos + 3: 0x1a
    |}];
  set_and_print ~test_name:"too large" ~pos (1 lsl 31);
  [%expect
    {|
    too large: (Invalid_argument
      "Bigstring.write_int32_int: 2147483648 is not a valid (signed) 32-bit integer")
    |}];
  set_and_print ~test_name:"too small" ~pos ((-1 lsl 31) - 1);
  [%expect
    {|
    too small: (Invalid_argument
      "Bigstring.write_int32_int: -2147483649 is not a valid (signed) 32-bit integer")
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_32: length(bstr) < pos + len") |}]
;;

let%expect_test ("set_int32_be" [@tags "64-bits-only"]) =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 12 in
  let set_and_print =
    mk_set_and_print_for_test
      ~set:Bigstring.set_int32_be_exn
      ~get:Bigstring.get_int32_be
      buf
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok pos" ~pos ((1 lsl 31) - 1);
  [%expect {| ok pos: 2147483647 |}];
  set_and_print ~test_name:"ok neg" ~pos (-1 lsl 31);
  [%expect {| ok neg: -2147483648 |}];
  set_and_print ~test_name:"endianness check" ~pos 0x1AFEDEAD;
  printf "@pos:     0x%x\n" (Bigstring.get_uint8 buf ~pos);
  printf "@pos + 1: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 1));
  printf "@pos + 2: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 2));
  printf "@pos + 3: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 3));
  [%expect
    {|
    endianness check: 452910765
    @pos:     0x1a
    @pos + 1: 0xfe
    @pos + 2: 0xde
    @pos + 3: 0xad
    |}];
  set_and_print ~test_name:"too large" ~pos (1 lsl 31);
  [%expect
    {|
    too large: (Invalid_argument
      "Bigstring.write_int32_int: 2147483648 is not a valid (signed) 32-bit integer")
    |}];
  set_and_print ~test_name:"too small" ~pos ((-1 lsl 31) - 1);
  [%expect
    {|
    too small: (Invalid_argument
      "Bigstring.write_int32_int: -2147483649 is not a valid (signed) 32-bit integer")
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_32: length(bstr) < pos + len") |}]
;;

let%expect_test "set_uint32_le_exn 32-bit" =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 12 in
  let set_and_print =
    mk_set_and_print_for_test
      ~set:Bigstring.set_uint32_le_exn
      ~get:Bigstring.get_uint32_le
      buf
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok" ~pos 1073741823;
  [%expect {| ok: 1073741823 |}];
  set_and_print ~test_name:"too small" ~pos (-1);
  [%expect
    {|
    too small: (Invalid_argument
      "Bigstring.set_uint32_le_exn: -1 is not a valid unsigned 32-bit integer")
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_32: length(bstr) < pos + len") |}]
;;

let%expect_test "get_int32_le oob" =
  let buf = Bigstring.init 4 ~f:(fun _ -> ' ') in
  test_get Bigstring.get_int32_le buf ~pos:1;
  [%expect {| (Invalid_argument "Bigstring.get_32: length(bstr) < pos + len") |}]
;;

let%expect_test "get_int32_be oob" =
  let buf = Bigstring.init 4 ~f:(fun _ -> ' ') in
  test_get Bigstring.get_int32_be buf ~pos:1;
  [%expect {| (Invalid_argument "Bigstring.get_32: length(bstr) < pos + len") |}]
;;

let%expect_test "set_uint32_be_exn 32-bit" =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 12 in
  let set_and_print =
    mk_set_and_print_for_test
      ~set:Bigstring.set_uint32_be_exn
      ~get:Bigstring.get_uint32_be
      buf
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok" ~pos 1073741823;
  [%expect {| ok: 1073741823 |}];
  set_and_print ~test_name:"too small" ~pos (-1);
  [%expect
    {|
    too small: (Invalid_argument
      "Bigstring.set_uint32_be_exn: -1 is not a valid unsigned 32-bit integer")
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_32: length(bstr) < pos + len") |}]
;;

let%expect_test ("set_uint32_le_exn" [@tags "64-bits-only"]) =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 12 in
  let set_and_print =
    mk_set_and_print_for_test
      ~set:Bigstring.set_uint32_le_exn
      ~get:Bigstring.get_uint32_le
      buf
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok" ~pos ((1 lsl 32) - 1);
  [%expect {| ok: 4294967295 |}];
  set_and_print ~test_name:"endianness check" ~pos ((0xCAFE lsl 16) lor 0xDEAD);
  printf "@pos:     0x%x\n" (Bigstring.get_uint8 buf ~pos);
  printf "@pos + 1: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 1));
  printf "@pos + 2: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 2));
  printf "@pos + 3: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 3));
  [%expect
    {|
    endianness check: 3405700781
    @pos:     0xad
    @pos + 1: 0xde
    @pos + 2: 0xfe
    @pos + 3: 0xca
    |}];
  set_and_print ~test_name:"too large" ~pos (1 lsl 32);
  [%expect
    {|
    too large: (Invalid_argument
      "Bigstring.set_uint32_le_exn: 4294967296 is not a valid unsigned 32-bit integer")
    |}];
  set_and_print ~test_name:"too small" ~pos (-1);
  [%expect
    {|
    too small: (Invalid_argument
      "Bigstring.set_uint32_le_exn: -1 is not a valid unsigned 32-bit integer")
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_32: length(bstr) < pos + len") |}]
;;

let%expect_test ("set_uint32_be_exn" [@tags "64-bits-only"]) =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 12 in
  let set_and_print =
    mk_set_and_print_for_test
      ~set:Bigstring.set_uint32_be_exn
      ~get:Bigstring.get_uint32_be
      buf
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok" ~pos ((1 lsl 32) - 1);
  [%expect {| ok: 4294967295 |}];
  set_and_print ~test_name:"endianness check" ~pos ((0xCAFE lsl 16) lor 0xDEAD);
  printf "@pos:     0x%x\n" (Bigstring.get_uint8 buf ~pos);
  printf "@pos + 1: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 1));
  printf "@pos + 2: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 2));
  printf "@pos + 3: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 3));
  [%expect
    {|
    endianness check: 3405700781
    @pos:     0xca
    @pos + 1: 0xfe
    @pos + 2: 0xde
    @pos + 3: 0xad
    |}];
  set_and_print ~test_name:"too large" ~pos (1 lsl 32);
  [%expect
    {|
    too large: (Invalid_argument
      "Bigstring.set_uint32_be_exn: 4294967296 is not a valid unsigned 32-bit integer")
    |}];
  set_and_print ~test_name:"too small" ~pos (-1);
  [%expect
    {|
    too small: (Invalid_argument
      "Bigstring.set_uint32_be_exn: -1 is not a valid unsigned 32-bit integer")
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_32: length(bstr) < pos + len") |}]
;;

let%expect_test "get_uint32_le oob" =
  let buf = Bigstring.init 4 ~f:(fun _ -> ' ') in
  test_get Bigstring.get_uint32_le buf ~pos:1;
  [%expect {| (Invalid_argument "Bigstring.get_32: length(bstr) < pos + len") |}]
;;

let%expect_test "get_uint32_be oob" =
  let buf = Bigstring.init 4 ~f:(fun _ -> ' ') in
  test_get Bigstring.get_uint32_be buf ~pos:1;
  [%expect {| (Invalid_argument "Bigstring.get_32: length(bstr) < pos + len") |}]
;;

let%expect_test "set_int32_t_le" =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 12 in
  let set_and_print =
    generic_mk_set_and_print_for_test
      ~set:Bigstring.set_int32_t_le
      ~get:Bigstring.get_int32_t_le
      buf
      ~to_string:Int32.to_string
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok pos" ~pos 0x7FFF_FFFFl;
  [%expect {| ok pos: 2147483647 |}];
  set_and_print ~test_name:"ok neg" ~pos 0x8000_0000l;
  [%expect {| ok neg: -2147483648 |}];
  set_and_print ~test_name:"endianness check" ~pos 0xCAFEDEADl;
  printf "@pos:     0x%x\n" (Bigstring.get_uint8 buf ~pos);
  printf "@pos + 1: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 1));
  printf "@pos + 2: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 2));
  printf "@pos + 3: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 3));
  [%expect
    {|
    endianness check: -889266515
    @pos:     0xad
    @pos + 1: 0xde
    @pos + 2: 0xfe
    @pos + 3: 0xca
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64l;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_32: length(bstr) < pos + len") |}]
;;

let%expect_test "set_int32_t_be" =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 12 in
  let set_and_print =
    generic_mk_set_and_print_for_test
      ~set:Bigstring.set_int32_t_be
      ~get:Bigstring.get_int32_t_be
      buf
      ~to_string:Int32.to_string
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok pos" ~pos 0x7FFF_FFFFl;
  [%expect {| ok pos: 2147483647 |}];
  set_and_print ~test_name:"ok neg" ~pos 0x8000_0000l;
  [%expect {| ok neg: -2147483648 |}];
  set_and_print ~test_name:"endianness check" ~pos 0xCAFEDEADl;
  printf "@pos:     0x%x\n" (Bigstring.get_uint8 buf ~pos);
  printf "@pos + 1: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 1));
  printf "@pos + 2: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 2));
  printf "@pos + 3: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 3));
  [%expect
    {|
    endianness check: -889266515
    @pos:     0xca
    @pos + 1: 0xfe
    @pos + 2: 0xde
    @pos + 3: 0xad
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64l;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_32: length(bstr) < pos + len") |}]
;;

let%expect_test "get_uint32_le oob" =
  let buf = Bigstring.init 4 ~f:(fun _ -> ' ') in
  generic_test_get ~to_string:Int32.to_string Bigstring.get_int32_t_le buf ~pos:1;
  [%expect {| (Invalid_argument "Bigstring.get_32: length(bstr) < pos + len") |}]
;;

let%expect_test "get_uint32_be oob" =
  let buf = Bigstring.init 4 ~f:(fun _ -> ' ') in
  generic_test_get ~to_string:Int32.to_string Bigstring.get_int32_t_be buf ~pos:1;
  [%expect {| (Invalid_argument "Bigstring.get_32: length(bstr) < pos + len") |}]
;;

let%expect_test ("set_int64_le" [@tags "64-bits-only"]) =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 8 in
  let set_and_print =
    mk_set_and_print_for_test
      ~set:Bigstring.set_int64_le
      ~get:Bigstring.get_int64_le_exn
      buf
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok pos" ~pos ((1 lsl 62) - 1);
  [%expect {| ok pos: 4611686018427387903 |}];
  set_and_print ~test_name:"ok neg" ~pos (-1 lsl 62);
  [%expect {| ok neg: -4611686018427387904 |}];
  set_and_print
    ~test_name:"endianness check"
    ~pos
    ((0x1AFE lsl 48) lor (0xBABE lsl 32) lor (0xDEAD lsl 16) lor 0xBEEF);
  printf "@pos:     0x%x\n" (Bigstring.get_uint8 buf ~pos);
  printf "@pos + 1: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 1));
  printf "@pos + 2: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 2));
  printf "@pos + 3: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 3));
  printf "@pos + 4: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 4));
  printf "@pos + 5: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 5));
  printf "@pos + 6: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 6));
  printf "@pos + 7: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 7));
  [%expect
    {|
    endianness check: 1945197418013114095
    @pos:     0xef
    @pos + 1: 0xbe
    @pos + 2: 0xad
    @pos + 3: 0xde
    @pos + 4: 0xbe
    @pos + 5: 0xba
    @pos + 6: 0xfe
    @pos + 7: 0x1a
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_64: length(bstr) < pos + len") |}]
;;

let%expect_test ("set_int64_be" [@tags "64-bits-only"]) =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 8 in
  let set_and_print =
    mk_set_and_print_for_test
      ~set:Bigstring.set_int64_be
      ~get:Bigstring.get_int64_be_exn
      buf
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok pos" ~pos ((1 lsl 62) - 1);
  [%expect {| ok pos: 4611686018427387903 |}];
  set_and_print ~test_name:"ok neg" ~pos (-1 lsl 62);
  [%expect {| ok neg: -4611686018427387904 |}];
  set_and_print
    ~test_name:"endianness check"
    ~pos
    ((0x1AFE lsl 48) lor (0xBABE lsl 32) lor (0xDEAD lsl 16) lor 0xBEEF);
  printf "@pos:     0x%x\n" (Bigstring.get_uint8 buf ~pos);
  printf "@pos + 1: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 1));
  printf "@pos + 2: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 2));
  printf "@pos + 3: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 3));
  printf "@pos + 4: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 4));
  printf "@pos + 5: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 5));
  printf "@pos + 6: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 6));
  printf "@pos + 7: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 7));
  [%expect
    {|
    endianness check: 1945197418013114095
    @pos:     0x1a
    @pos + 1: 0xfe
    @pos + 2: 0xba
    @pos + 3: 0xbe
    @pos + 4: 0xde
    @pos + 5: 0xad
    @pos + 6: 0xbe
    @pos + 7: 0xef
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_64: length(bstr) < pos + len") |}]
;;

let%expect_test "get_int64_le_exn oob" =
  let buf = Bigstring.init 8 ~f:(fun _ -> ' ') in
  test_get Bigstring.get_int64_le_exn buf ~pos:1;
  [%expect {| (Invalid_argument "Bigstring.get_64: length(bstr) < pos + len") |}]
;;

let%expect_test "get_int64_be_exn oob" =
  let buf = Bigstring.init 8 ~f:(fun _ -> ' ') in
  test_get Bigstring.get_int64_be_exn buf ~pos:1;
  [%expect {| (Invalid_argument "Bigstring.get_64: length(bstr) < pos + len") |}]
;;

let%expect_test "get_int64_le_trunc oob" =
  let buf = Bigstring.init 4 ~f:(fun _ -> ' ') in
  test_get Bigstring.get_int64_le_trunc buf ~pos:1;
  [%expect {| (Invalid_argument "Bigstring.get_64: length(bstr) < pos + len") |}]
;;

let%expect_test "get_int64_be_trunc oob" =
  let buf = Bigstring.init 4 ~f:(fun _ -> ' ') in
  test_get Bigstring.get_int64_be_trunc buf ~pos:1;
  [%expect {| (Invalid_argument "Bigstring.get_64: length(bstr) < pos + len") |}]
;;

let%expect_test "get_int64_le_exn raises" =
  let buf = Bigstring.init 8 ~f:(fun _ -> ' ') in
  Bigstring.set_int64_t_le buf ~pos:0 0x8000000000000000L;
  test_get Bigstring.get_int64_le_exn buf ~pos:0;
  [%expect {| (Failure "unsafe_read_int64: value cannot be represented unboxed!") |}]
;;

let%expect_test "get_int64_be_exn raises" =
  let buf = Bigstring.init 8 ~f:(fun _ -> ' ') in
  Bigstring.set_int64_t_be buf ~pos:0 0x8000000000000000L;
  test_get Bigstring.get_int64_be_exn buf ~pos:0;
  [%expect {| (Failure "unsafe_read_int64: value cannot be represented unboxed!") |}]
;;

let%expect_test "get_int64_le_trunc truncates" =
  let buf = Bigstring.init 8 ~f:(fun _ -> ' ') in
  Bigstring.set_int64_t_le buf ~pos:0 0x8000000000000000L;
  test_get Bigstring.get_int64_le_trunc buf ~pos:0;
  [%expect {| 0 |}]
;;

let%expect_test "get_int64_be_trunc truncates" =
  let buf = Bigstring.init 8 ~f:(fun _ -> ' ') in
  Bigstring.set_int64_t_be buf ~pos:0 0x8000000000000000L;
  test_get Bigstring.get_int64_be_trunc buf ~pos:0;
  [%expect {| 0 |}]
;;

let%expect_test ("set_uint64_le_exn" [@tags "64-bits-only"]) =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 8 in
  let set_and_print =
    mk_set_and_print_for_test
      ~set:Bigstring.set_uint64_le_exn
      ~get:Bigstring.get_uint64_le_exn
      buf
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok" ~pos ((1 lsl 62) - 1);
  [%expect {| ok: 4611686018427387903 |}];
  set_and_print
    ~test_name:"endianness check"
    ~pos
    ((0x1AFE lsl 48) lor (0xBABE lsl 32) lor (0xDEAD lsl 16) lor 0xBEEF);
  printf "@pos:     0x%x\n" (Bigstring.get_uint8 buf ~pos);
  printf "@pos + 1: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 1));
  printf "@pos + 2: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 2));
  printf "@pos + 3: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 3));
  printf "@pos + 4: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 4));
  printf "@pos + 5: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 5));
  printf "@pos + 6: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 6));
  printf "@pos + 7: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 7));
  [%expect
    {|
    endianness check: 1945197418013114095
    @pos:     0xef
    @pos + 1: 0xbe
    @pos + 2: 0xad
    @pos + 3: 0xde
    @pos + 4: 0xbe
    @pos + 5: 0xba
    @pos + 6: 0xfe
    @pos + 7: 0x1a
    |}];
  set_and_print ~test_name:"too small" ~pos (-1);
  [%expect
    {|
    too small: (Invalid_argument
      "Bigstring.set_uint64_le_exn: -1 is not a valid unsigned 64-bit integer")
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_64: length(bstr) < pos + len") |}]
;;

let%expect_test ("set_uint64_be_exn" [@tags "64-bits-only"]) =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 8 in
  let set_and_print =
    mk_set_and_print_for_test
      ~set:Bigstring.set_uint64_be_exn
      ~get:Bigstring.get_uint64_be_exn
      buf
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok" ~pos ((1 lsl 62) - 1);
  [%expect {| ok: 4611686018427387903 |}];
  set_and_print
    ~test_name:"endianness check"
    ~pos
    ((0x1AFE lsl 48) lor (0xBABE lsl 32) lor (0xDEAD lsl 16) lor 0xBEEF);
  printf "@pos:     0x%x\n" (Bigstring.get_uint8 buf ~pos);
  printf "@pos + 1: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 1));
  printf "@pos + 2: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 2));
  printf "@pos + 3: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 3));
  printf "@pos + 4: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 4));
  printf "@pos + 5: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 5));
  printf "@pos + 6: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 6));
  printf "@pos + 7: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 7));
  [%expect
    {|
    endianness check: 1945197418013114095
    @pos:     0x1a
    @pos + 1: 0xfe
    @pos + 2: 0xba
    @pos + 3: 0xbe
    @pos + 4: 0xde
    @pos + 5: 0xad
    @pos + 6: 0xbe
    @pos + 7: 0xef
    |}];
  set_and_print ~test_name:"too small" ~pos (-1);
  [%expect
    {|
    too small: (Invalid_argument
      "Bigstring.set_uint64_be_exn: -1 is not a valid unsigned 64-bit integer")
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_64: length(bstr) < pos + len") |}]
;;

let%expect_test "set_int64_t_le" =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 8 in
  let set_and_print =
    generic_mk_set_and_print_for_test
      ~set:(fun t ~pos v -> Bigstring.set_int64_t_le t ~pos v)
      ~get:(fun t ~pos -> Bigstring.get_int64_t_le t ~pos)
      buf
      ~to_string:Int64.to_string
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok pos" ~pos 0x7FFF_FFFF_FFFF_FFFFL;
  [%expect {| ok pos: 9223372036854775807 |}];
  set_and_print ~test_name:"ok neg" ~pos 0x8000_0000_0000_0000L;
  [%expect {| ok neg: -9223372036854775808 |}];
  set_and_print ~test_name:"endianness check" ~pos 0xCAFEBABEDEADBEEFL;
  printf "@pos:     0x%x\n" (Bigstring.get_uint8 buf ~pos);
  printf "@pos + 1: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 1));
  printf "@pos + 2: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 2));
  printf "@pos + 3: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 3));
  printf "@pos + 4: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 4));
  printf "@pos + 5: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 5));
  printf "@pos + 6: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 6));
  printf "@pos + 7: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 7));
  [%expect
    {|
    endianness check: -3819410105021120785
    @pos:     0xef
    @pos + 1: 0xbe
    @pos + 2: 0xad
    @pos + 3: 0xde
    @pos + 4: 0xbe
    @pos + 5: 0xba
    @pos + 6: 0xfe
    @pos + 7: 0xca
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64L;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_64: length(bstr) < pos + len") |}]
;;

let%expect_test "set_int64_t_be" =
  let buf = Bigstring.init 16 ~f:(fun _ -> ' ') in
  let pos = 8 in
  let set_and_print =
    generic_mk_set_and_print_for_test
      ~set:(fun t ~pos v -> Bigstring.set_int64_t_be t ~pos v)
      ~get:(fun t ~pos -> Bigstring.get_int64_t_be t ~pos)
      buf
      ~to_string:Int64.to_string
    |> Staged.unstage
  in
  set_and_print ~test_name:"ok pos" ~pos 0x7FFF_FFFF_FFFF_FFFFL;
  [%expect {| ok pos: 9223372036854775807 |}];
  set_and_print ~test_name:"ok neg" ~pos 0x8000_0000_0000_0000L;
  [%expect {| ok neg: -9223372036854775808 |}];
  set_and_print ~test_name:"endianness check" ~pos 0xCAFEBABEDEADBEEFL;
  printf "@pos:     0x%x\n" (Bigstring.get_uint8 buf ~pos);
  printf "@pos + 1: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 1));
  printf "@pos + 2: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 2));
  printf "@pos + 3: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 3));
  printf "@pos + 4: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 4));
  printf "@pos + 5: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 5));
  printf "@pos + 6: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 6));
  printf "@pos + 7: 0x%x\n" (Bigstring.get_uint8 buf ~pos:(pos + 7));
  [%expect
    {|
    endianness check: -3819410105021120785
    @pos:     0xca
    @pos + 1: 0xfe
    @pos + 2: 0xba
    @pos + 3: 0xbe
    @pos + 4: 0xde
    @pos + 5: 0xad
    @pos + 6: 0xbe
    @pos + 7: 0xef
    |}];
  set_and_print ~test_name:"out of bounds" ~pos:(pos + 1) 64L;
  [%expect
    {| out of bounds: (Invalid_argument "Bigstring.set_64: length(bstr) < pos + len") |}]
;;

let%expect_test "get_int64_t_le oob" =
  let buf = Bigstring.init 8 ~f:(fun _ -> ' ') in
  generic_test_get ~to_string:Int64.to_string Bigstring.get_int64_t_le buf ~pos:1;
  [%expect {| (Invalid_argument "Bigstring.get_64: length(bstr) < pos + len") |}]
;;

let%expect_test "get_int64_t_be oob" =
  let buf = Bigstring.init 8 ~f:(fun _ -> ' ') in
  generic_test_get ~to_string:Int64.to_string Bigstring.get_int64_t_be buf ~pos:1;
  [%expect {| (Invalid_argument "Bigstring.get_64: length(bstr) < pos + len") |}]
;;
