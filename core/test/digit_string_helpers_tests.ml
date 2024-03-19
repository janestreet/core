open! Core
open Core_private.Digit_string_helpers
open Import

let%test_unit "max_int63_with" =
  [%test_result: Int63.t]
    (max_int63_with ~digits:11 : Int63.t)
    ~expect:(Int63.of_string (String.init 11 ~f:(fun _ -> '9')))
;;

let%expect_test "divide_and_round_up" =
  let test numerator denominator =
    let numerator = Int63.of_int numerator in
    let denominator = Int63.of_int denominator in
    let quotient = Unsafe.divide_and_round_up ~numerator ~denominator in
    printf !"%{Int63#hum} / %{Int63#hum} = %{Int63#hum}\n" numerator denominator quotient
  in
  for numerator = -10 to 10 do
    test numerator 4
  done;
  [%expect
    {|
    -10 / 4 = -2
    -9 / 4 = -2
    -8 / 4 = -2
    -7 / 4 = -1
    -6 / 4 = -1
    -5 / 4 = -1
    -4 / 4 = -1
    -3 / 4 = 0
    -2 / 4 = 0
    -1 / 4 = 0
    0 / 4 = 0
    1 / 4 = 1
    2 / 4 = 1
    3 / 4 = 1
    4 / 4 = 1
    5 / 4 = 2
    6 / 4 = 2
    7 / 4 = 2
    8 / 4 = 2
    9 / 4 = 3
    10 / 4 = 3
    |}]
;;

let int63_ten = Int63.of_int 10

let rec digits_of int63 =
  if Int63.( < ) int63 int63_ten
  then 1
  else Int.succ (digits_of (Int63.( / ) int63 int63_ten))
;;

let max_int63_digits =
  (* subtract one because int63 cannot encode all values with as many digits as
     [max_value] *)
  digits_of Int63.max_value - 1
;;

let%expect_test "max_int63_digits" =
  print_s [%sexp (max_int63_digits : int)];
  [%expect {| 18 |}]
;;

let max_with ~digits = Int63.pred (Int63.pow int63_ten (Int63.of_int digits))

let test_write_int63 ~digits ?(verbose = true) ?(align = digits) write_int63 =
  let print_endline = if verbose then print_endline ?hide_positions:None else ignore in
  let require_does_raise here f =
    (* uses above print_endline, so if verbose is false, prints nothing on exn *)
    match f () with
    | _ -> require_does_raise here ignore
    | exception exn -> print_endline (Exn.to_string exn)
  in
  let max = max_with ~digits in
  print_endline "Expecting success:";
  (* show resulting strings at boundary values *)
  let show int63 =
    let bytes = Bytes.make digits '!' in
    write_int63 bytes ~pos:0 int63;
    printf !"%*Ld -> %S\n" align (Int63.to_int64 int63) (Bytes.to_string bytes)
  in
  show Int63.zero;
  show max;
  (* test success behavior for lots of correct values *)
  let expect_success_exn int63 =
    let bytes = Bytes.make (1 + digits + 1) '!' in
    write_int63 bytes ~pos:1 int63;
    [%test_result: string]
      (Bytes.to_string bytes)
      ~expect:(sprintf "!%0*Ld!" digits (Int63.to_int64 int63))
  in
  require_does_not_raise [%here] (fun () ->
    Quickcheck.test
      (Int63.gen_log_uniform_incl Int63.zero max)
      ~examples:[ Int63.zero; max ]
      ~sexp_of:Int63.sexp_of_t
      ~f:expect_success_exn);
  (* test failure cases *)
  print_endline "";
  print_endline "Expecting failure:";
  require_does_raise [%here] (fun () -> write_int63 (Bytes.make 0 '?') ~pos:0 Int63.zero);
  require_does_raise [%here] (fun () ->
    write_int63 (Bytes.make digits '?') ~pos:(-1) Int63.zero);
  require_does_raise [%here] (fun () ->
    write_int63 (Bytes.make digits '?') ~pos:1 Int63.zero);
  require_does_raise [%here] (fun () ->
    write_int63 (Bytes.make digits '?') ~pos:0 Int63.minus_one);
  require_does_raise [%here] (fun () ->
    write_int63 (Bytes.make digits '?') ~pos:0 (Int63.succ max))
;;

let test_write_int write_int ~digits =
  let write bytes ~pos int63 = write_int bytes ~pos (Int63.to_int_exn int63) in
  test_write_int63 write ~digits
;;

let%expect_test "write_1_digit_int" =
  test_write_int write_1_digit_int ~digits:1;
  [%expect
    {|
    Expecting success:
    0 -> "0"
    9 -> "9"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_1_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_1_digit_int: pos=-1 out of range for string of length 1")
    (Invalid_argument
      "Digit_string_helpers.write_1_digit_int: pos=1 out of range for string of length 1")
    (Invalid_argument
      "Digit_string_helpers.write_1_digit_int: -1 out of range [0, 9]")
    (Invalid_argument
      "Digit_string_helpers.write_1_digit_int: 10 out of range [0, 9]")
    |}]
;;

let%expect_test "write_2_digit_int" =
  test_write_int write_2_digit_int ~digits:2;
  [%expect
    {|
    Expecting success:
     0 -> "00"
    99 -> "99"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_2_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_2_digit_int: pos=-1 out of range for string of length 2")
    (Invalid_argument
      "Digit_string_helpers.write_2_digit_int: 2 digits do not fit at pos 1 in string of length 2")
    (Invalid_argument
      "Digit_string_helpers.write_2_digit_int: -1 out of range [0, 99]")
    (Invalid_argument
      "Digit_string_helpers.write_2_digit_int: 100 out of range [0, 99]")
    |}]
;;

let%expect_test "write_3_digit_int" =
  test_write_int write_3_digit_int ~digits:3;
  [%expect
    {|
    Expecting success:
      0 -> "000"
    999 -> "999"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_3_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_3_digit_int: pos=-1 out of range for string of length 3")
    (Invalid_argument
      "Digit_string_helpers.write_3_digit_int: 3 digits do not fit at pos 1 in string of length 3")
    (Invalid_argument
      "Digit_string_helpers.write_3_digit_int: -1 out of range [0, 999]")
    (Invalid_argument
      "Digit_string_helpers.write_3_digit_int: 1000 out of range [0, 999]")
    |}]
;;

let%expect_test "write_4_digit_int" =
  test_write_int write_4_digit_int ~digits:4;
  [%expect
    {|
    Expecting success:
       0 -> "0000"
    9999 -> "9999"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_4_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_4_digit_int: pos=-1 out of range for string of length 4")
    (Invalid_argument
      "Digit_string_helpers.write_4_digit_int: 4 digits do not fit at pos 1 in string of length 4")
    (Invalid_argument
      "Digit_string_helpers.write_4_digit_int: -1 out of range [0, 9999]")
    (Invalid_argument
      "Digit_string_helpers.write_4_digit_int: 10000 out of range [0, 9999]")
    |}]
;;

let%expect_test "write_5_digit_int" =
  test_write_int write_5_digit_int ~digits:5;
  [%expect
    {|
    Expecting success:
        0 -> "00000"
    99999 -> "99999"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_5_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_5_digit_int: pos=-1 out of range for string of length 5")
    (Invalid_argument
      "Digit_string_helpers.write_5_digit_int: 5 digits do not fit at pos 1 in string of length 5")
    (Invalid_argument
      "Digit_string_helpers.write_5_digit_int: -1 out of range [0, 99999]")
    (Invalid_argument
      "Digit_string_helpers.write_5_digit_int: 100000 out of range [0, 99999]")
    |}]
;;

let%expect_test "write_6_digit_int" =
  test_write_int write_6_digit_int ~digits:6;
  [%expect
    {|
    Expecting success:
         0 -> "000000"
    999999 -> "999999"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_6_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_6_digit_int: pos=-1 out of range for string of length 6")
    (Invalid_argument
      "Digit_string_helpers.write_6_digit_int: 6 digits do not fit at pos 1 in string of length 6")
    (Invalid_argument
      "Digit_string_helpers.write_6_digit_int: -1 out of range [0, 999999]")
    (Invalid_argument
      "Digit_string_helpers.write_6_digit_int: 1000000 out of range [0, 999999]")
    |}]
;;

let%expect_test "write_7_digit_int" =
  test_write_int write_7_digit_int ~digits:7;
  [%expect
    {|
    Expecting success:
          0 -> "0000000"
    9999999 -> "9999999"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_7_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_7_digit_int: pos=-1 out of range for string of length 7")
    (Invalid_argument
      "Digit_string_helpers.write_7_digit_int: 7 digits do not fit at pos 1 in string of length 7")
    (Invalid_argument
      "Digit_string_helpers.write_7_digit_int: -1 out of range [0, 9999999]")
    (Invalid_argument
      "Digit_string_helpers.write_7_digit_int: 10000000 out of range [0, 9999999]")
    |}]
;;

let%expect_test "write_8_digit_int" =
  test_write_int write_8_digit_int ~digits:8;
  [%expect
    {|
    Expecting success:
           0 -> "00000000"
    99999999 -> "99999999"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_8_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_8_digit_int: pos=-1 out of range for string of length 8")
    (Invalid_argument
      "Digit_string_helpers.write_8_digit_int: 8 digits do not fit at pos 1 in string of length 8")
    (Invalid_argument
      "Digit_string_helpers.write_8_digit_int: -1 out of range [0, 99999999]")
    (Invalid_argument
      "Digit_string_helpers.write_8_digit_int: 100000000 out of range [0, 99999999]")
    |}]
;;

let%expect_test "write_9_digit_int" =
  test_write_int write_9_digit_int ~digits:9;
  [%expect
    {|
    Expecting success:
            0 -> "000000000"
    999999999 -> "999999999"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_9_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_9_digit_int: pos=-1 out of range for string of length 9")
    (Invalid_argument
      "Digit_string_helpers.write_9_digit_int: 9 digits do not fit at pos 1 in string of length 9")
    (Invalid_argument
      "Digit_string_helpers.write_9_digit_int: -1 out of range [0, 999999999]")
    (Invalid_argument
      "Digit_string_helpers.write_9_digit_int: 1000000000 out of range [0, 999999999]")
    |}]
;;

let%expect_test "write_int63" =
  for digits = 1 to max_int63_digits do
    require_does_not_raise [%here] (fun () ->
      test_write_int63
        ~verbose:(digits = max_int63_digits)
        ~align:max_int63_digits
        ~digits
        (write_int63 ~digits))
  done;
  [%expect
    {|
                     0 -> "0"
                     9 -> "9"
                     0 -> "00"
                    99 -> "99"
                     0 -> "000"
                   999 -> "999"
                     0 -> "0000"
                  9999 -> "9999"
                     0 -> "00000"
                 99999 -> "99999"
                     0 -> "000000"
                999999 -> "999999"
                     0 -> "0000000"
               9999999 -> "9999999"
                     0 -> "00000000"
              99999999 -> "99999999"
                     0 -> "000000000"
             999999999 -> "999999999"
                     0 -> "0000000000"
            9999999999 -> "9999999999"
                     0 -> "00000000000"
           99999999999 -> "99999999999"
                     0 -> "000000000000"
          999999999999 -> "999999999999"
                     0 -> "0000000000000"
         9999999999999 -> "9999999999999"
                     0 -> "00000000000000"
        99999999999999 -> "99999999999999"
                     0 -> "000000000000000"
       999999999999999 -> "999999999999999"
                     0 -> "0000000000000000"
      9999999999999999 -> "9999999999999999"
                     0 -> "00000000000000000"
     99999999999999999 -> "99999999999999999"
    Expecting success:
                     0 -> "000000000000000000"
    999999999999999999 -> "999999999999999999"

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.write_int63: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.write_int63: pos=-1 out of range for string of length 18")
    (Invalid_argument
      "Digit_string_helpers.write_int63: 18 digits do not fit at pos 1 in string of length 18")
    (Invalid_argument
      "Digit_string_helpers.write_int63: -1 out of range [0, 999999999999999999]")
    (Invalid_argument
      "Digit_string_helpers.write_int63: 1000000000000000000 out of range [0, 999999999999999999]")
    |}];
  (* write more digits than Int63.max_value fills *)
  let bytes = Bytes.make 50 '_' in
  write_int63 bytes ~pos:10 ~digits:30 Int63.max_value;
  print_s [%sexp (bytes : Bytes.t)];
  [%expect {| __________000000000004611686018427387903__________ |}]
;;

let test_read_int63 ?(verbose = true) read_int63 ~digits =
  let print_endline = if verbose then print_endline ?hide_positions:None else ignore in
  let require_does_raise here f =
    match f () with
    | _ -> require_does_raise here ignore
    | exception exn -> print_endline (Exn.to_string exn)
  in
  let max = max_with ~digits in
  print_endline "Expecting success:";
  (* show resulting strings at boundary values *)
  let show int63 =
    let string = sprintf "%0*Ld" digits (Int63.to_int64 int63) in
    let parsed = read_int63 string ~pos:0 in
    printf !"%*S -> %{Int63}\n" (max_int63_digits + 2) string parsed
  in
  show Int63.zero;
  show max;
  (* test success behavior for lots of correct values *)
  let expect_success_exn int63 =
    let string = sprintf "!%0*Ld!" digits (Int63.to_int64 int63) in
    let parsed = read_int63 string ~pos:1 in
    [%test_result: Int63.t] parsed ~expect:int63
  in
  require_does_not_raise [%here] (fun () ->
    Quickcheck.test
      (Int63.gen_log_uniform_incl Int63.zero max)
      ~examples:[ Int63.zero; max ]
      ~sexp_of:Int63.sexp_of_t
      ~f:expect_success_exn);
  (* test failure cases *)
  print_endline "";
  print_endline "Expecting failure:";
  require_does_raise [%here] (fun () -> read_int63 "" ~pos:0);
  require_does_raise [%here] (fun () -> read_int63 (sprintf "%0*Ld" digits 0L) ~pos:(-1));
  require_does_raise [%here] (fun () -> read_int63 (sprintf "%0*Ld" digits 0L) ~pos:1);
  require_does_raise [%here] (fun () -> read_int63 (String.make digits '!') ~pos:0)
;;

let test_read_int read_int ~digits =
  let read string ~pos = Int63.of_int (read_int string ~pos) in
  test_read_int63 read ~digits
;;

let%expect_test "read_1_digit_int" =
  test_read_int read_1_digit_int ~digits:1;
  [%expect
    {|
    Expecting success:
                     "0" -> 0
                     "9" -> 9

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_1_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_1_digit_int: pos=-1 out of range for string of length 1")
    (Invalid_argument
      "Digit_string_helpers.read_1_digit_int: pos=1 out of range for string of length 1")
    (Failure "Char.get_digit_exn '!': not a digit")
    |}]
;;

let%expect_test "read_2_digit_int" =
  test_read_int read_2_digit_int ~digits:2;
  [%expect
    {|
    Expecting success:
                    "00" -> 0
                    "99" -> 99

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_2_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_2_digit_int: pos=-1 out of range for string of length 2")
    (Invalid_argument
      "Digit_string_helpers.read_2_digit_int: 2 digits do not fit at pos 1 in string of length 2")
    (Failure "Char.get_digit_exn '!': not a digit")
    |}]
;;

let%expect_test "read_3_digit_int" =
  test_read_int read_3_digit_int ~digits:3;
  [%expect
    {|
    Expecting success:
                   "000" -> 0
                   "999" -> 999

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_3_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_3_digit_int: pos=-1 out of range for string of length 3")
    (Invalid_argument
      "Digit_string_helpers.read_3_digit_int: 3 digits do not fit at pos 1 in string of length 3")
    (Failure "Char.get_digit_exn '!': not a digit")
    |}]
;;

let%expect_test "read_4_digit_int" =
  test_read_int read_4_digit_int ~digits:4;
  [%expect
    {|
    Expecting success:
                  "0000" -> 0
                  "9999" -> 9999

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_4_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_4_digit_int: pos=-1 out of range for string of length 4")
    (Invalid_argument
      "Digit_string_helpers.read_4_digit_int: 4 digits do not fit at pos 1 in string of length 4")
    (Failure "Char.get_digit_exn '!': not a digit")
    |}]
;;

let%expect_test "read_5_digit_int" =
  test_read_int read_5_digit_int ~digits:5;
  [%expect
    {|
    Expecting success:
                 "00000" -> 0
                 "99999" -> 99999

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_5_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_5_digit_int: pos=-1 out of range for string of length 5")
    (Invalid_argument
      "Digit_string_helpers.read_5_digit_int: 5 digits do not fit at pos 1 in string of length 5")
    (Failure "Char.get_digit_exn '!': not a digit")
    |}]
;;

let%expect_test "read_6_digit_int" =
  test_read_int read_6_digit_int ~digits:6;
  [%expect
    {|
    Expecting success:
                "000000" -> 0
                "999999" -> 999999

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_6_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_6_digit_int: pos=-1 out of range for string of length 6")
    (Invalid_argument
      "Digit_string_helpers.read_6_digit_int: 6 digits do not fit at pos 1 in string of length 6")
    (Failure "Char.get_digit_exn '!': not a digit")
    |}]
;;

let%expect_test "read_7_digit_int" =
  test_read_int read_7_digit_int ~digits:7;
  [%expect
    {|
    Expecting success:
               "0000000" -> 0
               "9999999" -> 9999999

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_7_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_7_digit_int: pos=-1 out of range for string of length 7")
    (Invalid_argument
      "Digit_string_helpers.read_7_digit_int: 7 digits do not fit at pos 1 in string of length 7")
    (Failure "Char.get_digit_exn '!': not a digit")
    |}]
;;

let%expect_test "read_8_digit_int" =
  test_read_int read_8_digit_int ~digits:8;
  [%expect
    {|
    Expecting success:
              "00000000" -> 0
              "99999999" -> 99999999

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_8_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_8_digit_int: pos=-1 out of range for string of length 8")
    (Invalid_argument
      "Digit_string_helpers.read_8_digit_int: 8 digits do not fit at pos 1 in string of length 8")
    (Failure "Char.get_digit_exn '!': not a digit")
    |}]
;;

let%expect_test "read_9_digit_int" =
  test_read_int read_9_digit_int ~digits:9;
  [%expect
    {|
    Expecting success:
             "000000000" -> 0
             "999999999" -> 999999999

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_9_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_9_digit_int: pos=-1 out of range for string of length 9")
    (Invalid_argument
      "Digit_string_helpers.read_9_digit_int: 9 digits do not fit at pos 1 in string of length 9")
    (Failure "Char.get_digit_exn '!': not a digit")
    |}]
;;

let%expect_test "read_int63" =
  for digits = 1 to max_int63_digits do
    require_does_not_raise [%here] (fun () ->
      test_read_int63 ~verbose:(digits = max_int63_digits) ~digits (read_int63 ~digits))
  done;
  [%expect
    {|
                     "0" -> 0
                     "9" -> 9
                    "00" -> 0
                    "99" -> 99
                   "000" -> 0
                   "999" -> 999
                  "0000" -> 0
                  "9999" -> 9999
                 "00000" -> 0
                 "99999" -> 99999
                "000000" -> 0
                "999999" -> 999999
               "0000000" -> 0
               "9999999" -> 9999999
              "00000000" -> 0
              "99999999" -> 99999999
             "000000000" -> 0
             "999999999" -> 999999999
            "0000000000" -> 0
            "9999999999" -> 9999999999
           "00000000000" -> 0
           "99999999999" -> 99999999999
          "000000000000" -> 0
          "999999999999" -> 999999999999
         "0000000000000" -> 0
         "9999999999999" -> 9999999999999
        "00000000000000" -> 0
        "99999999999999" -> 99999999999999
       "000000000000000" -> 0
       "999999999999999" -> 999999999999999
      "0000000000000000" -> 0
      "9999999999999999" -> 9999999999999999
     "00000000000000000" -> 0
     "99999999999999999" -> 99999999999999999
    Expecting success:
    "000000000000000000" -> 0
    "999999999999999999" -> 999999999999999999

    Expecting failure:
    (Invalid_argument
      "Digit_string_helpers.read_int63: pos=0 out of range for string of length 0")
    (Invalid_argument
      "Digit_string_helpers.read_int63: pos=-1 out of range for string of length 18")
    (Invalid_argument
      "Digit_string_helpers.read_int63: 18 digits do not fit at pos 1 in string of length 18")
    (Failure "Char.get_digit_exn '!': not a digit")
    |}];
  (* read more digits than Int63 can represent *)
  let string = String.make 50 '0' in
  let int63 = read_int63 string ~pos:10 ~digits:30 in
  print_s [%sexp (int63 : Int63.t)];
  require_equal [%here] (module Int63) int63 Int63.zero;
  [%expect {| 0 |}];
  (* read Int63.max_value without overflowing *)
  let string = sprintf "%010d%030Ld%010d" 0 (Int63.to_int64 Int63.max_value) 0 in
  let int63 = read_int63 string ~pos:10 ~digits:30 in
  print_s [%sexp (int63 : Int63.t)];
  require_equal [%here] (module Int63) int63 Int63.max_value;
  [%expect {| 4_611_686_018_427_387_903 |}];
  (* raise on overflow *)
  let string = String.make 50 '9' in
  require_does_raise [%here] (fun () -> read_int63 string ~pos:10 ~digits:30);
  [%expect
    {| (Invalid_argument "Digit_string_helpers.read_int63: overflow reading int63") |}]
;;

let require_no_allocation_if_64_bit_and_if_does_not_raise here f =
  match Base.Word_size.word_size with
  | W32 -> f ()
  | W64 ->
    (match f () with
     | exception _ -> f ()
     | _ -> require_no_allocation here f)
;;

let%expect_test "read_int63_decimal" =
  let read string ~pos ~decimals ~scale ~round_ties ~allow_underscore =
    require_no_allocation_if_64_bit_and_if_does_not_raise [%here] (fun () ->
      read_int63_decimal string ~pos ~decimals ~scale ~round_ties ~allow_underscore)
  in
  let test_read_at string ~pos ~decimals ~scale ~round_ties =
    let permissive =
      read string ~pos ~decimals ~scale ~round_ties ~allow_underscore:true
    in
    let restricted =
      Or_error.try_with (fun () ->
        read string ~pos ~decimals ~scale ~round_ties ~allow_underscore:false)
    in
    let has_underscore = String.mem (String.sub string ~pos ~len:decimals) '_' in
    (match restricted with
     | Ok restricted ->
       if has_underscore
       then print_cr [%here] [%message "ignored '_'" ~_:(restricted : Int63.t)]
       else require_equal [%here] (module Int63) permissive restricted
     | Error error ->
       if has_underscore
       then ()
       else print_cr [%here] [%message "failed" ~_:(error : Error.t)]);
    permissive
  in
  let test_read string ~decimals ~scale ~round_ties =
    let pos_0 = test_read_at string ~pos:0 ~decimals ~scale ~round_ties in
    let pos_1 = test_read_at ("!" ^ string ^ "!") ~pos:1 ~decimals ~scale ~round_ties in
    require_equal [%here] (module Int63) pos_0 pos_1;
    pos_0
  in
  let test63 string ~scale =
    let decimals = String.length string in
    let round_pos_inf =
      test_read string ~decimals ~scale ~round_ties:Toward_positive_infinity
    in
    let round_neg_inf =
      test_read string ~decimals ~scale ~round_ties:Toward_negative_infinity
    in
    if Int63.equal round_pos_inf round_neg_inf
    then print_s [%sexp (round_pos_inf : Int63.t)]
    else print_s [%message (round_pos_inf : Int63.t) (round_neg_inf : Int63.t)]
  in
  let test string ~scale =
    let scale = Int63.of_int scale in
    test63 string ~scale
  in
  test ~scale:1 "";
  test ~scale:1 "_";
  test ~scale:1 "0";
  [%expect {|
    0
    0
    0
    |}];
  test ~scale:1 "5";
  test ~scale:1 "500000";
  [%expect
    {|
    ((round_pos_inf 1)
     (round_neg_inf 0))
    ((round_pos_inf 1)
     (round_neg_inf 0))
    |}];
  test ~scale:1 "500001";
  [%expect {| 1 |}];
  test ~scale:1 "499999";
  [%expect {| 0 |}];
  test ~scale:7 "07142857142857142857142857142857142857142857142857";
  test ~scale:7 "071428571428571428571428571428571428571428571428572";
  [%expect {|
    0
    1
    |}];
  test ~scale:60_000 "0";
  [%expect {| 0 |}];
  test ~scale:60_000 "5";
  [%expect {| 30_000 |}];
  test ~scale:60_000 "333_333_333_333";
  test ~scale:60_000 "333333333333";
  [%expect {|
    20_000
    20_000
    |}];
  test ~scale:60_000 "333_341_666";
  test ~scale:60_000 "333_341_667";
  [%expect {|
    20_000
    20_001
    |}];
  test ~scale:60_000 "666_674_999";
  test ~scale:60_000 "666_675_000";
  test ~scale:60_000 "666_675";
  test ~scale:60_000 "666_675_001";
  [%expect
    {|
    40_000
    ((round_pos_inf 40_001)
     (round_neg_inf 40_000))
    ((round_pos_inf 40_001)
     (round_neg_inf 40_000))
    40_001
    |}];
  test ~scale:60_000 "111_111_111_111";
  test ~scale:60_000 "111_111_111";
  test ~scale:60_000 "111_111";
  test ~scale:60_000 "111";
  [%expect {|
    6_667
    6_667
    6_667
    6_660
    |}];
  test ~scale:1_000 "5";
  test ~scale:1_000 "05";
  test ~scale:1_000 "005";
  test ~scale:1_000 "000_5";
  test ~scale:1_000 "000_05";
  [%expect
    {|
    500
    50
    5
    ((round_pos_inf 1)
     (round_neg_inf 0))
    0
    |}];
  test ~scale:1_000 "5";
  test ~scale:1_000 "95";
  test ~scale:1_000 "995";
  test ~scale:1_000 "999_5";
  test ~scale:1_000 "999_95";
  [%expect
    {|
    500
    950
    995
    ((round_pos_inf 1_000)
     (round_neg_inf 999))
    1_000
    |}];
  (* Tests values close to overflow:
     100000000000000000 ~= Int63.max_value / 46 *)
  let big_power_of_ten = Int63.of_string "100000000000000000" in
  print_s [%sexp (Int63.( / ) Int63.max_value big_power_of_ten : Int63.t)];
  [%expect {| 46 |}];
  test63 ~scale:big_power_of_ten "1";
  test63 ~scale:big_power_of_ten "2";
  test63 ~scale:big_power_of_ten "3";
  test63 ~scale:big_power_of_ten "4";
  test63 ~scale:big_power_of_ten "5";
  test63 ~scale:big_power_of_ten "6";
  test63 ~scale:big_power_of_ten "7";
  test63 ~scale:big_power_of_ten "8";
  test63 ~scale:big_power_of_ten "9";
  test63 ~scale:big_power_of_ten "987654321011122235";
  [%expect
    {|
    10_000_000_000_000_000
    20_000_000_000_000_000
    30_000_000_000_000_000
    40_000_000_000_000_000
    50_000_000_000_000_000
    60_000_000_000_000_000
    70_000_000_000_000_000
    80_000_000_000_000_000
    90_000_000_000_000_000
    ((round_pos_inf 98_765_432_101_112_224)
     (round_neg_inf 98_765_432_101_112_223))
    |}];
  (* Tests values close to overflow:
     230584300921369395 = Int63.max_value / 20 *)
  let maximum_scale = Int63.( / ) Int63.max_value (Int63.of_int 20) in
  print_s [%sexp (maximum_scale : Int63.t)];
  [%expect {| 230_584_300_921_369_395 |}];
  test63 ~scale:maximum_scale "";
  test63 ~scale:maximum_scale "5";
  test63 ~scale:maximum_scale "9";
  test63 ~scale:maximum_scale "99";
  test63 ~scale:maximum_scale "999999999";
  test63 ~scale:maximum_scale "99999999999999999";
  test63 ~scale:maximum_scale "999999999999999999";
  [%expect
    {|
    0
    ((round_pos_inf 115_292_150_460_684_698)
     (round_neg_inf 115_292_150_460_684_697))
    ((round_pos_inf 207_525_870_829_232_456)
     (round_neg_inf 207_525_870_829_232_455))
    228_278_457_912_155_701
    230_584_300_690_785_094
    230_584_300_921_369_393
    230_584_300_921_369_395
    |}];
  let test_failure
    ?(pos = 0)
    ?decimals
    ?(scale = Int63.of_int 60_000)
    ?(round_ties = Round.Toward_positive_infinity)
    ?(allow_underscore = true)
    string
    =
    let decimals = Option.value decimals ~default:(String.length string - pos) in
    require_does_raise [%here] (fun () : Int63.t ->
      read_int63_decimal string ~pos ~decimals ~scale ~round_ties ~allow_underscore)
  in
  test_failure "not a decimal string at all";
  [%expect
    {|
    (Invalid_argument
     "Digit_string_helpers.read_int63_decimal: invalid decimal character")
    |}];
  test_failure "000_000" ~allow_underscore:false;
  [%expect
    {|
    (Invalid_argument
     "Digit_string_helpers.read_int63_decimal: invalid decimal character")
    |}];
  test_failure "0" ~pos:(-1);
  [%expect
    {|
    (Invalid_argument
     "Digit_string_helpers.read_int63_decimal: pos=-1 out of range for string of length 1")
    |}];
  test_failure "0" ~pos:100 ~decimals:0;
  [%expect
    {|
    (Invalid_argument
     "Digit_string_helpers.read_int63_decimal: pos=100 out of range for string of length 1")
    |}];
  test_failure "0" ~decimals:(-1);
  [%expect
    {|
    (Invalid_argument
     "Digit_string_helpers.read_int63_decimal: decimals=-1 is negative")
    |}];
  test_failure "0" ~decimals:100;
  [%expect
    {|
    (Invalid_argument
     "Digit_string_helpers.read_int63_decimal: 100 digits do not fit at pos 0 in string of length 1")
    |}];
  test_failure "0" ~scale:Int63.zero;
  [%expect
    {|
    (Invalid_argument
     "Digit_string_helpers.read_int63_decimal: scale=0 out of range [1, 230584300921369395]")
    |}];
  test_failure "0" ~scale:(Int63.of_string "1000000000000000000");
  [%expect
    {|
    (Invalid_argument
     "Digit_string_helpers.read_int63_decimal: scale=1000000000000000000 out of range [1, 230584300921369395]")
    |}];
  test_failure "0" ~scale:Int63.max_value;
  [%expect
    {|
    (Invalid_argument
     "Digit_string_helpers.read_int63_decimal: scale=4611686018427387903 out of range [1, 230584300921369395]")
    |}]
;;
