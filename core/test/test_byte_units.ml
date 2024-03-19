open! Core
open! Import
open! Byte_units

let bytes_per_word = Float.of_int (Int.( / ) (Word_size.num_bits Word_size.word_size) 8)

let%expect_test ("Byte_units.to_string_hum" [@tags "64-bits-only"]) =
  print_string (Byte_units.to_string_hum (Byte_units.of_bytes_int 1000));
  [%expect {| 1000B |}];
  print_string (Byte_units.to_string_hum (Byte_units.of_bytes_int 1500));
  [%expect {| 1.46K |}]
;;

let%test_module "{of,to}_string" =
  (module struct
    let f input expected_output =
      let observed_output = to_string (of_string input) in
      let result = String.equal expected_output observed_output in
      if not result
      then eprintf "\n%s -> %s != %s\n%!" input expected_output observed_output;
      result
    ;;

    let%test _ = f "3B" "3B"
    let%test _ = f "3w" (sprintf "%gB" (3.0 *. bytes_per_word))
    let%test _ = f "3K" "3K"
    let%test _ = f "3M" "3M"
    let%test _ = f "3G" "3G"
  end)
;;

let examples =
  [ Byte_units.zero
  ; Byte_units.of_bytes_int 1
  ; Byte_units.of_bytes_int 10
  ; Byte_units.of_bytes_int 100
  ; Byte_units.of_bytes_int 1000
  ; Byte_units.of_bytes_int 10000
  ; Byte_units.of_bytes_int 100000
  ; Byte_units.of_bytes_int 1000000
  ; Byte_units.of_kilobytes 0.1
  ; Byte_units.of_bytes_int 1023
  ; Byte_units.of_kilobytes 1.
  ; Byte_units.of_kilobytes 1.0499
  ; Byte_units.of_kilobytes 1.1
  ; Byte_units.of_kilobytes 10.
  ; Byte_units.of_kilobytes 99.99
  ; Byte_units.of_kilobytes 100.
  ; Byte_units.of_megabytes 1.
  ; Byte_units.of_megabytes 10.
  ; Byte_units.of_megabytes 100.
  ; Byte_units.of_gigabytes 1.
  ; Byte_units.of_gigabytes 10.
  ; Byte_units.of_gigabytes 100.
  ; Byte_units.of_terabytes 1.
  ; Byte_units.of_terabytes 10.
  ; Byte_units.of_terabytes 100.
  ; Byte_units.of_petabytes 1.
  ; Byte_units.of_petabytes 10.
  ; Byte_units.of_petabytes 100.
  ; Byte_units.of_exabytes 1.
  ; Byte_units.of_bytes_int (-1)
  ; Byte_units.of_bytes_int (-10000)
  ; Byte_units.of_bytes_int (-10000000)
  ]
;;

let%expect_test "[List.sum] to ensure [Byte_units] satisfies [Container_intf.Summable]" =
  print_s [%sexp (List.sum (module Byte_units) examples ~f:Fn.id : t)];
  [%expect {| 1190247535.1G |}]
;;

let print_all_byte_units fmt =
  List.iter examples ~f:(fun n ->
    printf !"%{sexp:Byte_units.Stable.V2.t} -> %s\n" n (sprintf fmt n))
;;

let%expect_test ("Byte_units.to_string" [@tags "no-js"]) =
  (* disabling test in javascript as it has subtly different rounding breaking this test.

     See https://github.com/ocsigen/js_of_ocaml/issues/796. *)
  print_all_byte_units !"%{Byte_units}";
  [%expect
    {|
    (Bytes 0) -> 0B
    (Bytes 1) -> 1B
    (Bytes 10) -> 10B
    (Bytes 100) -> 100B
    (Bytes 1_000) -> 1000B
    (Bytes 10_000) -> 9.765625K
    (Bytes 100_000) -> 97.65625K
    (Bytes 1_000_000) -> 976.5625K
    (Bytes 102) -> 102B
    (Bytes 1_023) -> 1023B
    (Bytes 1_024) -> 1K
    (Bytes 1_075) -> 1.049805K
    (Bytes 1_126) -> 1.099609K
    (Bytes 10_240) -> 10K
    (Bytes 102_390) -> 99.99023K
    (Bytes 102_400) -> 100K
    (Bytes 1_048_576) -> 1M
    (Bytes 10_485_760) -> 10M
    (Bytes 104_857_600) -> 100M
    (Bytes 1_073_741_824) -> 1G
    (Bytes 10_737_418_240) -> 10G
    (Bytes 107_374_182_400) -> 100G
    (Bytes 1_099_511_627_776) -> 1024G
    (Bytes 10_995_116_277_760) -> 10240G
    (Bytes 109_951_162_777_600) -> 102400G
    (Bytes 1_125_899_906_842_624) -> 1048576G
    (Bytes 11_258_999_068_426_240) -> 10485760G
    (Bytes 112_589_990_684_262_400) -> 104857600G
    (Bytes 1_152_921_504_606_846_976) -> 1073741824G
    (Bytes -1) -> -1B
    (Bytes -10_000) -> -9.765625K
    (Bytes -10_000_000) -> -9.536743164M
    |}]
;;

module Byte_units_with_to_string_hum = struct
  include Byte_units

  let to_string = to_string_hum
end

let%expect_test _ =
  print_all_byte_units !"%{Byte_units_with_to_string_hum}";
  [%expect
    {|
    (Bytes 0) -> 0B
    (Bytes 1) -> 1B
    (Bytes 10) -> 10B
    (Bytes 100) -> 100B
    (Bytes 1_000) -> 1000B
    (Bytes 10_000) -> 9.77K
    (Bytes 100_000) -> 97.7K
    (Bytes 1_000_000) -> 977K
    (Bytes 102) -> 102B
    (Bytes 1_023) -> 1023B
    (Bytes 1_024) -> 1K
    (Bytes 1_075) -> 1.05K
    (Bytes 1_126) -> 1.1K
    (Bytes 10_240) -> 10K
    (Bytes 102_390) -> 100K
    (Bytes 102_400) -> 100K
    (Bytes 1_048_576) -> 1M
    (Bytes 10_485_760) -> 10M
    (Bytes 104_857_600) -> 100M
    (Bytes 1_073_741_824) -> 1G
    (Bytes 10_737_418_240) -> 10G
    (Bytes 107_374_182_400) -> 100G
    (Bytes 1_099_511_627_776) -> 1T
    (Bytes 10_995_116_277_760) -> 10T
    (Bytes 109_951_162_777_600) -> 100T
    (Bytes 1_125_899_906_842_624) -> 1P
    (Bytes 11_258_999_068_426_240) -> 10P
    (Bytes 112_589_990_684_262_400) -> 100P
    (Bytes 1_152_921_504_606_846_976) -> 1024P
    (Bytes -1) -> -1B
    (Bytes -10_000) -> -9.77K
    (Bytes -10_000_000) -> -9.54M
    |}]
;;

let%expect_test "Byte_units.to_string_short" =
  print_all_byte_units !"%{Byte_units.Short}";
  [%expect
    {|
    (Bytes 0) -> 0B
    (Bytes 1) -> 1B
    (Bytes 10) -> 10B
    (Bytes 100) -> 100B
    (Bytes 1_000) -> 1000B
    (Bytes 10_000) -> 9.77K
    (Bytes 100_000) -> 97.7K
    (Bytes 1_000_000) -> 977K
    (Bytes 102) -> 102B
    (Bytes 1_023) -> 1023B
    (Bytes 1_024) -> 1.00K
    (Bytes 1_075) -> 1.05K
    (Bytes 1_126) -> 1.10K
    (Bytes 10_240) -> 10.0K
    (Bytes 102_390) -> 100.0K
    (Bytes 102_400) -> 100K
    (Bytes 1_048_576) -> 1.00M
    (Bytes 10_485_760) -> 10.0M
    (Bytes 104_857_600) -> 100M
    (Bytes 1_073_741_824) -> 1.00G
    (Bytes 10_737_418_240) -> 10.0G
    (Bytes 107_374_182_400) -> 100G
    (Bytes 1_099_511_627_776) -> 1.00T
    (Bytes 10_995_116_277_760) -> 10.0T
    (Bytes 109_951_162_777_600) -> 100T
    (Bytes 1_125_899_906_842_624) -> 1.00P
    (Bytes 11_258_999_068_426_240) -> 10.0P
    (Bytes 112_589_990_684_262_400) -> 100P
    (Bytes 1_152_921_504_606_846_976) -> 1.00E
    (Bytes -1) -> -1B
    (Bytes -10_000) -> -9.77K
    (Bytes -10_000_000) -> -9.54M
    |}]
;;

let%expect_test ("Byte_units.sexp_of_t" [@tags "no-js"]) =
  (* disabling test in javascript as it has subtly different rounding breaking this test.

     See https://github.com/ocsigen/js_of_ocaml/issues/796. *)
  print_all_byte_units !"%{sexp:Byte_units.t}";
  [%expect
    {|
    (Bytes 0) -> 0B
    (Bytes 1) -> 1B
    (Bytes 10) -> 10B
    (Bytes 100) -> 100B
    (Bytes 1_000) -> 1000B
    (Bytes 10_000) -> 9.765625K
    (Bytes 100_000) -> 97.65625K
    (Bytes 1_000_000) -> 976.5625K
    (Bytes 102) -> 102B
    (Bytes 1_023) -> 1023B
    (Bytes 1_024) -> 1K
    (Bytes 1_075) -> 1.049805K
    (Bytes 1_126) -> 1.099609K
    (Bytes 10_240) -> 10K
    (Bytes 102_390) -> 99.99023K
    (Bytes 102_400) -> 100K
    (Bytes 1_048_576) -> 1M
    (Bytes 10_485_760) -> 10M
    (Bytes 104_857_600) -> 100M
    (Bytes 1_073_741_824) -> 1G
    (Bytes 10_737_418_240) -> 10G
    (Bytes 107_374_182_400) -> 100G
    (Bytes 1_099_511_627_776) -> 1024G
    (Bytes 10_995_116_277_760) -> 10240G
    (Bytes 109_951_162_777_600) -> 102400G
    (Bytes 1_125_899_906_842_624) -> 1048576G
    (Bytes 11_258_999_068_426_240) -> 10485760G
    (Bytes 112_589_990_684_262_400) -> 104857600G
    (Bytes 1_152_921_504_606_846_976) -> 1073741824G
    (Bytes -1) -> -1B
    (Bytes -10_000) -> -9.765625K
    (Bytes -10_000_000) -> -9.536743164M
    |}]
;;

let%expect_test "Byte_units.Stable.V1.sexp_of_t" =
  print_all_byte_units !"%{sexp:Byte_units.Stable.V1.t}";
  [%expect
    {|
    (Bytes 0) -> (Bytes 0)
    (Bytes 1) -> (Bytes 1)
    (Bytes 10) -> (Bytes 10)
    (Bytes 100) -> (Bytes 100)
    (Bytes 1_000) -> (Bytes 1000)
    (Bytes 10_000) -> (Kilobytes 9.765625)
    (Bytes 100_000) -> (Kilobytes 97.65625)
    (Bytes 1_000_000) -> (Kilobytes 976.5625)
    (Bytes 102) -> (Bytes 102)
    (Bytes 1_023) -> (Bytes 1023)
    (Bytes 1_024) -> (Kilobytes 1)
    (Bytes 1_075) -> (Kilobytes 1.0498046875)
    (Bytes 1_126) -> (Kilobytes 1.099609375)
    (Bytes 10_240) -> (Kilobytes 10)
    (Bytes 102_390) -> (Kilobytes 99.990234375)
    (Bytes 102_400) -> (Kilobytes 100)
    (Bytes 1_048_576) -> (Megabytes 1)
    (Bytes 10_485_760) -> (Megabytes 10)
    (Bytes 104_857_600) -> (Megabytes 100)
    (Bytes 1_073_741_824) -> (Gigabytes 1)
    (Bytes 10_737_418_240) -> (Gigabytes 10)
    (Bytes 107_374_182_400) -> (Gigabytes 100)
    (Bytes 1_099_511_627_776) -> (Gigabytes 1024)
    (Bytes 10_995_116_277_760) -> (Gigabytes 10240)
    (Bytes 109_951_162_777_600) -> (Gigabytes 102400)
    (Bytes 1_125_899_906_842_624) -> (Gigabytes 1048576)
    (Bytes 11_258_999_068_426_240) -> (Gigabytes 10485760)
    (Bytes 112_589_990_684_262_400) -> (Gigabytes 104857600)
    (Bytes 1_152_921_504_606_846_976) -> (Gigabytes 1073741824)
    (Bytes -1) -> (Bytes -1)
    (Bytes -10_000) -> (Kilobytes -9.765625)
    (Bytes -10_000_000) -> (Megabytes -9.5367431640625)
    |}]
;;

let%expect_test "Byte_units.Stable.V2.sexp_of_t" =
  print_all_byte_units !"%{sexp:Byte_units.Stable.V2.t}";
  [%expect
    {|
    (Bytes 0) -> (Bytes 0)
    (Bytes 1) -> (Bytes 1)
    (Bytes 10) -> (Bytes 10)
    (Bytes 100) -> (Bytes 100)
    (Bytes 1_000) -> (Bytes 1_000)
    (Bytes 10_000) -> (Bytes 10_000)
    (Bytes 100_000) -> (Bytes 100_000)
    (Bytes 1_000_000) -> (Bytes 1_000_000)
    (Bytes 102) -> (Bytes 102)
    (Bytes 1_023) -> (Bytes 1_023)
    (Bytes 1_024) -> (Bytes 1_024)
    (Bytes 1_075) -> (Bytes 1_075)
    (Bytes 1_126) -> (Bytes 1_126)
    (Bytes 10_240) -> (Bytes 10_240)
    (Bytes 102_390) -> (Bytes 102_390)
    (Bytes 102_400) -> (Bytes 102_400)
    (Bytes 1_048_576) -> (Bytes 1_048_576)
    (Bytes 10_485_760) -> (Bytes 10_485_760)
    (Bytes 104_857_600) -> (Bytes 104_857_600)
    (Bytes 1_073_741_824) -> (Bytes 1_073_741_824)
    (Bytes 10_737_418_240) -> (Bytes 10_737_418_240)
    (Bytes 107_374_182_400) -> (Bytes 107_374_182_400)
    (Bytes 1_099_511_627_776) -> (Bytes 1_099_511_627_776)
    (Bytes 10_995_116_277_760) -> (Bytes 10_995_116_277_760)
    (Bytes 109_951_162_777_600) -> (Bytes 109_951_162_777_600)
    (Bytes 1_125_899_906_842_624) -> (Bytes 1_125_899_906_842_624)
    (Bytes 11_258_999_068_426_240) -> (Bytes 11_258_999_068_426_240)
    (Bytes 112_589_990_684_262_400) -> (Bytes 112_589_990_684_262_400)
    (Bytes 1_152_921_504_606_846_976) -> (Bytes 1_152_921_504_606_846_976)
    (Bytes -1) -> (Bytes -1)
    (Bytes -10_000) -> (Bytes -10_000)
    (Bytes -10_000_000) -> (Bytes -10_000_000)
    |}]
;;

let print_all_of_string of_string =
  List.iter ~f:(fun str ->
    match of_string str with
    | t -> printf !"%s -> %{sexp:Byte_units.Stable.V2.t}\n" str t
    | exception exn -> printf !"%s -> ERROR %{Exn}\n" str exn)
;;

let print_all_of_sexp of_sexp =
  print_all_of_string (fun str -> Sexp.of_string_conv_exn str of_sexp)
;;

let%expect_test "Byte_units.of_string" =
  print_all_of_string
    Byte_units.of_string
    [ "1b"; "100000b"; "0.1k"; "1k"; "1.1k"; "1m"; "10g"; "100t"; "10p"; "1e" ];
  [%expect
    {|
    1b -> (Bytes 1)
    100000b -> (Bytes 100_000)
    0.1k -> (Bytes 102)
    1k -> (Bytes 1_024)
    1.1k -> (Bytes 1_126)
    1m -> (Bytes 1_048_576)
    10g -> (Bytes 10_737_418_240)
    100t -> (Bytes 109_951_162_777_600)
    10p -> (Bytes 11_258_999_068_426_240)
    1e -> (Bytes 1_152_921_504_606_846_976)
    |}]
;;

let%expect_test "Byte_units.Stable.V1.t_of_sexp" =
  print_all_of_sexp
    Byte_units.Stable.V1.t_of_sexp
    [ "1b"
    ; "100b"
    ; "1.5k"
    ; "12m"
    ; "123g"
    ; "1.234t"
    ; "0.5p"
    ; "1.9e"
    ; "(Bytes 1)"
    ; "(Bytes 10.1)"
    ; "(Kilobytes 1.1)"
    ; "(Kilobytes 10)"
    ; "(Megabytes 100)"
    ; "(Gigabytes 1000)"
    ];
  [%expect
    {|
    1b -> (Bytes 1)
    100b -> (Bytes 100)
    1.5k -> (Bytes 1_536)
    12m -> (Bytes 12_582_912)
    123g -> (Bytes 132_070_244_352)
    1.234t -> (Bytes 1_356_797_348_676)
    0.5p -> (Bytes 562_949_953_421_312)
    1.9e -> (Bytes 2_190_550_858_753_009_152)
    (Bytes 1) -> (Bytes 1)
    (Bytes 10.1) -> (Bytes 10)
    (Kilobytes 1.1) -> (Bytes 1_126)
    (Kilobytes 10) -> (Bytes 10_240)
    (Megabytes 100) -> (Bytes 104_857_600)
    (Gigabytes 1000) -> (Bytes 1_073_741_824_000)
    |}]
;;

let%expect_test "Byte_units.Stable.V2.t_of_sexp" =
  print_all_of_sexp
    Byte_units.Stable.V2.t_of_sexp
    [ "(Bytes 1)"; "(Bytes 10)"; "(Bytes 100)" ];
  [%expect
    {|
    (Bytes 1) -> (Bytes 1)
    (Bytes 10) -> (Bytes 10)
    (Bytes 100) -> (Bytes 100)
    |}]
;;

let%expect_test "Byte_units.Stable.V1" =
  print_and_check_stable_type [%here] (module Byte_units.Stable.V1) examples;
  [%expect
    {|
    (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
    ((sexp (Bytes 0)) (bin_io "\000\000\000\000\000\000\000\000"))
    ((sexp (Bytes 1)) (bin_io "\000\000\000\000\000\000\240?"))
    ((sexp (Bytes 10)) (bin_io "\000\000\000\000\000\000$@"))
    ((sexp (Bytes 100)) (bin_io "\000\000\000\000\000\000Y@"))
    ((sexp (Bytes 1000)) (bin_io "\000\000\000\000\000@\143@"))
    ((sexp (Kilobytes 9.765625)) (bin_io "\000\000\000\000\000\136\195@"))
    ((sexp (Kilobytes 97.65625)) (bin_io "\000\000\000\000\000j\248@"))
    ((sexp (Kilobytes 976.5625)) (bin_io "\000\000\000\000\128\132.A"))
    ((sexp (Bytes 102)) (bin_io "\000\000\000\000\000\128Y@"))
    ((sexp (Bytes 1023)) (bin_io "\000\000\000\000\000\248\143@"))
    ((sexp (Kilobytes 1)) (bin_io "\000\000\000\000\000\000\144@"))
    ((sexp (Kilobytes 1.0498046875)) (bin_io "\000\000\000\000\000\204\144@"))
    ((sexp (Kilobytes 1.099609375)) (bin_io "\000\000\000\000\000\152\145@"))
    ((sexp (Kilobytes 10)) (bin_io "\000\000\000\000\000\000\196@"))
    ((sexp (Kilobytes 99.990234375)) (bin_io "\000\000\000\000`\255\248@"))
    ((sexp (Kilobytes 100)) (bin_io "\000\000\000\000\000\000\249@"))
    ((sexp (Megabytes 1)) (bin_io "\000\000\000\000\000\0000A"))
    ((sexp (Megabytes 10)) (bin_io "\000\000\000\000\000\000dA"))
    ((sexp (Megabytes 100)) (bin_io "\000\000\000\000\000\000\153A"))
    ((sexp (Gigabytes 1)) (bin_io "\000\000\000\000\000\000\208A"))
    ((sexp (Gigabytes 10)) (bin_io "\000\000\000\000\000\000\004B"))
    ((sexp (Gigabytes 100)) (bin_io "\000\000\000\000\000\0009B"))
    ((sexp (Gigabytes 1024)) (bin_io "\000\000\000\000\000\000pB"))
    ((sexp (Gigabytes 10240)) (bin_io "\000\000\000\000\000\000\164B"))
    ((sexp (Gigabytes 102400)) (bin_io "\000\000\000\000\000\000\217B"))
    ((sexp (Gigabytes 1048576)) (bin_io "\000\000\000\000\000\000\016C"))
    ((sexp (Gigabytes 10485760)) (bin_io "\000\000\000\000\000\000DC"))
    ((sexp (Gigabytes 104857600)) (bin_io "\000\000\000\000\000\000yC"))
    ((sexp (Gigabytes 1073741824)) (bin_io "\000\000\000\000\000\000\176C"))
    ((sexp (Bytes -1)) (bin_io "\000\000\000\000\000\000\240\191"))
    ((sexp (Kilobytes -9.765625)) (bin_io "\000\000\000\000\000\136\195\192"))
    ((sexp (Megabytes -9.5367431640625)) (bin_io "\000\000\000\000\208\018c\193"))
    |}]
;;

let%expect_test "Byte_units.Stable.V2" =
  print_and_check_stable_type [%here] (module Byte_units.Stable.V2) examples;
  [%expect
    {|
    (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
    ((sexp (Bytes 0)) (bin_io "\000"))
    ((sexp (Bytes 1)) (bin_io "\001"))
    ((sexp (Bytes 10)) (bin_io "\n"))
    ((sexp (Bytes 100)) (bin_io d))
    ((sexp (Bytes 1_000)) (bin_io "\254\232\003"))
    ((sexp (Bytes 10_000)) (bin_io "\254\016'"))
    ((sexp (Bytes 100_000)) (bin_io "\253\160\134\001\000"))
    ((sexp (Bytes 1_000_000)) (bin_io "\253@B\015\000"))
    ((sexp (Bytes 102)) (bin_io f))
    ((sexp (Bytes 1_023)) (bin_io "\254\255\003"))
    ((sexp (Bytes 1_024)) (bin_io "\254\000\004"))
    ((sexp (Bytes 1_075)) (bin_io "\2543\004"))
    ((sexp (Bytes 1_126)) (bin_io "\254f\004"))
    ((sexp (Bytes 10_240)) (bin_io "\254\000("))
    ((sexp (Bytes 102_390)) (bin_io "\253\246\143\001\000"))
    ((sexp (Bytes 102_400)) (bin_io "\253\000\144\001\000"))
    ((sexp (Bytes 1_048_576)) (bin_io "\253\000\000\016\000"))
    ((sexp (Bytes 10_485_760)) (bin_io "\253\000\000\160\000"))
    ((sexp (Bytes 104_857_600)) (bin_io "\253\000\000@\006"))
    ((sexp (Bytes 1_073_741_824)) (bin_io "\253\000\000\000@"))
    ((sexp (Bytes 10_737_418_240))
     (bin_io "\252\000\000\000\128\002\000\000\000"))
    ((sexp (Bytes 107_374_182_400))
     (bin_io "\252\000\000\000\000\025\000\000\000"))
    ((sexp (Bytes 1_099_511_627_776))
     (bin_io "\252\000\000\000\000\000\001\000\000"))
    ((sexp (Bytes 10_995_116_277_760))
     (bin_io "\252\000\000\000\000\000\n\000\000"))
    ((sexp (Bytes 109_951_162_777_600))
     (bin_io "\252\000\000\000\000\000d\000\000"))
    ((sexp (Bytes 1_125_899_906_842_624))
     (bin_io "\252\000\000\000\000\000\000\004\000"))
    ((sexp (Bytes 11_258_999_068_426_240))
     (bin_io "\252\000\000\000\000\000\000(\000"))
    ((sexp (Bytes 112_589_990_684_262_400))
     (bin_io "\252\000\000\000\000\000\000\144\001"))
    ((sexp (Bytes 1_152_921_504_606_846_976))
     (bin_io "\252\000\000\000\000\000\000\000\016"))
    ((sexp (Bytes -1)) (bin_io "\255\255"))
    ((sexp (Bytes -10_000)) (bin_io "\254\240\216"))
    ((sexp (Bytes -10_000_000)) (bin_io "\253\128ig\255"))
    |}]
;;

(** Helper to ensure that our conversion functions round trip. *)
let ensure_round_trippable
  (type via)
  ?(max = Int63.max_value |> Int63.to_int64)
  (to_ : Byte_units.t -> via)
  (from : via -> Byte_units.t)
  ~tolerance
  =
  let equal a b =
    let open Float.Robustly_comparable in
    match tolerance with
    | `Zero -> [%compare.equal: Byte_units.t] a b
    | `Epsilon ->
      let a = Byte_units.bytes_float a in
      let b = Byte_units.bytes_float b in
      (b <=. a +. Float.one_ulp `Up a || b -. Float.one_ulp `Down b <=. a)
      && (a <=. b +. Float.one_ulp `Up b || a -. Float.one_ulp `Down a <=. b)
    | `Three_dp ->
      let a = Byte_units.bytes_float a in
      let b = Byte_units.bytes_float b in
      (a <=. b *. 1.005 || a *. 0.995 <=. b) && (b <=. a *. 1.005 || b *. 0.995 <=. a)
  in
  Int63.gen_log_uniform_incl Int63.zero (Int63.of_int64_exn max)
  |> Quickcheck.Generator.map ~f:Byte_units.of_bytes_int63
  |> Quickcheck.test ~f:(fun t ->
       [%test_eq: Byte_units.Stable.V2.t] ~equal t (from (to_ t)))
;;

(** Quick check to excercise various round-trip relations *)

let%test_unit "Byte_units.to_string / Byte_units.of_string" =
  ensure_round_trippable Byte_units.to_string Byte_units.of_string ~tolerance:`Epsilon
;;

let%test_unit "Byte_units.to_string_hum / Byte_units.of_string" =
  ensure_round_trippable Byte_units.to_string_hum Byte_units.of_string ~tolerance:`Epsilon
;;

let%test_unit "Byte_units.to_string / Byte_units.of_string" =
  ensure_round_trippable
    Byte_units.to_string
    Byte_units.of_string
    ~tolerance:`Zero
    ~max:1_000_000_000_000L
;;

let%test_unit "Byte_units.to_string_short / Byte_units.of_string" =
  ensure_round_trippable
    Byte_units.Short.to_string
    Byte_units.of_string
    ~tolerance:`Three_dp
;;

let%test_unit "Byte_units.to_string / Byte_units.Stable.V1.t_of_sexp" =
  ensure_round_trippable
    Byte_units.to_string
    (Fn.flip Sexp.of_string_conv_exn Byte_units.Stable.V1.t_of_sexp)
    ~tolerance:`Epsilon
;;

let%test_unit "Byte_units.to_string_short / Byte_units.Stable.V1.t_of_sexp" =
  ensure_round_trippable
    Byte_units.Short.to_string
    (Fn.flip Sexp.of_string_conv_exn Byte_units.Stable.V1.t_of_sexp)
    ~tolerance:`Three_dp
;;

let%test_unit "Byte_units.to_string / Byte_units.Stable.V2.t_of_sexp" =
  ensure_round_trippable
    Byte_units.to_string
    (Fn.flip Sexp.of_string_conv_exn Byte_units.Stable.V2.t_of_sexp)
    ~tolerance:`Epsilon
;;

let%test_unit "Byte_units.to_string_short / Byte_units.Stable.V2.t_of_sexp" =
  ensure_round_trippable
    Byte_units.Short.to_string
    (Fn.flip Sexp.of_string_conv_exn Byte_units.Stable.V2.t_of_sexp)
    ~tolerance:`Three_dp
;;

let%test_unit "Byte_units.sexp_of_t / Byte_units.Stable.V1.t_of_sexp" =
  ensure_round_trippable
    Byte_units.sexp_of_t
    Byte_units.Stable.V1.t_of_sexp
    ~tolerance:`Epsilon
;;

let%test_unit "Byte_units.sexp_of_t / Byte_units.Stable.V2.t_of_sexp" =
  ensure_round_trippable
    Byte_units.sexp_of_t
    Byte_units.Stable.V2.t_of_sexp
    ~tolerance:`Epsilon
;;

let%test_unit "Byte_units.Stable.V1.sexp_of_t / Byte_units.Stable.V1.t_of_sexp" =
  ensure_round_trippable
    Byte_units.Stable.V1.sexp_of_t
    Byte_units.Stable.V1.t_of_sexp
    ~tolerance:`Epsilon
;;

let%test_unit "Byte_units.Stable.V1.sexp_of_t / Byte_units.Stable.V2.t_of_sexp" =
  ensure_round_trippable
    Byte_units.Stable.V1.sexp_of_t
    Byte_units.Stable.V2.t_of_sexp
    ~tolerance:`Epsilon
;;

let%test_unit "Byte_units.Stable.V2.sexp_of_t / Byte_units.Stable.V1.t_of_sexp" =
  ensure_round_trippable
    Byte_units.Stable.V2.sexp_of_t
    Byte_units.Stable.V1.t_of_sexp
    ~tolerance:`Epsilon
;;

let%test_unit "Byte_units.Stable.V2.sexp_of_t / Byte_units.Stable.V2.t_of_sexp" =
  ensure_round_trippable
    Byte_units.Stable.V2.sexp_of_t
    Byte_units.Stable.V2.t_of_sexp
    ~tolerance:`Zero
;;
