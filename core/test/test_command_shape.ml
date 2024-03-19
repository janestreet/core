open! Core
open! Command.Shape

module Entry = struct
  type t =
    { name_length : int
    ; doc_length : int
    ; aliases : string list
    }

  let create ?(aliases = []) name_length doc_length = { name_length; doc_length; aliases }

  let generate_string ~spaces length =
    List.init length ~f:(fun x -> if x % 10 = 0 && spaces then ' ' else 'a')
    |> String.of_char_list
  ;;

  let to_flag_info ~idx { name_length; doc_length; aliases } =
    let prefix = sprintf "%d-" idx in
    let name_suffix_length = name_length - String.length prefix in
    let name = generate_string name_suffix_length ~spaces:false in
    let doc = generate_string doc_length ~spaces:true in
    { Flag_info.name = prefix ^ name; doc; aliases }
  ;;
end

let display entries =
  List.mapi entries ~f:(fun idx entry -> Entry.to_flag_info entry ~idx)
  |> Flag_help_display.to_string
  |> print_endline
;;

let%expect_test "somewhat typical flag arrangement" =
  display
    [ Entry.create 7 10
    ; Entry.create 17 100 ~aliases:[ "hi" ]
    ; Entry.create 27 30
    ; Entry.create 37 40
    ];
  [%expect
    {|
    0-aaaaa                    . aaaaaaaaa
    1-aaaaaaaaaaaaaaa, hi      . aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
                                 aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
    2-aaaaaaaaaaaaaaaaaaaaaaaaa
                               . aaaaaaaaa aaaaaaaaa aaaaaaaaa
    3-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                               . aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
    |}]
;;

let%expect_test "one flag with lots of doc" =
  display
    [ Entry.create 26 400
    ; Entry.create 3 5
    ; Entry.create 15 50 ~aliases:[ "something1"; "alias2" ]
    ];
  [%expect
    {|
    0-aaaaaaaaaaaaaaaaaaaaaaaa . aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
                                 aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
                                 aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
                                 aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
                                 aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
                                 aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
                                 aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
                                 aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
    1-a                        . aaaa
    2-aaaaaaaaaaaaa, something1, alias2
                               . aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
    |}]
;;

let%expect_test "only one long flag" =
  display
    [ Entry.create 66 400
    ; Entry.create 3 5 ~aliases:[ "an-alias"; "another-alias" ]
    ; Entry.create 4 50
    ];
  [%expect
    {|
    0-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                               . aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
                                 aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
                                 aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
                                 aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
                                 aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
                                 aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
                                 aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
                                 aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
    1-a, an-alias, another-alias
                               . aaaa
    2-aa                       . aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa aaaaaaaaa
    |}]
;;

let%expect_test "some have docs some don't" =
  display
    [ Entry.create 20 5
    ; Entry.create 10 0 ~aliases:[ "an-alias"; "another-alias" ]
    ; Entry.create 12 0
    ; Entry.create 3 20
    ];
  [%expect
    {|
    0-aaaaaaaaaaaaaaaaaa       . aaaa
    1-aaaaaaaa, an-alias, another-alias
    2-aaaaaaaaaa               .
    3-a                        . aaaaaaaaa aaaaaaaaa
    |}]
;;
