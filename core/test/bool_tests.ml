open! Core
open Expect_test_helpers_core

let%test_module "[of_string_hum]" =
  (module struct
    let variations_of_string string =
      String.Set.of_list
        [ string
        ; String.lowercase string
        ; String.uppercase string
        ; String.capitalize (String.lowercase string)
        ; String.uncapitalize (String.uppercase string)
        ; String.lowercase (String.prefix string 1)
        ; String.uppercase (String.prefix string 1)
        ]
    ;;

    let%expect_test "success" =
      let test string ~expect =
        (* skip memoized allocation *)
        ignore (Or_error.try_with (fun () -> Bool.of_string_hum string) : bool Or_error.t);
        Set.iter (variations_of_string string) ~f:(fun string ->
          match require_no_allocation [%here] (fun () -> Bool.of_string_hum string) with
          | actual -> require_equal [%here] (module Bool) actual expect
          | exception exn ->
            print_cr [%here] [%message "raised" (string : string) (exn : exn)])
      in
      List.iter ~f:(test ~expect:true) [ "true"; "yes"; "1" ];
      [%expect {| |}];
      List.iter ~f:(test ~expect:false) [ "false"; "no"; "0" ];
      [%expect {| |}]
    ;;

    let%expect_test "failure" =
      (* print the exception once *)
      require_does_raise [%here] (fun () -> Bool.of_string_hum "");
      [%expect
        {|
        ("Bool.of_string_hum: invalid input"
          (input "")
          (expected_case_insensitive (0 1 f false n no t true y yes)))
        |}];
      let test string =
        Set.iter (variations_of_string string) ~f:(fun string ->
          (* the single-character prefixes might work, so we skip them *)
          if String.length string > 1
          then (
            match Bool.of_string_hum string with
            | bool ->
              print_cr [%here] [%message "did not raise" (string : string) (bool : bool)]
            | exception _ -> ()))
      in
      List.iter ~f:test [ "abc"; "tru"; "truth"; "fals"; "falsey"; "0x0"; "01"; "123" ];
      [%expect {| |}]
    ;;
  end)
;;
