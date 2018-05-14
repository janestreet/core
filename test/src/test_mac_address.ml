open! Core
open! Mac_address

let%test_module "Mac_address" =
  (module struct

    let gen_hex_char =
      Quickcheck.Generator.of_list
        [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'
        ; 'a'; 'b'; 'c'; 'd'; 'e'; 'f'
        ; 'A'; 'B'; 'C'; 'D'; 'E'; 'F'
        ]

    let gen_dash =
      let open Quickcheck.Generator.Let_syntax in
      let two_hex_chars = String.gen_with_length 2 gen_hex_char in
      let%bind pairs = List.gen_with_length 6 two_hex_chars in
      return (String.concat ~sep:"-" pairs)

    let gen_dotted =
      let open Quickcheck.Generator.Let_syntax in
      let four_hex_chars = String.gen_with_length 4 gen_hex_char in
      let%bind quads = List.gen_with_length 3 four_hex_chars in
      return (String.concat ~sep:"." quads)

    let gen_colon =
      let open Quickcheck.Generator.Let_syntax in
      let two_hex_chars = String.gen_with_length 2 gen_hex_char in
      let%bind pairs = List.gen_with_length 6 two_hex_chars in
      return (String.concat ~sep:":" pairs)

    let seed = `Deterministic "mac tests"
    let trials = 10_000

    let%test_unit "(of_string . to_string) = id[t -> t]" =
      Quickcheck.test ~seed ~trials gen ~f:(fun expect ->
        let actual = of_string (to_string expect) in
        [%test_result: t] ~expect actual)

    let%test_unit "(of_int63_exn . to_int63) = id[t -> t]" =
      Quickcheck.test ~seed ~trials gen ~f:(fun expect ->
        let actual = of_int63_exn (to_int63 expect) in
        [%test_result: t] ~expect actual)

    let%test_unit "(to_string . of_string) = id[string -> string]" =
      Quickcheck.test ~seed ~trials gen_dash ~f:(fun expect ->
        let actual = to_string_with_style ~style:Dash (of_string expect) in
        [%test_result: string] ~expect:(String.lowercase expect) actual);
      Quickcheck.test ~seed ~trials gen_dotted ~f:(fun expect ->
        let actual = to_string_with_style ~style:Dot (of_string expect) in
        [%test_result: string] ~expect:(String.lowercase expect) actual);
      Quickcheck.test ~seed ~trials gen_colon ~f:(fun expect ->
        let actual = to_string_with_style ~style:Colon (of_string expect) in
        [%test_result: string] ~expect:(String.lowercase expect) actual)

    let%test_unit "broadcast to_string" =
      [%test_result: string] ~expect:"ff-ff-ff-ff-ff-ff"
        (to_string_with_style ~style:Dash broadcast);
      [%test_result: string] ~expect:"ffff.ffff.ffff"
        (to_string_with_style ~style:Dot broadcast);
      [%test_result: string] ~expect:"ff:ff:ff:ff:ff:ff"
        (to_string_with_style ~style:Colon broadcast)

    let%test_unit "any to_string" =
      [%test_result: string] ~expect:"00-00-00-00-00-00"
        (to_string_with_style ~style:Dash any);
      [%test_result: string] ~expect:"0000.0000.0000"
        (to_string_with_style ~style:Dot any);
      [%test_result: string] ~expect:"00:00:00:00:00:00"
        (to_string_with_style ~style:Colon any)
  end)
