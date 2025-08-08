open! Core

module%test _ = struct
  module Input = struct
    (* Some record, not just a plain string. *)
    type t = { my_string : string } [@@deriving sexp, equal, compare]

    let create my_string = { my_string }
  end

  module To_test = Sexpable.To_stringable_utf8 (Input)

  let%expect_test "output is utf8" =
    Input.create "你好 world" |> To_test.to_string |> print_endline;
    [%expect {| ((my_string"你好 world")) |}];
    (* Even when the input, while valid utf8, is encoded as escaped bytes.
       (This is the output of just running [Sexp.to_string] on the above sexp) *)
    let encoded_sexp_repr = {| ((my_string"\228\189\160\229\165\189 world")) |} in
    Sexp.of_string_conv_exn encoded_sexp_repr Input.t_of_sexp
    |> To_test.to_string
    |> print_endline;
    [%expect {| ((my_string"你好 world")) |}]
  ;;

  let%expect_test "round trips" =
    let f str =
      let input = Input.create str in
      To_test.to_string input |> To_test.of_string |> [%test_eq: Input.t] input
    in
    f "hello";
    f "你好 world"
  ;;
end
