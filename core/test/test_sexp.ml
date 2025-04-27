open! Core
open! Import
open! Sexp

module _ = struct
  open! With_text

  let sexp_of_il = sexp_of_list sexp_of_int
  let il_of_sexp = list_of_sexp int_of_sexp
  let il_of_text text = Or_error.ok_exn (of_text il_of_sexp text)
  let il_of_value il = of_value sexp_of_il il

  module IL = struct
    type t = int list [@@deriving compare, sexp_of]

    let equal = [%compare.equal: t]
  end

  let t = il_of_value [ 3; 4 ]

  let%expect_test _ =
    print_s [%sexp (text t : string)];
    [%expect {| "(3 4)" |}]
  ;;

  let t' = il_of_text (text t)

  let%expect_test _ =
    require_equal (module IL) (value t') [ 3; 4 ];
    [%expect {| |}]
  ;;

  let%expect_test _ =
    print_s [%sexp (t : il t)];
    [%expect {| "(3 4)" |}]
  ;;

  let%expect_test _ =
    require_equal (module IL) (value (t_of_sexp il_of_sexp (Atom "(3 4)"))) [ 3; 4 ];
    [%expect {| |}]
  ;;

  let%expect_test _ =
    require_equal
      (module IL)
      [ 8; 9 ]
      (value (il_of_text ";this is a comment\n (8; foo\n 9)   \n "));
    [%expect {| |}]
  ;;

  let%expect_test _ =
    require_does_raise (fun () -> il_of_text "(1 2 bla)");
    [%expect
      {| (Of_sexp_error :1:5 "int_of_sexp: (Failure int_of_string)" (invalid_sexp bla)) |}]
  ;;

  let%expect_test _ =
    require_does_raise (fun () -> t_of_sexp il_of_sexp (Sexp.of_string "\"(1 2 bla)\""));
    [%expect
      {| (Of_sexp_error :1:5 "int_of_sexp: (Failure int_of_string)" (invalid_sexp bla)) |}]
  ;;
end

module%test _ = struct
  type t1 = Sexp.t [@@deriving bin_io]
  type t2 = Sexp.Stable.V1.t [@@deriving bin_io]
  type t3 = Core_stable.Sexp.V1.t [@@deriving bin_io]

  let%expect_test "Sexp.t, Sexp.Stable.V1.t and Core_stable.Sexp.V1.t bin_digests match" =
    print_endline [%bin_digest: t1];
    print_endline [%bin_digest: t2];
    print_endline [%bin_digest: t3];
    [%expect
      {|
      832b40ae394f2851da8ba67b3339b429
      832b40ae394f2851da8ba67b3339b429
      832b40ae394f2851da8ba67b3339b429
      |}]
  ;;
end

module%test [@name "of_sexp_allow_extra_fields_recursively"] _ = struct
  module V = struct
    type v1 =
      { a : string
      ; b : int
      ; suffix : string
      }
    [@@deriving sexp]

    type v2 =
      { a : string
      ; b : int
      }
    [@@deriving sexp]

    type t = v2 [@@deriving sexp_of]

    let t_of_sexp sexp : t =
      try v2_of_sexp sexp with
      | e ->
        (match v1_of_sexp sexp with
         | { a; b; suffix } -> { a = a ^ suffix; b }
         | exception _ -> raise e)
    ;;
  end

  type t = { v : V.t } [@@deriving sexp]

  let%expect_test "affect sexp converter globally" =
    let sexp = Sexp.of_string {|((v ((a a)(b 0)(suffix "-suffix"))))|} in
    let t = t_of_sexp sexp in
    print_s (sexp_of_t t);
    [%expect
      {|
      ((
        v (
          (a a-suffix)
          (b 0))))
      |}];
    let t = Sexp.of_sexp_allow_extra_fields_recursively t_of_sexp sexp in
    print_s (sexp_of_t t);
    [%expect
      {|
      ((
        v (
          (a a)
          (b 0))))
      |}]
  ;;
end

module%test Utf8 = struct
  open Base_quickcheck
  open Generator.Let_syntax

  let test_round_trip sexp function_name f ~hum =
    let string = (f sexp : String.Utf8.t :> string) in
    if not (String.Utf8.is_valid string)
    then
      print_cr
        [%message "invalid UTF-8 output" (function_name : string) (string : string)];
    if String.exists string ~f:(function
         | '\n' when hum -> false
         | '\x00' .. '\x1f' -> true
         | ' ' .. '\xff' -> false)
    then
      print_cr
        [%message
          "control character(s) in UTF-8 output"
            (function_name : string)
            (string : String.Hexdump.t)];
    match Sexp.of_string string with
    | round_trip ->
      if not (Sexp.equal sexp round_trip)
      then
        print_cr
          [%message
            "Sexp round-trip failed"
              (function_name : string)
              (sexp : Sexp.t)
              (string : string)
              (round_trip : Sexp.t)]
    | exception exn ->
      print_cr
        [%message
          "Sexp round-trip raised an exception"
            (function_name : string)
            (sexp : Sexp.t)
            (string : string)
            (exn : exn)]
  ;;

  let test_sexp sexp =
    test_round_trip sexp "to_string" Sexp.Utf8.to_string ~hum:false;
    test_round_trip sexp "to_string_mach" Sexp.Utf8.to_string_mach ~hum:false;
    test_round_trip sexp "to_string_hum" Sexp.Utf8.to_string_hum ~hum:true
  ;;

  let%expect_test "spot check for control characters" =
    test_sexp (Atom "\000\255");
    [%expect {| |}]
  ;;

  let is_ascii_string string =
    match String.Utf8.of_string string with
    | exception _ -> false
    | utf8 ->
      List.for_all (String.Utf8.to_list utf8) ~f:(fun uchar ->
        Int.equal (Uchar.Utf8.byte_length uchar) 1)
  ;;

  let is_valid_utf8_string = String.Utf8.is_valid
  let is_invalid_utf8_string string = not (String.Utf8.is_valid string)

  (* Based on [Stdlib.String.is_valid_utf_8].

     Each list element starts with an inclusive range of initial bytes. These ranges
     represent a disjoint partition of all 256 byte values.

     [None] means no valid UTF-8 characters start with a byte in that range.

     [Some ranges] means a byte in initial range is followed by a sequence of bytes
     corresponding to [ranges], with each byte falling in the respective range. *)
  let utf8_ranges =
    [ ('\x00', '\x7f'), Some []
    ; ('\x80', '\xc1'), None
    ; ('\xc2', '\xdf'), Some [ '\x80', '\xbf' ]
    ; ('\xe0', '\xe0'), Some [ '\xa0', '\xbf'; '\x80', '\xbf' ]
    ; ('\xe1', '\xec'), Some [ '\x80', '\xbf'; '\x80', '\xbf' ]
    ; ('\xed', '\xed'), Some [ '\x80', '\x9f'; '\x80', '\xbf' ]
    ; ('\xee', '\xef'), Some [ '\x80', '\xbf'; '\x80', '\xbf' ]
    ; ('\xf0', '\xf0'), Some [ '\x90', '\xbf'; '\x80', '\xbf'; '\x80', '\xbf' ]
    ; ('\xf1', '\xf3'), Some [ '\x80', '\xbf'; '\x80', '\xbf'; '\x80', '\xbf' ]
    ; ('\xf4', '\xf4'), Some [ '\x80', '\x8f'; '\x80', '\xbf'; '\x80', '\xbf' ]
    ; ('\xf5', '\xff'), None
    ]
  ;;

  let ascii_generator =
    Generator.char_uniform_inclusive '\x00' '\x7f' |> Generator.string_of
  ;;

  (* We generate uchars based on their UTF-8 encoding rather than using the [Uchar]
     generator because we want to test UTF-8 formats with equal weight, not Unicode
     character ranges with equal weight. *)
  let valid_uchar_generator =
    List.filter_map utf8_ranges ~f:(fun (prefix, maybe_suffix) ->
      let%map.Option suffix = maybe_suffix in
      let%map chars =
        List.map (prefix :: suffix) ~f:(fun (start, until) ->
          Generator.char_uniform_inclusive start until)
        |> Generator.all
      in
      String.of_list chars)
    |> Generator.union
  ;;

  let%expect_test "valid UTF-8 uchar generator" =
    quickcheck_m
      (module struct
        type t = String.Hexdump.t [@@deriving sexp_of]

        let quickcheck_generator = valid_uchar_generator
        let quickcheck_shrinker = Shrinker.atomic
      end)
      ~f:(fun string ->
        require (String.Utf8.is_valid string);
        require_equal
          (module Int)
          (String.Utf8.length_in_uchars (String.Utf8.of_string string))
          1);
    [%expect {| |}]
  ;;

  let invalid_uchar_generator =
    List.filter_map utf8_ranges ~f:(fun (prefix, maybe_suffix) ->
      let prefix_gen =
        let start, until = prefix in
        Generator.char_uniform_inclusive start until
      in
      match maybe_suffix with
      | None ->
        (* No unicode characters begin with a byte in this range, so we generate it. *)
        prefix_gen |> Generator.map ~f:String.of_char |> Option.return
      | Some [] ->
        (* A byte in this range is a valid character, so we skip it. *)
        None
      | Some (_ :: _ as suffix) ->
        let incorrect =
          (* Generate a suffix where exactly one byte is out of its range. *)
          List.mapi suffix ~f:(fun i _ ->
            prefix_gen
            :: List.mapi suffix ~f:(fun j (start, until) ->
              match Int.equal i j with
              | false -> Generator.char_uniform_inclusive start until
              | true ->
                Generator.char_uniform_inclusive Char.min_value Char.max_value
                |> Generator.filter ~f:(fun char ->
                  Char.( < ) char start || Char.( > ) char until))
            |> Generator.all
            |> Generator.map ~f:String.of_list)
        in
        let truncated =
          (* Generate a suffix that stops too soon. *)
          List.mapi suffix ~f:(fun i _ ->
            List.map (prefix :: List.take suffix i) ~f:(fun (start, until) ->
              Generator.char_uniform_inclusive start until)
            |> Generator.all
            |> Generator.map ~f:String.of_list)
        in
        Generator.union (incorrect @ truncated) |> Option.return)
    |> Generator.union
  ;;

  let%expect_test "invalid UTF-8 uchar generator" =
    quickcheck_m
      (module struct
        type t = String.Hexdump.t [@@deriving sexp_of]

        let quickcheck_generator = invalid_uchar_generator
        let quickcheck_shrinker = Shrinker.atomic
      end)
      ~f:(fun string -> require (not (String.Utf8.is_valid string)));
    [%expect {| |}]
  ;;

  let valid_utf8_generator =
    let%bind length = Generator.small_positive_or_zero_int in
    let%map uchars = valid_uchar_generator |> Generator.list_with_length ~length in
    String.concat uchars
  ;;

  let invalid_utf8_generator =
    (* Not always invalid, but often invalid. We filter below in [test] anyway. *)
    let%bind length = Generator.small_strictly_positive_int in
    let%map uchars =
      Generator.union [ valid_uchar_generator; invalid_uchar_generator ]
      |> Generator.list_with_length ~length
    in
    String.concat uchars
  ;;

  (* Run [test_sexp] against randomly generated sexps whose atoms are sometimes generated
     by [gen], where at least one atom satisfies [pred]. *)
  let test ~pred ~gen =
    quickcheck_m
      (module struct
        type t = Sexp.t [@@deriving sexp_of]

        let rec sexp_pred sexp =
          match (sexp : Sexp.t) with
          | Atom string -> pred string
          | List list -> List.exists list ~f:sexp_pred
        ;;

        let quickcheck_generator =
          Generator.union [ Generator.string; gen ]
          |> Generator.sexp_of
          |> Generator.filter ~f:sexp_pred
        ;;

        let quickcheck_shrinker = Shrinker.sexp |> Shrinker.filter ~f:sexp_pred
      end)
      ~f:test_sexp
  ;;

  let%expect_test "ASCII" =
    test ~pred:is_ascii_string ~gen:ascii_generator;
    [%expect {| |}]
  ;;

  let%expect_test "valid UTF-8" =
    test ~pred:is_valid_utf8_string ~gen:valid_utf8_generator;
    [%expect {| |}]
  ;;

  let%expect_test "invalid UTF-8" =
    test ~pred:is_invalid_utf8_string ~gen:invalid_utf8_generator;
    [%expect {| |}]
  ;;
end
