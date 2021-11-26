open! Import
open Std_internal
include Stable_unit_test_intf

module Make_sexp_deserialization_test (T : Stable_unit_test_intf.Arg) = struct
  let%test_unit "sexp deserialization" =
    Or_error.combine_errors_unit
      (List.map T.tests ~f:(fun (t, sexp_as_string, _) ->
         match
           Or_error.try_with (fun () ->
             sexp_as_string |> Sexp.of_string |> [%of_sexp: T.t])
         with
         | Error _ as error ->
           Or_error.tag_arg
             error
             "could not deserialize sexp"
             (sexp_as_string, `Expected t)
             [%sexp_of: string * [ `Expected of T.t ]]
         | Ok t' ->
           if T.equal t t'
           then Ok ()
           else
             Or_error.error
               "sexp deserialization mismatch"
               (`Expected t, `But_got t')
               [%sexp_of: [ `Expected of T.t ] * [ `But_got of T.t ]]))
    |> ok_exn
  ;;
end

module Make_sexp_serialization_test (T : Stable_unit_test_intf.Arg) = struct
  let%test_unit "sexp serialization" =
    Or_error.combine_errors_unit
      (List.map T.tests ~f:(fun (t, sexp_as_string, _) ->
         Or_error.try_with (fun () ->
           let sexp = Sexp.of_string sexp_as_string in
           let serialized_sexp = T.sexp_of_t t in
           if Sexp.( <> ) serialized_sexp sexp
           then
             failwiths
               ~here:[%here]
               "sexp serialization mismatch"
               (`Expected sexp, `But_got serialized_sexp)
               [%sexp_of: [ `Expected of Sexp.t ] * [ `But_got of Sexp.t ]])))
    |> ok_exn
  ;;
end

module Make_bin_io_test (T : Stable_unit_test_intf.Arg) = struct
  let%test_unit "bin_io" =
    List.iter T.tests ~f:(fun (t, _, expected_bin_io) ->
      let binable_m = (module T : Binable.S with type t = T.t) in
      let to_bin_string t = Binable.to_string binable_m t in
      let serialized_bin_io = to_bin_string t in
      if String.( <> ) serialized_bin_io expected_bin_io
      then
        failwiths
          ~here:[%here]
          "bin_io serialization mismatch"
          (t, `Expected expected_bin_io, `But_got serialized_bin_io)
          [%sexp_of: T.t * [ `Expected of string ] * [ `But_got of string ]];
      let t' = Binable.of_string binable_m serialized_bin_io in
      if not (T.equal t t')
      then
        failwiths
          ~here:[%here]
          "bin_io deserialization mismatch"
          (`Expected t, `But_got t')
          [%sexp_of: [ `Expected of T.t ] * [ `But_got of T.t ]])
  ;;
end

module Make (T : Stable_unit_test_intf.Arg) = struct
  include Make_sexp_deserialization_test (T)
  include Make_sexp_serialization_test (T)
  include Make_bin_io_test (T)
end

module Make_unordered_container (T : Stable_unit_test_intf.Unordered_container_arg) =
struct
  module Test = Stable_unit_test_intf.Unordered_container_test

  let%test_unit "sexp" =
    List.iter T.tests ~f:(fun (t, { Test.sexps; _ }) ->
      let sexps = List.map sexps ~f:Sexp.of_string in
      let serialized_elements =
        match T.sexp_of_t t with
        | Sexp.List sexps -> sexps
        | Sexp.Atom _ ->
          failwiths
            ~here:[%here]
            "expected list when serializing unordered container"
            t
            T.sexp_of_t
      in
      let sorted_sexps = List.sort ~compare:Sexp.compare sexps in
      let sorted_serialized = List.sort ~compare:Sexp.compare serialized_elements in
      if not (List.equal Sexp.( = ) sorted_sexps sorted_serialized)
      then
        failwiths
          ~here:[%here]
          "sexp serialization mismatch"
          (`Expected sexps, `But_got serialized_elements)
          [%sexp_of: [ `Expected of Sexp.t list ] * [ `But_got of Sexp.t list ]];
      let sexp_permutations = List.init 10 ~f:(fun _ -> List.permute sexps) in
      List.iter sexp_permutations ~f:(fun sexps ->
        let t' = T.t_of_sexp (Sexp.List sexps) in
        if not (T.equal t t')
        then
          failwiths
            ~here:[%here]
            "sexp deserialization msimatch"
            (`Expected t, `But_got t')
            [%sexp_of: [ `Expected of T.t ] * [ `But_got of T.t ]]))
  ;;

  let rec is_concatenation string strings =
    if String.is_empty string
    then List.for_all strings ~f:String.is_empty
    else (
      let rec loop rev_skipped strings =
        match strings with
        | [] -> false
        | prefix :: strings ->
          let continue () = loop (prefix :: rev_skipped) strings in
          (match String.chop_prefix ~prefix string with
           | None -> continue ()
           | Some string ->
             is_concatenation string (List.rev_append rev_skipped strings) || continue ())
      in
      loop [] strings)
  ;;

  let%test_unit "bin_io" =
    List.iter T.tests ~f:(fun (t, { Test.bin_io_header; bin_io_elements; _ }) ->
      let binable_m = (module T : Binable.S with type t = T.t) in
      let elements = bin_io_elements in
      let bin_io_of_elements elements = bin_io_header ^ String.concat elements in
      let serialized = Binable.to_string binable_m t in
      let serialization_matches =
        match String.chop_prefix ~prefix:bin_io_header serialized with
        | None -> false
        | Some elements_string -> is_concatenation elements_string elements
      in
      if not serialization_matches
      then
        failwiths
          ~here:[%here]
          "serialization mismatch"
          (`Expected (bin_io_header, elements), `But_got serialized)
          [%sexp_of: [ `Expected of string * string list ] * [ `But_got of string ]];
      let permutatations = List.init 10 ~f:(fun _ -> List.permute elements) in
      List.iter permutatations ~f:(fun elements ->
        let t' = Binable.of_string binable_m (bin_io_of_elements elements) in
        if not (T.equal t t')
        then
          failwiths
            ~here:[%here]
            "bin-io deserialization mismatch"
            (`Expected t, `But_got t')
            [%sexp_of: [ `Expected of T.t ] * [ `But_got of T.t ]]))
  ;;
end

