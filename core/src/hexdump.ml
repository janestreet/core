open! Import
module Char = Base.Char
module Int = Base.Int
module String = Base.String
include Hexdump_intf

let bytes_per_line = 16

(* Initialize to enough lines to display 4096 bytes -- large enough that, for example, a
   complete Ethernet packet can always be displayed -- including the line containing the
   final index. *)
let default_max_lines = Dynamic.make ((4096 / bytes_per_line) + 1)

module%template.portable Of_indexable3 (T : Indexable3) = struct
  module Hexdump = struct
    include T

    let hex_of_pos pos = Printf.sprintf "%08x" pos

    let hex_of_char t ~start ~until offset =
      let pos = start + offset in
      if pos >= until then "  " else Printf.sprintf "%02x" (Char.to_int (get t pos))
    ;;

    let hex_of_line t ~start ~until =
      Printf.sprintf
        "%s %s %s %s %s %s %s %s  %s %s %s %s %s %s %s %s"
        (hex_of_char t ~start ~until 0)
        (hex_of_char t ~start ~until 1)
        (hex_of_char t ~start ~until 2)
        (hex_of_char t ~start ~until 3)
        (hex_of_char t ~start ~until 4)
        (hex_of_char t ~start ~until 5)
        (hex_of_char t ~start ~until 6)
        (hex_of_char t ~start ~until 7)
        (hex_of_char t ~start ~until 8)
        (hex_of_char t ~start ~until 9)
        (hex_of_char t ~start ~until 10)
        (hex_of_char t ~start ~until 11)
        (hex_of_char t ~start ~until 12)
        (hex_of_char t ~start ~until 13)
        (hex_of_char t ~start ~until 14)
        (hex_of_char t ~start ~until 15)
    ;;

    let printable_string t ~start ~until =
      String.init (until - start) ~f:(fun i ->
        let char = get t (start + i) in
        if Char.is_print char then char else '.')
      [@nontail]
    ;;

    let line t ~pos ~len ~line_index =
      let start = pos + (line_index * bytes_per_line) in
      let until = min (start + bytes_per_line) (pos + len) in
      Printf.sprintf
        "%s  %s  |%s|"
        (hex_of_pos start)
        (hex_of_line t ~start ~until)
        (printable_string t ~start ~until)
    ;;

    let create ~max_lines ~pos ~len t ~f =
      let (pos : int), (len : int) =
        Ordered_collection_common.get_pos_len_exn () ?pos ?len ~total_length:(length t)
      in
      let max_lines =
        match max_lines with
        | Some max_lines -> max_lines
        | None -> Dynamic.get default_max_lines
      in
      (* always produce at least 3 lines: first line of hex, ellipsis, last line of hex *)
      let max_lines = max max_lines 3 in
      (* unabridged lines = lines of hex + line with final index *)
      let unabridged_lines =
        Int.round_up len ~to_multiple_of:bytes_per_line / bytes_per_line
      in
      (* Figure out where we need to skip from and to if [max_lines < unabridged_lines].
         Skip after half the actual hex lines (subtracting one line for the ellipsis).
         Skip to near the end, less the number of lines remaining to produce, plus the
         ellipsis line. *)
      let skip_from = (max_lines - 1) / 2 in
      let skip_to = unabridged_lines - (max_lines - skip_from) + 1 in
      f t ~len ~max_lines ~pos ~skip_from ~skip_to ~unabridged_lines
    ;;

    let create_sequence t ~len ~max_lines ~pos ~skip_from ~skip_to ~unabridged_lines =
      Sequence.unfold_step ~init:0 ~f:(fun line_index ->
        if line_index >= unabridged_lines
        then Done
        else if line_index = skip_from && max_lines < unabridged_lines
        then Yield { value = "..."; state = skip_to }
        else Yield { value = line t ~pos ~len ~line_index; state = line_index + 1 })
    ;;

    let create_string t ~len ~max_lines ~pos ~skip_from ~skip_to ~unabridged_lines =
      let lines =
        if unabridged_lines <= max_lines
        then
          List.init unabridged_lines ~f:(fun line_index -> line t ~pos ~len ~line_index)
        else
          List.concat
            [ List.init skip_from ~f:(fun line_index -> line t ~pos ~len ~line_index)
            ; [ "..." ]
            ; List.init (unabridged_lines - skip_to) ~f:(fun index ->
                line t ~pos ~len ~line_index:(index + skip_to))
            ]
      in
      String.concat lines ~sep:"\n"
    ;;

    let to_sequence ?max_lines ?pos ?len t =
      create ~max_lines ~pos ~len t ~f:(fun _ -> create_sequence t)
    ;;

    let to_string_hum ?max_lines ?pos ?len t =
      create ~max_lines ~pos ~len t ~f:create_string
    ;;

    let sexp_of_t _ _ _ t = to_sequence t |> Sequence.to_list |> [%sexp_of: string list]

    module Pretty = struct
      include T

      let printable =
        let rec printable_from t ~pos ~length =
          pos >= length
          || (Char.is_print (get t pos) && printable_from t ~pos:(pos + 1) ~length)
        in
        fun t -> printable_from t ~pos:0 ~length:(length t)
      ;;

      let to_string t = String.init (length t) ~f:(fun pos -> get t pos)

      let sexp_of_t sexp_of_a sexp_of_b sexp_of_c t =
        if printable t then [%sexp (to_string t : string)] else [%sexp (t : (a, b, c) t)]
      ;;
    end
  end
end

module%template.portable [@modality p] Of_indexable2 (T : Indexable2) = struct
  module M = Of_indexable3 [@modality p] (struct
      type ('a, 'b, _) t = ('a, 'b) T.t

      let length = T.length
      let get = T.get
    end)

  module Hexdump = struct
    include T

    let sexp_of_t x y t = M.Hexdump.sexp_of_t x y [%sexp_of: _] t
    let to_sequence = M.Hexdump.to_sequence
    let to_string_hum = M.Hexdump.to_string_hum

    module Pretty = struct
      include T

      let sexp_of_t sexp_of_a sexp_of_b t = [%sexp (t : (a, b, _) M.Hexdump.Pretty.t)]
    end
  end
end

module%template.portable [@modality p] Of_indexable1 (T : Indexable1) = struct
  module M = Of_indexable2 [@modality p] (struct
      type ('a, _) t = 'a T.t

      let length = T.length
      let get = T.get
    end)

  module Hexdump = struct
    include T

    let sexp_of_t x t = M.Hexdump.sexp_of_t x [%sexp_of: _] t
    let to_sequence = M.Hexdump.to_sequence
    let to_string_hum = M.Hexdump.to_string_hum

    module Pretty = struct
      include T

      let sexp_of_t sexp_of_a t = [%sexp (t : (a, _) M.Hexdump.Pretty.t)]
    end
  end
end

module%template.portable [@modality p] Of_indexable (T : Indexable) = struct
  module M = Of_indexable1 [@modality p] (struct
      type _ t = T.t

      let length = T.length
      let get = T.get
    end)

  module Hexdump = struct
    include T

    let sexp_of_t t = M.Hexdump.sexp_of_t [%sexp_of: _] t
    let to_sequence = M.Hexdump.to_sequence
    let to_string_hum = M.Hexdump.to_string_hum

    module Pretty = struct
      include T

      let sexp_of_t t = [%sexp (t : _ M.Hexdump.Pretty.t)]
    end
  end
end
