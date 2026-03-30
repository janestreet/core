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

type dummy = Dummy_this_should_never_be_used_ever_anywhere
[@@deriving sexp ~localize ~stackify]

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

    let%template[@alloc a = (heap, stack)] line t ~pos ~len ~line_index =
      let start = pos + (line_index * bytes_per_line) in
      let until = min (start + bytes_per_line) (pos + len) in
      [%string.alloc
        "%{hex_of_pos start}  %{hex_of_line t ~start ~until}  |%{printable_string t \
         ~start ~until}|"]
      [@alloc a] [@exclave_if_stack a]
    ;;

    let%template[@alloc a = (heap, stack)] create ~max_lines ~pos ~len t ~f =
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
      f t ~len ~max_lines ~pos ~skip_from ~skip_to ~unabridged_lines [@exclave_if_stack a]
    ;;

    let%template[@alloc a = (heap, stack)] create_list
      t
      ~len
      ~max_lines
      ~pos
      ~skip_from
      ~skip_to
      ~unabridged_lines
      =
      (let lines =
         if unabridged_lines <= max_lines
         then
           (List.init [@alloc a]) unabridged_lines ~f:(fun line_index ->
             line t ~pos ~len ~line_index)
         else
           (List.concat [@alloc a])
             [ (List.init [@alloc a]) skip_from ~f:(fun line_index ->
                 (line [@alloc a]) t ~pos ~len ~line_index [@exclave_if_stack a])
             ; [ "..." ]
             ; (List.init [@alloc a]) (unabridged_lines - skip_to) ~f:(fun index ->
                 (line [@alloc a])
                   t
                   ~pos
                   ~len
                   ~line_index:(index + skip_to) [@exclave_if_stack a])
             ]
       in
       lines)
      [@exclave_if_stack a]
    ;;

    let create_sequence t ~len ~max_lines ~pos ~skip_from ~skip_to ~unabridged_lines =
      Sequence.unfold_step ~init:0 ~f:(fun line_index ->
        if line_index >= unabridged_lines
        then Done
        else if line_index = skip_from && max_lines < unabridged_lines
        then Yield { value = "..."; state = skip_to }
        else Yield { value = line t ~pos ~len ~line_index; state = line_index + 1 })
    ;;

    let%template[@alloc a = (heap, stack)] create_string
      t
      ~len
      ~max_lines
      ~pos
      ~skip_from
      ~skip_to
      ~unabridged_lines
      =
      ((create_list [@alloc a])
         t
         ~len
         ~max_lines
         ~pos
         ~skip_from
         ~skip_to
         ~unabridged_lines
       |> (String.concat [@alloc a]) ~sep:"\n")
      [@exclave_if_stack a]
    ;;

    let to_sequence ?max_lines ?pos ?len t =
      create ~max_lines ~pos ~len t ~f:(fun _ -> create_sequence t)
    ;;

    let%template[@alloc a = (heap, stack)] to_list ?max_lines ?pos ?len t =
      (create [@alloc a])
        ~max_lines
        ~pos
        ~len
        t
        ~f:(fun t ~len ~max_lines ~pos ~skip_from ~skip_to ~unabridged_lines ->
          (create_list [@alloc a])
            t
            ~len
            ~max_lines
            ~pos
            ~skip_from
            ~skip_to
            ~unabridged_lines [@exclave_if_stack a])
      [@exclave_if_stack a]
    ;;

    let%template[@alloc a = (heap, stack)] to_string_hum ?max_lines ?pos ?len t =
      (create [@alloc a])
        ~max_lines
        ~pos
        ~len
        t
        ~f:(create_string [@alloc a]) [@exclave_if_stack a]
    ;;

    let%template[@alloc a = (heap, stack)] sexp_of_t _ _ _ t =
      ((to_list [@alloc a]) t |> ([%sexp_of: string list] [@alloc a]))
      [@exclave_if_stack a]
    ;;

    module Pretty = struct
      include T

      let printable =
        let rec printable_from t ~pos ~length =
          pos >= length
          || (Char.is_print (get t pos) && printable_from t ~pos:(pos + 1) ~length)
        in
        fun t -> printable_from t ~pos:0 ~length:(length t)
      ;;

      let%template[@alloc a = (heap, stack)] to_string t =
        (String.init [@alloc a]) (length t) ~f:(fun pos -> get t pos)
        [@nontail] [@exclave_if_stack a]
      ;;

      let%template[@alloc a = (heap, stack)] sexp_of_t _ _ _ t =
        (if printable t
         then [%sexp ((to_string [@alloc a]) t : string)] [@alloc a]
         else [%sexp (t : (_, _, _) t)] [@alloc a])
        [@exclave_if_stack a]
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

    type ('a, 'b) t = ('a, 'b, dummy) M.Hexdump.t [@@deriving sexp_of ~localize ~stackify]

    let to_sequence = M.Hexdump.to_sequence

    let%template[@alloc a = (heap, stack)] to_string_hum =
      (M.Hexdump.to_string_hum [@alloc a])
    ;;

    module Pretty = struct
      include T

      type ('a, 'b) t = ('a, 'b, dummy) M.Hexdump.Pretty.t
      [@@deriving sexp_of ~localize ~stackify]
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

    type 'a t = ('a, dummy) M.Hexdump.t [@@deriving sexp_of ~localize ~stackify]

    let to_sequence = M.Hexdump.to_sequence

    let%template[@alloc a = (heap, stack)] to_string_hum =
      (M.Hexdump.to_string_hum [@alloc a])
    ;;

    module Pretty = struct
      include T

      type 'a t = ('a, dummy) M.Hexdump.Pretty.t [@@deriving sexp_of ~localize ~stackify]
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

    type t = dummy M.Hexdump.t [@@deriving sexp_of ~localize ~stackify]

    let to_sequence = M.Hexdump.to_sequence

    let%template[@alloc a = (heap, stack)] to_string_hum =
      (M.Hexdump.to_string_hum [@alloc a])
    ;;

    module Pretty = struct
      include T

      type t = dummy M.Hexdump.Pretty.t [@@deriving sexp_of ~localize ~stackify]
    end
  end
end
