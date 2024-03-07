open! Import
open Std_internal
open Bigarray

module Stable = struct
  module V1 = struct
    include Base_bigstring

    module Z : sig
      type t = (char, int8_unsigned_elt, c_layout) Array1.t
      [@@deriving bin_io ~localize, stable_witness]
    end = struct
      type t = bigstring [@@deriving bin_io ~localize]

      (* bigstring's serialization is intended to be stable.  We assert it here rather
         than asserting it in two separate places for the bin_io and sexp definitions. *)
      let stable_witness : t Stable_witness.t = Stable_witness.assert_stable
    end

    include Z

    type t_frozen = t [@@deriving bin_io ~localize, stable_witness]
  end
end

module T = Stable.V1
include T
module Unstable = T

let create size = create size

let sub_shared ?(pos = 0) ?len (bstr : t) =
  let len = get_opt_len bstr ~pos len in
  Array1.sub bstr pos len
;;

(* Destruction *)

external unsafe_destroy : t -> unit = "bigstring_destroy_stub"
external unsafe_destroy_and_resize : t -> len:int -> t = "bigstring_realloc"

(* Reading / writing bin-prot *)

let read_bin_prot_verbose_errors t ?(pos = 0) ?len reader =
  let len = get_opt_len t len ~pos in
  let limit = pos + len in
  check_args ~loc:"read_bin_prot_verbose_errors" t ~pos ~len;
  let invalid_data message a sexp_of_a =
    `Invalid_data (Error.create message a sexp_of_a)
  in
  let read bin_reader ~pos ~len =
    if len > limit - pos
    then `Not_enough_data
    else (
      let pos_ref = ref pos in
      match
        try `Ok (bin_reader t ~pos_ref) with
        | exn -> `Invalid_data (Error.of_exn exn)
      with
      | `Invalid_data _ as x -> x
      | `Ok result ->
        let expected_pos = pos + len in
        if !pos_ref = expected_pos
        then `Ok (result, expected_pos)
        else
          invalid_data
            "pos_ref <> expected_pos"
            (!pos_ref, expected_pos)
            [%sexp_of: int * int])
  in
  match
    read Bin_prot.Utils.bin_read_size_header ~pos ~len:Bin_prot.Utils.size_header_length
  with
  | (`Not_enough_data | `Invalid_data _) as x -> x
  | `Ok (element_length, pos) ->
    if element_length < 0
    then invalid_data "negative element length %d" element_length [%sexp_of: int]
    else read reader.Bin_prot.Type_class.read ~pos ~len:element_length
;;

let read_bin_prot t ?pos ?len reader =
  match read_bin_prot_verbose_errors t ?pos ?len reader with
  | `Ok x -> Ok x
  | `Invalid_data e -> Error (Error.tag e ~tag:"Invalid data")
  | `Not_enough_data -> Or_error.error_string "not enough data"
;;

let write_bin_prot_known_size t ?(pos = 0) write ~size:data_len v =
  let total_len = data_len + Bin_prot.Utils.size_header_length in
  if pos < 0
  then
    failwiths ~here:[%here] "Bigstring.write_bin_prot: negative pos" pos [%sexp_of: int];
  if pos + total_len > length t
  then
    failwiths
      ~here:[%here]
      "Bigstring.write_bin_prot: not enough room"
      (`pos pos, `pos_after_writing (pos + total_len), `bigstring_length (length t))
      [%sexp_of:
        [ `pos of int ] * [ `pos_after_writing of int ] * [ `bigstring_length of int ]];
  let pos_after_size_header = Bin_prot.Utils.bin_write_size_header t ~pos data_len in
  let pos_after_data = write t ~pos:pos_after_size_header v in
  if pos_after_data - pos <> total_len
  then
    failwiths
      ~here:[%here]
      "Bigstring.write_bin_prot bug!"
      ( `pos_after_data pos_after_data
      , `start_pos pos
      , `bin_prot_size_header_length Bin_prot.Utils.size_header_length
      , `data_len data_len
      , `total_len total_len )
      [%sexp_of:
        [ `pos_after_data of int ]
        * [ `start_pos of int ]
        * [ `bin_prot_size_header_length of int ]
        * [ `data_len of int ]
        * [ `total_len of int ]];
  pos_after_data
;;

let write_bin_prot t ?pos (writer : _ Bin_prot.Type_class.writer) v =
  let size = writer.size v in
  write_bin_prot_known_size t ?pos writer.write ~size v
;;

(* Hex dump *)

include Hexdump.Of_indexable (struct
  type nonrec t = t

  let length = length
  let get = get
end)

let rec last_nonmatch_plus_one ~buf ~min_pos ~pos ~char =
  let pos' = pos - 1 in
  if pos' >= min_pos && Char.( = ) (get buf pos') char
  then last_nonmatch_plus_one ~buf ~min_pos ~pos:pos' ~char
  else pos
;;

let get_tail_padded_fixed_string ~padding t ~pos ~len () =
  let data_end =
    last_nonmatch_plus_one ~buf:t ~min_pos:pos ~pos:(pos + len) ~char:padding
  in
  get_string t ~pos ~len:(data_end - pos)
;;

let get_tail_padded_fixed_string_local ~padding t ~pos ~len () =
  let data_end =
    last_nonmatch_plus_one ~buf:t ~min_pos:pos ~pos:(pos + len) ~char:padding
  in
  let len = data_end - pos in
  Local.get_string t ~pos ~len
;;

let[@cold] set_padded_fixed_string_failed ~head_or_tail ~value ~len =
  Printf.failwithf
    "Bigstring.set_%s_padded_fixed_string: %S is longer than %d"
    head_or_tail
    ([%globalize: string] value)
    len
    ()
;;

let set_tail_padded_fixed_string ~padding t ~pos ~len value =
  let slen = String.length value in
  if slen > len then set_padded_fixed_string_failed ~head_or_tail:"tail" ~value ~len;
  From_string.blit ~src:value ~dst:t ~src_pos:0 ~dst_pos:pos ~len:slen;
  for i = pos + slen to pos + len - 1 do
    set t i padding
  done
;;

let rec first_nonmatch ~buf ~pos ~max_pos ~char =
  if pos <= max_pos && Char.( = ) (get buf pos) char
  then first_nonmatch ~buf ~pos:(Int.succ pos) ~max_pos ~char
  else pos
;;

let set_head_padded_fixed_string ~padding t ~pos ~len value =
  let slen = String.length value in
  if slen > len then set_padded_fixed_string_failed ~head_or_tail:"head" ~value ~len;
  From_string.blit ~src:value ~dst:t ~src_pos:0 ~dst_pos:(pos + len - slen) ~len:slen;
  for i = pos to pos + len - slen - 1 do
    set t i padding
  done
;;

let get_head_padded_fixed_string ~padding t ~pos ~len () =
  let data_begin = first_nonmatch ~buf:t ~pos ~max_pos:(pos + len - 1) ~char:padding in
  get_string t ~pos:data_begin ~len:(len - (data_begin - pos))
;;

let get_head_padded_fixed_string_local ~padding t ~pos ~len () =
  let data_begin = first_nonmatch ~buf:t ~pos ~max_pos:(pos + len - 1) ~char:padding in
  let len = len - (data_begin - pos) in
  Local.get_string t ~pos:data_begin ~len
;;

let quickcheck_generator = Base_quickcheck.Generator.bigstring
let quickcheck_observer = Base_quickcheck.Observer.bigstring
let quickcheck_shrinker = Base_quickcheck.Shrinker.bigstring
