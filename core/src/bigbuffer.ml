(* Some code taken from INRIA's buffer module. *)

open! Import
open Bigstring
include Bigbuffer_internal

let __internal (t : t) = t
let length t = t.pos

(* {[ let invariant t = assert (t.len == Bigstring.length t.bstr) ]} *)

let create n =
  let n = max 1 n in
  let bstr = Bigstring.create n in
  { bstr; pos = 0; len = n; init = bstr }
;;

let contents buf = Bigstring.to_string buf.bstr ~len:buf.pos
let contents_bytes buf = Bigstring.to_bytes buf.bstr ~len:buf.pos
let big_contents buf = subo ~len:buf.pos buf.bstr
let volatile_contents buf = buf.bstr

let add_char buf c =
  let pos = buf.pos in
  if pos >= buf.len then resize buf 1;
  buf.bstr.{pos} <- c;
  buf.pos <- pos + 1
;;

module To_bytes =
  Test_blit.Make_distinct_and_test
    (struct
      type t = char

      let equal = Char.equal
      let of_bool b = if b then 'a' else 'b'
    end)
    (struct
      type nonrec t = t [@@deriving sexp_of]

      let create ~len =
        let t = create len in
        for _ = 1 to len do
          add_char t 'a'
        done;
        t
      ;;

      let length = length
      let set t i c = Bigstring.set t.bstr i c
      let get t i = Bigstring.get t.bstr i
    end)
    (struct
      include Bytes

      let create ~len = create len

      let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
        Bigstring.To_bytes.unsafe_blit ~src:src.bstr ~src_pos ~dst ~dst_pos ~len
      ;;
    end)

include To_bytes
module To_string = Blit.Make_to_string (Bigbuffer_internal) (To_bytes)

let nth buf pos =
  if pos < 0 || pos >= buf.pos then invalid_arg "Bigbuffer.nth" else buf.bstr.{pos}
;;

let clear buf = buf.pos <- 0

let reset buf =
  buf.pos <- 0;
  buf.bstr <- buf.init;
  buf.len <- Bigstring.length buf.bstr
;;

let add_substring buf src ~pos:src_pos ~len =
  if src_pos < 0 || len < 0 || src_pos > String.length src - len
  then invalid_arg "Bigbuffer.add_substring";
  let new_pos = buf.pos + len in
  if new_pos > buf.len then resize buf len;
  Bigstring.From_string.blit ~src ~src_pos ~len ~dst:buf.bstr ~dst_pos:buf.pos;
  buf.pos <- new_pos
;;

let add_subbytes buf src ~pos:src_pos ~len =
  if src_pos < 0 || len < 0 || src_pos > Bytes.length src - len
  then invalid_arg "Bigbuffer.add_subbytes";
  let new_pos = buf.pos + len in
  if new_pos > buf.len then resize buf len;
  Bigstring.From_bytes.blit ~src ~src_pos ~len ~dst:buf.bstr ~dst_pos:buf.pos;
  buf.pos <- new_pos
;;

let add_bigstring buf src =
  let len = Bigstring.length src in
  let new_pos = buf.pos + len in
  if new_pos > buf.len then resize buf len;
  Bigstring.blito ~src ~src_len:len ~dst:buf.bstr ~dst_pos:buf.pos ();
  buf.pos <- new_pos
;;

let add_string buf src =
  let len = String.length src in
  let new_pos = buf.pos + len in
  if new_pos > buf.len then resize buf len;
  Bigstring.From_string.blito ~src ~src_len:len ~dst:buf.bstr ~dst_pos:buf.pos ();
  buf.pos <- new_pos
;;

let add_bytes buf src =
  let len = Bytes.length src in
  let new_pos = buf.pos + len in
  if new_pos > buf.len then resize buf len;
  Bigstring.From_bytes.blito ~src ~src_len:len ~dst:buf.bstr ~dst_pos:buf.pos ();
  buf.pos <- new_pos
;;

let add_buffer buf_dst buf_src =
  let len = buf_src.pos in
  let dst_pos = buf_dst.pos in
  let new_pos = dst_pos + len in
  if new_pos > buf_dst.len then resize buf_dst len;
  Bigstring.blito ~src:buf_src.bstr ~src_len:len ~dst:buf_dst.bstr ~dst_pos ();
  buf_dst.pos <- new_pos
;;

let add_bin_prot t (writer : _ Bin_prot.Type_class.writer) x =
  let new_pos =
    match writer.write t.bstr ~pos:t.pos x with
    | pos -> pos
    | exception _ ->
      (* It's likeky that the exception is due to a buffer overflow, so resize the
         internal buffer and try again. Technically we could match on
         [Bin_prot.Common.Buffer_short] only, however we can't easily enforce that custom
         bin_write_xxx functions do raise this particular exception and not
         [Invalid_argument] or [Failure] for instance. *)
      let size = writer.size x in
      if t.pos + size > t.len then resize t size;
      writer.write t.bstr ~pos:t.pos x
  in
  t.pos <- new_pos
;;

let closing = function
  | '(' -> ')'
  | '{' -> '}'
  | _ -> assert false
;;

(* opening and closing: open and close characters, typically ( and )
   k: balance of opening and closing chars
   s: the string where we are searching
   start: the index where we start the search. *)
let advance_to_closing opening closing k s start =
  let rec advance k i lim =
    if i >= lim
    then
      raise
        (Not_found_s
           [%message
             "Bigbuffer.add_substitute: cannot find closing delimiter"
               (opening : char)
               (closing : char)
               (start : int)
               s])
    else if Char.equal s.[i] opening
    then advance (k + 1) (i + 1) lim
    else if Char.equal s.[i] closing
    then if k = 0 then i else advance (k - 1) (i + 1) lim
    else advance k (i + 1) lim
  in
  advance k start (String.length s)
;;

let advance_to_non_alpha s start =
  let rec advance i lim =
    if i >= lim
    then lim
    else (
      match s.[i] with
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9'
      | '_'
      | 'é'
      | 'à'
      | 'á'
      | 'è'
      | 'ù'
      | 'â'
      | 'ê'
      | 'î'
      | 'ô'
      | 'û'
      | 'ë'
      | 'ï'
      | 'ü'
      | 'ç'
      | 'É'
      | 'À'
      | 'Á'
      | 'È'
      | 'Ù'
      | 'Â'
      | 'Ê'
      | 'Î'
      | 'Ô'
      | 'Û'
      | 'Ë'
      | 'Ï'
      | 'Ü'
      | 'Ç' -> advance (i + 1) lim
      | _ -> i)
  in
  advance start (String.length s)
;;

(* We are just at the beginning of an ident in s, starting at start. *)
let find_ident s start =
  match s.[start] with
  (* Parenthesized ident ? *)
  | ('(' | '{') as c ->
    let new_start = start + 1 in
    let stop = advance_to_closing c (closing c) 0 s new_start in
    String.sub s ~pos:new_start ~len:(stop - start - 1), stop + 1
  (* Regular ident *)
  | _ ->
    let stop = advance_to_non_alpha s (start + 1) in
    String.sub s ~pos:start ~len:(stop - start), stop
;;

(* Substitute $ident, $(ident), or ${ident} in s,
   according to the function mapping f. *)
let add_substitute buf f s =
  let lim = String.length s in
  let rec subst previous i =
    if i < lim
    then (
      match s.[i] with
      | '$' as current when Char.equal previous '\\' ->
        add_char buf current;
        subst current (i + 1)
      | '$' ->
        let ident, next_i = find_ident s (i + 1) in
        add_string buf (f ident);
        subst ' ' next_i
      | current when Char.equal previous '\\' ->
        add_char buf '\\';
        add_char buf current;
        subst current (i + 1)
      | '\\' as current ->
        subst current (i + 1)
      | current ->
        add_char buf current;
        subst current (i + 1))
  in
  subst ' ' 0
;;

module Format = struct
  let formatter_of_buffer buf =
    Format.make_formatter (fun s pos len -> add_substring buf s ~pos ~len) ignore
  ;;

  let bprintf buf = Format.kfprintf ignore (formatter_of_buffer buf)
end

module Printf = struct
  let bprintf buf = Printf.ksprintf (add_string buf)
end
