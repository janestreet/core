(* Some code taken from INRIA's buffer module. *)

open Bigstring

type t =
  {
    mutable bstr : Bigstring.t;
    mutable pos : int;
    mutable len : int;
    init : Bigstring.t;
  }

let invariant t =
  assert (t.len == Bigstring.length t.bstr);
;;

let create n =
  let n = max 1 n in
  let bstr = Bigstring.create n in
  {
    bstr = bstr;
    pos = 0;
    len = n;
    init = bstr;
  }

let contents buf = Bigstring.to_string buf.bstr ~len:buf.pos

let big_contents buf = sub ~len:buf.pos buf.bstr

let volatile_contents buf = buf.bstr

let sub buf pos len =
  if pos < 0 || len < 0 || pos > buf.pos - len
  then invalid_arg "Bigbuffer.sub"
  else Bigstring.to_string buf.bstr ~pos ~len
;;

let blit ~src ?src_pos ?src_len ~dst ?dst_pos () =
  let (pos, len) =
    Ordered_collection_common.get_pos_len_exn
      ?pos:src_pos ?len:src_len
      ~length:src.pos
  in
  Bigstring.blit_bigstring_string ~src:src.bstr ~src_pos:pos ~src_len:len ~dst ?dst_pos ()
;;

let nth buf pos =
  if pos < 0 || pos >= buf.pos then invalid_arg "Bigbuffer.nth"
  else buf.bstr.{pos}

let length buf = buf.pos

let clear buf = buf.pos <- 0

let reset buf =
  buf.pos <- 0;
  buf.bstr <- buf.init;
  buf.len <- Bigstring.length buf.bstr;
;;

let resize buf more =
  let min_len = buf.len + more in
  let new_len = min_len + min_len in
  let new_buf = Bigstring.create new_len in
  Bigstring.blit ~src:buf.bstr ~src_len:buf.pos ~dst:new_buf ();
  buf.bstr <- new_buf;
  buf.len <- new_len;
;;

let add_char buf c =
  let pos = buf.pos in
  if pos >= buf.len then resize buf 1;
  buf.bstr.{pos} <- c;
  buf.pos <- pos + 1;
;;

let add_substring buf src src_pos len =
  if src_pos < 0 || len < 0 || src_pos > String.length src - len
  then invalid_arg "Bigbuffer.add_substring";
  let new_pos = buf.pos + len in
  if new_pos > buf.len then resize buf len;
  blit_string_bigstring ~src ~src_pos ~src_len:len ~dst:buf.bstr ~dst_pos:buf.pos ();
  buf.pos <- new_pos;
;;

let add_string buf src =
  let len = String.length src in
  let new_pos = buf.pos + len in
  if new_pos > buf.len then resize buf len;
  blit_string_bigstring ~src ~src_len:len ~dst:buf.bstr ~dst_pos:buf.pos ();
  buf.pos <- new_pos;
;;

let add_buffer buf_dst buf_src =
  let len = buf_src.pos in
  let dst_pos = buf_dst.pos in
  let new_pos = dst_pos + len in
  if new_pos > buf_dst.len then resize buf_dst len;
  Bigstring.blit ~src:buf_src.bstr ~src_len:len ~dst:buf_dst.bstr ~dst_pos ();
  buf_dst.pos <- new_pos;
;;

let add_channel buf ic len =
  if len < 0 then invalid_arg "Bigbuffer.add_channel";
  let pos = buf.pos in
  if pos + len > buf.len then resize buf len;
  Bigstring.really_input ic buf.bstr ~pos ~len;
  buf.pos <- pos + len;
;;

let output_buffer oc buf = Bigstring.really_output oc buf.bstr ~len:buf.pos

let closing = function
  | '(' -> ')'
  | '{' -> '}'
  | _ -> assert false

(* opening and closing: open and close characters, typically ( and )
   k: balance of opening and closing chars
   s: the string where we are searching
   start: the index where we start the search. *)
let advance_to_closing opening closing k s start =
  let rec advance k i lim =
    if i >= lim then raise Not_found else
    if s.[i] = opening then advance (k + 1) (i + 1) lim else
    if s.[i] = closing then
      if k = 0 then i else advance (k - 1) (i + 1) lim
    else advance k (i + 1) lim in
  advance k start (String.length s)

let advance_to_non_alpha s start =
  let rec advance i lim =
    if i >= lim then lim else
    match s.[i] with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' |
      'é'|'à'|'á'|'è'|'ù'|'â'|'ê'|
      'î'|'ô'|'û'|'ë'|'ï'|'ü'|'ç'|
      'É'|'À'|'Á'|'È'|'Ù'|'Â'|'Ê'|
      'Î'|'Ô'|'Û'|'Ë'|'Ï'|'Ü'|'Ç' ->
        advance (i + 1) lim
    | _ -> i in
  advance start (String.length s)

(* We are just at the beginning of an ident in s, starting at start. *)
let find_ident s start =
  match s.[start] with
  (* Parenthesized ident ? *)
  | '(' | '{' as c ->
     let new_start = start + 1 in
     let stop = advance_to_closing c (closing c) 0 s new_start in
     String.sub s new_start (stop - start - 1), stop + 1
  (* Regular ident *)
  | _ ->
     let stop = advance_to_non_alpha s (start + 1) in
     String.sub s start (stop - start), stop

(* Substitute $ident, $(ident), or ${ident} in s,
    according to the function mapping f. *)
let add_substitute buf f s =
  let lim = String.length s in
  let rec subst previous i =
    if i < lim then begin
      match s.[i] with
      | '$' as current when previous = '\\' ->
         add_char buf current;
         subst current (i + 1)
      | '$' ->
         let ident, next_i = find_ident s (i + 1) in
         add_string buf (f ident);
         subst ' ' next_i
      | current when previous = '\\' ->
         add_char buf '\\';
         add_char buf current;
         subst current (i + 1)
      | '\\' as current ->
         subst current (i + 1)
      | current ->
         add_char buf current;
         subst current (i + 1)
    end in
  subst ' ' 0

module Format = struct
  let formatter_of_buffer buf = Format.make_formatter (add_substring buf) ignore
  let bprintf buf = Format.kfprintf ignore (formatter_of_buffer buf)
end

module Printf = struct
  let bprintf buf = Printf.ksprintf (add_string buf)
end
