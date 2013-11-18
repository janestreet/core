open Std_internal

module T = struct
  type t =
    (* WHEN YOU CHANGE THIS, CHANGE iobuf_fields IN iobuf_stubs.c AS WELL!!! *)
    { mutable buf : Bigstring.t sexp_opaque;
      (* The data in [buf] is at indices [lo], [lo+1], ... [hi-1]. *)
      mutable lo_min : int;
      mutable lo     : int;
      mutable hi     : int;
      mutable hi_max : int;
    } with fields, sexp_of
end
open T
type (+'read_write, +'seek) t = T.t with sexp_of
type    seek = Iobuf_intf.   seek with sexp_of
type no_seek = Iobuf_intf.no_seek with sexp_of
module type Bound = Iobuf_intf.Bound with type ('d, 'w) iobuf := ('d, 'w) t

let fail t message a sexp_of_a =
  (* Immediately convert the iobuf to sexp.  Otherwise, the iobuf could be modified before
     conversion and printing.  Since we plan to use iobufs for pooled network buffers in
     practice, this could be very confusing when debugging production systems. *)
  failwiths message (a, <:sexp_of< (_, _) t >> t)
    (Tuple.T2.sexp_of_t sexp_of_a ident)

module Lo_bound = struct
  let stale t iobuf =
    fail iobuf "Iobuf.Lo_bound.restore got stale snapshot" t <:sexp_of< int >>

  type t = int with sexp_of (* lo *)

  let window t = t.lo

  let restore t iobuf =
    if t < iobuf.lo_min || t > iobuf.hi then stale t iobuf;
    iobuf.lo <- t;
  ;;

  let limit t = t.lo_min
end

module Hi_bound = struct
  let stale t iobuf =
    fail iobuf "Iobuf.Hi_bound.restore got stale snapshot" t <:sexp_of< int >>

  type t = int with sexp_of (* hi *)

  let window t = t.hi

  let restore t iobuf =
    if t > iobuf.hi_max || t < iobuf.lo then stale t iobuf;
    iobuf.hi <- t;
  ;;

  let limit t = t.hi_max
end

let length t = t.hi - t.lo

let is_empty t = length t = 0

let rewind t = t.lo <- t.lo_min

let reset t =
  t.lo <- t.lo_min;
  t.hi <- t.hi_max
;;

let flip_lo t =
  t.hi <- t.lo;
  t.lo <- t.lo_min;
;;
let bounded_flip_lo_stale t lo_min =
  fail t "Iobuf.bounded_flip_lo got stale snapshot" lo_min <:sexp_of< Lo_bound.t >>
;;
let bounded_flip_lo t lo_min =
  if lo_min < t.lo_min || lo_min > t.lo then bounded_flip_lo_stale t lo_min
  else (t.hi <- t.lo; t.lo <- lo_min)
;;

let flip_hi t =
  t.lo <- t.hi;
  t.hi <- t.hi_max;
;;
let bounded_flip_hi_stale t hi_max =
  fail t "Iobuf.bounded_flip_hi got stale snapshot" hi_max <:sexp_of< Hi_bound.t >>
;;
let bounded_flip_hi t hi_max =
  if hi_max > t.hi_max || hi_max < t.hi then bounded_flip_hi_stale t hi_max
  else (t.lo <- t.hi; t.hi <- hi_max)
;;

let capacity t = t.hi_max - t.lo_min

let invariant _ _ t =
  try
    Fields.Direct.iter t
      ~buf:(fun _ _ _ -> ())
      ~lo_min:(fun _ _ lo_min ->
        assert (lo_min >= 0);
        assert (lo_min = t.hi_max - capacity t))
      ~hi_max:(fun _ _ hi_max ->
        assert (hi_max >= t.lo);
        assert (hi_max = t.lo_min + capacity t))
      ~lo:(fun _ _ lo ->
        assert (lo >= t.lo_min);
        assert (lo <= t.hi))
      ~hi:(fun _ _ hi ->
        assert (hi >= t.lo);
        assert (hi <= t.hi_max))
  with e -> fail t "Iobuf.invariant failed" e <:sexp_of< exn >>
;;

(* pszilagyi: Passing the names of functions is redundant with the stack backtrace and
   wreaks havoc with inlining. *)

(* We want [check_range] inlined, so we don't want a string constant in there. *)
let bad_range ~pos ~len t =
  fail t "Iobuf got invalid range" (`pos pos, `len len)
    <:sexp_of< [ `pos of int ] * [ `len of int ] >>;
;;

let check_range t ~pos ~len =
  if pos < 0 || len < 0 || len > length t - pos then
    bad_range ~pos ~len t;
;;

let of_bigstring ?pos ?len buf =
  let str_len = Bigstring.length buf in
  let pos =
    match pos with
    | None -> 0
    | Some pos ->
      if pos < 0 || pos > str_len then
        failwiths "Iobuf.of_bigstring got invalid pos" (pos, `str_len str_len)
          (<:sexp_of< int * [ `str_len of int ]>>);
      pos
  in
  let len =
    match len with
    | None -> str_len - pos
    | Some len ->
      let max_len = str_len - pos in
      if len < 0 || len > max_len then
        failwiths "Iobuf.of_bigstring got invalid len" (len, `max_len max_len)
          (<:sexp_of< int * [ `max_len of int ] >>);
      len
  in
  let lo = pos in
  let hi = pos + len in
  { buf; lo_min = lo; lo; hi; hi_max = hi }
;;

let sub ?(pos = 0) ?len t =
  let len =
    match len with
    | None -> length t - pos
    | Some len -> len
  in
  check_range t ~pos ~len;
  let lo = t.lo + pos in
  let hi = lo + len in
  { buf = t.buf;
    lo_min = lo;
    lo;
    hi;
    hi_max = hi;
  }
;;

let set_bounds_and_buffer_sub ?(pos = 0) ?len ~src ~dst () =
  let len =
    match len with
    | None -> length src - pos
    | Some len -> len
  in
  check_range src ~pos ~len;
  let lo = src.lo + pos in
  let hi = lo + len in
  dst.lo_min <- lo;
  dst.lo <- lo;
  dst.hi <- hi;
  dst.hi_max <- hi;
  dst.buf <- src.buf
;;

let set_bounds_and_buffer ~src ~dst =
  dst.lo_min <- src.lo_min;
  dst.lo <- src.lo;
  dst.hi <- src.hi;
  dst.hi_max <- src.hi_max;
  dst.buf <- src.buf
;;

let narrow t = t.lo_min <- t.lo; t.hi_max <- t.hi; ;;

let resize t ~len =
  if len < 0 then bad_range t ~len ~pos:0;
  let hi = t.lo + len in
  if hi > t.hi_max then bad_range t ~len ~pos:0;
  t.hi <- hi;
;;

let create ~len =
  if len < 0 then
    failwiths "Iobuf.create got negative len" len <:sexp_of< int >>;
  of_bigstring (Bigstring.create len);
;;

let to_string ?len t =
  let len =
    match len with
    | Some len -> check_range t ~pos:0 ~len; len
    | None -> length t
  in
  Bigstring.to_string t.buf ~pos:t.lo ~len

let of_string s = of_bigstring (Bigstring.of_string s)

(* We used to do it like {v

let unsafe_with_range t ~pos f =
  f t.buf ~pos:(t.lo + pos);
;;

let with_range t ~pos ~len f =
  check_range t ~pos ~len;
  unsafe_with_range t ~pos f;
;;

let inc_lo t amount = t.lo <- t.lo + amount

(** [unsafe_with_advance] and [unsafe_with_range] forego range checks for code that does
    macro range checks, like we want to do in [Parachute_fix.Std.Protocol].
    Esp. [Consume.Unsafe.int32_le] for unrolled character scanning. *)
let unsafe_with_advance t ~len f =
  let result = unsafe_with_range t ~pos:0 f in
  inc_lo t len;
  result;
;;

let with_advance t ~len f =
  check_range t ~pos:0 ~len;
  unsafe_with_advance t ~len f;
;;

(* pulled out and type-constrained for inlining *)
let ignore_range (_ : Bigstring.t) ~pos:(_ : int) = ()

let advance t len = with_advance t ~len ignore_range

   v} but higher order functions don't get inlined, even in simple uses like advance.
   Therefor, we stick to first order. *)

let unsafe_buf_pos t ~pos = t.lo + pos
let buf_pos t ~pos ~len = check_range t ~pos ~len; unsafe_buf_pos t ~pos

let unsafe_advance t n = t.lo <- t.lo + n
let advance t len = check_range t ~len ~pos:0; unsafe_advance t len

(* This is from /janelibs/ocaml-4.00.1+jane1+with-fp/lib/ocaml/bigarray.mli per jdimino
   via bnigito.  It's an unsafe, inline-able version of Bigstring.get. *)
external bigstring_unsafe_get : Bigstring.t -> pos:int -> char
  = "%caml_ba_unsafe_ref_1"
external bigstring_unsafe_set : Bigstring.t -> pos:int -> char -> unit
  = "%caml_ba_unsafe_set_1"
(* Note that we can get buf.{pos} inlined by ensuring that it's monomorphically typed,
   but we can't always get the containing function inlined. *)
(* Similarly, we need the following intermediate functions for the primitives to be
   inlined into.  (Not intuitive, but apparently necessary.) *)
let bigstring_unsafe_get b ~pos   = bigstring_unsafe_get b ~pos
let bigstring_unsafe_set b ~pos c = bigstring_unsafe_set b ~pos c

let consume_into_string ?(pos = 0) ?len t dst =
  let len = match len with Some l -> l | None -> String.length dst - pos in
  Bigstring.To_string.blit ~src:t.buf ~src_pos:(buf_pos t ~pos:0 ~len)
    ~len ~dst ~dst_pos:pos;
  unsafe_advance t len
;;

let consume_into_bigstring ?(pos = 0) ?len t dst =
  let len = match len with Some l -> l | None -> Bigstring.length dst - pos in
  Bigstring.blit ~src:t.buf ~src_pos:(buf_pos t ~pos:0 ~len)
    ~len ~dst ~dst_pos:pos;
  unsafe_advance t len
;;

let self_transfer t = fail t "Iobuf.self-transfer" () <:sexp_of< unit >>
let transfer ?len ~src ~dst =
  let len = match len with None -> min (length src) (length dst) | Some l -> l in
  if phys_equal src dst then self_transfer src;
  Bigstring.blit
    ~src:src.buf ~src_pos:(buf_pos src ~pos:0 ~len) ~len
    ~dst:dst.buf ~dst_pos:(buf_pos dst ~pos:0 ~len);
  unsafe_advance src len;
  unsafe_advance dst len
;;

let memmove t ~src_pos ~dst_pos ~len =
  Bigstring.blit
    ~src:t.buf ~src_pos:(buf_pos t ~pos:src_pos ~len) ~len
    ~dst:t.buf ~dst_pos:(buf_pos t ~pos:dst_pos ~len)
;;

let compact t =
  let len = t.hi - t.lo in
  Bigstring.blit ~src:t.buf ~src_pos:t.lo ~len ~dst:t.buf ~dst_pos:t.lo_min;
  t.lo <- t.lo_min + len;
  t.hi <- t.hi_max;
;;
let bounded_compact_stale t lo_min hi_max =
  fail t "Iobuf.bounded_compact got stale snapshot" (lo_min, hi_max)
    <:sexp_of< Lo_bound.t * Hi_bound.t >>
;;
let bounded_compact t lo_min hi_max =
  let len = t.hi - t.lo in
  if hi_max > t.hi_max || hi_max < lo_min + len || lo_min < t.lo_min
  then
    bounded_compact_stale t lo_min hi_max
  else
    (Bigstring.blit ~src:t.buf ~src_pos:t.lo ~len ~dst:t.buf ~dst_pos:lo_min;
     t.lo <- lo_min + len;
     t.hi <- hi_max)

(* Sys.word_size is determined only at runtime, but we need it to be a compile-time
   constant to generate good code for Consume.int8, etc. *)
let word_size = 64
TEST = Sys.word_size = word_size

let read_bin_prot reader t ~pos =
  let buf_pos = unsafe_buf_pos t ~pos in
  let pos_ref = ref buf_pos in
  let a = reader.Bin_prot.Type_class.read t.buf ~pos_ref in
  let len = !pos_ref - buf_pos in
  check_range t ~pos ~len;
  (a, len)

module Consume = struct
  type nonrec ('a, 'd, 'w) t = ('d, seek) t -> 'a

  (* pszilagyi: This polymorphic helper does get inlined. *)
  let unsafe_adv t n x = unsafe_advance t n; x
  let pos t len = buf_pos t ~pos:0 ~len

  let padded_fixed_string ~padding ~len t =
    unsafe_adv t len
      (Bigstring.get_padded_fixed_string t.buf ~pos:(pos t len) ~padding ~len ())
  ;;

  let string ?(str_pos = 0) ?len t =
    let len = match len with None -> length t | Some l -> l in
    let dst = String.create (len + str_pos) in
    consume_into_string t dst ~len ~pos:str_pos;
    dst
  ;;

  let bigstring ?(str_pos = 0) ?len t =
    let len = match len with None -> length t | Some l -> l in
    let dst = Bigstring.create (len + str_pos) in
    consume_into_bigstring t dst ~len ~pos:str_pos;
    dst
  ;;

  let bin_prot reader t =
    let (a, len) = read_bin_prot reader t ~pos:0 in
    unsafe_adv t len a;
  ;;
  TEST_UNIT "bin_prot char" =
    let t = of_string "abc" in
    let a = bin_prot Char.bin_reader_t t in
    let b = bin_prot Char.bin_reader_t t in
    <:test_eq< char >> a 'a';
    <:test_eq< char >> b 'b';
    <:test_eq< string >> (to_string t) "c";
  ;;
  TEST_UNIT "bin_prot int" =
    let ints = [ 0; 1; -1; 12345; -67890; Int.min_value; Int.max_value; 666 ] in
    let buf = Bigstring.create 1000 in
    let _end_pos = List.fold ints ~init:0 ~f:(fun pos i -> Int.bin_write_t buf ~pos i) in
    let t = of_bigstring buf in
    List.iter ints ~f:(fun i -> <:test_eq< int >> i (bin_prot Int.bin_reader_t t));
  ;;

  open Bigstring

  let len = 1
  let char        t = unsafe_adv t len (bigstring_unsafe_get    t.buf ~pos:(pos t len))
  (* int8 accessors are slow C calls.  Use the fast char primitive. *)
  let uint8       t = Char.to_int (char t)
  let  int8       t = (uint8 t lsl (word_size - 9)) asr (word_size - 9)
  let len = 2
  let  int16_be   t = unsafe_adv t len (unsafe_get_int16_be     t.buf ~pos:(pos t len))
  let  int16_le   t = unsafe_adv t len (unsafe_get_int16_le     t.buf ~pos:(pos t len))
  let uint16_be   t = unsafe_adv t len (unsafe_get_uint16_be    t.buf ~pos:(pos t len))
  let uint16_le   t = unsafe_adv t len (unsafe_get_uint16_le    t.buf ~pos:(pos t len))
  let len = 4
  let  int32_be   t = unsafe_adv t len (unsafe_get_int32_be     t.buf ~pos:(pos t len))
  let  int32_le   t = unsafe_adv t len (unsafe_get_int32_le     t.buf ~pos:(pos t len))
  let uint32_be   t = unsafe_adv t len (unsafe_get_uint32_be    t.buf ~pos:(pos t len))
  let uint32_le   t = unsafe_adv t len (unsafe_get_uint32_le    t.buf ~pos:(pos t len))
  let len = 8
  let  int64_be   t = unsafe_adv t len (unsafe_get_int64_be_exn t.buf ~pos:(pos t len))
  let  int64_le   t = unsafe_adv t len (unsafe_get_int64_le_exn t.buf ~pos:(pos t len))
  let  int64_t_be t = unsafe_adv t len (unsafe_get_int64_t_be   t.buf ~pos:(pos t len))
  let  int64_t_le t = unsafe_adv t len (unsafe_get_int64_t_le   t.buf ~pos:(pos t len))
end

let write_bin_prot writer t ~pos a =
  let len = writer.Bin_prot.Type_class.size a in
  let buf_pos = buf_pos t ~pos ~len in
  let stop_pos = writer.Bin_prot.Type_class.write t.buf ~pos:buf_pos a in
  if stop_pos - buf_pos = len then len
  else fail t "Iobuf.write_bin_prot got unexpected number of bytes written \
               (Bin_prot bug: Type_class.write disagrees with .size)"
         (`size_len len, `buf_pos buf_pos, `write_stop_pos stop_pos)
         <:sexp_of< [ `size_len of int ]
                    * [ `buf_pos of int ]
                    * [ `write_stop_pos of int ] >>

module Fill = struct
  type nonrec ('a, 'd, 'w) t = (read_write, seek) t -> 'a -> unit

  let pos t len = buf_pos t ~pos:0 ~len
  let unsafe_adv t n = unsafe_advance t n

  let padded_fixed_string ~padding ~len t src =
    Bigstring.set_padded_fixed_string ~padding ~len t.buf ~pos:(pos t len) src;
    unsafe_adv t len
  ;;

  let string ?str_pos:(src_pos = 0) ?len t src =
    let len = match len with Some l -> l | None -> String.length src - src_pos in
    Bigstring.From_string.blit ~src ~src_pos ~len
      ~dst:t.buf ~dst_pos:(pos t len);
    unsafe_adv t len
  ;;

  let bigstring ?str_pos:(src_pos = 0) ?len t src =
    let len = match len with Some l -> l | None -> Bigstring.length src - src_pos in
    Bigstring.blit ~src ~src_pos ~len ~dst:t.buf ~dst_pos:(pos t len);
    unsafe_adv t len
  ;;

  let bin_prot writer t a = write_bin_prot writer t ~pos:0 a |> unsafe_adv t

  open Bigstring

  let len = 1
  let char        t c = bigstring_unsafe_set  t.buf c ~pos:(pos t len); unsafe_adv t len
  (* int8 accessors are slow C calls.  Use the fast char primitive. *)
  let uint8       t i = char t (Char.unsafe_of_int i)
  let  int8       t i = char t (Char.unsafe_of_int i)
  let len = 2
  let  int16_be   t i = unsafe_set_int16_be   t.buf i ~pos:(pos t len); unsafe_adv t len
  let  int16_le   t i = unsafe_set_int16_le   t.buf i ~pos:(pos t len); unsafe_adv t len
  let uint16_be   t i = unsafe_set_uint16_be  t.buf i ~pos:(pos t len); unsafe_adv t len
  let uint16_le   t i = unsafe_set_uint16_le  t.buf i ~pos:(pos t len); unsafe_adv t len
  let len = 4
  let  int32_be   t i = unsafe_set_int32_be   t.buf i ~pos:(pos t len); unsafe_adv t len
  let  int32_le   t i = unsafe_set_int32_le   t.buf i ~pos:(pos t len); unsafe_adv t len
  let uint32_be   t i = unsafe_set_uint32_be  t.buf i ~pos:(pos t len); unsafe_adv t len
  let uint32_le   t i = unsafe_set_uint32_le  t.buf i ~pos:(pos t len); unsafe_adv t len
  let len = 8
  let  int64_be   t i = unsafe_set_int64_be   t.buf i ~pos:(pos t len); unsafe_adv t len
  let  int64_le   t i = unsafe_set_int64_le   t.buf i ~pos:(pos t len); unsafe_adv t len
  let  int64_t_be t i = unsafe_set_int64_t_be t.buf i ~pos:(pos t len); unsafe_adv t len
  let  int64_t_le t i = unsafe_set_int64_t_le t.buf i ~pos:(pos t len); unsafe_adv t len
end

module Peek = struct
  type nonrec ('a, 'd, 'w) t = ('d, 'w) t -> pos:int -> 'a

  let padded_fixed_string ~padding ~len t ~pos =
    Bigstring.get_padded_fixed_string t.buf ~padding ~len ~pos:(buf_pos t ~len ~pos) ()
  ;;

  let string ?str_pos:(dst_pos = 0) ?len t ~pos =
    let len = match len with None -> length t - pos | Some l -> l in
    let dst = String.create (len + dst_pos) in
    Bigstring.To_string.blit ~src:t.buf ~src_pos:(buf_pos t ~len ~pos)
      ~len ~dst ~dst_pos;
    dst
  ;;

  let bigstring ?str_pos:(dst_pos = 0) ?len t ~pos =
    let len = match len with None -> length t - pos | Some l -> l in
    let dst = Bigstring.create (len + dst_pos) in
    Bigstring.blit ~src:t.buf ~src_pos:(buf_pos t ~len ~pos)
      ~len ~dst ~dst_pos;
    dst
  ;;

  let bin_prot reader t ~pos = read_bin_prot reader t ~pos |> fst
  TEST_UNIT "bin_prot char" =
    let t = of_string "abc" in
    let a = bin_prot Char.bin_reader_t t ~pos:0 in
    let b = bin_prot Char.bin_reader_t t ~pos:1 in
    <:test_eq< char >> a 'a';
    <:test_eq< char >> b 'b';
    <:test_eq< string >> (to_string t) "abc";
  ;;
  TEST_UNIT "bin_prot int" =
    let ints = [ 0; 1; -1; 12345; -67890; Int.min_value; Int.max_value; 666 ] in
    let buf = Bigstring.create 1000 in
    let end_pos = List.fold ints ~init:0 ~f:(fun pos i -> Int.bin_write_t buf ~pos i) in
    let t = of_bigstring buf in
    List.fold ints ~init:0 ~f:(fun pos i ->
      <:test_eq< int >> i (bin_prot Int.bin_reader_t t ~pos);
      pos + Int.bin_size_t i)
    |> (fun end_pos' -> <:test_eq< int >> end_pos end_pos');
  ;;

  open Bigstring

  let len = 1
  let char        t ~pos = bigstring_unsafe_get    t.buf ~pos:(buf_pos t ~len ~pos)
  (* int8 accessors are slow C calls.  Use the fast char primitive. *)
  let uint8       t ~pos = Char.to_int (char t ~pos)
  let  int8       t ~pos = (uint8 t ~pos lsl (word_size - 9)) asr (word_size - 9)
  let len = 2
  let  int16_be   t ~pos = unsafe_get_int16_be     t.buf ~pos:(buf_pos t ~len ~pos)
  let  int16_le   t ~pos = unsafe_get_int16_le     t.buf ~pos:(buf_pos t ~len ~pos)
  let uint16_be   t ~pos = unsafe_get_uint16_be    t.buf ~pos:(buf_pos t ~len ~pos)
  let uint16_le   t ~pos = unsafe_get_uint16_le    t.buf ~pos:(buf_pos t ~len ~pos)
  let len = 4
  let  int32_be   t ~pos = unsafe_get_int32_be     t.buf ~pos:(buf_pos t ~len ~pos)
  let  int32_le   t ~pos = unsafe_get_int32_le     t.buf ~pos:(buf_pos t ~len ~pos)
  let uint32_be   t ~pos = unsafe_get_uint32_be    t.buf ~pos:(buf_pos t ~len ~pos)
  let uint32_le   t ~pos = unsafe_get_uint32_le    t.buf ~pos:(buf_pos t ~len ~pos)
  let len = 8
  let  int64_be   t ~pos = unsafe_get_int64_be_exn t.buf ~pos:(buf_pos t ~len ~pos)
  let  int64_le   t ~pos = unsafe_get_int64_le_exn t.buf ~pos:(buf_pos t ~len ~pos)
  let  int64_t_be t ~pos = unsafe_get_int64_t_be   t.buf ~pos:(buf_pos t ~len ~pos)
  let  int64_t_le t ~pos = unsafe_get_int64_t_le   t.buf ~pos:(buf_pos t ~len ~pos)
end

module Poke = struct
  type nonrec ('a, 'd, 'w) t = (read_write, 'w) t -> pos:int -> 'a -> unit

  let padded_fixed_string ~padding ~len t ~pos src =
    Bigstring.set_padded_fixed_string ~padding ~len t.buf ~pos:(buf_pos t ~len ~pos)
      src
  ;;

  let string ?str_pos:(src_pos = 0) ?len t ~pos src =
    let len = match len with None -> String.length src - src_pos | Some l -> l in
    Bigstring.From_string.blit ~src ~src_pos ~len
      ~dst:t.buf ~dst_pos:(buf_pos t ~len ~pos)
  ;;

  let bigstring ?str_pos:(src_pos = 0) ?len t ~pos src =
    let len = match len with None -> Bigstring.length src - src_pos | Some l -> l in
    Bigstring.blit ~src ~src_pos ~len ~dst:t.buf ~dst_pos:(buf_pos t ~len ~pos)
  ;;

  let bin_prot writer t ~pos a = write_bin_prot writer t ~pos a |> (ignore : int -> unit)
  TEST_UNIT =
    let t = of_string "abc" in
    bin_prot Char.bin_writer_t t 'd' ~pos:0;
    bin_prot Char.bin_writer_t t 'e' ~pos:1;
    <:test_eq< string >> "dec" (to_string t);
    flip_lo t;
    assert (try bin_prot String.bin_writer_t t "fgh" ~pos:0; false with _ -> true);
    assert (is_empty t);
    reset t;
    <:test_eq< string >> "dec" (to_string t);
    bin_prot Char.bin_writer_t t 'i' ~pos:0;
    <:test_eq< string >> "iec" (to_string t);
  ;;

  open Bigstring

  let len = 1
  let char        t ~pos c = bigstring_unsafe_set  t.buf ~pos:(buf_pos t ~len ~pos) c
  (* int8 accessors are slow C calls.  Use the fast char primitive. *)
  let uint8       t ~pos i = char t ~pos (Char.unsafe_of_int i)
  let  int8       t ~pos i = char t ~pos (Char.unsafe_of_int i)
  let len = 2
  let  int16_be   t ~pos i = unsafe_set_int16_be   t.buf ~pos:(buf_pos t ~len ~pos) i
  let  int16_le   t ~pos i = unsafe_set_int16_le   t.buf ~pos:(buf_pos t ~len ~pos) i
  let uint16_be   t ~pos i = unsafe_set_uint16_be  t.buf ~pos:(buf_pos t ~len ~pos) i
  let uint16_le   t ~pos i = unsafe_set_uint16_le  t.buf ~pos:(buf_pos t ~len ~pos) i
  let len = 4
  let  int32_be   t ~pos i = unsafe_set_int32_be   t.buf ~pos:(buf_pos t ~len ~pos) i
  let  int32_le   t ~pos i = unsafe_set_int32_le   t.buf ~pos:(buf_pos t ~len ~pos) i
  let uint32_be   t ~pos i = unsafe_set_uint32_be  t.buf ~pos:(buf_pos t ~len ~pos) i
  let uint32_le   t ~pos i = unsafe_set_uint32_le  t.buf ~pos:(buf_pos t ~len ~pos) i
  let len = 8
  let  int64_be   t ~pos i = unsafe_set_int64_be   t.buf ~pos:(buf_pos t ~len ~pos) i
  let  int64_le   t ~pos i = unsafe_set_int64_le   t.buf ~pos:(buf_pos t ~len ~pos) i
  let  int64_t_be t ~pos i = unsafe_set_int64_t_be t.buf ~pos:(buf_pos t ~len ~pos) i
  let  int64_t_le t ~pos i = unsafe_set_int64_t_le t.buf ~pos:(buf_pos t ~len ~pos) i
end

let bin_prot_length_prefix_bytes = 4

let consume_bin_prot t bin_prot_reader =
  let result =
    if length t < bin_prot_length_prefix_bytes then
      error "Iobuf.consume_bin_prot not enough data to read length" t
        (<:sexp_of< (_, _) t >>)
    else begin
      let mark = t.lo in
      let v_len = Consume.int32_be t in
      if v_len > length t then begin
        t.lo <- mark;
        error "Iobuf.consume_bin_prot not enough data to read value" (v_len, t)
          (<:sexp_of< int * (_, _) t >>);
      end else Ok (Consume.bin_prot bin_prot_reader t)
    end
  in
  result;
;;

let fill_bin_prot t writer v =
  let v_len = writer.Bin_prot.Type_class.size v in
  let need = v_len + bin_prot_length_prefix_bytes in
  let result =
    if need > length t then
      error "Iobuf.fill_bin_prot not enough space" (need, t)
        (<:sexp_of< int * (_, _) t >>)
    else begin
      Fill.int32_be t v_len;
      Fill.bin_prot writer t v;
      Ok ();
    end
  in
  result;
;;

module File_descr = Iobuf_intf.Unix.File_descr

let read_assume_fd_is_nonblocking t fd =
  let nread =
    Bigstring.read_assume_fd_is_nonblocking fd t.buf ~pos:t.lo ~len:(length t)
  in
  unsafe_advance t nread;
  nread
;;

let pread_assume_fd_is_nonblocking t fd ~offset =
  let nread =
    Bigstring.pread_assume_fd_is_nonblocking fd ~offset t.buf ~pos:t.lo ~len:(length t)
  in
  unsafe_advance t nread;
  nread
;;

let recvfrom_assume_fd_is_nonblocking t fd =
  let nread, sockaddr =
    Bigstring.recvfrom_assume_fd_is_nonblocking fd t.buf ~pos:t.lo ~len:(length t)
  in
  unsafe_advance t nread;
  nread, sockaddr
;;

(* recvmmsg based on bigstring.ml *)
INCLUDE "core_config.mlh"

IFDEF RECVMMSG THEN

external unsafe_recvmmsg_assume_fd_is_nonblocking
  : (File_descr.t
     -> (read_write, seek) t array
     -> int
     -> Unix.sockaddr array option
     -> int)
  = "iobuf_recvmmsg_assume_fd_is_nonblocking_stub"

let recvmmsg_assume_fd_is_nonblocking fd ?count ?srcs ts =
  let loc = "Iobuf.recvmmsg_assume_fd_is_nonblocking" in
  let count = Option.value count ~default:(Array.length ts) in
  if count < 0 then invalid_arg (loc ^ ": count < 0");
  if count > Array.length ts then invalid_arg (loc ^ ": count > n_iobufs");
  begin match srcs with
  | None -> ()
  | Some a -> if count > Array.length a then invalid_arg (loc ^ ": count > n_srcs")
  end;
  unsafe_recvmmsg_assume_fd_is_nonblocking fd ts count srcs
;;

let recvmmsg_assume_fd_is_nonblocking =
  (* At Jane Street, we link with [--wrap recvmmsg] so that we can use our own wrapper
     around [recvmmsg].  This allows us to compile an executable on a machine that has
     recvmmsg (e.g., CentOS 6) but then run the executable on a machine that does not
     (e.g., CentOS 5), but that has our wrapper library.  We set up our wrapper so that
     when running on a machine that doesn't have it, [recvmmsg] always returns -1 and sets
     errno to ENOSYS. *)
  let ok = Ok recvmmsg_assume_fd_is_nonblocking in
  try
    assert (recvmmsg_assume_fd_is_nonblocking (File_descr.of_int (-1)) [||] = 0);
    ok                                  (* maybe it will ignore the bogus sockfd *)
  with
  | Unix.Unix_error (Unix.ENOSYS, _, _) ->
    unimplemented "Iobuf.recvmmsg_assume_fd_is_nonblocking"
  | _ -> ok
;;

ELSE                                    (* NDEF RECVMMSG *)

let recvmmsg_assume_fd_is_nonblocking =
  unimplemented "Iobuf.recvmmsg_assume_fd_is_nonblocking"
;;

ENDIF                                   (* RECVMMSG *)

(* This function and the one below have a 'fun () ->' in front of them because of the
   following shortcoming in the typer:

   # match () with () -> fun x -> x;;
   - : '_a -> '_a = <fun>

   Without the unit, in iobuf_debug.ml, we would try to wrap these functions by doing
   matches like:

   let send_nonblocking_no_sigpipe =
     match send_nonblocking_no_sigpipe with
     | Ok f -> Ok (fun t fd -> ... f t fd ...)
     | Error _ as e -> e

   but the typing problem shown above would prevent the generalization of the phantom
   types variables in the iobuf types. *)
let send_nonblocking_no_sigpipe () =
  Or_error.map Bigstring.send_nonblocking_no_sigpipe ~f:(fun send ->
    fun t fd ->
      let nwritten_opt = send fd t.buf ~pos:t.lo ~len:(length t) in
      begin match nwritten_opt with
      | None -> ()
      | Some nwritten -> unsafe_advance t nwritten
      end;
      nwritten_opt)
;;

let sendto_nonblocking_no_sigpipe () =
  Or_error.map Bigstring.sendto_nonblocking_no_sigpipe ~f:(fun sendto ->
    fun t fd sockaddr ->
      let nwritten_opt = sendto fd t.buf ~pos:t.lo ~len:(length t) sockaddr in
      begin match nwritten_opt with
      | None -> ()
      | Some nwritten -> unsafe_advance t nwritten
      end;
      nwritten_opt)
;;

let write_assume_fd_is_nonblocking t fd =
  let nwritten =
    Bigstring.write_assume_fd_is_nonblocking fd t.buf ~pos:t.lo ~len:(length t)
  in
  unsafe_advance t nwritten;
  nwritten
;;

let pwrite_assume_fd_is_nonblocking t fd ~offset =
  let nwritten =
    Bigstring.pwrite_assume_fd_is_nonblocking fd ~offset t.buf ~pos:t.lo ~len:(length t)
  in
  unsafe_advance t nwritten;
  nwritten
;;

module Unsafe = struct
  module Consume = struct
    type ('a, 'd, 'w) t = ('a, 'd, 'w) Consume.t

    (* copy of Consume with pos replaced by an unsafe version *)

    (* pszilagyi: This polymorphic helper does get inlined. *)
    let unsafe_adv t n x = unsafe_advance t n; x
    let pos t _len = unsafe_buf_pos t ~pos:0

    let padded_fixed_string ~padding ~len t =
      unsafe_adv t len
        (Bigstring.get_padded_fixed_string t.buf ~pos:(pos t len) ~padding ~len ())
    ;;

    let string ?(str_pos = 0) ?len t =
      let len = match len with None -> length t | Some l -> l in
      let dst = String.create (len + str_pos) in
      consume_into_string t dst ~len ~pos:str_pos;
      dst
    ;;

    let bigstring ?(str_pos = 0) ?len t =
      let len = match len with None -> length t | Some l -> l in
      let dst = Bigstring.create (len + str_pos) in
      consume_into_bigstring t dst ~len ~pos:str_pos;
      dst
    ;;

    let bin_prot = Consume.bin_prot

    open Bigstring

    let len = 1
    let char        t = unsafe_adv t len (bigstring_unsafe_get    t.buf ~pos:(pos t len))
    (* int8 accessors are slow C calls.  Use the fast char primitive. *)
    let uint8       t = Char.to_int (char t)
    let  int8       t = (uint8 t lsl (word_size - 9)) asr (word_size - 9)
    let len = 2
    let  int16_be   t = unsafe_adv t len (unsafe_get_int16_be     t.buf ~pos:(pos t len))
    let  int16_le   t = unsafe_adv t len (unsafe_get_int16_le     t.buf ~pos:(pos t len))
    let uint16_be   t = unsafe_adv t len (unsafe_get_uint16_be    t.buf ~pos:(pos t len))
    let uint16_le   t = unsafe_adv t len (unsafe_get_uint16_le    t.buf ~pos:(pos t len))
    let len = 4
    let  int32_be   t = unsafe_adv t len (unsafe_get_int32_be     t.buf ~pos:(pos t len))
    let  int32_le   t = unsafe_adv t len (unsafe_get_int32_le     t.buf ~pos:(pos t len))
    let uint32_be   t = unsafe_adv t len (unsafe_get_uint32_be    t.buf ~pos:(pos t len))
    let uint32_le   t = unsafe_adv t len (unsafe_get_uint32_le    t.buf ~pos:(pos t len))
    let len = 8
    let  int64_be   t = unsafe_adv t len (unsafe_get_int64_be_exn t.buf ~pos:(pos t len))
    let  int64_le   t = unsafe_adv t len (unsafe_get_int64_le_exn t.buf ~pos:(pos t len))
    let  int64_t_be t = unsafe_adv t len (unsafe_get_int64_t_be   t.buf ~pos:(pos t len))
    let  int64_t_le t = unsafe_adv t len (unsafe_get_int64_t_le   t.buf ~pos:(pos t len))
  end

  module Fill = struct
    type ('a, 'd, 'w) t = ('a, 'd, 'w) Fill.t

    (* copy with unsafe pos *)

    let pos t _len = unsafe_buf_pos t ~pos:0
    let unsafe_adv t n = unsafe_advance t n

    let padded_fixed_string ~padding ~len t src =
      Bigstring.set_padded_fixed_string ~padding ~len t.buf ~pos:(pos t len) src;
      unsafe_adv t len
    ;;

    let string ?str_pos:(src_pos = 0) ?len t src =
      let len = match len with Some l -> l | None -> String.length src - src_pos in
      Bigstring.From_string.blit ~src ~src_pos ~len
        ~dst:t.buf ~dst_pos:(pos t len);
      unsafe_adv t len
    ;;

    let bigstring ?str_pos:(src_pos = 0) ?len t src =
      let len = match len with Some l -> l | None -> Bigstring.length src - src_pos in
      Bigstring.blit ~src ~src_pos ~len ~dst:t.buf ~dst_pos:(pos t len);
      unsafe_adv t len
    ;;

    let bin_prot = Fill.bin_prot

    open Bigstring

    let len = 1
    let char        t c = bigstring_unsafe_set  t.buf c ~pos:(pos t len); unsafe_adv t len
    (* int8 accessors are slow C calls.  Use the fast char primitive. *)
    let uint8       t i = char t (Char.unsafe_of_int i)
    let  int8       t i = char t (Char.unsafe_of_int i)
    let len = 2
    let  int16_be   t i = unsafe_set_int16_be   t.buf i ~pos:(pos t len); unsafe_adv t len
    let  int16_le   t i = unsafe_set_int16_le   t.buf i ~pos:(pos t len); unsafe_adv t len
    let uint16_be   t i = unsafe_set_uint16_be  t.buf i ~pos:(pos t len); unsafe_adv t len
    let uint16_le   t i = unsafe_set_uint16_le  t.buf i ~pos:(pos t len); unsafe_adv t len
    let len = 4
    let  int32_be   t i = unsafe_set_int32_be   t.buf i ~pos:(pos t len); unsafe_adv t len
    let  int32_le   t i = unsafe_set_int32_le   t.buf i ~pos:(pos t len); unsafe_adv t len
    let uint32_be   t i = unsafe_set_uint32_be  t.buf i ~pos:(pos t len); unsafe_adv t len
    let uint32_le   t i = unsafe_set_uint32_le  t.buf i ~pos:(pos t len); unsafe_adv t len
    let len = 8
    let  int64_be   t i = unsafe_set_int64_be   t.buf i ~pos:(pos t len); unsafe_adv t len
    let  int64_le   t i = unsafe_set_int64_le   t.buf i ~pos:(pos t len); unsafe_adv t len
    let  int64_t_be t i = unsafe_set_int64_t_be t.buf i ~pos:(pos t len); unsafe_adv t len
    let  int64_t_le t i = unsafe_set_int64_t_le t.buf i ~pos:(pos t len); unsafe_adv t len
  end

  module Peek = struct
    type ('a, 'd, 'w) t = ('a, 'd, 'w) Peek.t

    (* copy with unsafe buf_pos *)

    let buf_pos t ~len:_ ~pos = unsafe_buf_pos t ~pos

    let padded_fixed_string ~padding ~len t ~pos =
      Bigstring.get_padded_fixed_string t.buf ~padding ~len ~pos:(buf_pos t ~len ~pos) ()
    ;;

    let string ?str_pos:(dst_pos = 0) ?len t ~pos =
      let len = match len with None -> length t - pos | Some l -> l in
      let dst = String.create (len + dst_pos) in
      Bigstring.To_string.blit ~src:t.buf ~src_pos:(buf_pos t ~len ~pos)
        ~len ~dst ~dst_pos;
      dst
    ;;

    let bigstring ?str_pos:(dst_pos = 0) ?len t ~pos =
      let len = match len with None -> length t - pos | Some l -> l in
      let dst = Bigstring.create (len + dst_pos) in
      Bigstring.blit ~src:t.buf ~src_pos:(buf_pos t ~len ~pos)
        ~len ~dst ~dst_pos;
      dst
    ;;

    let bin_prot = Peek.bin_prot

    open Bigstring

    let len = 1
    let char        t ~pos = bigstring_unsafe_get    t.buf ~pos:(buf_pos t ~len ~pos)
    (* int8 accessors are slow C calls.  Use the fast char primitive. *)
    let uint8       t ~pos = Char.to_int (char t ~pos)
    let  int8       t ~pos = (uint8 t ~pos lsl (word_size - 9)) asr (word_size - 9)
    let len = 2
    let  int16_be   t ~pos = unsafe_get_int16_be     t.buf ~pos:(buf_pos t ~len ~pos)
    let  int16_le   t ~pos = unsafe_get_int16_le     t.buf ~pos:(buf_pos t ~len ~pos)
    let uint16_be   t ~pos = unsafe_get_uint16_be    t.buf ~pos:(buf_pos t ~len ~pos)
    let uint16_le   t ~pos = unsafe_get_uint16_le    t.buf ~pos:(buf_pos t ~len ~pos)
    let len = 4
    let  int32_be   t ~pos = unsafe_get_int32_be     t.buf ~pos:(buf_pos t ~len ~pos)
    let  int32_le   t ~pos = unsafe_get_int32_le     t.buf ~pos:(buf_pos t ~len ~pos)
    let uint32_be   t ~pos = unsafe_get_uint32_be    t.buf ~pos:(buf_pos t ~len ~pos)
    let uint32_le   t ~pos = unsafe_get_uint32_le    t.buf ~pos:(buf_pos t ~len ~pos)
    let len = 8
    let  int64_be   t ~pos = unsafe_get_int64_be_exn t.buf ~pos:(buf_pos t ~len ~pos)
    let  int64_le   t ~pos = unsafe_get_int64_le_exn t.buf ~pos:(buf_pos t ~len ~pos)
    let  int64_t_be t ~pos = unsafe_get_int64_t_be   t.buf ~pos:(buf_pos t ~len ~pos)
    let  int64_t_le t ~pos = unsafe_get_int64_t_le   t.buf ~pos:(buf_pos t ~len ~pos)
  end

  module Poke = struct
    type ('a, 'd, 'w) t = ('a, 'd, 'w) Poke.t

    (* copy with unsafe buf_pos *)

    let buf_pos t ~pos ~len:_ = unsafe_buf_pos t ~pos

    let padded_fixed_string ~padding ~len t ~pos src =
      Bigstring.set_padded_fixed_string ~padding ~len t.buf ~pos:(buf_pos t ~len ~pos)
        src
    ;;

    let string ?str_pos:(src_pos = 0) ?len t ~pos src =
      let len = match len with None -> String.length src - src_pos | Some l -> l in
      Bigstring.From_string.blit ~src ~src_pos ~len
        ~dst:t.buf ~dst_pos:(buf_pos t ~len ~pos)
    ;;

    let bigstring ?str_pos:(src_pos = 0) ?len t ~pos src =
      let len = match len with None -> Bigstring.length src - src_pos | Some l -> l in
      Bigstring.blit ~src ~src_pos ~len ~dst:t.buf ~dst_pos:(buf_pos t ~len ~pos)
    ;;

    let bin_prot = Poke.bin_prot

    open Bigstring

    let len = 1
    let char        t ~pos c = bigstring_unsafe_set  t.buf ~pos:(buf_pos t ~len ~pos) c
    (* int8 accessors are slow C calls.  Use the fast char primitive. *)
    let uint8       t ~pos i = char t ~pos (Char.unsafe_of_int i)
    let  int8       t ~pos i = char t ~pos (Char.unsafe_of_int i)
    let len = 2
    let  int16_be   t ~pos i = unsafe_set_int16_be   t.buf ~pos:(buf_pos t ~len ~pos) i
    let  int16_le   t ~pos i = unsafe_set_int16_le   t.buf ~pos:(buf_pos t ~len ~pos) i
    let uint16_be   t ~pos i = unsafe_set_uint16_be  t.buf ~pos:(buf_pos t ~len ~pos) i
    let uint16_le   t ~pos i = unsafe_set_uint16_le  t.buf ~pos:(buf_pos t ~len ~pos) i
    let len = 4
    let  int32_be   t ~pos i = unsafe_set_int32_be   t.buf ~pos:(buf_pos t ~len ~pos) i
    let  int32_le   t ~pos i = unsafe_set_int32_le   t.buf ~pos:(buf_pos t ~len ~pos) i
    let uint32_be   t ~pos i = unsafe_set_uint32_be  t.buf ~pos:(buf_pos t ~len ~pos) i
    let uint32_le   t ~pos i = unsafe_set_uint32_le  t.buf ~pos:(buf_pos t ~len ~pos) i
    let len = 8
    let  int64_be   t ~pos i = unsafe_set_int64_be   t.buf ~pos:(buf_pos t ~len ~pos) i
    let  int64_le   t ~pos i = unsafe_set_int64_le   t.buf ~pos:(buf_pos t ~len ~pos) i
    let  int64_t_be t ~pos i = unsafe_set_int64_t_be t.buf ~pos:(buf_pos t ~len ~pos) i
    let  int64_t_le t ~pos i = unsafe_set_int64_t_le t.buf ~pos:(buf_pos t ~len ~pos) i
  end
end
