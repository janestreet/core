[%%import "config.h"]

module Bigstring_in_this_directory = Bigstring
open! Import
module Bigstring = Bigstring_in_this_directory

module Unix = Core_unix

let arch_sixtyfour = Sys.word_size = 64

module T = struct
  type t =
    (* WHEN YOU CHANGE THIS, CHANGE iobuf_fields IN iobuf.h AS WELL!!! *)
    { mutable buf : Bigstring.t sexp_opaque;
      (* The data in [buf] is at indices [lo], [lo+1], ... [hi-1]. *)
      mutable lo_min : int;
      mutable lo     : int;
      mutable hi     : int;
      mutable hi_max : int;
    } [@@deriving fields, sexp_of]
end
open T
type (-'read_write, +'seek) t = T.t [@@deriving sexp_of]
type    seek = Iobuf_intf.   seek [@@deriving sexp_of]
type no_seek = Iobuf_intf.no_seek [@@deriving sexp_of]
module type Bound = Iobuf_intf.Bound with type ('d, 'w) iobuf := ('d, 'w) t

let read_only t = t
let no_seek t = t

let fail t message a sexp_of_a =
  (* Immediately convert the iobuf to sexp.  Otherwise, the iobuf could be modified before
     conversion and printing.  Since we plan to use iobufs for pooled network buffers in
     practice, this could be very confusing when debugging production systems. *)
  failwiths message (a, [%sexp_of: (_, _) t] t)
    (Tuple.T2.sexp_of_t sexp_of_a ident)

module Lo_bound = struct
  let stale t iobuf =
    fail iobuf "Iobuf.Lo_bound.restore got stale snapshot" t [%sexp_of: int]

  type t = int [@@deriving compare, sexp_of] (* lo *)

  let window t = t.lo

  let restore t iobuf =
    if t < iobuf.lo_min || t > iobuf.hi then stale t iobuf;
    iobuf.lo <- t;
  ;;

  let limit t = t.lo_min
end

module Hi_bound = struct
  let stale t iobuf =
    fail iobuf "Iobuf.Hi_bound.restore got stale snapshot" t [%sexp_of: int]

  type t = int [@@deriving compare, sexp_of] (* hi *)

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
  fail t "Iobuf.bounded_flip_lo got stale snapshot" lo_min [%sexp_of: Lo_bound.t]
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
  fail t "Iobuf.bounded_flip_hi got stale snapshot" hi_max [%sexp_of: Hi_bound.t]
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
  with e -> fail t "Iobuf.invariant failed" e [%sexp_of: exn]
;;


(* We want [check_range] inlined, so we don't want a string constant in there. *)
let bad_range ~pos ~len t =
  fail t "Iobuf got invalid range" (`pos pos, `len len)
    [%sexp_of: [ `pos of int ] * [ `len of int ]];
[@@inline never]
;;

let check_range t ~pos ~len =
  if pos < 0 || len < 0 || len > length t - pos then
    bad_range ~pos ~len t;
[@@inline always]
;;

let of_bigstring ?pos ?len buf =
  let str_len = Bigstring.length buf in
  let pos =
    match pos with
    | None -> 0
    | Some pos ->
      if pos < 0 || pos > str_len then
        failwiths "Iobuf.of_bigstring got invalid pos" (pos, `str_len str_len)
          ([%sexp_of: int * [ `str_len of int ]]);
      pos
  in
  let len =
    match len with
    | None -> str_len - pos
    | Some len ->
      let max_len = str_len - pos in
      if len < 0 || len > max_len then
        failwiths "Iobuf.of_bigstring got invalid len" (len, `max_len max_len)
          ([%sexp_of: int * [ `max_len of int ]]);
      len
  in
  let lo = pos in
  let hi = pos + len in
  { buf; lo_min = lo; lo; hi; hi_max = hi }
;;

let sub_shared ?(pos = 0) ?len t =
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

let set_bounds_and_buffer_sub ~pos ~len ~src ~dst =
  check_range src ~pos ~len;
  let lo = src.lo + pos in
  let hi = lo + len in
  dst.lo_min <- lo;
  dst.lo <- lo;
  dst.hi <- hi;
  dst.hi_max <- hi;
  if not (phys_equal dst.buf src.buf)
  then dst.buf <- src.buf
;;

let set_bounds_and_buffer ~src ~dst =
  dst.lo_min <- src.lo_min;
  dst.lo <- src.lo;
  dst.hi <- src.hi;
  dst.hi_max <- src.hi_max;
  if not (phys_equal dst.buf src.buf)
  then dst.buf <- src.buf
;;

let narrow_lo t = t.lo_min <- t.lo
let narrow_hi t = t.hi_max <- t.hi

let narrow t = narrow_lo t; narrow_hi t

let unsafe_resize t ~len =
  t.hi <- t.lo + len

let resize t ~len =
  if len < 0 then bad_range t ~len ~pos:0;
  let hi = t.lo + len in
  if hi > t.hi_max then bad_range t ~len ~pos:0;
  t.hi <- hi;
[@@inline always]
;;

let protect_window_and_bounds t ~f =
  let lo = t.lo in
  let hi = t.hi in
  let lo_min = t.lo_min in
  let hi_max = t.hi_max in
  let buf = t.buf in  (* also mutable *)
  try
    t.lo_min <- lo;
    t.hi_max <- hi;
    let result = f t in
    t.lo <- lo;
    t.hi <- hi;
    t.lo_min <- lo_min;
    t.hi_max <- hi_max;
    t.buf <- buf;
    result
  with exn ->
    t.lo <- lo;
    t.hi <- hi;
    t.lo_min <- lo_min;
    t.hi_max <- hi_max;
    t.buf <- buf;
    raise exn

let create ~len =
  if len < 0 then
    failwiths "Iobuf.create got negative len" len [%sexp_of: int];
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

let [@inline always] unsafe_buf_pos t ~pos = t.lo + pos
let buf_pos_exn t ~pos ~len = check_range t ~pos ~len; unsafe_buf_pos t ~pos

let unsafe_advance t n = t.lo <- t.lo + n
let advance t len = check_range t ~len ~pos:0; unsafe_advance t len [@@inline always]

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

module Char_elt = struct
  include Char
  let of_bool = function true -> '0' | false -> '1'
end

module T_src = struct
  type t = T.t [@@deriving sexp_of]
  let create = create
  let length = length
  let get t pos   = bigstring_unsafe_get t.buf ~pos:(buf_pos_exn t ~len:1 ~pos)
  let set t pos c = bigstring_unsafe_set t.buf ~pos:(buf_pos_exn t ~len:1 ~pos) c
end

module Bytes_dst = struct
  include Bytes
  let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
    Bigstring.To_bytes.unsafe_blit
      ~src:src.buf ~src_pos:(unsafe_buf_pos src ~pos:src_pos)
      ~dst ~dst_pos
      ~len
  let create ~len = create len
end

module String_dst = struct
  let sub src ~pos ~len =
    Bigstring.To_string.sub
      src.buf ~pos:(buf_pos_exn src ~pos ~len)
      ~len
  let subo ?(pos = 0) ?len src =
    let len = match len with None -> length src - pos | Some len -> len in
    Bigstring.To_string.subo
      src.buf ~pos:(buf_pos_exn src ~pos ~len)
      ~len
end

module Bigstring_dst = struct
  include Bigstring
  let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
    Bigstring.unsafe_blit
      ~src:src.buf ~src_pos:(unsafe_buf_pos src ~pos:src_pos)
      ~dst ~dst_pos
      ~len
  let create ~len = create len
end

let compact t =
  let len = t.hi - t.lo in
  Bigstring.blit ~src:t.buf ~src_pos:t.lo ~len ~dst:t.buf ~dst_pos:t.lo_min;
  t.lo <- t.lo_min + len;
  t.hi <- t.hi_max;
;;
let bounded_compact_stale t lo_min hi_max =
  fail t "Iobuf.bounded_compact got stale snapshot" (lo_min, hi_max)
    [%sexp_of: Lo_bound.t * Hi_bound.t]
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

let read_bin_prot reader t ~pos =
  let buf_pos = unsafe_buf_pos t ~pos in
  let pos_ref = ref buf_pos in
  let a = reader.Bin_prot.Type_class.read t.buf ~pos_ref in
  let len = !pos_ref - buf_pos in
  check_range t ~pos ~len;
  (a, len)

module Consume = struct
  type src = (read, seek) t
  module To (Dst : sig
      type t [@@deriving sexp_of]
      val create : len:int -> t
      val length : t -> int
      val get : t -> int -> char
      val set : t -> int -> char -> unit
      val unsafe_blit : (T.t, t) Blit.blit
    end) = struct
    include Test_blit.Make_distinct_and_test (Char_elt) (T_src) (Dst)

    let blit ~src ~dst ~dst_pos ~len =
      blit ~src ~src_pos:0 ~dst ~dst_pos ~len;
      unsafe_advance src len

    let blito ~src ?(src_len = length src) ~dst ?dst_pos () =
      blito ~src ~src_pos:0 ~src_len ~dst ?dst_pos ();
      unsafe_advance src src_len

    let unsafe_blit ~src ~dst ~dst_pos ~len =
      unsafe_blit ~src ~src_pos:0 ~dst ~dst_pos ~len;
      unsafe_advance src len

    let sub src ~len =
      let dst = sub src ~pos:0 ~len in
      unsafe_advance src len;
      dst

    let subo ?len src =
      let len = match len with None -> length src | Some len -> len in
      let dst = subo ~pos:0 ~len src in
      unsafe_advance src len;
      dst
  end
  module To_bytes    = To (Bytes_dst)
  module To_bigstring = To (Bigstring_dst)

  module To_string = struct
    include To_bytes

    let sub src ~len =
      let dst = String_dst.sub src ~len ~pos:0 in
      unsafe_advance src len;
      dst

    let subo ?len src =
      let len = match len with None -> length src | Some len -> len in
      let dst = String_dst.subo ~pos:0 ~len src in
      unsafe_advance src len;
      dst
  end

  type nonrec ('a, 'd, 'w) t = ('d, seek) t -> 'a
    constraint 'd = [> read ]

  let uadv t n x = unsafe_advance t n; x
  [@@inline always]

  let pos t len = buf_pos_exn t ~pos:0 ~len

  let tail_padded_fixed_string ~padding ~len t =
    uadv t len (Bigstring.get_tail_padded_fixed_string t.buf ~pos:(pos t len) ~padding ~len ())
  ;;

  let head_padded_fixed_string ~padding ~len t =
    uadv t len (Bigstring.get_head_padded_fixed_string t.buf ~pos:(pos t len) ~padding ~len ())
  ;;

  let bytes ~str_pos ~len t =
    let dst = Bytes.create (len + str_pos) in
    To_bytes.blit ~src:t ~dst ~len ~dst_pos:str_pos;
    dst
  ;;

  let string ~str_pos ~len t =
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:(bytes ~str_pos ~len t)

  let bigstring ~str_pos ~len t =
    let dst = Bigstring.create (len + str_pos) in
    To_bigstring.blit ~src:t ~dst ~len ~dst_pos:str_pos;
    dst
  ;;

  let byteso ?(str_pos = 0) ?len t =
    bytes t ~str_pos ~len:(match len with None -> length t | Some len -> len)
  ;;

  let stringo ?(str_pos = 0) ?len t =
    string t ~str_pos ~len:(match len with None -> length t | Some len -> len)
  ;;

  let bigstringo ?(str_pos = 0) ?len t =
    bigstring t ~str_pos ~len:(match len with None -> length t | Some len -> len)
  ;;

  let bin_prot reader t =
    let (a, len) = read_bin_prot reader t ~pos:0 in
    uadv t len a
  ;;
  let%test_unit "bin_prot char" =
    let t = of_string "abc" in
    let a = bin_prot Char.bin_reader_t t in
    let b = bin_prot Char.bin_reader_t t in
    [%test_eq: char] a 'a';
    [%test_eq: char] b 'b';
    [%test_eq: string] (to_string t) "c"
  ;;
  let%test_unit "bin_prot int" =
    let ints = [ 0; 1; -1; 12345; -67890; Int.min_value; Int.max_value; 666 ] in
    let buf = Bigstring.create 1000 in
    let _end_pos = List.fold ints ~init:0 ~f:(fun pos i -> Int.bin_write_t buf ~pos i) in
    let t = of_bigstring buf in
    List.iter ints ~f:(fun i -> [%test_eq: int] i (bin_prot Int.bin_reader_t t))
  ;;

  open Bigstring

  let len = 1
  let [@inline always] char            t = uadv t len (bigstring_unsafe_get      t.buf ~pos:(pos t len))
  let [@inline always] uint8           t = uadv t len (unsafe_get_uint8          t.buf ~pos:(pos t len))
  let [@inline always]  int8           t = uadv t len (unsafe_get_int8           t.buf ~pos:(pos t len))
  let len = 2
  let [@inline always]  int16_be       t = uadv t len (unsafe_get_int16_be       t.buf ~pos:(pos t len))
  let [@inline always]  int16_le       t = uadv t len (unsafe_get_int16_le       t.buf ~pos:(pos t len))
  let [@inline always] uint16_be       t = uadv t len (unsafe_get_uint16_be      t.buf ~pos:(pos t len))
  let [@inline always] uint16_le       t = uadv t len (unsafe_get_uint16_le      t.buf ~pos:(pos t len))
  let len = 4
  let [@inline always]  int32_be       t = uadv t len (unsafe_get_int32_be       t.buf ~pos:(pos t len))
  let [@inline always]  int32_le       t = uadv t len (unsafe_get_int32_le       t.buf ~pos:(pos t len))
  let [@inline always] uint32_be       t = uadv t len (unsafe_get_uint32_be      t.buf ~pos:(pos t len))
  let [@inline always] uint32_le       t = uadv t len (unsafe_get_uint32_le      t.buf ~pos:(pos t len))
  let len = 8
  let [@inline always]  int64_be       t = uadv t len (unsafe_get_int64_be_exn   t.buf ~pos:(pos t len))
  let [@inline always]  int64_le       t = uadv t len (unsafe_get_int64_le_exn   t.buf ~pos:(pos t len))
  let [@inline always] uint64_be       t = uadv t len (unsafe_get_uint64_be_exn  t.buf ~pos:(pos t len))
  let [@inline always] uint64_le       t = uadv t len (unsafe_get_uint64_le_exn  t.buf ~pos:(pos t len))
  let [@inline always]  int64_t_be     t = uadv t len (unsafe_get_int64_t_be     t.buf ~pos:(pos t len))
  let [@inline always]  int64_t_le     t = uadv t len (unsafe_get_int64_t_le     t.buf ~pos:(pos t len))
  let [@inline always]  int64_be_trunc t = uadv t len (unsafe_get_int64_be_trunc t.buf ~pos:(pos t len))
  let [@inline always]  int64_le_trunc t = uadv t len (unsafe_get_int64_le_trunc t.buf ~pos:(pos t len))
end

let write_bin_prot writer t ~pos a =
  let len = writer.Bin_prot.Type_class.size a in
  let buf_pos = buf_pos_exn t ~pos ~len in
  let stop_pos = writer.Bin_prot.Type_class.write t.buf ~pos:buf_pos a in
  if stop_pos - buf_pos = len then len
  else fail t "Iobuf.write_bin_prot got unexpected number of bytes written \
               (Bin_prot bug: Type_class.write disagrees with .size)"
         (`size_len len, `buf_pos buf_pos, `write_stop_pos stop_pos)
         [%sexp_of: [ `size_len of int ]
                    * [ `buf_pos of int ]
                    * [ `write_stop_pos of int ]]

(* [Itoa] provides a range of functions for integer to ASCII conversion, used by [Poke],
   [Fill] and their [Unsafe] versions.

   The implementation here is done in terms of negative decimals due to the properties of
   [Int.min_value]. Since the result of [Int.(abs min_value)] is [Int.min_value], an
   attempt to utilize a positive decimal loop by writing the sign and calling [Int.abs x]
   fails. The converse, with [- Int.max_value] works for both cases. *)
module Itoa = struct
  (* [num_digits x] returns the number of digits in [x] for non-positive integers
     ([num_digits 0] is defined as 1).

     The below tends to perform better than a binary search or [/= 10 while <> 0], likely
     due to decimal values for our applications skewing towards smaller numbers. *)
  let num_digits x =
    if x > -10 then 1
    else if x > -100 then 2
    else if x > -1000 then 3
    else if x > -10000 then 4
    else if x > -100000 then 5
    else if x > -1000000 then 6
    else if x > -10000000 then 7
    else if x > -100000000 then 8
    else if x > -1000000000 then 9
    else if arch_sixtyfour
    then
      if      x > -1000000000 * 10 then 10
      else if x > -1000000000 * 100 then 11
      else if x > -1000000000 * 1000 then 12
      else if x > -1000000000 * 10000 then 13
      else if x > -1000000000 * 100000 then 14
      else if x > -1000000000 * 1000000 then 15
      else if x > -1000000000 * 10000000 then 16
      else if x > -1000000000 * 100000000 then 17
      else if x > -1000000000 * 1000000000 then 18
      else 19
    else
      10

  let%test _ = String.length (Int.to_string Int.min_value) <= 19 + 1

  (* Despite the div/mod by a constant optimizations, it's a slight savings to avoid a
     second div/mod. Note also that passing in an [int ref], rather than creating the ref
     locally here, results in allocation on the benchmarks. *)
  let unsafe_poke_negative_decimal_without_sign t ~pos ~len int =
    let int = ref int in
    for pos = pos + len - 1 downto pos do
      let x = !int in
      int := !int / 10;
      bigstring_unsafe_set t.buf ~pos (Char.unsafe_of_int (48 + ((- x) + !int * 10)));
    done

  let unsafe_poke_negative_decimal t ~pos ~len int =
    bigstring_unsafe_set t.buf ~pos '-';
    (* +1 and -1 to account for '-' *)
    unsafe_poke_negative_decimal_without_sign t ~pos:(pos + 1) ~len:(len - 1) int

  let poke_decimal t ~pos int =
    if int < 0 then (
      let len = 1 + num_digits int in
      unsafe_poke_negative_decimal t ~pos:(buf_pos_exn t ~pos ~len) ~len int;
      len)
    else (
      let len = num_digits (- int) in
      unsafe_poke_negative_decimal_without_sign
        t ~pos:(buf_pos_exn t ~pos ~len) ~len (- int);
      len)

  let unsafe_poke_decimal t ~pos int =
    if int < 0 then (
      let len = 1 + num_digits int in
      unsafe_poke_negative_decimal t ~pos:(unsafe_buf_pos t ~pos) ~len int;
      len)
    else (
      let len = num_digits (- int) in
      unsafe_poke_negative_decimal_without_sign
        t ~pos:(unsafe_buf_pos t ~pos) ~len (- int);
      len)
end

module Fill = struct
  type nonrec ('a, 'd, 'w) t = (read_write, seek) t -> 'a -> unit
    constraint 'd = [> read ]

  let pos t len = buf_pos_exn t ~pos:0 ~len
  let uadv = unsafe_advance

  let tail_padded_fixed_string ~padding ~len t src =
    Bigstring.set_tail_padded_fixed_string ~padding ~len t.buf ~pos:(pos t len) src;
    uadv t len
  ;;

  let head_padded_fixed_string ~padding ~len t src =
    Bigstring.set_head_padded_fixed_string ~padding ~len t.buf ~pos:(pos t len) src;
    uadv t len
  ;;

  let bytes ~str_pos ~len t src =
    Bigstring.From_bytes.blit ~src ~src_pos:str_pos ~len ~dst:t.buf ~dst_pos:(pos t len);
    uadv t len
  ;;

  let string ~str_pos ~len t src =
    Bigstring.From_string.blit ~src ~src_pos:str_pos ~len ~dst:t.buf ~dst_pos:(pos t len);
    uadv t len
  ;;

  let bigstring ~str_pos ~len t src =
    Bigstring.blit ~src ~src_pos:str_pos ~len ~dst:t.buf ~dst_pos:(pos t len);
    uadv t len
  ;;

  let byteso ?(str_pos = 0) ?len t src =
    bytes t src ~str_pos
      ~len:(match len with None -> Bytes.length src - str_pos | Some len -> len)
  ;;

  let stringo ?(str_pos = 0) ?len t src =
    string t src ~str_pos
      ~len:(match len with None -> String.length src - str_pos | Some len -> len)
  ;;

  let bigstringo ?(str_pos = 0) ?len t src =
    bigstring t src ~str_pos
      ~len:(match len with None -> Bigstring.length src - str_pos | Some len -> len)
  ;;

  let bin_prot writer t a = write_bin_prot writer t ~pos:0 a |> uadv t

  open Bigstring

  let len = 1
  let [@inline always] char           t c = bigstring_unsafe_set  t.buf c ~pos:(pos t len); uadv t len
  let [@inline always] uint8          t i = unsafe_set_uint8      t.buf i ~pos:(pos t len); uadv t len
  let [@inline always]  int8          t i = unsafe_set_int8       t.buf i ~pos:(pos t len); uadv t len
  let len = 2
  let [@inline always]  int16_be      t i = unsafe_set_int16_be   t.buf i ~pos:(pos t len); uadv t len
  let [@inline always]  int16_le      t i = unsafe_set_int16_le   t.buf i ~pos:(pos t len); uadv t len
  let [@inline always] uint16_be      t i = unsafe_set_uint16_be  t.buf i ~pos:(pos t len); uadv t len
  let [@inline always] uint16_le      t i = unsafe_set_uint16_le  t.buf i ~pos:(pos t len); uadv t len
  let len = 4
  let [@inline always] int32_be       t i = unsafe_set_int32_be   t.buf i ~pos:(pos t len); uadv t len
  let [@inline always] int32_le       t i = unsafe_set_int32_le   t.buf i ~pos:(pos t len); uadv t len
  let [@inline always] uint32_be      t i = unsafe_set_uint32_be  t.buf i ~pos:(pos t len); uadv t len
  let [@inline always] uint32_le      t i = unsafe_set_uint32_le  t.buf i ~pos:(pos t len); uadv t len
  let len = 8
  let [@inline always] int64_be       t i = unsafe_set_int64_be   t.buf i ~pos:(pos t len); uadv t len
  let [@inline always] int64_le       t i = unsafe_set_int64_le   t.buf i ~pos:(pos t len); uadv t len
  let [@inline always] uint64_be      t i = unsafe_set_uint64_be  t.buf i ~pos:(pos t len); uadv t len
  let [@inline always] uint64_le      t i = unsafe_set_uint64_le  t.buf i ~pos:(pos t len); uadv t len
  let [@inline always] int64_t_be     t i = unsafe_set_int64_t_be t.buf i ~pos:(pos t len); uadv t len
  let [@inline always] int64_t_le     t i = unsafe_set_int64_t_le t.buf i ~pos:(pos t len); uadv t len
  let [@inline always] int64_be_trunc t i = unsafe_set_int64_be   t.buf i ~pos:(pos t len); uadv t len
  let [@inline always] int64_le_trunc t i = unsafe_set_int64_le   t.buf i ~pos:(pos t len); uadv t len

  let decimal        t i = uadv t (Itoa.poke_decimal t ~pos:0 i)
end

module Peek = struct
  type src = (read, no_seek) t
  module To_bytes    = Test_blit.Make_distinct_and_test (Char_elt) (T_src) (Bytes_dst)
  module To_bigstring = Test_blit.Make_distinct_and_test (Char_elt) (T_src) (Bigstring_dst)

  module To_string = struct
    include To_bytes
    include String_dst
  end

  type nonrec ('a, 'd, 'w) t = ('d, 'w) t -> pos:int -> 'a
    constraint 'd = [> read ]

  let spos = buf_pos_exn (* "safe position" *)

  let tail_padded_fixed_string ~padding ~len t ~pos =
    Bigstring.get_tail_padded_fixed_string t.buf ~padding ~len ~pos:(spos t ~len ~pos) ()
  ;;

  let head_padded_fixed_string ~padding ~len t ~pos =
    Bigstring.get_head_padded_fixed_string t.buf ~padding ~len ~pos:(spos t ~len ~pos) ()
  ;;

  let bytes ~str_pos ~len t ~pos =
    let dst = Bytes.create (len + str_pos) in
    Bigstring.To_bytes.blit ~src:t.buf ~src_pos:(spos t ~len ~pos) ~len
      ~dst ~dst_pos:str_pos;
    dst
  ;;

  let string ~str_pos ~len t ~pos =
    Bytes.unsafe_to_string
      ~no_mutation_while_string_reachable:(bytes ~str_pos ~len t ~pos)

  let bigstring ~str_pos ~len t ~pos =
    let dst = Bigstring.create (len + str_pos) in
    Bigstring.blit ~src:t.buf ~src_pos:(spos t ~len ~pos) ~len ~dst ~dst_pos:str_pos;
    dst
  ;;

  let byteso ?(str_pos = 0) ?len t ~pos =
    bytes t ~pos ~str_pos
      ~len:(match len with None -> length t - pos | Some len -> len)
  ;;

  let stringo ?(str_pos = 0) ?len t ~pos =
    string t ~pos ~str_pos
      ~len:(match len with None -> length t - pos | Some len -> len)
  ;;

  let bigstringo ?(str_pos = 0) ?len t ~pos =
    bigstring t ~pos ~str_pos
      ~len:(match len with None -> length t - pos | Some len -> len)
  ;;

  let bin_prot reader t ~pos = read_bin_prot reader t ~pos |> fst
  let%test_unit "bin_prot char" =
    let t = of_string "abc" in
    let a = bin_prot Char.bin_reader_t t ~pos:0 in
    let b = bin_prot Char.bin_reader_t t ~pos:1 in
    [%test_eq: char] a 'a';
    [%test_eq: char] b 'b';
    [%test_eq: string] (to_string t) "abc"
  ;;
  let%test_unit "bin_prot int" =
    let ints = [ 0; 1; -1; 12345; -67890; Int.min_value; Int.max_value; 666 ] in
    let buf = Bigstring.create 1000 in
    let end_pos = List.fold ints ~init:0 ~f:(fun pos i -> Int.bin_write_t buf ~pos i) in
    let t = of_bigstring buf in
    List.fold ints ~init:0 ~f:(fun pos i ->
      [%test_eq: int] i (bin_prot Int.bin_reader_t t ~pos);
      pos + Int.bin_size_t i)
    |> (fun end_pos' -> [%test_eq: int] end_pos end_pos')
  ;;

  let index t ?(pos = 0) ?(len = length t - pos) c =
    let pos = spos t ~len ~pos in
    Option.map (Bigstring.find ~pos ~len c t.buf) ~f:(fun x -> x - t.lo)
  ;;

  open Bigstring

  let len = 1
  let [@inline always] char            t ~pos = T_src.get                 t                        pos
  let [@inline always] uint8           t ~pos = unsafe_get_uint8          t.buf ~pos:(spos t ~len ~pos)
  let [@inline always]  int8           t ~pos = unsafe_get_int8           t.buf ~pos:(spos t ~len ~pos)
  let len = 2
  let [@inline always]  int16_be       t ~pos = unsafe_get_int16_be       t.buf ~pos:(spos t ~len ~pos)
  let [@inline always]  int16_le       t ~pos = unsafe_get_int16_le       t.buf ~pos:(spos t ~len ~pos)
  let [@inline always] uint16_be       t ~pos = unsafe_get_uint16_be      t.buf ~pos:(spos t ~len ~pos)
  let [@inline always] uint16_le       t ~pos = unsafe_get_uint16_le      t.buf ~pos:(spos t ~len ~pos)
  let len = 4
  let [@inline always]  int32_be       t ~pos = unsafe_get_int32_be       t.buf ~pos:(spos t ~len ~pos)
  let [@inline always]  int32_le       t ~pos = unsafe_get_int32_le       t.buf ~pos:(spos t ~len ~pos)
  let [@inline always] uint32_be       t ~pos = unsafe_get_uint32_be      t.buf ~pos:(spos t ~len ~pos)
  let [@inline always] uint32_le       t ~pos = unsafe_get_uint32_le      t.buf ~pos:(spos t ~len ~pos)
  let len = 8
  let [@inline always] int64_be        t ~pos = unsafe_get_int64_be_exn   t.buf ~pos:(spos t ~len ~pos)
  let [@inline always] int64_le        t ~pos = unsafe_get_int64_le_exn   t.buf ~pos:(spos t ~len ~pos)
  let [@inline always] uint64_be       t ~pos = unsafe_get_uint64_be_exn  t.buf ~pos:(spos t ~len ~pos)
  let [@inline always] uint64_le       t ~pos = unsafe_get_uint64_le_exn  t.buf ~pos:(spos t ~len ~pos)
  let [@inline always]  int64_t_be     t ~pos = unsafe_get_int64_t_be     t.buf ~pos:(spos t ~len ~pos)
  let [@inline always]  int64_t_le     t ~pos = unsafe_get_int64_t_le     t.buf ~pos:(spos t ~len ~pos)
  let [@inline always]  int64_be_trunc t ~pos = unsafe_get_int64_be_trunc t.buf ~pos:(spos t ~len ~pos)
  let [@inline always]  int64_le_trunc t ~pos = unsafe_get_int64_le_trunc t.buf ~pos:(spos t ~len ~pos)
end

module Poke = struct
  type nonrec ('a, 'd, 'w) t = (read_write, 'w) t -> pos:int -> 'a -> unit
    constraint 'd = [> read ]

  let spos = buf_pos_exn (* "safe position" *)

  let tail_padded_fixed_string ~padding ~len t ~pos src =
    Bigstring.set_tail_padded_fixed_string ~padding ~len t.buf ~pos:(spos t ~len ~pos) src
  ;;

  let head_padded_fixed_string ~padding ~len t ~pos src =
    Bigstring.set_head_padded_fixed_string ~padding ~len t.buf ~pos:(spos t ~len ~pos) src
  ;;

  let bytes ~str_pos ~len t ~pos src =
    Bigstring.From_bytes.blit ~src ~src_pos:str_pos ~len
      ~dst:t.buf ~dst_pos:(spos t ~len ~pos)
  ;;

  let string ~str_pos ~len t ~pos src =
    Bigstring.From_string.blit ~src ~src_pos:str_pos ~len
      ~dst:t.buf ~dst_pos:(spos t ~len ~pos)
  ;;

  let bigstring ~str_pos ~len t ~pos src =
    Bigstring.blit ~src ~src_pos:str_pos ~len ~dst:t.buf ~dst_pos:(spos t ~len ~pos)
  ;;

  let byteso ?(str_pos = 0) ?len t ~pos src =
    bytes t ~str_pos ~pos src
      ~len:(match len with None -> Bytes.length src - str_pos | Some len -> len)
  ;;

  let stringo ?(str_pos = 0) ?len t ~pos src =
    string t ~str_pos ~pos src
      ~len:(match len with None -> String.length src - str_pos | Some len -> len)
  ;;

  let bigstringo ?(str_pos = 0) ?len t ~pos src =
    bigstring t ~str_pos ~pos src
      ~len:(match len with None -> Bigstring.length src - str_pos | Some len -> len)
  ;;

  let bin_prot writer t ~pos a = write_bin_prot writer t ~pos a |> (ignore : int -> unit)
  let%test_unit _ =
    let t = of_string "abc" in
    bin_prot Char.bin_writer_t t 'd' ~pos:0;
    bin_prot Char.bin_writer_t t 'e' ~pos:1;
    [%test_eq: string] "dec" (to_string t);
    flip_lo t;
    assert (try bin_prot String.bin_writer_t t "fgh" ~pos:0; false with _ -> true);
    assert (is_empty t);
    reset t;
    [%test_eq: string] "dec" (to_string t);
    bin_prot Char.bin_writer_t t 'i' ~pos:0;
    [%test_eq: string] "iec" (to_string t)
  ;;

  open Bigstring

  let len = 1
  let [@inline always] char            t ~pos c = T_src.set             t                        pos  c
  let [@inline always] uint8           t ~pos i = unsafe_set_uint8      t.buf ~pos:(spos t ~len ~pos) i
  let [@inline always]  int8           t ~pos i = unsafe_set_int8       t.buf ~pos:(spos t ~len ~pos) i
  let len = 2
  let [@inline always]  int16_be       t ~pos i = unsafe_set_int16_be   t.buf ~pos:(spos t ~len ~pos) i
  let [@inline always]  int16_le       t ~pos i = unsafe_set_int16_le   t.buf ~pos:(spos t ~len ~pos) i
  let [@inline always] uint16_be       t ~pos i = unsafe_set_uint16_be  t.buf ~pos:(spos t ~len ~pos) i
  let [@inline always] uint16_le       t ~pos i = unsafe_set_uint16_le  t.buf ~pos:(spos t ~len ~pos) i
  let len = 4
  let [@inline always]  int32_be       t ~pos i = unsafe_set_int32_be   t.buf ~pos:(spos t ~len ~pos) i
  let [@inline always]  int32_le       t ~pos i = unsafe_set_int32_le   t.buf ~pos:(spos t ~len ~pos) i
  let [@inline always] uint32_be       t ~pos i = unsafe_set_uint32_be  t.buf ~pos:(spos t ~len ~pos) i
  let [@inline always] uint32_le       t ~pos i = unsafe_set_uint32_le  t.buf ~pos:(spos t ~len ~pos) i
  let len = 8
  let [@inline always]  int64_be       t ~pos i = unsafe_set_int64_be   t.buf ~pos:(spos t ~len ~pos) i
  let [@inline always]  int64_le       t ~pos i = unsafe_set_int64_le   t.buf ~pos:(spos t ~len ~pos) i
  let [@inline always] uint64_be       t ~pos i = unsafe_set_uint64_be  t.buf ~pos:(spos t ~len ~pos) i
  let [@inline always] uint64_le       t ~pos i = unsafe_set_uint64_le  t.buf ~pos:(spos t ~len ~pos) i
  let [@inline always]  int64_t_be     t ~pos i = unsafe_set_int64_t_be t.buf ~pos:(spos t ~len ~pos) i
  let [@inline always]  int64_t_le     t ~pos i = unsafe_set_int64_t_le t.buf ~pos:(spos t ~len ~pos) i
  let [@inline always]  int64_be_trunc t ~pos i = unsafe_set_int64_be   t.buf ~pos:(spos t ~len ~pos) i
  let [@inline always]  int64_le_trunc t ~pos i = unsafe_set_int64_le   t.buf ~pos:(spos t ~len ~pos) i

  let decimal = Itoa.poke_decimal
end

let crc32 { buf ; lo ; hi ; _ } =
  Crc.bigstring_crc32 buf ~pos:lo ~len:(hi - lo)

module Blit = struct
  type 'rw t_no_seek = ('rw, no_seek) t
  module T_dst = struct
    include T_src
    let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
      Bigstring.unsafe_blit ~len
        ~src:src.buf ~src_pos:(unsafe_buf_pos src ~pos:src_pos)
        ~dst:dst.buf ~dst_pos:(unsafe_buf_pos dst ~pos:dst_pos)
  end
  include Test_blit.Make_and_test (Char_elt) (T_dst)
  (* Workaround the inability of the compiler to inline in the presence of functors. *)
  let unsafe_blit = T_dst.unsafe_blit
end

module Blit_consume = struct
  let unsafe_blit ~src ~dst ~dst_pos ~len =
    Blit.unsafe_blit ~src ~src_pos:0 ~dst ~dst_pos ~len;
    unsafe_advance src len
  let blit ~src ~dst ~dst_pos ~len =
    Blit.blit ~src ~src_pos:0 ~dst ~dst_pos ~len;
    unsafe_advance src len
  let blito ~src ?(src_len = length src) ~dst ?(dst_pos = 0) () =
    blit ~src ~dst ~dst_pos ~len:src_len
  let sub src ~len =
    let dst = Blit.sub src ~pos:0 ~len in
    unsafe_advance src len;
    dst
  let subo ?len src =
    let len = match len with None -> length src | Some len -> len in
    sub src ~len
end

module Blit_fill = struct
  let unsafe_blit ~src ~src_pos ~dst ~len =
    Blit.unsafe_blit ~src ~src_pos ~dst ~dst_pos:0 ~len;
    unsafe_advance dst len
  let blit ~src ~src_pos ~dst ~len =
    Blit.blit ~src ~src_pos ~dst ~dst_pos:0 ~len;
    unsafe_advance dst len
  let blito ~src ?(src_pos = 0) ?(src_len = length src - src_pos) ~dst () =
    blit ~src ~src_pos ~dst ~len:src_len
end

module Blit_consume_and_fill = struct
  let unsafe_blit ~src ~dst ~len =
    if phys_equal src dst then advance src len else begin
      Blit.unsafe_blit ~src ~src_pos:0 ~dst ~dst_pos:0 ~len;
      unsafe_advance src len;
      unsafe_advance dst len
    end
  let blit ~src ~dst ~len =
    if phys_equal src dst then advance src len else begin
      Blit.blit ~src ~src_pos:0 ~dst ~dst_pos:0 ~len;
      unsafe_advance src len;
      unsafe_advance dst len
    end
  let blito ~src ?(src_len = length src) ~dst () =
    blit ~src ~dst ~len:src_len
end

let bin_prot_length_prefix_bytes = 4

let consume_bin_prot t bin_prot_reader =
  let result =
    if length t < bin_prot_length_prefix_bytes then
      error "Iobuf.consume_bin_prot not enough data to read length" t
        ([%sexp_of: (_, _) t])
    else begin
      let mark = t.lo in
      let v_len = Consume.int32_be t in
      if v_len > length t then begin
        t.lo <- mark;
        error "Iobuf.consume_bin_prot not enough data to read value" (v_len, t)
          ([%sexp_of: int * (_, _) t]);
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
        ([%sexp_of: int * (_, _) t])
    else begin
      Fill.int32_be t v_len;
      Fill.bin_prot writer t v;
      Ok ();
    end
  in
  result;
;;

module Expert = struct
  let buf t    = t.buf
  let hi_max t = t.hi_max
  let hi t     = t.hi
  let lo t     = t.lo
  let lo_min t = t.lo_min

  let to_bigstring_shared ?pos ?len t =
    let t_len = length t in
    let pos, len = Ordered_collection_common.get_pos_len_exn ?pos ?len ~length:t_len in
    Bigstring.sub_shared t.buf ~pos:(t.lo + pos) ~len
  ;;

  let to_iovec_shared ?pos ?len t =
    let t_len = length t in
    let pos, len = Ordered_collection_common.get_pos_len_exn ?pos ?len ~length:t_len in
    Unix.IOVec.of_bigstring t.buf ~pos:(t.lo + pos) ~len
  ;;

  let reinitialize_of_bigstring t ~pos ~len buf =
    let str_len = Bigstring.length buf in
    if pos < 0 || pos > str_len then
      raise_s [%message "Expert.reinitialize_of_bigstring got invalid pos"
                          (pos     : int)
                          (str_len : int)];
    let max_len = str_len - pos in
    if len < 0 || len > max_len then
      raise_s [%message "Expert.reinitialize_of_bigstring got invalid len"
                          (len     : int)
                          (max_len : int)];
    let lo = pos in
    let hi = pos + len in
    Fields.Direct.set_all_mutable_fields
      t
      ~buf
      ~lo_min:lo
      ~lo
      ~hi
      ~hi_max:hi
  ;;

  let set_bounds_and_buffer ~src ~dst = set_bounds_and_buffer ~src ~dst
  let set_bounds_and_buffer_sub ~pos ~len ~src ~dst =
    (set_bounds_and_buffer_sub [@inlined]) ~pos ~len ~src ~dst
end

type ok_or_eof = Ok | Eof [@@deriving compare, sexp_of]

module File_descr = Iobuf_intf.Unix.File_descr

let input t ch =
  match Bigstring.input ch t.buf ~pos:t.lo ~len:(length t) with
  | n                                            -> unsafe_advance t n; Ok
  | exception Bigstring.IOError (n, End_of_file) -> unsafe_advance t n; Eof
;;

let read t fd =
  match Bigstring.read fd t.buf ~pos:t.lo ~len:(length t) with
  | n                                            -> unsafe_advance t n; Ok
  | exception Bigstring.IOError (n, End_of_file) -> unsafe_advance t n; Eof
;;

let read_assume_fd_is_nonblocking t fd =
  let nread =
    Bigstring.read_assume_fd_is_nonblocking fd t.buf ~pos:t.lo ~len:(length t)
  in
  if Syscall_result.Int.is_ok nread then
    unsafe_advance t (Syscall_result.Int.ok_exn nread);
  Syscall_result.ignore_ok_value nread
;;

let pread_assume_fd_is_nonblocking t fd ~offset =
  let nread =
    Bigstring.pread_assume_fd_is_nonblocking fd ~offset t.buf ~pos:t.lo ~len:(length t)
  in
  unsafe_advance t nread
;;

let recvfrom_assume_fd_is_nonblocking t fd =
  let nread, sockaddr =
    Bigstring.recvfrom_assume_fd_is_nonblocking fd t.buf ~pos:t.lo ~len:(length t)
  in
  unsafe_advance t nread;
  sockaddr
;;

[%%ifdef JSC_RECVMMSG]

(* Allocate and pre-populate the [struct mmsghdr]s and associated [struct iovec]s. Reusing
   this context reduces the cost of calls to [recvmmsg] considerably if the iobuf array is
   large. *)
module Recvmmsg_context = struct
  type ctx

  external unsafe_ctx : ([> write], seek) t array -> ctx = "iobuf_recvmmsg_ctx"

  let ctx ts =
    if Array.for_all ts ~f:(fun t -> length t = capacity t) then
      unsafe_ctx ts
    else
      failwiths "Recvmmsg_context.create: all buffers must be reset" ts
        [%sexp_of: (_, _) t array]
  ;;

  (* we retain a reference to the underlying bigstrings, in the event that callers
     mistakenly use set_bounds_and_buffer. Since we've cached the underlying memory
     referenced by the bigstring, we want to prevent it from being garbage collected and
     released. *)
  type nonrec t = {
    iobufs : (read_write, seek) t array;
    bstrs  : Bigstring.t array;
    ctx    : ctx;
  }

  let create iobufs = {
    iobufs;
    bstrs = Array.map iobufs ~f:(fun iobuf -> iobuf.buf);
    ctx = ctx iobufs;
  }
end

external unsafe_recvmmsg_assume_fd_is_nonblocking
  : File_descr.t
    -> (read_write, seek) t array
    -> Recvmmsg_context.ctx
    -> Unix.Syscall_result.Int.t
  = "iobuf_recvmmsg_assume_fd_is_nonblocking_stub" [@@noalloc]

let recvmmsg_assume_fd_is_nonblocking fd { Recvmmsg_context. iobufs; ctx; _ } =
  unsafe_recvmmsg_assume_fd_is_nonblocking fd iobufs ctx
;;

let recvmmsg_assume_fd_is_nonblocking =
  (* We link with [--wrap recvmmsg].  If we have compiled on a machine with recvmmsg
     (e.g., CentOS 6) but then run on a machine without (e.g., CentOS 5), our wrapped
     [recvmmsg] always returns -1 and sets errno to ENOSYS. *)
  match
    Unix.Syscall_result.Int.to_result
      (let fd = File_descr.of_int (-1) in
       recvmmsg_assume_fd_is_nonblocking fd (Recvmmsg_context.create [||]))
  with
  | Error ENOSYS -> Or_error.unimplemented "Iobuf.recvmmsg_assume_fd_is_nonblocking"
  | _            -> Ok                     recvmmsg_assume_fd_is_nonblocking
;;

[%%else]
(* not JSC_RECVMMSG *)

module Recvmmsg_context = struct
  type t = unit
  let create = ignore
end

let recvmmsg_assume_fd_is_nonblocking =
  Or_error.unimplemented "Iobuf.recvmmsg_assume_fd_is_nonblocking"
;;

[%%endif]
(* JSC_RECVMMSG *)

let unsafe_sent t result =
  if Syscall_result.Int.is_ok result
  then (unsafe_advance t (Syscall_result.Int.ok_exn result);
        Syscall_result.unit)
  else Syscall_result.Int.reinterpret_error_exn result

(* Don't use [Or_error.map].  The natural usage results in a partially applied function,
   which is slower to call. *)
let send_nonblocking_no_sigpipe () =
  match Bigstring.send_nonblocking_no_sigpipe with
  | Error _ as e -> e
  | Ok send ->
    Ok (fun t fd -> unsafe_sent t (send fd t.buf ~pos:t.lo ~len:(length t)))
;;

let sendto_nonblocking_no_sigpipe () =
  match Bigstring.sendto_nonblocking_no_sigpipe with
  | Error _ as e -> e
  | Ok sendto ->
    Ok (fun t fd addr -> unsafe_sent t (sendto fd t.buf ~pos:t.lo ~len:(length t) addr))
;;

let output t ch =
  let nwritten =
    Bigstring.output ch t.buf ~pos:t.lo ~len:(length t)
  in
  unsafe_advance t nwritten
;;

let write t fd =
  let nwritten =
    Bigstring.write fd t.buf ~pos:t.lo ~len:(length t)
  in
  unsafe_advance t nwritten
;;

let write_assume_fd_is_nonblocking t fd =
  (* This is safe because of the invariant of [t] that the window is within the buffer
     (unless the user has violated the invariant with an unsafe operation). *)
  let nwritten =
    Bigstring.unsafe_write_assume_fd_is_nonblocking fd t.buf ~pos:t.lo ~len:(length t)
  in
  unsafe_advance t nwritten
;;

let pwrite_assume_fd_is_nonblocking t fd ~offset =
  let nwritten =
    Bigstring.pwrite_assume_fd_is_nonblocking fd ~offset t.buf ~pos:t.lo ~len:(length t)
  in
  unsafe_advance t nwritten
;;

module Unsafe = struct
  module Consume = struct
    (* copy of Consume with pos replaced by an unsafe version *)

    type src = Consume.src
    module To_bytes = struct
      include Consume.To_bytes
      let blit = unsafe_blit
    end
    module To_bigstring = struct
      include Consume.To_bigstring
      let blit = unsafe_blit
    end

    module To_string = Consume.To_string

    type ('a, 'd, 'w) t = ('a, 'd, 'w) Consume.t

    let uadv t n x = unsafe_advance t n; x [@@inline always]
    let upos t = unsafe_buf_pos t ~pos:0

    let tail_padded_fixed_string ~padding ~len t =
      uadv t len
        (Bigstring.get_tail_padded_fixed_string t.buf ~pos:(upos t) ~padding ~len ())
    ;;

    let head_padded_fixed_string ~padding ~len t =
      uadv t len
        (Bigstring.get_head_padded_fixed_string t.buf ~pos:(upos t) ~padding ~len ())
    ;;

    let bytes      = Consume.bytes
    let string     = Consume.string
    let bigstring  = Consume.bigstring
    let byteso     = Consume.byteso
    let stringo    = Consume.stringo
    let bigstringo = Consume.bigstringo

    let bin_prot = Consume.bin_prot

    open Bigstring

    let len = 1
    let [@inline always] char            t = uadv t len (bigstring_unsafe_get      t.buf ~pos:(upos t))
    let [@inline always] uint8           t = uadv t len (unsafe_get_uint8          t.buf ~pos:(upos t))
    let [@inline always]  int8           t = uadv t len (unsafe_get_int8           t.buf ~pos:(upos t))
    let len = 2
    let [@inline always]  int16_be       t = uadv t len (unsafe_get_int16_be       t.buf ~pos:(upos t))
    let [@inline always]  int16_le       t = uadv t len (unsafe_get_int16_le       t.buf ~pos:(upos t))
    let [@inline always] uint16_be       t = uadv t len (unsafe_get_uint16_be      t.buf ~pos:(upos t))
    let [@inline always] uint16_le       t = uadv t len (unsafe_get_uint16_le      t.buf ~pos:(upos t))
    let len = 4
    let [@inline always]  int32_be       t = uadv t len (unsafe_get_int32_be       t.buf ~pos:(upos t))
    let [@inline always]  int32_le       t = uadv t len (unsafe_get_int32_le       t.buf ~pos:(upos t))
    let [@inline always] uint32_be       t = uadv t len (unsafe_get_uint32_be      t.buf ~pos:(upos t))
    let [@inline always] uint32_le       t = uadv t len (unsafe_get_uint32_le      t.buf ~pos:(upos t))
    let len = 8
    let [@inline always]  int64_be       t = uadv t len (unsafe_get_int64_be_exn   t.buf ~pos:(upos t))
    let [@inline always]  int64_le       t = uadv t len (unsafe_get_int64_le_exn   t.buf ~pos:(upos t))
    let [@inline always] uint64_be       t = uadv t len (unsafe_get_uint64_be_exn  t.buf ~pos:(upos t))
    let [@inline always] uint64_le       t = uadv t len (unsafe_get_uint64_le_exn  t.buf ~pos:(upos t))
    let [@inline always]  int64_t_be     t = uadv t len (unsafe_get_int64_t_be     t.buf ~pos:(upos t))
    let [@inline always]  int64_t_le     t = uadv t len (unsafe_get_int64_t_le     t.buf ~pos:(upos t))
    let [@inline always]  int64_be_trunc t = uadv t len (unsafe_get_int64_be_trunc t.buf ~pos:(upos t))
    let [@inline always]  int64_le_trunc t = uadv t len (unsafe_get_int64_le_trunc t.buf ~pos:(upos t))
  end

  module Fill = struct
    type ('a, 'd, 'w) t = ('a, 'd, 'w) Fill.t

    (* copy with unsafe pos *)

    let upos t _len = unsafe_buf_pos t ~pos:0
    let uadv t n = unsafe_advance t n

    let tail_padded_fixed_string ~padding ~len t src =
      Bigstring.set_tail_padded_fixed_string ~padding ~len t.buf ~pos:(upos t len) src;
      uadv t len
    ;;

    let head_padded_fixed_string ~padding ~len t src =
      Bigstring.set_head_padded_fixed_string ~padding ~len t.buf ~pos:(upos t len) src;
      uadv t len
    ;;

    let bytes ~str_pos ~len t src =
      Bigstring.From_bytes.blit ~src ~src_pos:str_pos ~len
        ~dst:t.buf ~dst_pos:(upos t len);
      uadv t len
    ;;

    let string ~str_pos ~len t src =
      Bigstring.From_string.blit ~src ~src_pos:str_pos ~len
        ~dst:t.buf ~dst_pos:(upos t len);
      uadv t len
    ;;

    let bigstring ~str_pos ~len t src =
      Bigstring.blit ~src ~src_pos:str_pos ~len ~dst:t.buf ~dst_pos:(upos t len);
      uadv t len
    ;;

    let byteso ?(str_pos = 0) ?len t src =
      bytes t src ~str_pos
        ~len:(match len with None -> Bytes.length src - str_pos | Some len -> len)
    ;;

    let stringo ?(str_pos = 0) ?len t src =
      string t src ~str_pos
        ~len:(match len with None -> String.length src - str_pos | Some len -> len)
    ;;

    let bigstringo ?(str_pos = 0) ?len t src =
      bigstring t src ~str_pos
        ~len:(match len with None -> Bigstring.length src - str_pos | Some len -> len)
    ;;

    let bin_prot = Fill.bin_prot

    open Bigstring

    let len = 1
    let [@inline always] char        t c = bigstring_unsafe_set  t.buf c ~pos:(upos t len); uadv t len
    let len = 2
    let [@inline always]  int16_be   t i = unsafe_set_int16_be   t.buf i ~pos:(upos t len); uadv t len
    let [@inline always]  int16_le   t i = unsafe_set_int16_le   t.buf i ~pos:(upos t len); uadv t len
    let [@inline always] uint16_be   t i = unsafe_set_uint16_be  t.buf i ~pos:(upos t len); uadv t len
    let [@inline always] uint16_le   t i = unsafe_set_uint16_le  t.buf i ~pos:(upos t len); uadv t len
    let len = 4
    let [@inline always]  int32_be   t i = unsafe_set_int32_be   t.buf i ~pos:(upos t len); uadv t len
    let [@inline always]  int32_le   t i = unsafe_set_int32_le   t.buf i ~pos:(upos t len); uadv t len
    let [@inline always] uint32_be   t i = unsafe_set_uint32_be  t.buf i ~pos:(upos t len); uadv t len
    let [@inline always] uint32_le   t i = unsafe_set_uint32_le  t.buf i ~pos:(upos t len); uadv t len
    let len = 8
    let [@inline always] int64_be    t i = unsafe_set_int64_be   t.buf i ~pos:(upos t len); uadv t len
    let [@inline always] int64_le    t i = unsafe_set_int64_le   t.buf i ~pos:(upos t len); uadv t len
    let [@inline always] uint64_be   t i = unsafe_set_uint64_be  t.buf i ~pos:(upos t len); uadv t len
    let [@inline always] uint64_le   t i = unsafe_set_uint64_le  t.buf i ~pos:(upos t len); uadv t len
    let [@inline always] int64_t_be  t i = unsafe_set_int64_t_be t.buf i ~pos:(upos t len); uadv t len
    let [@inline always] int64_t_le  t i = unsafe_set_int64_t_le t.buf i ~pos:(upos t len); uadv t len

    (* Bigstring int8 accessors are slow C calls.  Use the fast char primitive. *)
    let [@inline always] uint8 t i = char t (Char.unsafe_of_int i)
    let [@inline always]  int8 t i = char t (Char.unsafe_of_int i)

    let [@inline always] int64_be_trunc t i = unsafe_set_int64_be t.buf i ~pos:(upos t len); uadv t len
    let [@inline always] int64_le_trunc t i = unsafe_set_int64_le t.buf i ~pos:(upos t len); uadv t len

    let decimal        t i = uadv t (Itoa.unsafe_poke_decimal t ~pos:0 i)
  end

  module Peek = struct
    type src = Peek.src
    module To_bytes     = struct include Peek.To_bytes     let blit = unsafe_blit end
    module To_bigstring = struct include Peek.To_bigstring let blit = unsafe_blit end

    module To_string = Peek.To_string

    type ('a, 'd, 'w) t = ('a, 'd, 'w) Peek.t

    let upos = unsafe_buf_pos

    let tail_padded_fixed_string ~padding ~len t ~pos =
      Bigstring.get_tail_padded_fixed_string t.buf ~padding ~len ~pos:(upos t ~pos) ()
    ;;

    let head_padded_fixed_string ~padding ~len t ~pos =
      Bigstring.get_head_padded_fixed_string t.buf ~padding ~len ~pos:(upos t ~pos) ()
    ;;

    let bytes ~str_pos ~len t ~pos =
      let dst = Bytes.create (len + str_pos) in
      Bigstring.To_bytes.unsafe_blit ~src:t.buf ~src_pos:(upos t ~pos)
        ~len ~dst ~dst_pos:str_pos;
      dst
    ;;

    let string ~str_pos ~len t ~pos =
      Bytes.unsafe_to_string
        ~no_mutation_while_string_reachable:(bytes ~str_pos ~len t ~pos)
    ;;

    let bigstring ~str_pos ~len t ~pos =
      let dst = Bigstring.create (len + str_pos) in
      Bigstring.unsafe_blit ~src:t.buf ~src_pos:(upos t ~pos) ~len ~dst ~dst_pos:str_pos;
      dst
    ;;

    let byteso ?(str_pos = 0) ?len t ~pos =
      bytes t ~pos ~str_pos
        ~len:(match len with None -> length t - pos | Some len -> len)
    ;;

    let stringo ?(str_pos = 0) ?len t ~pos =
      string t ~pos ~str_pos
        ~len:(match len with None -> length t - pos | Some len -> len)
    ;;

    let bigstringo ?(str_pos = 0) ?len t ~pos =
      bigstring t ~pos ~str_pos
        ~len:(match len with None -> length t - pos | Some len -> len)
    ;;

    let bin_prot = Peek.bin_prot

    let index = Peek.index

    open Bigstring

    let [@inline always] char            t ~pos = bigstring_unsafe_get      t.buf ~pos:(upos t ~pos)
    let [@inline always] uint8           t ~pos = unsafe_get_uint8          t.buf ~pos:(upos t ~pos)
    let [@inline always]  int8           t ~pos = unsafe_get_int8           t.buf ~pos:(upos t ~pos)
    let [@inline always]  int16_be       t ~pos = unsafe_get_int16_be       t.buf ~pos:(upos t ~pos)
    let [@inline always]  int16_le       t ~pos = unsafe_get_int16_le       t.buf ~pos:(upos t ~pos)
    let [@inline always] uint16_be       t ~pos = unsafe_get_uint16_be      t.buf ~pos:(upos t ~pos)
    let [@inline always] uint16_le       t ~pos = unsafe_get_uint16_le      t.buf ~pos:(upos t ~pos)
    let [@inline always]  int32_be       t ~pos = unsafe_get_int32_be       t.buf ~pos:(upos t ~pos)
    let [@inline always]  int32_le       t ~pos = unsafe_get_int32_le       t.buf ~pos:(upos t ~pos)
    let [@inline always] uint32_be       t ~pos = unsafe_get_uint32_be      t.buf ~pos:(upos t ~pos)
    let [@inline always] uint32_le       t ~pos = unsafe_get_uint32_le      t.buf ~pos:(upos t ~pos)
    let [@inline always]  int64_be       t ~pos = unsafe_get_int64_be_exn   t.buf ~pos:(upos t ~pos)
    let [@inline always]  int64_le       t ~pos = unsafe_get_int64_le_exn   t.buf ~pos:(upos t ~pos)
    let [@inline always] uint64_be       t ~pos = unsafe_get_uint64_be_exn  t.buf ~pos:(upos t ~pos)
    let [@inline always] uint64_le       t ~pos = unsafe_get_uint64_le_exn  t.buf ~pos:(upos t ~pos)
    let [@inline always]  int64_t_be     t ~pos = unsafe_get_int64_t_be     t.buf ~pos:(upos t ~pos)
    let [@inline always]  int64_t_le     t ~pos = unsafe_get_int64_t_le     t.buf ~pos:(upos t ~pos)
    let [@inline always]  int64_be_trunc t ~pos = unsafe_get_int64_be_trunc t.buf ~pos:(upos t ~pos)
    let [@inline always]  int64_le_trunc t ~pos = unsafe_get_int64_le_trunc t.buf ~pos:(upos t ~pos)
  end

  module Poke = struct
    type ('a, 'd, 'w) t = ('a, 'd, 'w) Poke.t

    let upos = unsafe_buf_pos

    let tail_padded_fixed_string ~padding ~len t ~pos src =
      Bigstring.set_tail_padded_fixed_string ~padding ~len t.buf ~pos:(upos t ~pos) src
    ;;

    let head_padded_fixed_string ~padding ~len t ~pos src =
      Bigstring.set_head_padded_fixed_string ~padding ~len t.buf ~pos:(upos t ~pos) src
    ;;

    let bytes ~str_pos ~len t ~pos src =
      Bigstring.From_bytes.unsafe_blit ~src ~src_pos:str_pos ~len
        ~dst:t.buf ~dst_pos:(upos t ~pos)
    ;;

    let string ~str_pos ~len t ~pos src =
      Bigstring.From_string.unsafe_blit ~src ~src_pos:str_pos ~len
        ~dst:t.buf ~dst_pos:(upos t ~pos)
    ;;

    let bigstring ~str_pos ~len t ~pos src =
      Bigstring.unsafe_blit ~src ~src_pos:str_pos ~len ~dst:t.buf ~dst_pos:(upos t ~pos)
    ;;

    let byteso ?(str_pos = 0) ?len t ~pos src =
      bytes t ~str_pos ~pos src
        ~len:(match len with None -> Bytes.length src - str_pos | Some len -> len)
    ;;

    let stringo ?(str_pos = 0) ?len t ~pos src =
      string t ~str_pos ~pos src
        ~len:(match len with None -> String.length src - str_pos | Some len -> len)
    ;;

    let bigstringo ?(str_pos = 0) ?len t ~pos src =
      bigstring t ~str_pos ~pos src
        ~len:(match len with None -> Bigstring.length src - str_pos | Some len -> len)
    ;;

    let bin_prot = Poke.bin_prot

    open Bigstring

    let [@inline always] char            t ~pos c = bigstring_unsafe_set  t.buf ~pos:(upos t ~pos) c
    let [@inline always] uint8           t ~pos i = unsafe_set_uint8      t.buf ~pos:(upos t ~pos) i
    let [@inline always]  int8           t ~pos i = unsafe_set_int8       t.buf ~pos:(upos t ~pos) i
    let [@inline always]  int16_be       t ~pos i = unsafe_set_int16_be   t.buf ~pos:(upos t ~pos) i
    let [@inline always]  int16_le       t ~pos i = unsafe_set_int16_le   t.buf ~pos:(upos t ~pos) i
    let [@inline always] uint16_be       t ~pos i = unsafe_set_uint16_be  t.buf ~pos:(upos t ~pos) i
    let [@inline always] uint16_le       t ~pos i = unsafe_set_uint16_le  t.buf ~pos:(upos t ~pos) i
    let [@inline always]  int32_be       t ~pos i = unsafe_set_int32_be   t.buf ~pos:(upos t ~pos) i
    let [@inline always]  int32_le       t ~pos i = unsafe_set_int32_le   t.buf ~pos:(upos t ~pos) i
    let [@inline always] uint32_be       t ~pos i = unsafe_set_uint32_be  t.buf ~pos:(upos t ~pos) i
    let [@inline always] uint32_le       t ~pos i = unsafe_set_uint32_le  t.buf ~pos:(upos t ~pos) i
    let [@inline always]  int64_be       t ~pos i = unsafe_set_int64_be   t.buf ~pos:(upos t ~pos) i
    let [@inline always]  int64_le       t ~pos i = unsafe_set_int64_le   t.buf ~pos:(upos t ~pos) i
    let [@inline always] uint64_be       t ~pos i = unsafe_set_uint64_be  t.buf ~pos:(upos t ~pos) i
    let [@inline always] uint64_le       t ~pos i = unsafe_set_uint64_le  t.buf ~pos:(upos t ~pos) i
    let [@inline always]  int64_t_be     t ~pos i = unsafe_set_int64_t_be t.buf ~pos:(upos t ~pos) i
    let [@inline always]  int64_t_le     t ~pos i = unsafe_set_int64_t_le t.buf ~pos:(upos t ~pos) i
    let [@inline always]  int64_be_trunc t ~pos i = unsafe_set_int64_be   t.buf ~pos:(upos t ~pos) i
    let [@inline always]  int64_le_trunc t ~pos i = unsafe_set_int64_le   t.buf ~pos:(upos t ~pos) i

    let decimal = Itoa.unsafe_poke_decimal
  end
end

module For_hexdump = struct

  module T2 = struct
    type nonrec ('rw, 'seek) t = ('rw, 'seek) t
  end

  module Window_indexable = struct
    include T2
    let length t = length t
    let get t pos = Peek.char t ~pos
  end

  module Limits_indexable = struct
    include T2
    let length t = t.hi_max - t.lo_min
    let get t pos = Bigstring.get t.buf (t.lo_min + pos)
  end

  module Buffer_indexable = struct
    include T2
    let length t = Bigstring.length t.buf
    let get t pos = Bigstring.get t.buf pos
  end

  module Window = Hexdump.Of_indexable2 (Window_indexable)
  module Limits = Hexdump.Of_indexable2 (Limits_indexable)
  module Buffer = Hexdump.Of_indexable2 (Buffer_indexable)

  module type Relative_indexable = sig
    val name : string
    val lo : (_, _) t -> int
    val hi : (_, _) t -> int
  end

  module type Compound_indexable = sig
    include Hexdump.S2 with type ('rw, 'seek) t := ('rw, 'seek) t
    val parts : (module Relative_indexable) list
  end

  module Make_compound_hexdump (Compound : Compound_indexable) = struct
    module Hexdump = struct
      include T2

      let relative_sequence ?max_lines t (module Relative : Relative_indexable) =
        let lo = Relative.lo t in
        let hi = Relative.hi t in
        Compound.Hexdump.to_sequence ?max_lines ~pos:lo ~len:(hi - lo) t

      let to_sequence ?max_lines t =
        List.concat_map Compound.parts ~f:(fun (module Relative) ->
          [ Sequence.singleton (String.capitalize Relative.name)
          ; relative_sequence ?max_lines t (module Relative)
            |> Sequence.map ~f:(fun line -> "  " ^ line)
          ])
        |> Sequence.of_list
        |> Sequence.concat

      let to_string_hum ?max_lines t =
        to_sequence ?max_lines t
        |> Sequence.to_list
        |> String.concat ~sep:"\n"

      let sexp_of_t _ _ t =
        List.map Compound.parts ~f:(fun (module Relative) ->
          Relative.name, Sequence.to_list (relative_sequence t (module Relative)))
        |> [%sexp_of: (string * string list) list]
    end
  end

  module Window_within_limits = struct
    let name = "window"
    let lo t = t.lo - t.lo_min
    let hi t = t.hi - t.lo_min
  end

  module Limits_within_limits = struct
    let name = "limits"
    let lo _ = 0
    let hi t = t.hi_max - t.lo_min
  end

  module Window_within_buffer = struct
    let name = "window"
    let lo t = t.lo
    let hi t = t.hi
  end

  module Limits_within_buffer = struct
    let name = "limits"
    let lo t = t.lo_min
    let hi t = t.hi_max
  end

  module Buffer_within_buffer = struct
    let name = "buffer"
    let lo _ = 0
    let hi t = Bigstring.length t.buf
  end

  module Window_and_limits = Make_compound_hexdump (struct
      include Limits
      let parts =
        [ (module Window_within_limits : Relative_indexable)
        ; (module Limits_within_limits : Relative_indexable)
        ]
    end)

  module Window_and_limits_and_buffer = Make_compound_hexdump (struct
      include Buffer
      let parts =
        [ (module Window_within_buffer : Relative_indexable)
        ; (module Limits_within_buffer : Relative_indexable)
        ; (module Buffer_within_buffer : Relative_indexable)
        ]
    end)
end

module Window = For_hexdump.Window
module Limits = For_hexdump.Limits
module Debug  = For_hexdump.Window_and_limits_and_buffer

include For_hexdump.Window_and_limits

let to_string_hum = Hexdump.to_string_hum

(* Minimal blit benchmarks. *)
(* ┌────────────────────────────────────────────────────────┬────────────┬────────────┐
 * │ Name                                                   │   Time/Run │ Percentage │
 * ├────────────────────────────────────────────────────────┼────────────┼────────────┤
 * │ [iobuf.ml:Blit tests] string blit:5                    │    15.30ns │      1.11% │
 * │ [iobuf.ml:Blit tests] string blit:10                   │    15.57ns │      1.13% │
 * │ [iobuf.ml:Blit tests] string blit:100                  │    19.26ns │      1.39% │
 * │ [iobuf.ml:Blit tests] string blit:1000                 │    47.83ns │      3.46% │
 * │ [iobuf.ml:Blit tests] string blit:10000                │   197.90ns │     14.32% │
 * │ [iobuf.ml:Blit tests] Blit:5                           │    24.38ns │      1.76% │
 * │ [iobuf.ml:Blit tests] Blit:10                          │    26.88ns │      1.94% │
 * │ [iobuf.ml:Blit tests] Blit:100                         │    30.01ns │      2.17% │
 * │ [iobuf.ml:Blit tests] Blit:1000                        │    57.83ns │      4.18% │
 * │ [iobuf.ml:Blit tests] Blit:10000                       │   391.42ns │     28.31% │
 * │ [iobuf.ml:Blit tests] Blit_consume:5                   │    23.00ns │      1.66% │
 * │ [iobuf.ml:Blit tests] Blit_consume:10                  │    25.36ns │      1.83% │
 * │ [iobuf.ml:Blit tests] Blit_consume:100                 │    29.79ns │      2.15% │
 * │ [iobuf.ml:Blit tests] Blit_consume:1000                │    58.93ns │      4.26% │
 * │ [iobuf.ml:Blit tests] Blit_consume:10000               │   395.19ns │     28.59% │
 * │ [iobuf.ml:Blit tests] Blit_fill:5                      │    24.28ns │      1.76% │
 * │ [iobuf.ml:Blit tests] Blit_fill:10                     │    26.84ns │      1.94% │
 * │ [iobuf.ml:Blit tests] Blit_fill:100                    │    29.54ns │      2.14% │
 * │ [iobuf.ml:Blit tests] Blit_fill:1000                   │    57.05ns │      4.13% │
 * │ [iobuf.ml:Blit tests] Blit_fill:10000                  │   395.72ns │     28.62% │
 * │ [iobuf.ml:Blit tests] Blit_consume_and_fill:5          │    25.43ns │      1.84% │
 * │ [iobuf.ml:Blit tests] Blit_consume_and_fill:10         │    27.19ns │      1.97% │
 * │ [iobuf.ml:Blit tests] Blit_consume_and_fill:100        │    30.96ns │      2.24% │
 * │ [iobuf.ml:Blit tests] Blit_consume_and_fill:1000       │    58.38ns │      4.22% │
 * │ [iobuf.ml:Blit tests] Blit_consume_and_fill:10000      │   383.62ns │     27.75% │
 * │ [iobuf.ml:Blit tests] Blit.unsafe_blit [overlap]:5     │    14.25ns │      1.03% │
 * │ [iobuf.ml:Blit tests] Blit.unsafe_blit [overlap]:10    │    16.92ns │      1.23% │
 * │ [iobuf.ml:Blit tests] Blit.unsafe_blit [overlap]:100   │    37.17ns │      2.70% │
 * │ [iobuf.ml:Blit tests] Blit.unsafe_blit [overlap]:1000  │   169.60ns │     12.32% │
 * │ [iobuf.ml:Blit tests] Blit.unsafe_blit [overlap]:10000 │ 1_377.01ns │    100.00% │
 * └────────────────────────────────────────────────────────┴────────────┴────────────┘ *)
let%bench_module "Blit tests" = (module struct
  let lengths = [5; 10; 100; 1000; 10_000]

  let%bench_fun "string blit" [@indexed len = lengths] =
    let buf = create ~len in
    let str = Bytes.create len in
    (fun () -> Peek.To_bytes.blit ~src:buf ~dst:str ~src_pos:0 ~dst_pos:0 ~len)

  let%bench_fun "Blit" [@indexed len = lengths] =
    let src = create ~len in
    let dst = create ~len in
    (fun () -> Blit.blito () ~src ~dst)

  let%bench_fun "Blit_consume" [@indexed len = lengths] =
    let src = create ~len in
    let dst = create ~len in
    (fun () -> Blit_consume.blito () ~src ~dst; reset src)

  let%bench_fun "Blit_fill" [@indexed len = lengths] =
    let src = create ~len in
    let dst = create ~len in
    (fun () -> Blit_fill.blito () ~src ~dst; reset dst)

  let%bench_fun "Blit_consume_and_fill" [@indexed len = lengths] =
    let src = create ~len in
    let dst = create ~len in
    (fun () -> Blit_consume_and_fill.blito () ~src ~dst; reset src; reset dst)

  let%bench_fun "Blit.unsafe_blit [overlap]" [@indexed len = lengths] =
    let t = create ~len:(len + 1) in
    (fun () -> Blit.unsafe_blit ~src:t ~dst:t ~len ~src_pos:0 ~dst_pos:1)

end)

let%bench_module "Poke tests" = (module struct
  let offsets = List.init 9 ~f:Fn.id
  let iobuf = create ~len:100

  (* We test at different offsets to see if various byte alignments have a significant
     effect on performance. *)
  let%bench_fun "char"      [@indexed pos = offsets] = (fun () -> Poke.char      iobuf ~pos 'a')
  let%bench_fun "uint8"     [@indexed pos = offsets] = (fun () -> Poke.uint8     iobuf ~pos pos)
  let%bench_fun "int8"      [@indexed pos = offsets] = (fun () -> Poke.int8      iobuf ~pos pos)
  let%bench_fun "int16_be"  [@indexed pos = offsets] = (fun () -> Poke.int16_be  iobuf ~pos pos)
  let%bench_fun "int16_le"  [@indexed pos = offsets] = (fun () -> Poke.int16_le  iobuf ~pos pos)
  let%bench_fun "uint16_be" [@indexed pos = offsets] = (fun () -> Poke.uint16_be iobuf ~pos pos)
  let%bench_fun "uint16_le" [@indexed pos = offsets] = (fun () -> Poke.uint16_le iobuf ~pos pos)
  let%bench_fun "int32_be"  [@indexed pos = offsets] = (fun () -> Poke.int32_be  iobuf ~pos pos)
  let%bench_fun "int32_le"  [@indexed pos = offsets] = (fun () -> Poke.int32_le  iobuf ~pos pos)
  let%bench_fun "uint32_be" [@indexed pos = offsets] = (fun () -> Poke.uint32_be iobuf ~pos pos)
  let%bench_fun "uint32_le" [@indexed pos = offsets] = (fun () -> Poke.uint32_le iobuf ~pos pos)
  let%bench_fun "int64_be"  [@indexed pos = offsets] = (fun () -> Poke.int64_be  iobuf ~pos pos)
  let%bench_fun "int64_le"  [@indexed pos = offsets] = (fun () -> Poke.int64_le  iobuf ~pos pos)
end)

let%bench_module "Peek tests" = (module struct
  let offsets = List.init 9 ~f:Fn.id
  let iobuf = of_string (String.make 100 '\000')

  let%bench_fun "char"      [@indexed pos = offsets] = (fun () -> ignore (Peek.char      iobuf ~pos))
  let%bench_fun "uint8"     [@indexed pos = offsets] = (fun () -> ignore (Peek.uint8     iobuf ~pos))
  let%bench_fun "int8"      [@indexed pos = offsets] = (fun () -> ignore (Peek.int8      iobuf ~pos))
  let%bench_fun "int16_be"  [@indexed pos = offsets] = (fun () -> ignore (Peek.int16_be  iobuf ~pos))
  let%bench_fun "int16_le"  [@indexed pos = offsets] = (fun () -> ignore (Peek.int16_le  iobuf ~pos))
  let%bench_fun "uint16_be" [@indexed pos = offsets] = (fun () -> ignore (Peek.uint16_be iobuf ~pos))
  let%bench_fun "uint16_le" [@indexed pos = offsets] = (fun () -> ignore (Peek.uint16_le iobuf ~pos))
  let%bench_fun "int32_be"  [@indexed pos = offsets] = (fun () -> ignore (Peek.int32_be  iobuf ~pos))
  let%bench_fun "int32_le"  [@indexed pos = offsets] = (fun () -> ignore (Peek.int32_le  iobuf ~pos))
  let%bench_fun "uint32_be" [@indexed pos = offsets] = (fun () -> ignore (Peek.uint32_be iobuf ~pos))
  let%bench_fun "uint32_le" [@indexed pos = offsets] = (fun () -> ignore (Peek.uint32_le iobuf ~pos))
  let%bench_fun "int64_be"  [@indexed pos = offsets] = (fun () -> ignore (Peek.int64_be  iobuf ~pos))
  let%bench_fun "int64_le"  [@indexed pos = offsets] = (fun () -> ignore (Peek.int64_le  iobuf ~pos))
end)

let%bench_module "Fill.decimal tests" = (module struct
  (* Quantify the gain from our version of [Fill.decimal] over [Int.to_string]. *)
  let values =
    [ Int.min_value; Int.min_value + 1; -10_000; 0; 35; 1_000; 1_000_000; Int.max_value ]
  let iobuf = create ~len:32

  let%bench_fun "Fill.decimal" [@indexed x = values] =
    (fun () -> reset iobuf; Fill.decimal iobuf x)
  let%bench_fun "Unsafe.Fill.decimal" [@indexed x = values] =
    (fun () -> reset iobuf; Unsafe.Fill.decimal iobuf x)
  let%bench_fun "Unsafe.Fill.string Int.to_string" [@indexed x = values] =
    (fun () -> reset iobuf; Fill.stringo iobuf (Int.to_string x))
end)

(* In an attempt to verify how much a phys_equal check could optimize
   [set_bounds_and_buffer], in the soon-to-be-common protogen use case,
   here is the result with cross module inlining:


   ┌────────────────────────────────────────────────────────┬──────────┬─────────┬────────────┐
   │ Name                                                   │ Time/Run │ mWd/Run │ Percentage │
   ├────────────────────────────────────────────────────────┼──────────┼─────────┼────────────┤
   │ [iobuf.ml:set_bounds_and_buffer] with-write-barrier    │   7.78ns │         │    100.00% │
   │ [iobuf.ml:set_bounds_and_buffer] without-write-barrier │   3.92ns │         │     50.40% │
   │ [iobuf.ml:set_bounds_and_buffer] with-gc               │   5.34ns │   6.00w │     68.65% │
   └────────────────────────────────────────────────────────┴──────────┴─────────┴────────────┘

   and here is the code for the benchmark:

   {[
     let orig_set_bounds_and_buffer ~src ~dst =
       dst.lo_min <- src.lo_min;
       dst.lo <- src.lo;
       dst.hi <- src.hi;
       dst.hi_max <- src.hi_max;
       dst.buf <- src.buf
     ;;

     let new_set_bounds_and_buffer = set_bounds_and_buffer

     let%bench_module "set_bounds_and_buffer" = (
       module struct
         let src = create ~len:32
         let dst = create ~len:32

         let%bench_fun "with-write-barrier" =
           (fun () -> orig_set_bounds_and_buffer ~src ~dst)
         let%bench_fun "without-write-barrier" =
           (fun () -> new_set_bounds_and_buffer ~src ~dst)
         let%bench_fun "with-gc" =
           (fun () -> sub_shared src)
       end)
   ]} *)
