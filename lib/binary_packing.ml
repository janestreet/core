open Std_internal

module Char = Caml.Char
module Int32 = Caml.Int32
module Int64 = Caml.Int64

type endian = [ `Big_endian | `Little_endian ]

(* Computes the offset based on the total number of bytes, the byte order, and the
   byte number. The byte number is ordered by decreasing significance starting at zero
   (big endian). So the most significant byte is 0, and the least significant byte is (len
   - 1). *)

exception Binary_packing_invalid_byte_number of int * int with sexp
let offset ~len ~byte_order byte_nr =
  if byte_nr >= len || byte_nr < 0 then
    raise (Binary_packing_invalid_byte_number (byte_nr, len));
  match byte_order with
  | `Little_endian -> len - 1 - byte_nr
  | `Big_endian -> byte_nr
;;

(* byte order added to the _8 functions to make testing easier (uniformity) *)
exception Pack_unsigned_8_argument_out_of_range of int with sexp
let pack_unsigned_8 ~buf ~pos n =
  if n > 0xFF || n < 0 then
    raise (Pack_unsigned_8_argument_out_of_range n)
  else buf.[pos] <- Char.unsafe_chr n;
;;

let unpack_unsigned_8 ~buf ~pos = Char.code buf.[pos]

exception Pack_signed_8_argument_out_of_range of int with sexp
let pack_signed_8 ~buf ~pos n =
  if n > 0x7F || n < -0x80 then
    raise (Pack_signed_8_argument_out_of_range n)
  else buf.[pos] <- Char.unsafe_chr n
;;

let unpack_signed_8 ~buf ~pos =
  let n = unpack_unsigned_8 ~buf ~pos in
  if n >= 0x80 then
    -(0x100 - n)
  else
    n
;;

exception Pack_signed_16_argument_out_of_range of int with sexp
let pack_signed_16 ~byte_order ~buf ~pos n =
  if n > 0x7FFF || n < -0x8000 then
    raise (Pack_signed_16_argument_out_of_range n)
  else begin
    buf.[pos + offset ~len:2 ~byte_order 0] <- Char.unsafe_chr (0xFF land (n asr 8));
    buf.[pos + offset ~len:2 ~byte_order 1] <- Char.unsafe_chr (0xFF land n)
  end
;;

let unpack_unsigned_16 ~byte_order ~buf ~pos =
  let b1 = Char.code buf.[pos + offset ~len:2 ~byte_order 0] lsl 8 in
  let b2 = Char.code buf.[pos + offset ~len:2 ~byte_order 1] in
  b1 lor b2
;;

let unpack_signed_16 ~byte_order ~buf ~pos =
  let n = unpack_unsigned_16 ~byte_order ~buf ~pos in
  if n >= 0x8000 then -(0x10000 - n)
  else n
;;

exception Pack_signed_32_argument_out_of_range of int with sexp
let pack_signed_32_int ~byte_order ~buf ~pos n =
  assert (Sys.word_size = 64);
  if n > 0x7FFFFFFF || n < -(0x7FFFFFFF + 1) then
    raise (Pack_signed_32_argument_out_of_range n)
  else begin
    buf.[pos + offset ~len:4 ~byte_order 0] <- Char.unsafe_chr (0xFF land (n asr 24)); (* MSB *)
    buf.[pos + offset ~len:4 ~byte_order 1] <- Char.unsafe_chr (0xFF land (n asr 16));
    buf.[pos + offset ~len:4 ~byte_order 2] <- Char.unsafe_chr (0xFF land (n asr 8));
    buf.[pos + offset ~len:4 ~byte_order 3] <- Char.unsafe_chr (0xFF land n) (* LSB *)
  end

let pack_signed_32 ~byte_order ~buf ~pos n =
  buf.[pos + offset ~len:4 ~byte_order 0] <-
    Char.unsafe_chr (0xFF land Int32.to_int (Int32.shift_right n 24));
  buf.[pos + offset ~len:4 ~byte_order 1] <-
    Char.unsafe_chr (0xFF land Int32.to_int (Int32.shift_right n 16));
  buf.[pos + offset ~len:4 ~byte_order 2] <-
    Char.unsafe_chr (0xFF land Int32.to_int (Int32.shift_right n 8));
  buf.[pos + offset ~len:4 ~byte_order 3] <- Char.unsafe_chr (0xFF land Int32.to_int n)
;;

let unpack_signed_32 ~byte_order ~buf ~pos =
  let b1 = (* MSB *)
    Int32.shift_left (Int32.of_int (Char.code buf.[pos + offset ~len:4 ~byte_order 0])) 24
  in
  let b2 = Char.code buf.[pos + offset ~len:4 ~byte_order 1] lsl 16 in
  let b3 = Char.code buf.[pos + offset ~len:4 ~byte_order 2] lsl 8 in
  let b4 = Char.code buf.[pos + offset ~len:4 ~byte_order 3] in (* LSB *)
  Int32.logor b1 (Int32.of_int (b2 lor b3 lor b4))
;;

let unpack_signed_32_int ~byte_order ~buf ~pos =
  assert (Sys.word_size = 64);
  let b1 = Char.code buf.[pos + offset ~len:4 ~byte_order 0] lsl 24 in (* msb *)
  let b2 = Char.code buf.[pos + offset ~len:4 ~byte_order 1] lsl 16 in
  let b3 = Char.code buf.[pos + offset ~len:4 ~byte_order 2] lsl 8 in
  let b4 = Char.code buf.[pos + offset ~len:4 ~byte_order 3] in (* lsb *)
  let n = b1 lor b2 lor b3 lor b4 in
  if n > 0x7FFFFFFF then -(((0x7FFFFFFF + 1) lsl 1) - n)
  else n
;;

let pack_signed_64 ~byte_order ~buf ~pos v =
  let top3 = Int64.to_int (Int64.shift_right v 40) in
  let mid3 = Int64.to_int (Int64.shift_right v 16) in
  let bot2 = Int64.to_int v in
  buf.[pos + offset ~len:8 ~byte_order 0] <- Char.unsafe_chr (0xFF land (top3 lsr 16));
  buf.[pos + offset ~len:8 ~byte_order 1] <- Char.unsafe_chr (0xFF land (top3 lsr 8));
  buf.[pos + offset ~len:8 ~byte_order 2] <- Char.unsafe_chr (0xFF land top3);
  buf.[pos + offset ~len:8 ~byte_order 3] <- Char.unsafe_chr (0xFF land (mid3 lsr 16));
  buf.[pos + offset ~len:8 ~byte_order 4] <- Char.unsafe_chr (0xFF land (mid3 lsr 8));
  buf.[pos + offset ~len:8 ~byte_order 5] <- Char.unsafe_chr (0xFF land mid3);
  buf.[pos + offset ~len:8 ~byte_order 6] <- Char.unsafe_chr (0xFF land (bot2 lsr 8));
  buf.[pos + offset ~len:8 ~byte_order 7] <- Char.unsafe_chr (0xFF land bot2)
;;

let unpack_signed_64 ~byte_order ~buf ~pos =
  Int64.logor
    (Int64.logor
       (Int64.shift_left
          (Int64.of_int (Char.code buf.[pos + offset ~len:8 ~byte_order 0] lsl 16
                         lor Char.code buf.[pos + offset ~len:8 ~byte_order 1] lsl 8
                         lor Char.code buf.[pos + offset ~len:8 ~byte_order 2]))
          40)
       (Int64.shift_left
          (Int64.of_int (Char.code buf.[pos + offset ~len:8 ~byte_order 3] lsl 16
                         lor Char.code buf.[pos + offset ~len:8 ~byte_order 4] lsl 8
                         lor Char.code buf.[pos + offset ~len:8 ~byte_order 5]))
          16))
    (Int64.of_int (Char.code buf.[pos + offset ~len:8 ~byte_order 6] lsl 8
                   lor Char.code buf.[pos + offset ~len:8 ~byte_order 7]))
;;

let pack_signed_64_int ~byte_order ~buf ~pos n =
  assert (Sys.word_size = 64);
  buf.[pos + offset ~len:8 ~byte_order 0] <- Char.unsafe_chr (0xFF land (n asr 56));
  buf.[pos + offset ~len:8 ~byte_order 1] <- Char.unsafe_chr (0xFF land (n asr 48));
  buf.[pos + offset ~len:8 ~byte_order 2] <- Char.unsafe_chr (0xFF land (n asr 40));
  buf.[pos + offset ~len:8 ~byte_order 3] <- Char.unsafe_chr (0xFF land (n asr 32));
  buf.[pos + offset ~len:8 ~byte_order 4] <- Char.unsafe_chr (0xFF land (n asr 24));
  buf.[pos + offset ~len:8 ~byte_order 5] <- Char.unsafe_chr (0xFF land (n asr 16));
  buf.[pos + offset ~len:8 ~byte_order 6] <- Char.unsafe_chr (0xFF land (n asr 8));
  buf.[pos + offset ~len:8 ~byte_order 7] <- Char.unsafe_chr (0xFF land n)
;;

let unpack_signed_64_int ~byte_order ~buf ~pos =
  assert (Sys.word_size = 64);
  (Char.code buf.[pos + offset ~len:8 ~byte_order 0] lsl 56)
  lor (Char.code buf.[pos + offset ~len:8 ~byte_order 1] lsl 48)
  lor (Char.code buf.[pos + offset ~len:8 ~byte_order 2] lsl 40)
  lor (Char.code buf.[pos + offset ~len:8 ~byte_order 3] lsl 32)
  lor (Char.code buf.[pos + offset ~len:8 ~byte_order 4] lsl 24)
  lor (Char.code buf.[pos + offset ~len:8 ~byte_order 5] lsl 16)
  lor (Char.code buf.[pos + offset ~len:8 ~byte_order 6] lsl 8)
  lor (Char.code buf.[pos + offset ~len:8 ~byte_order 7])
;;

let pack_float ~byte_order ~buf ~pos f =
  pack_signed_64 ~byte_order ~buf ~pos (Int64.bits_of_float f)

let unpack_float ~byte_order ~buf ~pos =
  Int64.float_of_bits (unpack_signed_64 ~byte_order ~buf ~pos)

let test byte_order =
  let buf = String.make 8 'a' in
  let test name to_string p u ns =
    List.iter ns ~f:(fun n ->
      p ~byte_order ~buf ~pos:0 n;
      let n' = u ~byte_order ~buf ~pos:0 in
      if n <> n' then
        failwith (sprintf "%s = unpack_%s (pack_%s %s)"
                    (to_string n') name name (to_string n)))
  in
  test "signed_8" string_of_int
    (fun ~byte_order:_ ~buf ~pos i -> pack_signed_8 ~buf ~pos i)
    (fun ~byte_order:_ ~buf ~pos -> unpack_signed_8 ~buf ~pos)
    [-0x80; -0x7F; -0xF; -1; 0; 1; 0xF; 0x7F];
  test "signed_16" string_of_int
    pack_signed_16 unpack_signed_16
    [-0x8000; -0x7ABC; -0xFF; -1; 0; 1; 0xFF; 0x7ABC; 0x7FFF];
  test "signed_32" Int32.to_string
    pack_signed_32 unpack_signed_32
    [-0x80000000l; -0x76543210l; -0xFFl; Int32.minus_one; Int32.zero; Int32.one; 0x76543210l; 0x7FFFFFFFl];
  test "signed_64" Int64.to_string
    pack_signed_64 unpack_signed_64
    [-0x8000_0000_0000_0000L;
     -0x789A_BCDE_F012_3456L;
     -0xFFL;
     Int64.minus_one;
     Int64.zero;
     Int64.one;
     0x789A_BCDE_F012_3456L;
     0x7FFF_FFFF_FFFF_FFFFL]
;;

let test () = test `Big_endian; test `Little_endian
