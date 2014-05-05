INCLUDE "core_config.mlh"

open Core_kernel.Std

module Helpers = struct

(*IFDEF ARCH_SIXTYFOUR THEN
  (* http://www.hackersdelight.org/magic.htm *)
  let div_by_10 i = (i * 1717986919) lsr 34
ELSE*)
  let div_by_10 i = i / 10
(*ENDIF*)

  let char_of_digit n = Char.unsafe_of_int (Char.to_int '0' + n)

  let invalid_range ~digits ~max ~i =
    invalid_argf
      "Time.string_of_int_%d_digits: argument must be (0, %d) %d"
      digits max i ()
  ;;

  let blit_string_of_int_4_digits s ~pos i =
    if i >= 10000 || i < 0 then invalid_range ~digits:4 ~max:9999 ~i;
    let j = div_by_10 i in
    s.[pos + 3] <- char_of_digit (i - j * 10);
    let k = div_by_10 j in
    s.[pos + 2] <- char_of_digit (j - k * 10);
    let l = div_by_10 k in
    s.[pos + 1] <- char_of_digit (k - l * 10);
    s.[pos    ] <- char_of_digit l;
  ;;

  TEST_UNIT =
    for i = 0 to 9999 do
      let s = String.make 4 ' ' in
      blit_string_of_int_4_digits s ~pos:0 i;
      <:test_result< string >> ~expect:(Printf.sprintf "%04d" i) s
    done
  ;;

  let blit_string_of_int_2_digits s ~pos i =
    if i >= 100 || i < 0 then invalid_range ~digits:4 ~max:99 ~i;
    let j = div_by_10 i in
    s.[pos + 1] <- char_of_digit (i - j * 10);
    s.[pos    ] <- char_of_digit j;
  ;;

  TEST_UNIT =
    for i = 0 to 99 do
      let s = String.make 2 ' ' in
      blit_string_of_int_2_digits s ~pos:0 i;
      <:test_result< string >> ~expect:(Printf.sprintf "%02d" i) s
    done
  ;;

  let blit_string_of_int_3_digits s ~pos i =
    if i >= 1000 || i < 0 then invalid_range ~digits:4 ~max:999 ~i;
    let j = div_by_10 i in
    s.[pos + 2] <- char_of_digit (i - j * 10);
    let k = div_by_10 j in
    s.[pos + 1] <- char_of_digit (j - k * 10);
    s.[pos    ] <- char_of_digit k;
  ;;

  TEST_UNIT =
    for i = 0 to 999 do
      let s = String.make 3 ' ' in
      blit_string_of_int_3_digits s ~pos:0 i;
      <:test_result< string >> ~expect:(Printf.sprintf "%03d" i) s
    done
  ;;

  let parse_two_digits str pos =
    let d1 = Char.get_digit_exn str.[pos] in
    let d2 = Char.get_digit_exn str.[pos + 1] in
    10 * d1 + d2

  let parse_four_digits str pos =
    parse_two_digits str pos * 100 + parse_two_digits str (pos + 2)
end

(* Create an abstract type for Time to prevent us from confusing it with
   other floats.
*)
module T : sig
  type t = private float with bin_io

  include Comparable.S_common with type t := t
  include Hashable_binable    with type t := t
  include Robustly_comparable with type t := t
  include Stringable          with type t := t
  include Floatable           with type t := t

  val add : t -> Span.t -> t
  val sub : t -> Span.t -> t
  val diff : t -> t -> Span.t
  val abs_diff : t -> t -> Span.t
  val now : unit -> t
end = struct
  (* IF THIS REPRESENTATION EVER CHANGES, ENSURE THAT EITHER
      (1) all values serialize the same way in both representations, or
      (2) you add a new Time version to stable.ml *)
  include Float

  (* due to precision limitations in float we can't expect better than microsecond
     precision *)
  include Core_kernel.Float_robust_compare.Make(struct let epsilon = 1E-6 end)

  let diff t1 t2 = Span.of_sec (t1 - t2)

  let abs_diff t1 t2 = Span.abs (diff t1 t2)
  let add t span = t +. (Span.to_sec span)
  let sub t span = t -. (Span.to_sec span)
  let now () = Unix.gettimeofday ()
end

let float_of_hh_mm_ss hh mm ss =
  if hh < 0 then
    (Float.of_int (((hh * 60) - mm) * 60)) -. ss
  else
    (Float.of_int (((hh * 60) + mm) * 60)) +. ss

let to_tm t = Unix.localtime (T.to_float t)
let to_tm_utc t = Unix.gmtime (T.to_float t)

(* this is a recreation of the algorithm used internally by the linux kernel
   (supposedly invented by Gauss).  In this case it is used to produce the number
   of seconds since 1970-01-01 00:00:00 using epoch time semantics (86,400 seconds
   per day) *)
let utc_mktime ~year ~month ~day ~hour ~min ~sec ~ms ~us =
  (* move February to the conceptual end of the ordering - 1..12 -> 11,12,1..10 -
     because it carries the leap day.  The months are 0 indexed for this calculation,
     so 1 is February. *)
  let shuffle_year_month year month =
    let month = month - 2 in
    if month <= 0 then (year - 1, month + 12) else (year,month)
  in
  let hour       = Float.of_int hour in
  let min        = Float.of_int min in
  let sec        = Float.of_int sec in
  let year,month = shuffle_year_month year month in
  let days       = year / 4 - year / 100 + year / 400 + 367 * month / 12 + day in
  let days       = Float.of_int days +. 365. *. Float.of_int year -. 719499. in
  let hours      = 24. *. days +. hour in
  let mins       = 60. *. hours +. min in
  60. *. mins +. sec +. (Float.of_int ms /. 1000.)
  +. (Float.of_int us /. 1000. /. 1000.)
;;
