open Std_internal

module Helpers = struct
  (* These specialzed string converters are about 20 times faster than using sprintf.
     sprintf is just horribly horribly slow. On a very fast machine it takes 1us to run
     sprintf "%02d:%02d" i j, which is just ridiculous. *)
  let blit_string_of_int_4_digits =
    let tbl = Array.init 10000 ~f:(fun i -> Printf.sprintf "%04d" i) in
    fun s ~pos i ->
      if i >= 10000 || i < 0 then
        invalid_argf
          "Time.string_of_int_4_digits: argument must be (0, 9999) %d" i ();
      String.blit ~src:tbl.(i) ~dst:s ~src_pos:0 ~dst_pos:pos ~len:4

  let blit_string_of_int_2_digits =
    let tbl = Array.init 100 ~f:(fun i -> Printf.sprintf "%02d" i) in
    fun s ~pos i ->
      if i >= 100 || i < 0 then
        invalid_argf
          "Time.string_of_int_2_digits: argument must be (0, 99) %d" i ();
      String.blit ~src:tbl.(i) ~dst:s ~src_pos:0 ~dst_pos:pos ~len:2

  let blit_string_of_int_3_digits =
    let tbl = Array.init 1000 ~f:(fun i -> Printf.sprintf "%03d" i) in
    fun s ~pos i ->
      if i >= 1000 || i < 0 then
        invalid_argf
          "Time.string_of_int_3_digits: argument must be (0, 999) %d" i ();
      String.blit ~src:tbl.(i) ~dst:s ~src_pos:0 ~dst_pos:pos ~len:3

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
  let diff t1 t2 = Span.of_sec (t1 - t2)

  let abs_diff t1 t2 = Span.abs (diff t1 t2)
  let add t span = t +. (Span.to_sec span)
  let sub t span = t -. (Span.to_sec span)
  let now () = Unix.gettimeofday ()
end

(* due to precision limitations in float we can't expect better than microsecond
    precision *)
include Float_robust_compare.Make(struct let epsilon = 1E-6 end)

let float_of_hh_mm_ss hh mm ss =
  if hh < 0 then
    (Float.of_int (((hh * 60) - mm) * 60)) -. ss
  else
    (Float.of_int (((hh * 60) + mm) * 60)) +. ss

let to_tm t = Unix.localtime (T.to_float t)
let to_tm_utc t = Unix.gmtime (T.to_float t)
