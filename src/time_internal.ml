open! Import
include Core_kernel.Time_internal

module Time_ns = Core_kernel.Std.Time_ns

(* Create an abstract type for Time to prevent us from confusing it with
   other floats.
*)
module T : sig
  type t = private float [@@deriving bin_io, typerep, hash]

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
  include Float.Robust_compare.Make
            (struct let robust_comparison_tolerance = 1E-6 end)

  let diff t1 t2 = Span.of_sec (t1 - t2)

  let abs_diff t1 t2 = Span.abs (diff t1 t2)
  let add t span = t +. (Span.to_sec span)
  let sub t span = t -. (Span.to_sec span)

  let now () =
    let float_ns =
      Time_ns.now ()
      |> Time_ns.to_int63_ns_since_epoch
      |> Int63.to_float
    in
    float_ns *. 1E-9

  let%bench_module "now"=
    (module struct
      let%bench_fun "gettimeofday" =
        Unix.gettimeofday

      let%bench_fun "now" =
        now
    end)
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
let utc_mktime ~year ~month ~day ~ofday =
  (* move February to the conceptual end of the ordering - 1..12 -> 11,12,1..10 -
     because it carries the leap day.  The months are 0 indexed for this calculation,
     so 1 is February. *)
  let shuffle_year_month year month =
    let month = month - 2 in
    if month <= 0 then (year - 1, month + 12) else (year,month)
  in
  let year,month = shuffle_year_month year month in
  let days       = year / 4 - year / 100 + year / 400 + 367 * month / 12 + day in
  let days       = Float.of_int days +. 365. *. Float.of_int year -. 719499. in
  (days *. 86400. +. ofday)
;;
