INCLUDE "core_config.mlh"
open Core_kernel.Std

module Core_time = Time

module KSpan = Core_kernel.Time_ns.Span
module KTime_ns = Core_kernel.Time_ns

let round_nearest = KTime_ns.Platform_specific.internal_round_nearest

(* This signature constraint is semi-temporary and serves to make the implementation more
   type-safe (so the compiler can help us more).  It would go away if we broke the
   implementation into multiple files. *)
module Span : sig
  include module type of
  (struct
    include KSpan
  end)

  include Identifiable with type t := t

  val to_short_string : t -> string
  val randomize       : t -> percent : float -> t
  val to_span         : t -> Time.Span.t
  val to_span_round_nearest : t -> Time.Span.t
  val of_span         : Time.Span.t -> t

  module Option : sig
    type span

    type t = private Int63.t with typerep
    include Identifiable with type t := t

    val none : t
    val some : span -> t
    val is_none : t -> bool
    val is_some : t -> bool
    val value : t -> default : span -> span
    val value_exn : t -> span

    module Stable : sig
      module V1 : sig
        type nonrec t = t with sexp, bin_io
      end
    end
  end with type span := t

  module Stable : sig
    module V1 : sig
      type nonrec t = t with bin_io, compare, sexp
    end
  end
end = struct
  let half_microsecond = Int63.of_int 500


  let to_span t =
    Time.Span.of_us
      Int63.(to_float ((KSpan.to_int63_ns t + half_microsecond) /  of_int 1000))
  ;;

  let to_span_round_nearest t =
    Time.Span.of_us
      Int63.(to_float ((KSpan.to_int63_ns t + half_microsecond) /% of_int 1000))
  ;;

  let min_span_value = to_span KSpan.min_value
  let max_span_value = to_span KSpan.max_value

  let of_span s =
    if Time.Span.( > ) s max_span_value
    || Time.Span.( < ) s min_span_value
    then failwiths "Time_ns.Span does not support this span" s <:sexp_of< Time.Span.t >>;
    KSpan.of_int63_ns
      (Int63.( * ) (round_nearest (Time.Span.to_us s)) (Int63.of_int 1000))
  ;;

  (* check round trip *)
  TEST_UNIT =
    <:test_result< Time.Span.t >> ~expect:min_span_value
      (to_span_round_nearest (of_span min_span_value))
  ;;

  TEST_UNIT =
    <:test_result< Time.Span.t >> ~expect:max_span_value
      (to_span_round_nearest (of_span max_span_value))
  ;;

  module T = struct
    include KSpan

    let sexp_of_t t = Time.Span.Stable.V1.sexp_of_t (to_span t)
    let t_of_sexp s = of_span (Time.Span.Stable.V1.t_of_sexp s)

    let module_name = "Core.Std.Time_ns.Span"
    let to_string t = Time.Span.to_string (to_span t)
    let of_string s = of_span (Time.Span.of_string s)
    let hash t = Int63.hash (KSpan.to_int63_ns t)
    let compare = compare
  end
  include T
  include Comparable.Validate_with_zero (T)
  include Identifiable.Make (T)

  TEST = Time.Span.is_positive (to_span max_value)  (* make sure no overflow *)

  let to_short_string t = Time.Span.to_short_string (to_span t)
  let randomize t ~percent = of_span (Time.Span.randomize (to_span t) ~percent)

  module Option = struct
    type span = t with sexp
    type t = KSpan.t with bin_io, typerep (* ns since epoch or min_value = none *)
    let none = KSpan.min_value
    let some t = assert (t <> none); t
    let is_none t = t = none
    let is_some t = t <> none
    let value t ~default = if is_none t then default else t
    let value_exn t = assert (is_some t); t
    let of_option = function None -> none | Some t -> some t
    let to_option t = if is_none t then None else Some t
    let sexp_of_t t = <:sexp_of< span option >> (to_option t)
    let t_of_sexp s = of_option (<:of_sexp< span option >> s)

    include Identifiable.Make (struct
      type nonrec t = t with sexp, compare, bin_io
      let hash = hash
      let module_name = "Core.Std.Time_ns.Span.Option"
      include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
    end)

    module Stable = struct
      module V1 = struct
        type nonrec t = t with sexp, bin_io
      end

      TEST_MODULE "Time_ns.Span.Stable.V1" = Core_kernel.Stable_unit_test.Make (struct
        include V1

        let equal = KSpan.equal

        let tests =
          let mk_some i = some (of_int63_ns (Int63.of_int64_exn i)) in
          [ none, "()", "\252\000\000\000\000\000\000\000\192"
          ; mk_some 1_234_560_000_000L, "(20.576m)", "\252\000\160\130q\031\001\000\000"
          ; mk_some 1_000L,  "(0.001ms)", "\254\232\003"
          ; mk_some 80_000_006_400_000_000L,
            "(925.926d)",
            "\252\000@\128\251\1487\028\001"
          ]
      end)
    end
  end

  module Stable = struct
    module V1 = struct
      type nonrec t = t with bin_io, compare, sexp
    end

    TEST_MODULE "Time_ns.Span.Stable.V1" = Core_kernel.Stable_unit_test.Make (struct
      include V1

      let equal = KSpan.equal

      let tests =
        let t i = of_int63_ns (Int63.of_int64_exn i) in
        [ t      1_234_560_000_000L,  "20.576m", "\252\000\160\130q\031\001\000\000"
        ; t                  1_000L,  "0.001ms", "\254\232\003"
        ; t 80_000_006_400_000_000L, "925.926d", "\252\000@\128\251\1487\028\001"
        ]
    end)
  end
end

include (KTime_ns
         : module type of struct
           include KTime_ns
         end with module Span := Span)

let to_time t =
  Time.add Time.epoch (Span.to_span               (to_span_since_epoch t))
;;

let to_time_round_nearest t =
  Time.add Time.epoch (Span.to_span_round_nearest (to_span_since_epoch t))
;;

let min_time_value = to_time min_value
let max_time_value = to_time max_value

let of_time t =
  if Time.( < ) t min_time_value
  || Time.( > ) t max_time_value
  then failwiths "Time_ns does not support this time" t <:sexp_of< Time.t >>;
  of_span_since_epoch (Span.of_span (Time.diff t Time.epoch))
;;

(* check round trip *)
TEST_UNIT =
  <:test_result< Time.t >> ~expect:max_time_value
    (to_time_round_nearest (of_time max_time_value))
;;

TEST_UNIT =
  <:test_result< Time.t >> ~expect:min_time_value
    (to_time_round_nearest (of_time min_time_value))
;;

let sexp_of_t (t : t) : Sexp.t = Time.sexp_of_t (to_time t)
let sexp_of_t_abs ~zone t =
  Sexp.List
    (List.map (String.split ~on:' ' (Time.to_string_abs ~zone (to_time t)))
       ~f:(fun s -> Sexp.Atom s)
    )
let t_of_sexp s : t = of_time (Time.t_of_sexp s)

let to_string t = Time.to_string (to_time t)
let of_string s = of_time (Time.of_string s)

module Stable0 = struct
  module V1 = struct
    type nonrec t = t with bin_io, compare, sexp
  end

  TEST_MODULE "Time_ns.Stable.V1" = Core_kernel.Stable_unit_test.Make (struct
    include V1
    let sexp_of_t = sexp_of_t_abs ~zone:Zone.utc

    let equal = equal

    let tests =
      let t i = of_span_since_epoch (Span.of_int63_ns (Int63.of_int64_exn i)) in
      [ t 1_234_560_000_000L,
        "(1970-01-01 00:20:34.560000Z)",
        "\252\000\160\130q\031\001\000\000"
      ; t 1_000L, "(1970-01-01 00:00:00.000001Z)", "\254\232\003"
      ; t 80_000_006_400_000_000L,
        "(1972-07-14 22:13:26.400000Z)",
        "\252\000@\128\251\1487\028\001"
      ]
  end)
end

module Option = struct
  type time = t with sexp, compare

  type t = Span.Option.t with bin_io, compare, typerep

  let none = Span.Option.none
  let some time = Span.Option.some (to_span_since_epoch time)
  let is_none = Span.Option.is_none
  let is_some = Span.Option.is_some
  let value t ~default =
    of_span_since_epoch
      (Span.Option.value
         ~default:(to_span_since_epoch default) t)
  let value_exn t = of_span_since_epoch (Span.Option.value_exn t)
  let roundtrip t = (value_exn (some t))
  TEST_UNIT = <:test_result< time >> (roundtrip epoch) ~expect:epoch
  TEST_UNIT = let t = now () in <:test_result< time >> (roundtrip t) ~expect:t
  TEST = is_error (Result.try_with (fun () -> value_exn none))

  let of_option = function None -> none | Some t -> some t
  let to_option t = if is_none t then None else Some (value_exn t)

  let sexp_of_t t = <:sexp_of< time option >> (to_option t)
  let sexp_of_t_abs ~zone t =
    <:sexp_of< Sexp.t option >>
      (Option.map (to_option t)
         ~f:(sexp_of_t_abs ~zone)
      )
  let t_of_sexp s = of_option (<:of_sexp< time option >> s)

  include Identifiable.Make (struct
    type nonrec t = t with sexp, compare, bin_io
    let module_name = "Core.Std.Time_ns.Option"
    let hash = Span.Option.hash
    include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
  end)

  module Stable = struct
    module V1 = struct
      type nonrec t = t with sexp, bin_io
    end

    TEST_MODULE "Time_ns.Option.Stable.V1" = Core_kernel.Stable_unit_test.Make (struct
      include V1
      let sexp_of_t = sexp_of_t_abs ~zone:Zone.utc

      let equal = Span.Option.equal

      let tests =
        let t i = of_span_since_epoch (Span.of_int63_ns (Int63.of_int64_exn i)) in
        [ none, "()", "\252\000\000\000\000\000\000\000\192"
        ; some (t 1_234_560_000_000L),
          "((1970-01-01 00:20:34.560000Z))",
          "\252\000\160\130q\031\001\000\000"
        ; some (t 1_000L), "((1970-01-01 00:00:00.000001Z))", "\254\232\003"
        ; some (t 80_000_006_400_000_000L),
          "((1972-07-14 22:13:26.400000Z))",
          "\252\000@\128\251\1487\028\001"
        ]
    end)
  end
end

let to_string_fix_proto zone t = Time.to_string_fix_proto zone (to_time t)
let of_string_fix_proto zone s = of_time (Time.of_string_fix_proto zone s)

include Identifiable.Make (struct
  type nonrec t = t with sexp, bin_io, compare
  let module_name = "Core.Std.Time_ns"
  let hash t = Int63.hash (to_int63_ns_since_epoch t)
  let of_string, to_string = of_string, to_string
end)

TEST_MODULE = struct
  TEST = epoch = of_span_since_epoch Span.zero

  TEST_UNIT "round trip from [Time.t] to [t] and back" =
    let times = List.map ~f:Time.of_float [ 0.0; 1.0; 1.123456789 ] in
    List.iter times ~f:(fun time ->
      let res = to_time (of_time time) in
      <:test_result< Time.t >> ~equal:Time.(=.) ~expect:time res
    )

  TEST_UNIT "round trip from [t] to [Time.t] and back" =
    List.iter Span.([ zero; second; scale day 365. ]) ~f:(fun since_epoch ->
      let t = of_span_since_epoch since_epoch in
      let res = of_time (to_time t) in
      (* Allow up to 100ns discrepancy in a year due to float precision issues. *)
      let discrepancy = diff res t in
      if Span.(abs discrepancy > of_ns 100.) then
        failwiths "Failed on span since epoch"
          (`since_epoch since_epoch, t, `res res, `discrepancy discrepancy)
          <:sexp_of< [ `since_epoch of Span.t ]
                     * t * [ `res of t ]
                     * [ `discrepancy of Span.t ] >>)
end

(** Presently this is not zoned. *)
module Ofday = struct
  type t = Span.t (* since midnight *)
  with typerep, compare, bin_io


  let start_of_day : t = Span.zero
  let end_of_day : t = Span.day
  let end_of_day_with_dst_and_leap_second_allowance : t =
    Span.(end_of_day + hour + minute)

  let to_span_since_start_of_day t = t
  let of_span_since_start_of_day_exn (s : Span.t) =
    if Span.(<) s start_of_day || Span.(>) s end_of_day_with_dst_and_leap_second_allowance
    then failwith "Time_ns.Ofday.of_span_since_start_of_day_exn: input out of bounds"
    else s

  let local_midnight time =
    let zone = Time.Zone.local in
    let date = Time.to_date (to_time time) ~zone in
    let midnight = Time.of_date_ofday date Time.Ofday.start_of_day ~zone in
    of_time midnight
  ;;

  let local_midnight_cache = ref (local_midnight (now ()))

  let of_local_time time =
    let t = diff time !local_midnight_cache in
    if Span.(>=) t start_of_day && Span.(<) t end_of_day then t
    else begin
      local_midnight_cache := local_midnight time;
      diff time !local_midnight_cache
    end
  ;;

  let local_now () = of_local_time (now ())

  let to_string t =
    if Span.(<=) start_of_day t && Span.(<) t end_of_day then
      let ns = Span.to_int63_ns t in
      let s = Span.to_int_sec t in
      let m = s / 60 in
      let h = m / 60 in
      sprintf "%02d:%02d:%02d.%09d"
        h
        (m mod 60)
        (s mod 60)
        Int63.(to_int_exn (rem ns Span.(to_int63_ns second)))
    else "Incorrect day"
  ;;

  let of_string s = failwiths "unimplemented" s <:sexp_of< string >>

  let to_millisecond_string t =
    if Span.(<=) start_of_day t && Span.(<) t end_of_day then
      let ms = Int63.(Span.to_int63_ns t / of_int 1_000_000) in
      let s = Int63.(ms / of_int 1000) in
      let m = Int63.(s / of_int 60) in
      let h = Int63.(m / of_int 60) in
      sprintf "%02d:%02d:%02d.%03d"
        Int63.(to_int_exn h)
        Int63.(to_int_exn (rem m (of_int 60)))
        Int63.(to_int_exn (rem s (of_int 60)))
        Int63.(to_int_exn (rem ms (of_int 1000)))
    else "Incorrect day"
  ;;

  let of_ofday core = Span.of_span (Time.Ofday.to_span_since_start_of_day core)
  let to_ofday t = Time.Ofday.of_span_since_start_of_day (Span.to_span t)

  let t_of_sexp s : t = of_ofday (Time.Ofday.t_of_sexp s)
  let sexp_of_t (t : t) = Time.Ofday.sexp_of_t (to_ofday t)

  include Identifiable.Make (struct
    type nonrec t = t with sexp, compare, bin_io
    let module_name = "Core.Std.Time_ns.Ofday"
    let hash = Span.hash
    let of_string, to_string = of_string, to_string
  end)

  module Stable = struct
    module V1 = struct
      type nonrec t = t with sexp, bin_io
    end

    TEST_MODULE "Time_ns.Ofday.Stable.V1" = Core_kernel.Stable_unit_test.Make (struct
      include V1

      let equal = Span.equal

      let tests =
        let t i = Span.of_int63_ns (Int63.of_int64_exn i) in
        [ t                 0L, "00:00:00.000000", "\000"
        ; t             1_000L, "00:00:00.000001", "\254\232\003"
        ; t 1_234_560_000_000L, "00:20:34.560000", "\252\000\160\130q\031\001\000\000"
        ]
    end)
  end
end

TEST_MODULE = struct

  TEST_UNIT =
    let span = Span.create ~hr:8 ~min:27 ~sec:14 ~ms:359 () in
    let ofday = Ofday.of_span_since_start_of_day_exn span in
    let expected = "08:27:14.359" in
    let ms_str = Ofday.to_millisecond_string ofday    in
    if String.(<>) ms_str expected then
      failwithf "Failed on Ofday.to_millisecond_string Got (%s) expected (%s)"
        ms_str expected ()

  TEST_UNIT =
    (* Ensure that local_midnight_cache doesn't interfere with converting times that are
       much earlier or later than each other. *)
    let check ofday =
      let to_string t = Span.to_int63_ns t |> Int63.to_string in
      if Ofday.(<) ofday Ofday.start_of_day
      then failwithf "too small: %s" (to_string ofday) ()
      else if Ofday.(>=) ofday Ofday.end_of_day
      then failwithf "too large: %s" (to_string ofday) ()
      else ()
    in
    check (Ofday.of_local_time epoch);
    check (Ofday.of_local_time (now ()));
    check (Ofday.of_local_time epoch)

end

let of_date_ofday ~zone date ofday =
  of_time (Core_time.of_date_ofday ~zone date (Ofday.to_ofday ofday))
;;

let to_date t ~zone = Core_time.to_date (to_time t) ~zone

let occurrence what t ~ofday ~zone =
  of_time (Core_time.occurrence what (to_time t) ~ofday:(Ofday.to_ofday ofday) ~zone)
;;

module Stable = struct
  include Stable0
  module Span   = Span   .Stable
  module Option = Option .Stable
  module Ofday  = Ofday  .Stable
end
