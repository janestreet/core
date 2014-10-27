INCLUDE "core_config.mlh"
open Core_kernel.Std

module Core_time = Time

let round_nearest_portable_alloc f = Int63.of_float (Float.round_nearest f)
let round_nearest_arch64_noalloc f = Int63.of_int (Float.iround_nearest_exn f)
BENCH_MODULE "round_nearest portability/performance" = struct
  let f = if Random.bool () then 1.0 else 2.0
  BENCH "round_nearest_portable_alloc" = round_nearest_portable_alloc f
  BENCH "round_nearest_arch64_noalloc" = round_nearest_arch64_noalloc f
  (* Here is a comparison of both of these rounding operators on a 64-bit machine.  Hence
     we have special-cased this so that we get the faster operation on 64-bit machines.

     ┌───────────────────────────────────────────┬──────────┬─────────┬────────────┐
     │ Name                                      │ Time/Run │ mWd/Run │ Percentage │
     ├───────────────────────────────────────────┼──────────┼─────────┼────────────┤
     │ [time_ns.ml] round_nearest_portable_alloc │  17.31ns │   2.00w │    100.00% │
     │ [time_ns.ml] round_nearest_arch64_noalloc │   4.53ns │         │     26.16% │
     └───────────────────────────────────────────┴──────────┴─────────┴────────────┘
  *)
end
IFDEF ARCH_SIXTYFOUR THEN
let round_nearest = round_nearest_arch64_noalloc
ELSE
let round_nearest = round_nearest_portable_alloc
ENDIF

let float x = Int63.to_float x

TEST_UNIT =
  (* Set the timezone to UTC for unit test... *)
  Core_unix.putenv ~key:"TZ" ~data:"utc";
  ignore (Time.Zone.machine_zone ~refresh:true () : Time.Zone.t);
;;

(* This signature constraint is semi-temporary and serves to make the implementation more
   type-safe (so the compiler can help us more).  It would go away if we broke the
   implementation into multiple files. *)
module Span : sig
  type t = private Int63.t with typerep
  include Identifiable with type t := t

  val nanosecond  : t
  val microsecond : t
  val millisecond : t
  val second      : t
  val minute      : t
  val hour        : t
  val day         : t

  val of_ns  : float -> t
  val of_us  : float -> t
  val of_ms  : float -> t
  val of_sec : float -> t
  val of_min : float -> t
  val of_hr  : float -> t
  val of_day : float -> t
  val to_ns  : t     -> float
  val to_us  : t     -> float
  val to_ms  : t     -> float
  val to_sec : t     -> float
  val to_min : t     -> float
  val to_hr  : t     -> float
  val to_day : t     -> float

  val of_int_sec : int -> t
  val to_int_sec : t -> int

  val of_int63_ns : Int63.t -> t
  val to_int63_ns : t -> Int63.t
  val of_int_ns : int -> t
  val to_int_ns : t   -> int

  val zero : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val abs : t -> t
  val neg : t -> t
  val scale : t -> float -> t
  val ( / ) : t -> float -> t
  val ( // ) : t -> t -> float

  val create
    :  ?sign : Float.Sign.t
    -> ?day : int
    -> ?hr  : int
    -> ?min : int
    -> ?sec : int
    -> ?ms  : int
    -> ?us  : int
    -> ?ns  : int
    -> unit
    -> t

  val to_short_string : t -> string
  val randomize : t -> percent : float -> t

  module Parts : sig
    type t =
      { sign : Float.Sign.t
      ; hr   : int
      ; min  : int
      ; sec  : int
      ; ms   : int
      ; us   : int
      ; ns   : int
      }
    with sexp
  end

  val to_parts : t -> Parts.t
  val of_parts : Parts.t -> t

  val to_span : t -> Time.Span.t
  val of_span : Time.Span.t -> t

  include Robustly_comparable with type t := t

  module Option : sig
    type span

    type t = private int with typerep
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

  val since_unix_epoch : unit -> t

  module Stable : sig
    module V1 : sig
      type nonrec t = t with bin_io, compare, sexp
    end
  end
end = struct
  let to_span t = Time.Span.of_ns (Int63.to_float t)
  let of_span s = round_nearest (Time.Span.to_ns s)

  module T = struct
    type t = Int63.t (** nanoseconds *)
    with bin_io, typerep

    let sexp_of_t t = Time.Span.Stable.V1.sexp_of_t (to_span t)
    let t_of_sexp s = of_span (Time.Span.Stable.V1.t_of_sexp s)
    let compare = Int63.compare
    let zero = Int63.zero
    let module_name = "Time_ns.Span"
    let to_string t = Time.Span.to_string (to_span t)
    let of_string s = of_span (Time.Span.of_string s)
    let hash t = Int63.hash t
    let compare = compare
  end
  include T
  include Comparable.Validate_with_zero (T)
  include Identifiable.Make (T)

  let zero = Int63.zero

  module Parts = struct
    type t =
      { sign : Float.Sign.t
      ; hr   : int
      ; min  : int
      ; sec  : int
      ; ms   : int
      ; us   : int
      ; ns   : int
      }
    with sexp

    let compare = Poly.compare
  end

  let nanosecond  = Int63.of_int 1
  let microsecond = Int63.(of_int 1000 * nanosecond)
  let millisecond = Int63.(of_int 1000 * microsecond)
  let second      = Int63.(of_int 1000 * millisecond)
  let minute      = Int63.(of_int 60 * second)
  let hour        = Int63.(of_int 60 * minute)
  let day         = Int63.(of_int 24 * hour)

  let create
        ?(sign = Float.Sign.Pos)
        ?day:(d = 0)
        ?(hr  = 0)
        ?(min = 0)
        ?(sec = 0)
        ?(ms  = 0)
        ?(us  = 0)
        ?(ns  = 0)
        () =
    let minutes = min in
    let open Int63 in
    let t =
      of_int d * day
      + of_int hr * hour
      + of_int minutes * minute
      + of_int sec * second
      + of_int ms * millisecond
      + of_int us * microsecond
      + of_int ns * nanosecond
    in
    Float.Sign.(match sign with Neg -> -t | Pos | Zero -> t)

  let to_parts t =
    let open Int63 in
    let mag = abs t in
    { Parts.
      sign = Float.Sign.(if t < zero then Neg else if t > zero then Pos else Zero)
    ; hr = to_int_exn (mag / hour)
    ; min = to_int_exn ((rem mag hour) / minute)
    ; sec = to_int_exn ((rem mag minute) / second)
    ; ms = to_int_exn ((rem mag second) / millisecond)
    ; us = to_int_exn ((rem mag millisecond) / microsecond)
    ; ns = to_int_exn ((rem mag microsecond) / nanosecond)
    }

  let of_parts { Parts. sign; hr; min; sec; ms; us; ns } =
    create ~sign ~hr ~min ~sec ~ms ~us ~ns ()

  TEST_MODULE = struct
    let ( * ) = Int63.( * )
    let of_int = Int63.of_int

    let round_trip t = <:test_result< t >> (of_parts (to_parts t)) ~expect:t
    let eq t expect =
      <:test_result< t >> t ~expect;
      <:test_result< Parts.t >> (to_parts t) ~expect:(to_parts expect);
      round_trip t

    TEST_UNIT = eq (create ~us:2                       ()) (of_int 2    * microsecond)
    TEST_UNIT = eq (create ~min:3                      ()) (of_int 3    * minute)
    TEST_UNIT = eq (create ~ms:4                       ()) (of_int 4    * millisecond)
    TEST_UNIT = eq (create ~sec:5                      ()) (of_int 5    * second)
    TEST_UNIT = eq (create ~hr:6                       ()) (of_int 6    * hour)
    TEST_UNIT = eq (create ~day:7                      ()) (of_int 7    * day)
    TEST_UNIT = eq (create ~us:8 ~sign:Float.Sign.Neg  ()) (of_int (-8) * microsecond)
    TEST_UNIT = eq (create ~ms:9 ~sign:Float.Sign.Zero ()) (of_int 9    * millisecond)
    TEST_UNIT =
      let open Int63 in
      for _i = 1 to 1_000_000 do
        let t =
          (of_int64_exn (Random.int64 (to_int64 max_value)))
          + if Random.bool () then zero else min_value
        in
        round_trip t
      done

    let round_trip parts =
      <:test_result< Parts.t >> (to_parts (of_parts parts)) ~expect:parts
    let eq parts expect =
      <:test_result< Parts.t >> parts ~expect;
      <:test_result< t >> (of_parts parts) ~expect:(of_parts expect);
      round_trip parts

    TEST_UNIT =
      eq (to_parts (create ~sign:Float.Sign.Neg ~hr:2 ~min:3 ~sec:4 ~ms:5 ~us:6 ~ns:7 ()))
        { Parts. sign = Float.Sign.Neg; hr = 2; min = 3; sec = 4; ms = 5; us = 6; ns = 7 }
    TEST_UNIT = round_trip (to_parts (create ~hr:25 ()))
    TEST_UNIT = round_trip (to_parts (create ~hr:2217989799822798757 ()))
  end

  let of_ns       f = round_nearest f
  let of_int63_ns i = i
  let of_int_sec  i = Int63.(of_int i * second)
  let of_us       f = round_nearest (f *. float microsecond)
  let of_ms       f = round_nearest (f *. float millisecond)
  let of_sec      f = round_nearest (f *. float second)
  let of_min      f = round_nearest (f *. float minute)
  let of_hr       f = round_nearest (f *. float hour)
  let of_day      f = round_nearest (f *. float day)

  let to_ns       t = float t
  let to_int63_ns t =       t
  let to_us       t = float t /. float microsecond
  let to_ms       t = float t /. float millisecond
  let to_sec      t = float t /. float second
  let to_min      t = float t /. float minute
  let to_hr       t = float t /. float hour
  let to_day      t = float t /. float day
  let to_int_sec  t = Int63.(to_int_exn (t / second))
  TEST = Int.(>) (to_int_sec Int63.max_value) 0 (* and doesn't raise *)

IFDEF ARCH_SIXTYFOUR THEN
  let of_int_ns i = of_int63_ns (Int63.of_int i)
  let to_int_ns t = Int63.to_int_exn (to_int63_ns t)
ELSE
  let of_int_ns _i = failwith "unsupported on 32bit machines"
  let to_int_ns _i = failwith "unsupported on 32bit machines"
ENDIF


  let (+)       = Int63.(+)
  let (-)       = Int63.(-)
  let abs       = Int63.(abs)
  let neg       = Int63.(neg)
  let scale t f = round_nearest (float t *. f)
  let (/)   t f = round_nearest (float t /. f)
  let (//)      = Int63.(//)

  let to_short_string t = Time.Span.to_short_string (to_span t)

  let randomize t ~percent = of_span (Time.Span.randomize (to_span t) ~percent)

  (* Functions required by [Robustly_comparable]: allows for [epsilon] granularity.

     A microsecond is a reasonable granularity because there is very little network
     activity that can be measured to sub-microsecond resolution. *)
  let epsilon = microsecond
  let (>=.) t u = t >= Int63.(u - epsilon)
  let (<=.) t u = t <= Int63.(u + epsilon)
  let (=.) t u = Int63.(abs (t - u)) <= epsilon
  let (>.) t u = t > Int63.(u + epsilon)
  let (<.) t u = t < Int63.(u - epsilon)
  let (<>.) t u = Int63.(abs (t - u)) > epsilon
  let robustly_compare t u = if t <. u then -1 else if t >. u then 1 else 0

  module Option = struct
    type span = t with sexp
    type t = Int63.t with bin_io, typerep (* ns since epoch or min_value = none *)
    let none = Int63.min_value
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
      let hash = Int63.hash
      let module_name = "Time_ns.Span.Option"
      include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
    end)

    module Stable = struct
      module V1 = struct
        type nonrec t = t with sexp, bin_io
      end

      TEST_MODULE "Time_ns.Span.Stable.V1" = Core_kernel.Stable_unit_test.Make (struct
        include V1

        let equal = Int63.equal

        let tests =
          let mk_some i = some (Int63.of_int64_exn i) in
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

  external since_unix_epoch_or_zero : unit -> t
    = "core_time_ns_clock_rt_gettime_or_zero" "noalloc"

  let since_unix_epoch () =
    let t = since_unix_epoch_or_zero () in
    if t <> zero then t else failwith "clock_gettime(CLOCK_REALTIME) failed"

  module Stable = struct
    module V1 = struct
      type nonrec t = t with bin_io, compare, sexp
    end

    TEST_MODULE "Time_ns.Span.Stable.V1" = Core_kernel.Stable_unit_test.Make (struct
      include V1

      let equal = Int63.equal

      let tests =
        let t i = of_int63_ns (Int63.of_int64_exn i) in
        [ t      1_234_560_000_000L,  "20.576m", "\252\000\160\130q\031\001\000\000"
        ; t                  1_000L,  "0.001ms", "\254\232\003"
        ; t 80_000_006_400_000_000L, "925.926d", "\252\000@\128\251\1487\028\001"
        ]
    end)
  end
end

type t = Span.t (** since the Unix epoch (1970-01-01 00:00:00 UTC) *)
with bin_io, compare, typerep

let now = Span.since_unix_epoch

let epoch = Span.zero
let add = Span.(+)
let sub = Span.(-)
let diff = Span.(-)
let abs_diff t u = Span.abs (diff t u)

let to_time t = Time.add Time.epoch (Span.to_span t)
let of_time t = Span.of_span (Time.diff t Time.epoch)

let sexp_of_t t = Time.sexp_of_t (to_time t)
let t_of_sexp s = of_time (Time.t_of_sexp s)

let to_string t = Time.to_string (to_time t)
let of_string s = of_time (Time.of_string s)

module Stable0 = struct
  module V1 = struct
    type nonrec t = t with bin_io, compare, sexp
  end

  TEST_MODULE "Time_ns.Stable.V1" = Core_kernel.Stable_unit_test.Make (struct
    include V1

    let equal = Span.equal

    let tests =
      let t i = Span.of_int63_ns (Int63.of_int64_exn i) in
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
  let some = Span.Option.some
  let is_none = Span.Option.is_none
  let is_some = Span.Option.is_some
  let value = Span.Option.value
  let value_exn = Span.Option.value_exn
  TEST_UNIT = <:test_result< time >> (value_exn (some epoch)) ~expect:epoch
  TEST_UNIT = let t = now () in <:test_result< time >> (value_exn (some t)) ~expect:t
  TEST = is_error (Result.try_with (fun () -> value_exn none))

  let of_option = function None -> none | Some t -> some t
  let to_option t = if is_none t then None else Some (value_exn t)

  let sexp_of_t t = <:sexp_of< time option >> (to_option t)
  let t_of_sexp s = of_option (<:of_sexp< time option >> s)

  include Identifiable.Make (struct
    type nonrec t = t with sexp, compare, bin_io
    let module_name = "Time_ns.Option"
    let hash = Span.Option.hash
    include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
  end)

  module Stable = struct
    module V1 = struct
      type nonrec t = t with sexp, bin_io
    end

    TEST_MODULE "Time_ns.Option.Stable.V1" = Core_kernel.Stable_unit_test.Make (struct
      include V1

      let equal t1 t2 = Int.(=) (t1 : t :> int) (t2 : t :> int)

      let tests =
        let t i = Span.of_int63_ns (Int63.of_int64_exn i) in
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
  let module_name = "Time_ns"
  let hash = Span.hash
  let of_string, to_string = of_string, to_string
end)

let to_span_since_epoch t = t
let of_span_since_epoch s = s


TEST_MODULE = struct
  TEST = epoch = Span.zero

  TEST_UNIT "round trip from [Time.t] to [t] and back" =
    let times = List.map ~f:Time.of_float [ 0.0; 1.0; 1.123456789 ] in
    List.iter times ~f:(fun time ->
      let res = to_time (of_time time) in
      if not (Time.equal res time)
      then failwithf "Failed on time %s (%f)"
             (Time.to_string time)
             (Time.to_float time) ())

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
  with typerep


  let start_of_day = Span.zero
  let end_of_day = Span.day

  let to_span_since_start_of_day t = t
  let of_span_since_start_of_day s = s

  let local_midnight time =
    let date, _ = Time.to_local_date_ofday (to_time time) in
    let midnight = Time.of_local_date_ofday date Time.Ofday.start_of_day in
    of_time midnight
  ;;

  let local_midnight_cache = ref (local_midnight (now ()))

  let of_local_time time =
    let t = diff time !local_midnight_cache in
    if t >= start_of_day && t < end_of_day then t
    else begin
      local_midnight_cache := local_midnight time;
      diff time !local_midnight_cache
    end
  ;;

  let local_now () = of_local_time (now ())

  let to_string t =
    if start_of_day <= t && t < end_of_day then
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
    if start_of_day <= t && t < end_of_day then
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

  let t_of_sexp s = of_ofday (Time.Ofday.t_of_sexp s)
  let sexp_of_t t = Time.Ofday.sexp_of_t (to_ofday t)

  include Identifiable.Make (struct
    type nonrec t = t with sexp, compare, bin_io
    let module_name = "Time_ns.Ofday"
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
    let ofday = Ofday.of_span_since_start_of_day span in
    let expected = "08:27:14.359" in
    let ms_str = Ofday.to_millisecond_string ofday    in
    if String.(<>) ms_str expected then
      failwithf "Failed on Ofday.to_millisecond_string Got (%s) expected (%s)"
        ms_str expected ()

  TEST_UNIT =
    (* Ensure that local_midnight_cache doesn't interfere with converting times that are
       much earlier or later than each other. *)
    let check ofday =
      if Ofday.(<) ofday Ofday.start_of_day
      then failwithf !"too small: %d" (ofday : Ofday.t :> int) ()
      else if Ofday.(>=) ofday Ofday.end_of_day
      then failwithf !"too large: %d" (ofday : Ofday.t :> int) ()
      else ()
    in
    check (Ofday.of_local_time epoch);
    check (Ofday.of_local_time (now ()));
    check (Ofday.of_local_time epoch)

end

let to_int63_ns_since_epoch t = Span.to_int63_ns (to_span_since_epoch t)
let of_int63_ns_since_epoch i = of_span_since_epoch (Span.of_int63_ns i)

IFDEF ARCH_SIXTYFOUR THEN
  let to_int_ns_since_epoch t = Int63.to_int_exn (to_int63_ns_since_epoch t)
  let of_int_ns_since_epoch i = of_int63_ns_since_epoch (Int63.of_int i)
ELSE
  let to_int_ns_since_epoch t = failwith "unsupported on 32bit machines"
  let of_int_ns_since_epoch i = failwith "unsupported on 32bit machines"
ENDIF


let of_date_ofday zone date ofday =
  of_time (Core_time.of_date_ofday zone date (Ofday.to_ofday ofday))
;;

let to_date t zone = Core_time.to_date (to_time t) zone

let occurrence what t ~ofday ~zone =
  of_time (Core_time.occurrence what (to_time t) ~ofday:(Ofday.to_ofday ofday) ~zone)
;;

module Stable = struct
  include Stable0
  module Span   = Span   .Stable
  module Option = Option .Stable
  module Ofday  = Ofday  .Stable
end
