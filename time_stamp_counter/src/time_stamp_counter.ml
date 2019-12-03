(* Time stamp counter
   ==================

   This module tries to estimate time based on the CPU time stamp counter (TSC).  The time
   estimates reported by this module are monotonically increasing.  It uses [Time.now ()]
   as its measure of "real time" to do this.

   Historically, the rate of increment of the TSC (sometimes referred to as the TSC
   frequency) varied based of CPU overclocking, temperature, load etc.  On modern Intel
   CPU's the TSC is expected to be stable.  On Linux systems, the "constant_tsc" in
   /proc/cpuinfo indicates that the machine has a stable TSC rate.  While this module
   assumes that the TSC is relatively stable, it can adapt to small variations in the TSC
   frequency.

   Simple Overview
   ===============
   Here is an explanation of how this module works.  The module measures the change in
   real time and the change in TSC at every calibration call and maintains an EWMA of
   these deltas.  It then uses the EWMA values to do linear regression where time is the
   estimated value and TSC is the predictor.  The linear regression done at every
   calibration step produces an updated time/tsc slope.  Using this time/tsc slope and
   the latest value of real time, the module estimates time in terms of tsc.

   Ensuring Monotonicity of Time
   =============================
   The simple picture above is complicated by the presence of noise.  There are two
   significant sources of noise.  The first is the noise caused by variations in the
   frequency of TSC.  The second, and probably the more significant one, is noise in real
   time, i.e. noise in the [Time.now ()] call.

   (1) [Time.now ()] calls suffer from the overhead of transition from the user program to
   a kernel vdso and

   (2) It is affected by NTP updates.

   (3) Another significant source of error comes from loss of precision.  [Time.now]
   reports a 64-bit float of which it has 52 bits of mantissa.  The 52 bits of mantissa
   for time in seconds from Unix epoch only allows for precision in the order of
   micro-seconds.  Consequently the measurement of time using [Time.now] can only be
   precise in the order of micro-seconds.

   Noise in measuring real time and in the rate of time/tsc implies that at each
   calibration point the estimated time "jumps" up or down with respect to the estimate
   value of time before calibration.  In other words, the time estimated using the EWMA
   linear regression is not strictly monotonic.

   We report a monotonic time in terms of the estimated time, by maintaining a separate
   slope called the "monotonic time/TSC slope".  At every calibration point, we take the
   last estimated time and adjust the monotonic time/TSC slope such that it catches up to
   the estimated time in a fixed number of cycles.  If the expected change in slope is too
   high, we bound the rate of change of the monotonic time/TSC slope.  As long as
   monotonic time has not caught up with the estimated time we report time in terms of the
   adjusted monotonic time slope.  Once we have caught up to the estimated time, we start
   reporting the estimated time.

   We can chose the number of cycles to allow for catchup to be any number we wish.  A
   number in the order of 1E6-1E9 TSC steps allows for a gradual catchup rate without too
   many abrupt changes in the rate of reported time.  The bound to rate of change is
   expressed in percentage terms of slope and is at max the ratio by which we expect the
   underlying TSC frequency to change on the machine.  It is defined as
   [max_perc_change_from_real_slope] below.

   It is worth noting that the approximation of the monotonic slope trying to catch up
   with the estimate slope can be achieved in many other ways.  A more principled approach
   to this would be to use a PID controller that adapts to error and gets the reported
   monotonic time to smoothly fit the estimated time.  However PID controllers are
   computationally more expensive and we use a simpler linear approximation.
*)

[%%import
  "config.h"]

open! Core
open Poly
open! Import

let max_percent_change_from_real_slope = 0.20

let () =
  assert (0. <= max_percent_change_from_real_slope);
  assert (max_percent_change_from_real_slope <= 1.)
;;

let ewma ~alpha ~old ~add = ((1. -. alpha) *. old) +. (alpha *. add)


type t = Int63.t [@@deriving bin_io, compare, sexp]
type tsc = t [@@deriving bin_io, compare, sexp]

include (Int63 : Comparisons.S with type t := t)

let diff t1 t2 = Int63.( - ) t1 t2
let add t s = Int63.( + ) t s
let of_int63 t = t
let to_int63 t = t
let zero = Int63.zero

[%%ifdef
  JSC_ARCH_SIXTYFOUR]

(* noalloc on x86_64 only *)
external now : unit -> tsc = "tsc_get" [@@noalloc]

module Calibrator = struct
  (* performance hack: prevent writes to this record from boxing floats by making all
     fields mutable floats *)
  type float_fields =
    { (* the most recent observations and regression results *)
      mutable time : float
    ; mutable sec_per_cycle : float
    (* mutable sec_error_intercept      : float; *)

    (* this time value is monotonically increasing *)
    ; mutable monotonic_time : float
    ; mutable monotonic_sec_per_cycle : float (* for linear regression *)
    ; mutable ewma_time_tsc : float
    ; mutable ewma_tsc_square : float
    ; mutable ewma_time : float
    ; mutable ewma_tsc : float (* for computing time in nanos *)
    ; mutable nanos_per_cycle : float
    ; mutable monotonic_nanos_per_cycle : float
    }
  [@@deriving bin_io, sexp]

  type t =
    { (* the most recent observations and regression results *)
      mutable tsc : tsc (* this time value is monotonically increasing *)
    ; mutable monotonic_until_tsc : tsc (* for computing time in nanos *)
    ; mutable time_nanos : Int63.t
    ; mutable monotonic_time_nanos : Int63.t
    ; floats : float_fields
    }
  [@@deriving bin_io, fields, sexp]

  let tsc_to_seconds_since_epoch =
    let[@inline] convert t tsc base mul =
      base +. (mul *. Int63.to_float (diff tsc t.tsc))
    in
    fun [@inline] t tsc ->
      0.
      +.
      (* performance hack: stops float boxing *)
      if tsc < t.monotonic_until_tsc
      then
        0.
        +. (* performance hack: stops float boxing *)
        convert t tsc t.floats.monotonic_time t.floats.monotonic_sec_per_cycle
      else
        0.
        +. (* performance hack: stops float boxing *)
        convert t tsc t.floats.time t.floats.sec_per_cycle
  ;;

  let tsc_to_nanos_since_epoch =
    let convert t tsc base mul =
      (* Scale an int by a float without intermediate allocation and overflow. *)
      Int63.( + )
        base
        (Float.int63_round_nearest_exn (mul *. Int63.to_float (diff tsc t.tsc)))
    in
    fun t tsc ->
      if tsc < t.monotonic_until_tsc
      then convert t tsc t.monotonic_time_nanos t.floats.monotonic_nanos_per_cycle
      else convert t tsc t.time_nanos t.floats.nanos_per_cycle
  ;;

  (* The rate of response to the variations in TSC frequency can be controlled via alpha.
     Alpha should be in (0,1] and controls the decay of the subsequent EWMA calculation.
     A low number such as 0.01 suggests that the TSC is largely stable and small
     variations should be treated as noise.  Setting this number to 0.6 or higher
     indicates that each new measurement of the TSC should significantly outweigh past
     measurements which has the effect of making time calibration more responsive to
     frequency changes.  In this module we have chosen a value of alpha that varies with
     the duration of time, i.e. longer time samples are given more weight and shorter time
     samples are given lesser weight. *)
  let alpha_for_interval time_diff = 0. +. Float.max 0. (1. -. exp (-0.5 *. time_diff))
  let catchup_cycles = 1E9

  let initial_alpha = 1.

  (* performance hack: This function is the same as

     {[
       match Float.iround_up float with
       | None   -> if_iround_up_fails
       | Some i -> Int63.(+) int i
     ]}

     but I couldn't find a way to make the simple version stop allocating, even with
     flambda turned on *)
  let iround_up_and_add int ~if_iround_up_fails float =
    if Float.( > ) float 0.0
    then (
      let float' = Caml.ceil float in
      if Float.( <= ) float' Float.iround_ubound
      then Int63.( + ) int (Int63.of_float_unchecked float')
      else if_iround_up_fails)
    else if Float.( >= ) float Float.iround_lbound
    then Int63.( + ) int (Int63.of_float_unchecked float)
    else if_iround_up_fails
  ;;

  let[@inline] calibrate_using t ~tsc ~time ~am_initializing =
    let estimated_time =
      0. +. (* performance hack: stops float boxing *)
      tsc_to_seconds_since_epoch t tsc
    in
    let time_diff_est = time -. estimated_time in
    let time_diff = time -. t.floats.time in
    let tsc_diff = Int63.to_float (diff tsc t.tsc) in
    let alpha =
      if am_initializing then initial_alpha else alpha_for_interval time_diff
    in
    (* update current times *)
    t.floats.time <- time;
    t.tsc <- tsc;
    (* update ewma and regression. *)
    t.floats.ewma_time_tsc
    <- ewma ~alpha ~old:t.floats.ewma_time_tsc ~add:(tsc_diff *. time_diff);
    t.floats.ewma_tsc_square
    <- ewma ~alpha ~old:t.floats.ewma_tsc_square ~add:(tsc_diff *. tsc_diff);
    t.floats.ewma_tsc <- ewma ~alpha ~old:t.floats.ewma_tsc ~add:tsc_diff;
    t.floats.ewma_time <- ewma ~alpha ~old:t.floats.ewma_time ~add:time_diff;
    (* linear regression *)
    t.floats.sec_per_cycle <- t.floats.ewma_time_tsc /. t.floats.ewma_tsc_square;
    (* t.sec_error_intercept <- t.ewma_time -. t.sec_per_cycle *. t.ewma_tsc; *)
    (* monotonic predicted time and slope. *)
    t.floats.monotonic_time <- estimated_time;
    if not am_initializing
    then (
      let catchup_sec_per_cycle =
        (* The slope so that after [catchup_cycles], the monotonic estimated time equals
           the estimated time, i.e. solve for [monotonic_sec_per_cycle] in:

           {[
             t.monotonic_time + monotonic_sec_per_cycle * catchup_cycles
             = t.time         + t.sec_per_cycle         * catchup_cycles
           ]}

           Note that [time_diff_est = t.time - t.monotonic_time]. *)
        t.floats.sec_per_cycle +. (time_diff_est /. catchup_cycles)
      in
      t.floats.monotonic_sec_per_cycle
      <- (if Float.is_positive time_diff_est
          then
            0.
            +. (* performance hack: stops float boxing *)
            Float.min
              catchup_sec_per_cycle
              (t.floats.sec_per_cycle *. (1. +. max_percent_change_from_real_slope))
          else
            0.
            +. (* performance hack: stops float boxing *)
            Float.max
              catchup_sec_per_cycle
              (t.floats.sec_per_cycle *. (1. -. max_percent_change_from_real_slope)));
      (* Compute the number of cycles in the future at which monotonic estimated time
         equals estimated time, i.e. solve for [cycles] in:

         {[
           t.monotonic_time + t.monotonic_sec_per_cycle * cycles
           = t.time         + t.sec_per_cycle           * cycles
         ]}

         This value might get very small when the two slopes are about the same.  In such
         cases we just use the estimated slope always. *)
      t.monotonic_until_tsc
      <- time_diff_est /. (t.floats.monotonic_sec_per_cycle -. t.floats.sec_per_cycle)
         |> iround_up_and_add tsc ~if_iround_up_fails:Int63.zero);
    (* Precompute values required for [tsc_to_nanos_since_epoch]. *)
    t.time_nanos <- Float.int63_round_nearest_exn (t.floats.time *. 1E9);
    t.floats.nanos_per_cycle <- t.floats.sec_per_cycle *. 1E9;
    t.monotonic_time_nanos
    <- Float.int63_round_nearest_exn (t.floats.monotonic_time *. 1E9);
    t.floats.monotonic_nanos_per_cycle <- t.floats.monotonic_sec_per_cycle *. 1E9
  ;;

  let now_float () =
    1E-9 *. Int.to_float (Time_ns.to_int_ns_since_epoch (Time_ns.now ()))
  ;;

  let initialize t samples =
    List.iter samples ~f:(fun (tsc, time) ->
      calibrate_using t ~tsc ~time ~am_initializing:true)
  ;;

  let collect_samples ~num_samples ~interval =
    assert (Int.( >= ) num_samples 1);
    (* We sleep at differing intervals to improve the estimation of [sec_per_cycle]. *)
    let rec loop n sleep =
      let sample = now (), now_float () in
      if Int.( = ) n 1
      then [ sample ]
      else (
        ignore (Unix.nanosleep sleep);
        sample :: loop (n - 1) (sleep +. interval))
    in
    loop num_samples interval
  ;;

  let create_using ~tsc ~time ~samples =
    let t =
      { monotonic_until_tsc = Int63.zero
      ; tsc
      ; time_nanos = Int63.zero
      ; monotonic_time_nanos = Int63.zero
      ; floats =
          { monotonic_time = time
          ; sec_per_cycle = 0.
          ; monotonic_sec_per_cycle = 0.
          ; time
          ; ewma_time_tsc = 0.
          ; ewma_tsc_square = 0.
          ; ewma_time = 0.
          ; ewma_tsc = 0.
          ; nanos_per_cycle = 0.
          ; monotonic_nanos_per_cycle = 0.
          }
      }
    in
    initialize t samples;
    t
  ;;

  let create () =
    let time = now_float () in
    let tsc = now () in
    let samples = collect_samples ~num_samples:3 ~interval:0.0005 in
    create_using ~tsc ~time ~samples
  ;;

  (* Creating a calibrator takes about 3ms. *)
  let t = lazy (create ())
  let cpu_mhz = Ok (fun t -> 1. /. (t.floats.sec_per_cycle *. 1E6))

  (* performance hack: [@cold] so [time] is always unboxed. [now_float] and
     [calibrate_using] need to be inlined into the same function for unboxed [time].
     Preventing [calibrate] from being inlined makes the compiler's inlining decision
     more predictable. *)
  let[@cold] calibrate t =
    calibrate_using t ~tsc:(now ()) ~time:(now_float ()) ~am_initializing:false
  ;;

  module Private = struct
    let create_using = create_using
    let calibrate_using = calibrate_using
    let initialize = initialize
    let nanos_per_cycle t = t.floats.nanos_per_cycle
  end
end

[%%else]

(* noalloc on x86_64 only *)
external now : unit -> tsc = "tsc_get"

(* Outside of x86_64, [now] returns the result of clock_gettime(), i.e. the current time
   in nanos past epoch. *)

module Calibrator = struct
  type t = unit [@@deriving bin_io, sexp]

  let tsc_to_seconds_since_epoch _t tsc = Int63.to_float tsc *. 1e-9
  let tsc_to_nanos_since_epoch _t tsc = tsc
  let create_using ~tsc:_ ~time:_ ~samples:_ = ()
  let create () = ()
  let initialize _t _samples = ()
  let calibrate_using _t ~tsc:_ ~time:_ ~am_initializing:_ = ()
  let calibrate _ = ()
  let t = lazy (create ())

  let cpu_mhz =
    Or_error.unimplemented
      "Time_stamp_counter.Calibrator.cpu_mhz is not defined for 32-bit platforms"
  ;;

  module Private = struct
    let create_using = create_using
    let calibrate_using = calibrate_using
    let initialize = initialize
    let nanos_per_cycle _ = 1.
  end
end

[%%endif]

module Span = struct
  include Int63

  module Private = struct
    let of_int63 t = t
    let to_int63 t = t
  end

  [%%ifdef
    JSC_ARCH_SIXTYFOUR]

  let to_ns t ~(calibrator : Calibrator.t) =
    Float.int63_round_nearest_exn (Int63.to_float t *. calibrator.floats.nanos_per_cycle)
  ;;

  (* If the calibrator has not been well calibrated and [ns] is a large value, the
     following can overflow. This happens rarely in hydra in a way that difficult to
     reproduce. We've improved the exn here so that we have more information to debug
     these spurious errors when they come up. *)
  let of_ns ns ~(calibrator : Calibrator.t) =
    try
      Float.int63_round_nearest_exn
        (Int63.to_float ns /. calibrator.floats.nanos_per_cycle)
    with
    | exn -> raise_s [%message "" ~_:(exn : Exn.t) (calibrator : Calibrator.t)]
  ;;

  [%%else]

  (* [tsc_get] already returns the current time in ns *)

  let to_ns t ~calibrator:_ = t
  let of_ns ns ~calibrator:_ = ns

  [%%endif]

  let to_time_span t ~calibrator = Time.Span.of_ns (Int63.to_float (to_ns t ~calibrator))
end

let calibrator = Calibrator.t

let to_time t ~calibrator =
  Calibrator.tsc_to_seconds_since_epoch calibrator t
  |> Time.Span.of_sec
  |> Time.of_span_since_epoch
;;

let to_nanos_since_epoch t ~calibrator = Calibrator.tsc_to_nanos_since_epoch calibrator t

let to_time_ns t ~calibrator =
  Time_ns.of_int63_ns_since_epoch (to_nanos_since_epoch ~calibrator t)
;;

module Private = struct
  let ewma = ewma
  let of_int63 = of_int63
  let max_percent_change_from_real_slope = max_percent_change_from_real_slope
  let to_nanos_since_epoch = to_nanos_since_epoch
end
