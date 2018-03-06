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

   (1) [Time.now ()] calls suffer from the overhead of transition from the the user
   program to a kernel vdso and

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

[%%import "config.h"]

open! Import

module Unix = Core_unix

let max_percent_change_from_real_slope = 0.20
let%test_unit _ =
  assert (0. <= max_percent_change_from_real_slope);
  assert (max_percent_change_from_real_slope <= 1.)
;;

let ewma ~alpha ~old ~add = ((1. -. alpha) *. old) +. (alpha *. add)


type t = Int63.t [@@deriving bin_io, compare, sexp]
type tsc = t     [@@deriving bin_io, compare, sexp]

let diff t1 t2 = Int63.(-) t1 t2
let add t s = Int63.(+) t s
let to_int63 t = t

[%%ifdef JSC_ARCH_SIXTYFOUR]

(* noalloc on x86_64 only *)
external now : unit -> tsc = "tsc_get" [@@noalloc]

module Calibrator = struct

  type t =
    {
      (* the most recent observations and regression results *)
      mutable time                      : float
    ; mutable tsc                       : tsc
    ; mutable sec_per_cycle             : float
    (* mutable sec_error_intercept               : float; *)

    (* this time value is monotonically increasing *)
    ; mutable monotonic_time            : float
    ; mutable monotonic_sec_per_cycle   : float
    ; mutable monotonic_until_tsc       : tsc

    (* for linear regression *)
    ; mutable ewma_time_tsc             : float
    ; mutable ewma_tsc_square           : float
    ; mutable ewma_time                 : float
    ; mutable ewma_tsc                  : float

    (* for computing time in nanos *)
    ; mutable time_nanos                : Int63.t
    ; mutable nanos_per_cycle           : float
    ; mutable monotonic_time_nanos      : Int63.t
    ; mutable monotonic_nanos_per_cycle : float
    }
  [@@deriving bin_io, sexp]

  let tsc_to_time =
    let convert t tsc base mul =
      Time.of_span_since_epoch
        (Time.Span.of_sec
           (base +. (mul *. Int63.to_float (diff tsc t.tsc))))
    in
    fun t tsc ->
      if tsc < t.monotonic_until_tsc
      then convert t tsc t.monotonic_time t.monotonic_sec_per_cycle
      else convert t tsc t.time           t.sec_per_cycle
  ;;

  let tsc_to_nanos_since_epoch =
    let convert t tsc base mul =
      (* Scale an int by a float without intermediate allocation and overflow. *)
      Int63.(+) base (Float.int63_round_nearest_exn (mul *. Int63.to_float (diff tsc t.tsc)))
    in
    fun t tsc ->
      if tsc < t.monotonic_until_tsc
      then convert t tsc t.monotonic_time_nanos t.monotonic_nanos_per_cycle
      else convert t tsc t.time_nanos           t.nanos_per_cycle
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
  let alpha_for_interval time_diff =
    1. -. exp (-0.5 *. time_diff)
  ;;

  let catchup_cycles                  = 1E9
  let initial_alpha                   = 1.


  let calibrate_using t ~tsc ~time ~am_initializing =
    let estimated_time = Time.Span.to_sec (Time.to_span_since_epoch (tsc_to_time t tsc)) in
    let time_diff_est  = time -. estimated_time              in
    let time_diff      = time -. t.time                      in
    let tsc_diff       = Int63.to_float (diff tsc t.tsc) in
    let alpha =
      if am_initializing
      then initial_alpha
      else alpha_for_interval time_diff
    in
    (* update current times *)
    t.time <- time;
    t.tsc  <- tsc;
    (* update ewma and regression. *)
    t.ewma_time_tsc   <- ewma ~alpha ~old:t.ewma_time_tsc   ~add:(tsc_diff *. time_diff);
    t.ewma_tsc_square <- ewma ~alpha ~old:t.ewma_tsc_square ~add:(tsc_diff *. tsc_diff);
    t.ewma_tsc        <- ewma ~alpha ~old:t.ewma_tsc        ~add:tsc_diff;
    t.ewma_time       <- ewma ~alpha ~old:t.ewma_time       ~add:time_diff;
    (* linear regression *)
    t.sec_per_cycle <- t.ewma_time_tsc /. t.ewma_tsc_square;
    (* t.sec_error_intercept <- t.ewma_time -. t.sec_per_cycle *. t.ewma_tsc; *)
    (* monotonic predicted time and slope. *)
    t.monotonic_time <- estimated_time;
    if not am_initializing then begin
      let catchup_sec_per_cycle =
        (* The slope so that after [catchup_cycles], the monotonic estimated time equals
           the estimated time, i.e. solve for [monotonic_sec_per_cycle] in:

           {[
             t.monotonic_time + monotonic_sec_per_cyle * catchup_cycles
             = t.time           + t.sec_per_cycle        * catchup_cycles
           ]}

           Note that [time_diff_est = t.time - t.monotonic_time]. *)
        t.sec_per_cycle +. (time_diff_est /. catchup_cycles)
      in
      t.monotonic_sec_per_cycle <-
        if Float.is_positive time_diff_est
        then Float.min catchup_sec_per_cycle
               (t.sec_per_cycle *. (1. +. max_percent_change_from_real_slope))
        else Float.max catchup_sec_per_cycle
               (t.sec_per_cycle *. (1. -. max_percent_change_from_real_slope));
      (* Compute the number of cycles in the future at which monotonic estimated time
         equals estimated time, i.e. solve for [cycles] in:

         {[
           t.monotonic_time + t.monotonic_sec_per_cyle * cycles
           = t.time         + t.sec_per_cycle          * cycles
         ]}

         This value might get very small when the two slopes are about the same.  In such
         cases we just use the estimated slope always. *)
      t.monotonic_until_tsc <-
        (match
           Float.iround_up (time_diff_est /. (t.monotonic_sec_per_cycle -. t.sec_per_cycle))
         with
         | Some x -> add tsc (Int63.of_int x)
         | None   -> Int63.zero);
    end;

    (* Precompute values required for [tsc_to_nanos_since_epoch]. *)
    t.time_nanos                <- Float.int63_round_nearest_exn (t.time *. 1E9);
    t.nanos_per_cycle           <- t.sec_per_cycle *. 1E9;
    t.monotonic_time_nanos      <- Float.int63_round_nearest_exn (t.monotonic_time *. 1E9);
    t.monotonic_nanos_per_cycle <- t.monotonic_sec_per_cycle *. 1E9;
  ;;

  let now_float () = Time.Span.to_sec (Time.to_span_since_epoch (Time.now ()))

  let initialize t samples =
    List.iter samples ~f:(fun (tsc, time) ->
      calibrate_using t ~tsc ~time ~am_initializing:true);
  ;;

  let collect_samples ~num_samples ~interval =
    assert (num_samples >= 1);
    (* We sleep at differing intervals to improve the estimation of [sec_per_cycle]. *)
    let rec loop n sleep =
      let sample = (now (), now_float ()) in
      if n = 1
      then [sample]
      else begin
        ignore (Unix.nanosleep sleep);
        sample :: loop (n-1) (sleep +. interval)
      end in
    loop num_samples interval
  ;;

  let create () =
    let now_float = now_float () in
    let t =
      { monotonic_time            = now_float
      ; monotonic_sec_per_cycle   = 0.
      ; monotonic_until_tsc       = Int63.zero

      ; time                      = now_float
      ; tsc                       = now ()
      ; sec_per_cycle             = 0.

      ; ewma_time_tsc             = 0.
      ; ewma_tsc_square           = 0.
      ; ewma_time                 = 0.
      ; ewma_tsc                  = 0.

      ; time_nanos                = Int63.zero
      ; nanos_per_cycle           = 0.
      ; monotonic_time_nanos      = Int63.zero
      ; monotonic_nanos_per_cycle = 0.
      }
    in
    initialize t (collect_samples ~num_samples:3 ~interval:0.0005);
    t
  ;;

  (* Creating a calibrator takes about 3ms and is fast enough that we don't mind paying
     for it at startup. *)
  let local = create ()

  let cpu_mhz = Ok (fun ?(t = local) () -> 1. /. (t.sec_per_cycle *. 1E6))

  let calibrate ?(t = local) () =
    calibrate_using t ~tsc:(now ()) ~time:(now_float ()) ~am_initializing:false
  ;;
end

[%%else]

(* noalloc on x86_64 only *)
external now : unit -> tsc = "tsc_get"
(* Outside of x86_64, [now] returns the result of clock_gettime(), i.e. the current time
   in nanos past epoch. *)

module Calibrator = struct
  type t = unit [@@deriving bin_io, sexp]

  let tsc_to_time _t tsc = Time.of_span_since_epoch (Time.Span.of_sec (Int63.to_float tsc *. 1e-9))

  let tsc_to_nanos_since_epoch _t tsc = tsc

  let create () = ()

  let initialize _t _samples = ()

  let calibrate_using _t ~tsc:_ ~time:_ ~am_initializing:_ = ()

  let calibrate ?t:_ () = ()

  let local = create ()

  let cpu_mhz = Or_error.unimplemented "\
Time_stamp_counter.Calibrator.cpu_mhz is not defined for 32-bit platforms"
  ;;
end

[%%endif]

module Span = struct
  include Int63
  [%%ifdef JSC_ARCH_SIXTYFOUR]
  let to_ns ?(calibrator = Calibrator.local) t =
    Float.int63_round_nearest_exn
      (Int63.to_float t *. calibrator.Calibrator.nanos_per_cycle)
  ;;

  (* If the calibrator has not been well calibrated and [ns] is a large value, the
     following can overflow. This happens rarely in hydra in a way that difficult to
     reproduce. We've improved the exn here so that we have more information to debug
     these spurious errors when they come up. *)
  let of_ns ?(calibrator = Calibrator.local) ns =
    try
      Float.int63_round_nearest_exn
        (Int63.to_float ns /. calibrator.Calibrator.nanos_per_cycle)
    with exn ->
      raise_s [%message
        ""
          ~_:(exn : Exn.t)
          (calibrator : Calibrator.t)]
  ;;
  [%%else]
  (* [tsc_get] already returns the current time in ns *)

  let to_ns ?calibrator:_ t = t

  let of_ns ?calibrator:_ ns = ns
  [%%endif]

  let to_time_span ?calibrator t =
    Time.Span.of_ns (Int63.to_float (to_ns ?calibrator t))
  ;;
end

let to_time ?(calibrator = Calibrator.local) t = Calibrator.tsc_to_time calibrator t

let to_nanos_since_epoch ~calibrator t =
  Calibrator.tsc_to_nanos_since_epoch calibrator t;
;;

let to_time_ns ?(calibrator = Calibrator.local) t =
  Time_ns.of_int63_ns_since_epoch (to_nanos_since_epoch ~calibrator t)
;;

let%test_module _ = (module struct

  let _unused_in_32bit = ewma, Calibrator.initialize, Calibrator.calibrate_using

  [%%ifdef JSC_ARCH_SIXTYFOUR]
  (* monotonicity testing *)
  let%test_unit _ =
    let calibrator = Calibrator.create () in
    let last = ref 0. in
    for i = 1 to 10_000_000 do
      let cur =
        Time.Span.to_sec (Time.to_span_since_epoch (to_time ~calibrator (now ())))
      in
      (* printf "%d %.9f\n%!" i (cur -. !last); *)
      if Float.(<) (cur -. !last) 0.
      then failwithf "Time is not monotonic (diff %.12f)" (cur -. !last) ();
      last := cur;
      if i mod 100_000 = 0
      then Calibrator.calibrate ~t:calibrator ();
    done
  ;;

  module Samples = struct
    type t = (tsc * float) list [@@deriving sexp]

    let load file = Sexp.load_sexp_conv_exn file [%of_sexp: t]
  end

  (* The following tests check to see that the errors in presampled data are within
     acceptable bounds.  Errors are checked at two different sampling rates to simulate
     calls to [Calibrator.calibrate] at different rates to tests how errors accumulate in
     this module.*)

  let test_time_and_cycles samples_file ~error_limit ~alpha ~verbose =
    let samples = Samples.load samples_file in
    let init_samples, samples = List.split_n samples 3 in
    let calibrator = Calibrator.create () in
    let scale_us_abs t = Float.abs (t *. 1_000_000.) in
    Calibrator.initialize calibrator init_samples;
    let ewma_error = ref 0. in
    List.iter samples ~f:(fun (tsc, time) ->
      let cur_error =
        scale_us_abs
          (time -. Time.Span.to_sec
                     (Time.to_span_since_epoch (to_time ~calibrator tsc)))
      in
      ewma_error := ewma ~alpha ~old:!ewma_error ~add:cur_error;
      if verbose then
        printf "%f %f %s %f\n%!" cur_error !ewma_error (Int63.to_string tsc) time;
      if Float.(>=) (Float.abs !ewma_error) error_limit
      then failwithf "Average error %fus (current error %fus) of estimated time is \
                      beyond acceptable limits of %fus."
             !ewma_error cur_error error_limit ();
      Calibrator.calibrate_using calibrator ~tsc ~time ~am_initializing:false)
  ;;

  (* For this test, the data sample consists of 2,000 samples of Time.t and TSC.t sampled
     randomly with intervals up to 1sec.

     The error ewma of absolute error is atmost 1.2us, and the actual error ranges between
     7.6us and -1.4us.  It is worth noting that the errors are usually in the range of 1us
     and only occassionaly spike to the max/min values mentioned.*)
  let%test_unit _ = test_time_and_cycles ~error_limit:3. ~alpha:0.1 ~verbose:false
                      "time_stamp_counter_samples_at_1sec.sexp"
  ;;
  (* For this test, the data sample consists of 600 samples of Time.t and TSC.t sampled
     randomly with intervals up to 1minute.

     Errors range between -8.5 and 7.6 us and ewma of absolute error goes upto about
     2.4us.  The errors in this case tend to oscillate between +/-5us. *)
  let%test_unit _ = test_time_and_cycles ~error_limit:3. ~alpha:0.1 ~verbose:false
                      "time_stamp_counter_samples_at_60sec.sexp"
  ;;

  (* Test error magnitude in pre-sampled data. *)
  let test_time_and_cycles_nanos samples_file ~error_limit ~alpha ~verbose =
    let samples = Samples.load samples_file in
    let init_samples, samples = List.split_n samples 3 in
    let calibrator = Calibrator.create () in
    let scale_us_abs t = Float.abs (t *. 0.001) in
    Calibrator.initialize calibrator init_samples;
    let ewma_error = ref 0. in
    List.iter samples ~f:(fun (tsc, time) ->
      let time_nanos = Float.int63_round_nearest_exn (time *. 1E9) in
      let cur_error =
        scale_us_abs (Int63.to_float (diff
                                        time_nanos
                                        (to_nanos_since_epoch ~calibrator tsc)))
      in
      ewma_error := ewma ~alpha ~old:!ewma_error ~add:cur_error;
      if verbose then printf "%f %f\n%!" cur_error !ewma_error;
      if Float.(>=) (Float.abs !ewma_error) error_limit
      then failwithf "Average error %fus (current error %fus) of estimated time is \
                      beyond acceptable limits of %fus."
             !ewma_error cur_error error_limit ();
      Calibrator.calibrate_using calibrator ~tsc ~time ~am_initializing:false)
  ;;

  (* Error profiles for the nanos tests are similar to those of the float cases above. *)
  let%test_unit _ =
    test_time_and_cycles_nanos ~error_limit:500. ~alpha:0.1 ~verbose:false
      "time_stamp_counter_samples_at_1sec.sexp"
  ;;
  let%test_unit _ =
    test_time_and_cycles_nanos ~error_limit:3. ~alpha:0.1 ~verbose:false
      "time_stamp_counter_samples_at_60sec.sexp"
  ;;

  let%test_unit _ =
    let calibrator = Calibrator.local in
    for x = 1 to 100_000 do
      let y =
        x
        |> Int63.of_int
        |> Span.of_ns ~calibrator
        |> Span.to_ns ~calibrator
        |> Int63.to_int_exn
      in
      (* Accept a difference of at most [nanos_per_cycle] because of the precision
         lost during float truncation.
         [trunc (x / nanos_per_cycle) * nanos_per_cycle]
      *)
      assert (abs (x - y) <= Float.to_int calibrator.nanos_per_cycle);
    done
  ;;

  let%test_unit _ =
    let calibrator = Calibrator.local in
    for x = 1 to 100_000 do
      let y =
        x
        |> Int63.of_int
        |> Span.to_ns ~calibrator
        |> Span.of_ns ~calibrator
        |> Int63.to_int_exn
      in
      (* Accept a difference of at most [1/nanos_per_cycle] because of the precision
         lost during float truncation.
         [trunc (x * nanos_per_cycle) / nanos_per_cycle]
      *)
      assert (abs (x - y) <= Float.to_int (1. /. calibrator.nanos_per_cycle));
    done
  ;;
  [%%endif]
end)
