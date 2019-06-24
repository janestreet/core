[%%import
  "config.h"]

open! Core
open! Import
open! Time_stamp_counter
open! Time_stamp_counter.Private

let%test_module _ =
  (module struct
    [%%ifdef
      JSC_ARCH_SIXTYFOUR]

    let%expect_test "time moves backwards" =
      (* even if the system time goes backwards, we don't acknowledge this change. *)
      let y2000 =
        Time.of_string "2000-01-01 00:00:00 UTC"
        |> Time.to_span_since_epoch
        |> Time.Span.to_sec
      in
      let one_hour = 3600. in
      let half_a_year = 3600. *. 24. *. 30. *. 6. in
      let tsc = ref (of_int63 (Int63.of_int 1000)) in
      let time = ref y2000 in
      let calibrator =
        Calibrator.Private.create_using ~tsc:!tsc ~time:!time ~samples:[]
      in
      let calibrate =
        Calibrator.Private.calibrate_using calibrator ~am_initializing:false
      in
      let rec tick_tock n =
        if Int.( = ) n 0
        then ()
        else (
          tsc := add !tsc (Span.Private.of_int63 (Int63.of_int 1000000000));
          time := !time +. 1.;
          calibrate ~tsc:!tsc ~time:!time;
          print_s [%sexp (to_time ~calibrator !tsc : Time.t)];
          tick_tock (n - 1))
      in
      tick_tock 10;
      [%expect
        {|
        (1999-12-31 19:00:00.000000-05:00)
        (1999-12-31 19:00:01.200000-05:00)
        (1999-12-31 19:00:02.400000-05:00)
        (1999-12-31 19:00:03.600000-05:00)
        (1999-12-31 19:00:04.800000-05:00)
        (1999-12-31 19:00:06.000000-05:00)
        (1999-12-31 19:00:07.000000-05:00)
        (1999-12-31 19:00:08.000000-05:00)
        (1999-12-31 19:00:09.000000-05:00)
        (1999-12-31 19:00:10.000000-05:00) |}];
      time := !time +. one_hour;
      tick_tock 15;
      [%expect
        {|
        (1999-12-31 19:00:11.000000-05:00)
        (1999-12-31 20:12:12.200000-05:00)
        (1999-12-31 20:41:19.808300-05:00)
        (1999-12-31 20:59:00.101091-05:00)
        (1999-12-31 21:09:43.515952-05:00)
        (1999-12-31 21:16:14.081568-05:00)
        (1999-12-31 21:20:11.286364-05:00)
        (1999-12-31 21:22:35.473120-05:00)
        (1999-12-31 21:24:03.241585-05:00)
        (1999-12-31 21:24:56.790625-05:00)
        (1999-12-31 21:25:29.584535-05:00)
        (1999-12-31 21:25:49.789822-05:00)
        (1999-12-31 21:26:02.359724-05:00)
        (1999-12-31 21:26:10.298530-05:00)
        (1999-12-31 21:26:15.428435-05:00) |}];
      time := !time -. 30.;
      (* moving time backwards does not violate monotonicity *)
      tick_tock 10;
      [%expect
        {|
        (1999-12-31 21:26:18.854655-05:00)
        (1999-12-31 21:26:22.280875-05:00)
        (1999-12-31 21:26:24.673758-05:00)
        (1999-12-31 21:26:26.439890-05:00)
        (1999-12-31 21:26:27.825879-05:00)
        (1999-12-31 21:26:28.981299-05:00)
        (1999-12-31 21:26:29.996873-05:00)
        (1999-12-31 21:26:30.927624-05:00)
        (1999-12-31 21:26:31.806929-05:00)
        (1999-12-31 21:26:32.655030-05:00) |}];
      time := !time -. half_a_year;
      (* even if we attempt to move time backwards a lot *)
      tick_tock 10;
      [%expect
        {|
        (1999-12-31 21:26:33.484205-05:00)
        (1999-12-31 21:26:34.313380-05:00)
        (1999-12-31 21:26:35.131075-05:00)
        (1999-12-31 21:26:35.941808-05:00)
        (1999-12-31 21:26:36.748317-05:00)
        (1999-12-31 21:26:37.552266-05:00)
        (1999-12-31 21:26:38.354661-05:00)
        (1999-12-31 21:26:39.156113-05:00)
        (1999-12-31 21:26:39.956994-05:00)
        (1999-12-31 21:26:40.757529-05:00) |}]
    ;;

    (* monotonicity testing *)
    let%test_unit _ =
      let calibrator = Calibrator.create () in
      let last = ref 0. in
      for i = 1 to 10_000_000 do
        let cur =
          to_time ~calibrator (now ()) |> Time.to_span_since_epoch |> Time.Span.to_sec
        in
        (* printf "%d %.9f\n%!" i (cur -. !last); *)
        if Float.( < ) (cur -. !last) 0.
        then failwithf "Time is not monotonic (diff %.12f)" (cur -. !last) ();
        last := cur;
        if Int.( = ) (i mod 100_000) 0 then Calibrator.calibrate calibrator
      done
    ;;

    module Samples = struct
      type t = (Time_stamp_counter.t * float) list [@@deriving sexp]

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
      Calibrator.Private.initialize calibrator init_samples;
      let ewma_error = ref 0. in
      List.iter samples ~f:(fun (tsc, time) ->
        let cur_error =
          scale_us_abs
            (time
             -. Time.Span.to_sec (Time.to_span_since_epoch (to_time ~calibrator tsc)))
        in
        ewma_error := ewma ~alpha ~old:!ewma_error ~add:cur_error;
        if verbose
        then
          printf
            "%f %f %s %f\n%!"
            cur_error
            !ewma_error
            (Int63.to_string (to_int63 tsc))
            time;
        if Float.( >= ) (Float.abs !ewma_error) error_limit
        then
          failwithf
            "Average error %fus (current error %fus) of estimated time is beyond \
             acceptable limits of %fus."
            !ewma_error
            cur_error
            error_limit
            ();
        Calibrator.Private.calibrate_using calibrator ~tsc ~time ~am_initializing:false)
    ;;

    (* For this test, the data sample consists of 2,000 samples of Time.t and TSC.t sampled
       randomly with intervals up to 1sec.

       The error ewma of absolute error is atmost 1.2us, and the actual error ranges between
       7.6us and -1.4us.  It is worth noting that the errors are usually in the range of 1us
       and only occassionaly spike to the max/min values mentioned.*)
    let%test_unit _ =
      test_time_and_cycles
        ~error_limit:3.
        ~alpha:0.1
        ~verbose:false
        "time_stamp_counter_samples_at_1sec.sexp"
    ;;

    (* For this test, the data sample consists of 600 samples of Time.t and TSC.t sampled
       randomly with intervals up to 1minute.

       Errors range between -8.5 and 7.6 us and ewma of absolute error goes upto about
       2.4us.  The errors in this case tend to oscillate between +/-5us. *)
    let%test_unit _ =
      test_time_and_cycles
        ~error_limit:3.
        ~alpha:0.1
        ~verbose:false
        "time_stamp_counter_samples_at_60sec.sexp"
    ;;

    (* Test error magnitude in pre-sampled data. *)
    let test_time_and_cycles_nanos samples_file ~error_limit ~alpha ~verbose =
      let samples = Samples.load samples_file in
      let init_samples, samples = List.split_n samples 3 in
      let calibrator = Calibrator.create () in
      let scale_us_abs t = Float.abs (t *. 0.001) in
      Calibrator.Private.initialize calibrator init_samples;
      let ewma_error = ref 0. in
      List.iter samples ~f:(fun (tsc, time) ->
        let time_nanos = Float.int63_round_nearest_exn (time *. 1E9) in
        let cur_error =
          scale_us_abs
            (Int63.to_float
               (Span.Private.to_int63
                  (diff (of_int63 time_nanos) (to_nanos_since_epoch ~calibrator tsc))))
        in
        ewma_error := ewma ~alpha ~old:!ewma_error ~add:cur_error;
        if verbose then printf "%f %f\n%!" cur_error !ewma_error;
        if Float.( >= ) (Float.abs !ewma_error) error_limit
        then
          failwithf
            "Average error %fus (current error %fus) of estimated time is beyond \
             acceptable limits of %fus."
            !ewma_error
            cur_error
            error_limit
            ();
        Calibrator.Private.calibrate_using calibrator ~tsc ~time ~am_initializing:false)
    ;;

    (* Error profiles for the nanos tests are similar to those of the float cases above. *)
    let%test_unit _ =
      test_time_and_cycles_nanos
        ~error_limit:500.
        ~alpha:0.1
        ~verbose:false
        "time_stamp_counter_samples_at_1sec.sexp"
    ;;

    let%test_unit _ =
      test_time_and_cycles_nanos
        ~error_limit:3.
        ~alpha:0.1
        ~verbose:false
        "time_stamp_counter_samples_at_60sec.sexp"
    ;;

    let%test_unit _ =
      let calibrator = Calibrator.create () in
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
        assert (
          Int.(
            abs (x - y) <= Float.to_int (Calibrator.Private.nanos_per_cycle calibrator))
        )
      done
    ;;

    let%test_unit _ =
      let calibrator = Calibrator.create () in
      for x = 1 to 100_000 do
        let y =
          x
          |> Int63.of_int
          |> Span.Private.of_int63
          |> Span.to_ns ~calibrator
          |> Span.of_ns ~calibrator
          |> Span.Private.to_int63
          |> Int63.to_int_exn
        in
        (* Accept a difference of at most [1/nanos_per_cycle] because of the precision
           lost during float truncation.
           [trunc (x * nanos_per_cycle) / nanos_per_cycle]
        *)
        assert (
          Int.(
            abs (x - y)
            <= Float.to_int (1. /. Calibrator.Private.nanos_per_cycle calibrator)))
      done
    ;;

    [%%endif]
  end)
;;
