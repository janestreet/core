open! Core
module TSC = Time_stamp_counter

let%bench "Time.now" = Time.now ()
let%bench "Time_ns.now" = Time_ns.now ()

let%bench_fun "TSC.Calibrator.calibrate" =
  let calibrator = force TSC.calibrator in
  fun () -> TSC.Calibrator.calibrate calibrator
;;

let%bench "TSC.now" = TSC.now ()

let%bench_fun "TSC.to_time" =
  let calibrator = force TSC.calibrator in
  let c = TSC.now () in
  fun () -> ignore (TSC.to_time c ~calibrator)
;;

let%bench_fun "TSC.to_time (TSC.now ())" =
  let calibrator = force TSC.calibrator in
  fun () -> TSC.to_time (TSC.now ()) ~calibrator
;;

let%bench_fun "TSC.to_time_ns" =
  let calibrator = force TSC.calibrator in
  let c = TSC.now () in
  fun () -> ignore (TSC.to_time_ns c ~calibrator)
;;

let%bench_fun "TSC.to_time_ns(TSC.now ())" =
  let calibrator = force TSC.calibrator in
  fun () -> TSC.to_time_ns (TSC.now ()) ~calibrator
;;

let%bench "id" = ()

let%bench_fun "TSC.Span.of_ns" =
  let calibrator = force TSC.calibrator in
  let c = Int63.of_int_exn 123 in
  fun () -> TSC.Span.of_ns c ~calibrator
;;

let%bench_fun "TSC.Span.to_ns" =
  let calibrator = force TSC.calibrator in
  let c = Int63.of_int_exn 123 |> TSC.Span.of_ns ~calibrator in
  fun () -> TSC.Span.to_ns c ~calibrator
;;
