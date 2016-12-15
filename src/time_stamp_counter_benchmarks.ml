open! Import

module TSC = Time_stamp_counter

let%bench "Time.now" = Time.now ()
let%bench "Time_ns.now" = Time_ns.now ()
let%bench "TSC.Calibrator.calibrate" = TSC.Calibrator.calibrate ()
let%bench "TSC.now" = TSC.now ()

let%bench_fun "TSC.to_time" =
  let c = TSC.now () in
  (fun () -> ignore (TSC.to_time c))

let%bench "TSC.to_time (TSC.now ())" = TSC.to_time (TSC.now ())

let%bench_fun "TSC.to_time_ns" =
  let c = TSC.now () in
  (fun () -> ignore (TSC.to_time_ns c))

let%bench "TSC.to_time_ns(TSC.now ())" = TSC.to_time_ns (TSC.now ())
let%bench "id" = ()

let%bench_fun "TSC.Span.of_ns" =
  let c = Int63.of_int_exn 123 in
  (fun () -> TSC.Span.of_ns c)

let%bench_fun "TSC.Span.to_ns" =
  let c = Int63.of_int_exn 123 |> TSC.Span.of_ns in
  (fun () -> TSC.Span.to_ns c)
