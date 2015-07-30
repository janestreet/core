module TSC = Time_stamp_counter

BENCH "Time.now" = Time.now ()
BENCH "Time_ns.now" = Time_ns.now ()
BENCH "TSC.Calibrator.calibrate" = TSC.Calibrator.calibrate ()
BENCH "TSC.now" = TSC.now ()

BENCH_FUN "TSC.to_time" =
  let c = TSC.now () in
  (fun () -> ignore (TSC.to_time c))

BENCH "TSC.to_time (TSC.now ())" = TSC.to_time (TSC.now ())

BENCH_FUN "TSC.to_time_ns" =
  let c = TSC.now () in
  (fun () -> ignore (TSC.to_time_ns c))

BENCH "TSC.to_time_ns(TSC.now ())" = TSC.to_time_ns (TSC.now ())
BENCH "id" = ()

BENCH_FUN "TSC.Span.of_ns" =
  let c = Core_kernel.Std.Int63.of_int_exn 123 in
  (fun () -> TSC.Span.of_ns c)

BENCH_FUN "TSC.Span.to_ns" =
  let c = Core_kernel.Std.Int63.of_int_exn 123 |> TSC.Span.of_ns in
  (fun () -> TSC.Span.to_ns c)
