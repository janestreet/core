(* Several benchmarks for [Time_ns].

   These are not written inline in the [time_ns.ml] because they also try to test cost of
   assignment and hence rely on compiler optimization enabled by the [private int]
   declarations of [time_ns.mli].

┌──────────────────────────────────────────────────────────────────────────────┬────────────┬─────────┬──────────┬──────────┬────────────┐
│ Name                                                                         │   Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├──────────────────────────────────────────────────────────────────────────────┼────────────┼─────────┼──────────┼──────────┼────────────┤
│ [time_ns_benchmarks.ml] Time_ns.now                                          │    28.93ns │         │          │          │      1.96% │
│ [time_ns_benchmarks.ml] Time_ns.Ofday.local_now                              │    33.49ns │         │          │          │      2.27% │
│ [time_ns_benchmarks.ml] Time_ns.to_string                                    │   726.16ns │ 188.00w │          │          │     49.30% │
│ [time_ns_benchmarks.ml] Time_ns.to_ofday                                     │     7.30ns │         │          │          │      0.50% │
│ [time_ns_benchmarks.ml] Time_ns.to_int                                       │     1.90ns │         │          │          │      0.13% │
│ [time_ns_benchmarks.ml] Time_ns.of_int                                       │     1.91ns │         │          │          │      0.13% │
│ [time_ns_benchmarks.ml] Time_ns.Ofday.to_string                              │ 1_472.82ns │ 391.01w │    0.10w │    0.10w │    100.00% │
│ [time_ns_benchmarks.ml] Time_ns.Span.of_hr                                   │     4.15ns │   2.00w │          │          │      0.28% │
│ [time_ns_benchmarks.ml] Time_ns.Span.of_min                                  │     4.51ns │   2.00w │          │          │      0.31% │
│ [time_ns_benchmarks.ml] Time_ns.Span.of_sec                                  │     4.15ns │   2.00w │          │          │      0.28% │
│ [time_ns_benchmarks.ml] Time_ns.Span.of_ms                                   │     4.97ns │   2.00w │          │          │      0.34% │
│ [time_ns_benchmarks.ml] Time_ns.Span.of_int_sec                              │     2.70ns │         │          │          │      0.18% │
│ [time_ns_benchmarks.ml] Time_ns.Span.to_int_sec                              │     9.14ns │         │          │          │      0.62% │
│ [time_ns_benchmarks.ml] Time_ns.t assignment                                 │     2.18ns │         │          │          │      0.15% │
│ [time_ns_benchmarks.ml] Time.t assignment                                    │     7.23ns │         │          │          │            │
│ [time_ns_benchmarks.ml] Time_ns.of_time                                      │    11.86ns │   4.00w │          │          │            │
│ [time_ns_benchmarks.ml] Time_ns.of_time (Async.Std.Scheduler.cycle_start ()) │    16.45ns │   4.00w │          │          │            │
└──────────────────────────────────────────────────────────────────────────────┴────────────┴─────────┴──────────┴──────────┴────────────┘
*)
open Core_kernel.Std

BENCH "Time_ns.now" = Time_ns.now ()
BENCH "Time_ns.Ofday.local_now" = Time_ns.Ofday.local_now ()

BENCH_FUN "Time_ns.to_string" =
  let t = Time_ns.now () in
  (fun () -> Time_ns.to_string t)

BENCH_FUN "Time_ns.to_ofday" =
  let t = Time_ns.now () in
  (fun () -> Time_ns.Ofday.of_local_time t)

BENCH_FUN "Time_ns.to_int63_ns_since_epoch" =
  let t = Time_ns.now () in
  (fun () -> Time_ns.Span.to_int63_ns (Time_ns.to_span_since_epoch t))

BENCH_FUN "Time_ns.of_int63_ns_since_epoch" =
  let t = Int63.of_string "100000" in
  (fun () -> Time_ns.of_span_since_epoch (Time_ns.Span.of_int63_ns t))

BENCH_FUN "Time_ns.Ofday.to_string" =
  let t = Time_ns.Ofday.of_local_time (Time_ns.now ()) in
  (fun () -> Time_ns.Ofday.to_string t)


BENCH "Time_ns.Span.of_day"     = Time_ns.Span.of_day       0.1
BENCH "Time_ns.Span.of_hr"      = Time_ns.Span.of_hr        0.1
BENCH "Time_ns.Span.of_min"     = Time_ns.Span.of_min       0.1
BENCH "Time_ns.Span.of_sec"     = Time_ns.Span.of_sec       0.1
BENCH "Time_ns.Span.of_ms"      = Time_ns.Span.of_ms        0.1
BENCH "Time_ns.Span.of_int_sec" = Time_ns.Span.of_int_sec 10000

BENCH_FUN "Time_ns.Span.to_int_sec" =
  let t = Time_ns.to_span_since_epoch (Time_ns.now ()) in
  (fun () -> Time_ns.Span.to_int_sec t)

BENCH_FUN "Time_ns.t assignment" =
  let t = Time_ns.now () in
  let x : Time_ns.t ref = ref t in
  (fun () -> x := t)

BENCH_FUN "Time.t assignment" =
  let t = Time.now () in
  let x : Time.t ref = ref t in
  (fun () -> x := t)

BENCH_FUN "Time_ns.of_time" =
  let time = Time.now () in
  (fun () -> Time_ns.of_time time)
