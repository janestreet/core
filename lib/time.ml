module Zone = Zone
module Span = Span

module Ofday = struct
  include (Ofday : (module type of Ofday
                     with type t = Ofday.t
                     with module Zoned := Ofday.Zoned))

  module Zoned = struct
    include Ofday.Zoned

    let to_time t date = Time0.of_date_ofday (zone t) date (ofday t)
  end

  (* can't be defined in Ofday directly because it would create a circular reference *)
  let now () = snd (Time0.to_local_date_ofday (Time0.now ()))
end

include Time0

BENCH_MODULE "Time" = struct
  BENCH_FUN "Time.to_string" =
    let t = of_float 100000.99999999999 in
    (fun () -> ignore (to_string t))

  BENCH_FUN "Time.to_ofday" =
    let t = now () in
    (fun () -> ignore (to_ofday t Zone.local))

  BENCH "Time.now" = now ()
  BENCH "Time.Zone.find_office" = Zone.find_office `nyc

  let x = Float.of_string "1.1"
  BENCH "Time.Span.of_hr"  = Span.of_hr  x
  BENCH "Time.Span.of_min" = Span.of_min x
  BENCH "Time.Span.of_sec" = Span.of_sec x
  BENCH "Time.Span.of_ms"  = Span.of_ms  x
  BENCH "Time.Span.of_ns"  = Span.of_ns  x
end
