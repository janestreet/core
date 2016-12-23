open! Import

module Zone = Zone
module Span = Span

module Ofday = struct
  include Core_kernel.Ofday
  module Zoned = Ofday_zoned

  let now ~zone = Time0.to_ofday ~zone (Time0.now ())
end

include Time0

let%bench_module "Time" = (module struct
  let%bench_fun "Time.to_string" =
    let t = of_float 100000.99999999999 in
    (fun () -> ignore (to_string t))

  let%bench_fun "Time.to_ofday" =
    let t = now () in
    (fun () -> ignore (to_ofday t ~zone:(Lazy.force Zone.local)))

  let%bench "Time.now" = now ()

  let x = Float.of_string "1.1"
  let%bench "Time.Span.of_hr"  = Span.of_hr  x
  let%bench "Time.Span.of_min" = Span.of_min x
  let%bench "Time.Span.of_sec" = Span.of_sec x
  let%bench "Time.Span.of_ms"  = Span.of_ms  x
  let%bench "Time.Span.of_ns"  = Span.of_ns  x
end)


module Stable = struct
  include Stable0
  module Span = Span.Stable
  module Ofday = struct
    include Ofday.Stable
    module Zoned = Ofday.Zoned.Stable
  end
end
