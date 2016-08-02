open Core.Std

module Bench = Core_bench.Std.Bench
module Test  = Bench.Test

module Token_bucket = struct
  module M =  Core.Limiter.Token_bucket

  let always_limited =
    let t = M.create_exn
              ~now:(Time_ns.now ())
              ~burst_size:1
              ~sustained_rate_per_sec:1.
              ()
    in
    let now = ref (Time_ns.now ()) in
    fun () ->
      M.try_take ~now:!now t 1
  ;;

  let never_limited =
    let now = ref (Time_ns.now ()) in
    let t = M.create_exn
              ~now:!now
              ~burst_size:1
              ~sustained_rate_per_sec:1.
              ()
    in
    let second = Time_ns.Span.of_sec 1.1 in
    fun () ->
      now := Time_ns.add !now second;
      M.try_take ~now:!now t 1
end

let command =
  Bench.make_command [
    Test.create ~name:"Token_bucket.always_limited" Token_bucket.always_limited
  ; Test.create ~name:"Token_bucket.never_limited" Token_bucket.never_limited
  ]
;;

let () = Command.run command

