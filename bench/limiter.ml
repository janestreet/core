open Core.Std

module Bench = Core_bench.Std.Bench
module Test  = Bench.Test

module Token_bucket = struct
  module M =  Core.Limiter.Token_bucket

  let always_limited =
    let t = M.create_exn
              ~now:(Time.now ())
              ~burst_size:1.
              ~sustained_rate_per_sec:1.
              ()
    in
    let now = ref (Time.now ()) in
    fun () ->
      M.try_take ~now:!now t 1.0
  ;;

  let never_limited =
    let now = ref (Time.now ()) in
    let t = M.create_exn
              ~now:!now
              ~burst_size:1.
              ~sustained_rate_per_sec:1.
              ()
    in
    let second = Time.Span.of_sec 1.1 in
    (* this math costs 2W of allocation and ~9ns on a uid box circa 2015 *)
    fun () ->
      now := Time.add !now second;
      M.try_take ~now:!now t 1.0
end

let command =
  Bench.make_command [
    Test.create ~name:"Token_bucket.always_limited" Token_bucket.always_limited
  ; Test.create ~name:"Token_bucket.never_limited" Token_bucket.never_limited
  ]
;;

let () = Command.run command

