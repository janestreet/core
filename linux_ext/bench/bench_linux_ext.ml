open! Core
open! Linux_ext

(* We expect set_after to allocate nothing. *)
let%bench_fun "Linux_ext.Timerfd.set_after" =
  match Timerfd.create with
  | Error _ -> ignore
  | Ok create ->
    let t = create Timerfd.Clock.realtime in
    fun () -> Timerfd.set_after t Time_ns.Span.second
;;
