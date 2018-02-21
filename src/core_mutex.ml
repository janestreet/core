[%%import "config.h"]

open! Import
open Import_time

[%%ifdef JSC_MUTEX_TIMED_LOCK]

include Mutex0

(* POSIX thread functions *)
external mutex_timedlock : Mutex.t -> float -> bool = "unix_mutex_timedlock"

let timedlock mtx time =
  mutex_timedlock mtx (Time.to_span_since_epoch time |> Time.Span.to_sec)
;;

let timedlock = Ok timedlock

[%%else]

include Mutex0

let timedlock = Or_error.unimplemented "Mutex.timedlock"

[%%endif]
