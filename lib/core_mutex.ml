INCLUDE "config.mlh"

open Result.Export

include Mutex0

IFDEF MUTEX_TIMED_LOCK THEN
(* POSIX thread functions *)
external mutex_timedlock : Mutex.t -> float -> bool = "unix_mutex_timedlock"

let timedlock mtx time = mutex_timedlock mtx (Time.to_float time)

let timedlock = Ok timedlock

ELSE

let timedlock = Or_error.unimplemented "Mutex.timedlock"

ENDIF
