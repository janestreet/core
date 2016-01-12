#import "config.mlh"

#if JSC_MUTEX_TIMED_LOCK

open Core_kernel.Std

include Mutex0

(* POSIX thread functions *)
external mutex_timedlock : Mutex.t -> float -> bool = "unix_mutex_timedlock"

let timedlock mtx time = mutex_timedlock mtx (Time.to_float time)

let timedlock = Ok timedlock

#else

include Mutex0

let timedlock = Or_error.unimplemented "Mutex.timedlock"

#endif
