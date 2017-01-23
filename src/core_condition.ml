open! Import
open Import_time

include Condition

let phys_equal = Caml.(==)

let equal (t : t) t' = phys_equal t t'

external condition_timedwait
  : Condition.t -> Mutex.t -> float -> bool = "unix_condition_timedwait"

let timedwait cnd mtx time =
  condition_timedwait cnd mtx (Time.to_span_since_epoch time |> Time.Span.to_sec)
