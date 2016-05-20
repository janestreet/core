(* A key idea behind this implementation is that it only deals with [Time_ns] and
   [Time_ns.Span] values that are multiples of a microsecond.  This follows from the fact
   that every [Time] or [Time.Span] is converted to [Time_ns] or [Time_ns.Span] using
   [Time_ns.of_time] or [Time_ns.Span.of_span] before doing anything with it.  Because
   [Time_ns.Span.of_span] and [Time_ns.of_time] round to the nearest microsecond, all
   [Time_ns] and [Time_ns.Span] values in this implementation will be a multiple of one
   microsecond.  Because of this, they satisfy:

   - [Time_ns.of_time      (Time_ns.to_time      time_ns)      = time_ns]
   - [Time_ns.Span.of_span (Time_ns.Span.to_span time_ns_span) = time_ns_span]
*)

module Time_ns_in_this_directory = Time_ns
open Core_kernel.Std
module Time_ns = Time_ns_in_this_directory

module Time = Time (* for the .mli *)

module Interval_num   = Timing_wheel_ns.Interval_num
module Level_bits     = Timing_wheel_ns.Level_bits
module Priority_queue = Timing_wheel_ns.Priority_queue

let to_span = Time_ns.Span.to_span
let to_time = Time_ns.to_time

let to_time_option = function
  | None -> None
  | Some time -> Some (to_time time)
;;

type 'a t = 'a Timing_wheel_ns.t [@@deriving sexp_of]
type 'a t_now = 'a t [@@deriving sexp_of]
type 'a timing_wheel = 'a t

module Alarm = struct
  include Timing_wheel_ns.Alarm

  let at timing_wheel t = to_time (at timing_wheel t)
end

let nanoseconds_per_microsecond = Int63.of_int 1000

let invariant_span span =
  [%test_result: Int63.t] ~expect:Int63.zero
    (Int63.rem (Time_ns.Span.to_int63_ns span) nanoseconds_per_microsecond)
;;

let invariant_time time =
  [%test_result: Int63.t] ~expect:Int63.zero
    (Int63.rem (Time_ns.to_int63_ns_since_epoch time) nanoseconds_per_microsecond)
;;

let invariant invariant_a t =
  invariant_span (Timing_wheel_ns.alarm_precision t);
  invariant_time (Timing_wheel_ns.now t);
  invariant_time (Timing_wheel_ns.start t);
  Timing_wheel_ns.invariant invariant_a t;
  Timing_wheel_ns.iter t ~f:(fun alarm ->
    invariant_time (Timing_wheel_ns.Alarm.at t alarm));
;;

module Config = struct
  include Timing_wheel_ns.Config

  let create ?level_bits ~alarm_precision () =
    create ()
      ~alarm_precision:(alarm_precision |> Time_ns.Span.of_span)
      ?level_bits
  ;;

  let alarm_precision t = to_span (alarm_precision t)

  let durations t = List.map (durations t) ~f:to_span
end

let add t ~at a = Timing_wheel_ns.add t ~at:(Time_ns.of_time at) a

let add_at_interval_num = Timing_wheel_ns.add_at_interval_num

let advance_clock t ~to_ ~handle_fired =
  Timing_wheel_ns.advance_clock t ~to_:(Time_ns.of_time to_) ~handle_fired;
;;

let fire_past_alarms t ~handle_fired =
  Timing_wheel_ns.fire_past_alarms t ~handle_fired;
;;

let alarm_precision t = to_span (Timing_wheel_ns.alarm_precision t)

let alarm_upper_bound t = to_time (Timing_wheel_ns.alarm_upper_bound t)

let clear = Timing_wheel_ns.clear

let create ~config ~start = Timing_wheel_ns.create ~config ~start:(Time_ns.of_time start)

let interval_num t time = Timing_wheel_ns.interval_num t (Time_ns.of_time time)

let interval_num_start t n = to_time (Timing_wheel_ns.interval_num_start t n)

let interval_start t time =
  to_time (Timing_wheel_ns.interval_start t (Time_ns.of_time time))
;;

let is_empty = Timing_wheel_ns.is_empty

let iter = Timing_wheel_ns.iter

let length = Timing_wheel_ns.length

let mem = Timing_wheel_ns.mem

let max_alarm_time_in_min_interval t =
  to_time_option (Timing_wheel_ns.max_alarm_time_in_min_interval t)
;;

let max_alarm_time_in_min_interval_exn t =
  to_time (Timing_wheel_ns.max_alarm_time_in_min_interval_exn t)
;;

let min_alarm_interval_num = Timing_wheel_ns.min_alarm_interval_num

let min_alarm_interval_num_exn = Timing_wheel_ns.min_alarm_interval_num_exn

let next_alarm_fires_at t = to_time_option (Timing_wheel_ns.next_alarm_fires_at t)

let next_alarm_fires_at_exn t = to_time (Timing_wheel_ns.next_alarm_fires_at_exn t)

let now t = to_time (Timing_wheel_ns.now t)

let now_interval_num = Timing_wheel_ns.now_interval_num

let remove = Timing_wheel_ns.remove

let reschedule t alarm ~at = Timing_wheel_ns.reschedule t alarm ~at:(Time_ns.of_time at)

let reschedule_at_interval_num = Timing_wheel_ns.reschedule_at_interval_num

let start t = to_time (Timing_wheel_ns.start t)

(* Here is a proof that [interval_num] is the inverse of [interval_num_start], i.e.:

   {[
     interval_num t (interval_num_start t n) = n
   ]}

   Expanding the definitions, we have that:

   {[
     interval_num t (interval_num_start t n)
     = Timing_wheel.interval_num t
         (Time_ns.of_time (to_time (Timing_wheel.interval_num_start t n)))
   ]}

   Because [start t] and [alarm_precision t] are multiples of one microsecond,
   [Timing_wheel.interval_num_start] returns a time that is a multiple of one microsecond;
   hence [Time_ns.of_time] is the inverse of [to_time].  Hence, the above is equal
   to:

   {[
     Timing_wheel.interval_num t (Timing_wheel.interval_num_start t n)
   ]}

   But this is [n], by the analogous inverse property on [Timing_wheel_ns].
*)
