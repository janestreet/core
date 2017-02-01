open Core

let ok_exn = Or_error.ok_exn

let () = Sexp.of_int_style := `Underscores

let user_plus_sys () =
  let { Unix.tms_utime; tms_stime; _ } = Unix.times () in
  sec (tms_utime +. tms_stime)
;;

let log message a sexp_of_a =
  eprintf "%s\n%!" (Sexp.to_string_hum (Info.sexp_of_t (Info.create message a sexp_of_a)));
;;

module Timing_wheel = Timing_wheel_float

module Alarm_precision = Timing_wheel.Alarm_precision

module Q = Timing_wheel.Priority_queue

module Gc = Core.Gc

module Report = struct
  type t =
    { num_queue_elements : int;
      num_steps : int;
      user_plus_sys : Time.Span.t;
      nanoseconds_per_step : int;
      gc_stat : Gc.Stat.t;
    }
  [@@deriving sexp_of]
end

let test ~num_queue_elements ~num_steps =
  let q = Q.create ~level_bits:(Timing_wheel.Level_bits.create_exn [16]) () in
  let user_plus_sys_at_start = user_plus_sys () in
  for key = 1 to num_steps do
    ignore (Q.add q ~key:(Q.Key.of_int key) () : _ Q.Elt.t);
    if key > num_queue_elements then
      Q.increase_min_allowed_key q ~key:(Q.Key.of_int (key - num_queue_elements))
        ~handle_removed:ignore;
  done;
  let user_plus_sys = Time.Span.(-) (user_plus_sys ()) user_plus_sys_at_start in
  let report =
    { Report.
      num_queue_elements;
      num_steps;
      user_plus_sys;
      nanoseconds_per_step =
        Float.iround_nearest_exn
          (Time.Span.to_ns user_plus_sys /. Float.of_int num_steps);
      gc_stat = Gc.stat ();
    }
  in
  log "report" report [%sexp_of: Report.t]
;;

(* Same as [test] but uses [Timing_wheel] instead of [Timing_wheel.Priority_queue]. This
   way all times are newly allocated floats. *)
let test_with_allocations ~num_queue_elements ~num_steps =
  let tw =
    Timing_wheel.create
      ~config:(Timing_wheel.Config.create ()
                 ~alarm_precision:Alarm_precision.about_one_millisecond)
      ~start:Time.epoch
  in
  let user_plus_sys_at_start = user_plus_sys () in
  let q : unit Timing_wheel.Alarm.t Queue.t = Queue.create () in
  for key = 1 to num_steps do
    Queue.enqueue q (Timing_wheel.add tw ()
                       ~at:(Time.of_span_since_epoch (Time.Span.of_sec (float key /. float num_steps))));
    if key > num_queue_elements then
      Timing_wheel.advance_clock tw
        ~to_:(Time.of_span_since_epoch (Time.Span.of_sec
                                          (float (key - num_queue_elements) /. float num_steps)))
        ~handle_fired:(fun _ -> ignore (Queue.dequeue_exn q));
  done;
  let user_plus_sys = Time.Span.(-) (user_plus_sys ()) user_plus_sys_at_start in
  let report =
    { Report.
      num_queue_elements;
      num_steps;
      user_plus_sys;
      nanoseconds_per_step =
        Float.iround_nearest_exn
          (Time.Span.to_ns user_plus_sys /. Float.of_int num_steps);
      gc_stat = Gc.stat ();
    }
  in
  log "report" report [%sexp_of: Report.t]
;;

let () = ignore (test, test_with_allocations)

let () =
  test ~num_queue_elements:2_048 ~num_steps:40_000_000;
;;
