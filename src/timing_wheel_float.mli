(** A timing wheel in which time is represented by [Time.t], i.e., by a floating-point
    number of seconds since the epoch.  This is a wrapper around [Timing_wheel_ns], which
    is preferable both because it has better performance and because it avoids issues
    having to do with floating point (im)precision.

    The implementation uses [Timing_wheel_ns] and rounds all [Time.t] values to the
    nearest microsecond before feeding them to the underlying timing wheel.  Thus, it is
    possible that [Time.( < ) (Alarm.at t (add t ~at a)) at], with the alarm's [at] being
    up to a microsecond earlier than requested.  However, because the alarm precision of
    the underlying timing wheel is an integer number of microseconds, we are still
    guaranteed when an alarm fires that [at < now t], i.e., the timing-wheel's clock is
    beyond the time requested for the alarm to fire.

    Due to floating-point arithmetic, the guarantee that alarms don't fire too late needs
    to be weakened slightly to use [ >= ] rather than [ > ].  For all alarms [a] in [t]:

    {[
      Alarm.at a >= now t - alarm_precision t
    ]}
*)

open Import_time

include Core_kernel.Timing_wheel_ns_intf.Timing_wheel with module Time = Time (** @open *)
