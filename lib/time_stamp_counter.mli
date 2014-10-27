(** High-performance timing.

    This module provides the fast function [now ()] which is our best effort
    high-performance cycle counter for a given platform.  For x86 systems this retrieves
    the CPU's internal time stamp counter using the RDTSC instruction.  For systems that
    do not have a RDTSC instruction, we fallback to using
    [clock_gettime(CLOCK_MONOTONIC)].

    Here is a benchmark of execution time in nanos and allocations in words:

    {v
      Name                            Time/Run   Minor
      ------------------------------- ---------- -------
      Time.now                           39.02    2.00
      TSC.now                             7.54
      TSC.to_time                         4.88    2.00
      TSC.to_time (TSC.now ())            8.54    2.00
      TSC.to_time_nanos                   4.49
      TSC.to_time_nanos(TSC.now ())       8.95
      Calibrator.calibrate                 279   34.00
    v}


    Type [t] is an [Int63.t] and consequently has no allocation overhead (on 64-bit
    machines), unlike [Time.now ()] which returns a boxed float.

    Functions are also provided to estimate the relationship of CPU time-stamp-counter
    frequency to real time, thereby allowing one to convert from [t] to [Time.t].  There
    are some caveats to this that are worth noting:

    - The conversion to [Time.t] depends on an estimate of the time-stamp-counter
    frequency.  This frequency may be volatile on some systems, thereby reducing the
    utility of this conversion.  See the [Calibrator] module below for details.

    - The captured [t] can only be converted to a [Time.t] if one also has a
    recently calibrated [Calibrator.t] from the same machine.

    See also: http://en.wikipedia.org/wiki/Time_Stamp_Counter
*)

INCLUDE "core_config.mlh"
open Std_internal

type t = private Core_int63.t with bin_io, compare, sexp

(** A calibrator contains a snapshot of machine-specific information that is used to
    convert between TSC values and clock time.  This information needs to be calibrated
    periodically such that it stays updated w.r.t. changes in the CPU's time-stamp-counter
    frequency, which can vary depending on load, heat etc.  (Also see the comment in the
    [.ml] file)

    Calibration at the rate of 0.1, 1 or 2 secs produces errors (measured as the
    difference between [Time.now] and the reported time here) on the order of 1-2us.
    Given the precision of 52bit float mantissa values, this is very close to least error
    one can have on these values.  Calibration once per 10sec produces errors that are
    +/-4us. Calibration once per minute produces errors that are +/-15us and calibration
    once in 3mins produces errors +/-30us.  (It is worth remarking that the error has a
    positive bias of 1us -- i.e. the error dances around the 1us mark, rather than around
    0. It is unclear where this bias is introduced, though it probably does not matter for
    most applications.)

    This module maintains an instance of [t] internal to the module.  The internal
    instance of [t] can be updated via calls to [calibrate ()], i.e. without specifying
    the [t] parameter.  In all the functions below that take an optional [Calibrator.t]
    argument, the internal instance is used when no calibrator is explicitly specified.
*)
module Calibrator : sig
  type t with bin_io, sexp

  (** [create ()] creates an uninitialized calibrator instance.  Creating a calibrator
      takes about 3ms.  One needs a recently calibrated [Calibrator.t] and the TSC value
      from the same machine to meaningfully convert the TSC value to a [Time.t]. *)
  val create : unit -> t

  (** [calibrate ~t] updates [t] by measuring the current value of the TSC and
      [Time.now]. *)
  val calibrate : ?t:t -> unit -> unit

  (** Returns the estimated MHz of the CPU's time-stamp-counter based on the TSC and
      [Time.now ()].  This function is undefined on 32bit machines. *)
  val cpu_mhz : (?t:t -> unit -> float) Or_error.t
end

(** [Span] indicates some integer number of cycles. *)
module Span : sig
  type t = private Core_int63.t with bin_io, sexp

  include Comparable with type t := t
  include Intable    with type t := t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t

  val to_time_span : ?calibrator:Calibrator.t -> t -> Time.Span.t
  val to_ns        : ?calibrator:Calibrator.t -> t -> Core_int63.t
  val of_ns        : ?calibrator:Calibrator.t -> Core_int63.t -> t
end

IFDEF ARCH_SIXTYFOUR THEN
external now : unit -> t = "tsc_get" "noalloc"
ELSE
external now : unit -> t = "tsc_get"
ENDIF

val diff            : t -> t -> Span.t
val add             : t -> Span.t -> t

(** [to_int63 t] returns the TSC value represented by [t] as an [Int63.t]. *)
val to_int63 : t -> Core_int63.t

(** [to_time t] converts a [t] to a [Time.t].  It is guaranteed that repeated calls
    of [to_time ()] will return nondecreasing [Time.t] values. *)
val to_time : ?calibrator:Calibrator.t -> t -> Time.t

(** [to_time_ns t] converts a [t] to an integer number of nanos since the epoch. *)
val to_time_ns : ?calibrator:Calibrator.t -> t -> Time_ns.t


