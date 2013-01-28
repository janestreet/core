(** Timed events

    This implementation uses a priority queue (heap) for efficient
    adding of timed events.  A timer thread wakes up whenever an event
    is supposed to be run or when the set of watched events changes.
    The timer can handle more than a million scheduled events.
    Timed events can also be efficiently removed (in O(log(n))).

    This interface is thread-safe.
*)


(** Type of timers *)
type t

(** Type of events.  Returned when adding event handlers to the timer, and
    needed for removing them again. *)
type event

(** Type of intervals for repeated events *)
type interval = private
  (** No repetition of events *)
  | INone
  (** Regular repetition of events.  [span] is greater than zero.
      Note that this interval will be added to the wakeup time of
      the timer and hence not correspond to exact periods of time.
      This means that e.g. if the process is put to sleep, it will not
      experience repetitive calls for intermediate events. *)
  | INormal of Span.t
  (** Randomized repetition of events.  [span] is greater than zero.
      The float specifies the maximum ratio with which [span] will be
      multiplied and added to itself, and is in the range \[0.0, 1.0\]. *)
  | IRandom of Span.t * float

(** [create ?min_size ()] creates a new timer.  The minimum size of the
    heap used by the timer is [min_size].

    @return the timer.

    @param min_size default = 1000
*)
val create : ?min_size : int -> unit -> t

(** [size timer] returns the number of events in a timer's heap. Repeating events only
    count once. *)
val size : t -> int

(** [deactivate timer] deactives a timer.  No scheduled event
    will get executed after this function returns. *)
val deactivate : t -> unit

(** [add timer handler ?randomize ?interval span] @return a scheduled
    event.  [handler] will be executed [span] seconds at the earliest
    after this function returns in the timer's thread, and it gets
    its associated event as argument (useful for letting interval
    timers remove themselves) and the time at which the timer thread
    woke up.

    NOTE: the [handler] must not allow any exceptions to escape.  [span]
    must be greater than zero.  If the same handler is used in multiple
    timers, then the handler must be reentrant.  The handler must not
    block, and should return as quickly as possible, eventually passing
    off work to other threads if handling an event would take too long.

    An [interval] can be specified to keep rescheduling the event.
    [interval] can be randomized (e.g. for proteanism): the float
    specifies the maximum ratio with which [interval] will be multiplied
    and added to itself, and must be in the range \[0.0, 1.0\].

    @raise Failure if timer is deactivated.
    @raise Invalid_argument if [interval] is a time span <= 0.0.
    @raise Invalid_argument if maximum random ratio not within \[0.0, 1.0\].

    @param randomize default = none
    @param interval default = none
*)
val add :
  t ->
  (event -> Time.t -> unit) ->
  ?randomize : float ->
  ?interval : Span.t ->
  Span.t -> event

(** [add_abs timer handler ?randomize ?interval time] same as {!add}, but
    takes an absolute time [time] for scheduling the event rather than
    a span.  This prevents a time-induced race condition if there is
    a long time between the internal reading of the current time and
    the scheduling of the event, which would artificially delay event
    execution.  This function is also more efficient than {!add}. *)
val add_abs :
  t ->
  (event -> Time.t -> unit) ->
  ?randomize : float ->
  ?interval : Span.t ->
  Time.t -> event

(** [remove event] removes [event] from its associated timer.

    NOTE: there is no guarantee that the event will not happen anymore
    if this function returns.  The timer thread may be about to start
    the callback, which leads to an inevitable race condition here.
    Users should be aware of this situation and make sure to handle
    it correctly.

    @raise Failure if timer is deactivated.
*)
val remove : event -> unit

(** [reschedule event ?randomize ?interval span] reschedules [event]
    to start by time span [span] later than originally scheduled, and
    change its interval behaviour as described for {!Timer.add}.

    @raise Failure if timer is deactivated.
    @raise Invalid_argument if [interval] is a time span <= 0.0.
    @raise Invalid_argument if maximum random ratio not within \[0.0, 1.0\].
    @raise Failure if [event] was not already scheduled.
*)
val reschedule
  :  event
  -> ?randomize : float
  -> ?interval : Span.t
  -> Span.t
  -> unit

(** [get_timer event] @return timer associated with [event]. *)
val get_timer : event -> t

(** [get_event_time event] @return the time at which [event] is scheduled
    to be run. *)
val get_event_time : event -> Time.t

(** [get_event_interval event] @return event interval associated with
    [event]. *)
val get_event_interval : event -> interval

(** [is_activated timer] @return [true] iff timer is activated. *)
val is_activated : t -> bool
