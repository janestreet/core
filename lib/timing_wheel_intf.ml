(** A specialized priority queue for a set of time-based alarms.

    A timing wheel is a data structure that maintains a clock with the current time and a
    set of alarms scheduled to fire in the future.  One can add and remove alarms, and
    advance the clock to cause alarms to fire.  There is nothing asynchronous about a
    timing wheel.  Alarms only fire in response to an [advance_clock] call.

    When one [create]s a timing wheel, one supplies an initial time, [start], and an
    [alarm_precision].  The timing wheel breaks all time from [start] onwards into
    half-open intervals of size [alarm_precision], with the bottom half of each interval
    closed, and the top half open.  Alarms in the same interval fire in the same call to
    [advance_clock], as soon as [now t] is greater than all the times in the interval.
    When an alarm [a] fires on a timing wheel [t], the implementation guarantees that:

    {[
      Alarm.at a < now t
    ]}

    That is, alarms never fire early.  Furthermore, the implementation guarantees that
    alarms don't go off too late.  More precisely, for all alarms [a] in [t]:

    {[
      interval_start t (Alarm.at a) >= interval_start t (now t)
    ]}

    This implies that for all alarms [a] in [t]:

    {[
      Alarm.at a >= now t - alarm_precision t
    ]}

    One would like to have the strict inequality, [Alarm.at a > now t - alarm_precision
    t], but that does not hold due to floating-point imprecision.

    Of course, an [advance_clock] call can advance the clock to an arbitrary time in the
    future, and thus alarms may fire at a clock time arbitrarily far beyond the time for
    which they were set.  But the implementation has no control over the times supplied to
    [advance_clock]; it can only guarantee that alarms will fire when [advance_clock] is
    called with a time at least [alarm_precision] greater than their scheduled time.

    {1 Implementation}
    ==================
    A timing wheel is implemented using a specialized priority queue in which the
    half-open intervals from [start] onwards are numbered 0, 1, 2, etc.  Each time is
    stored in the priority queue with the key of its interval number.  Thus all alarms
    with a time in the same interval get the same key, and hence fire at the same time.
    More specifically, an alarm is fired when the clock reaches or passes the time at the
    start of the next interval.

    The priority queue is implemented with an array of levels of decreasing precision,
    with the lowest level having the most precision and storing the closest upcoming
    alarms, while the highest level has the least precision and stores the alarms farthest
    in the future.  As time increases, the timing wheel does a lazy radix sort of the
    alarm keys.

    This implementation makes [add_alarm] and [remove_alarm] constant time, while
    [advance_clock] takes time proportional to the amount of time the clock is advanced.
    With a sufficient number of alarms, this is more efficient than a log(N) heap
    implementation of a priority queue.

    {1 Representable times}
    =======================
    A timing wheel [t] can only handle a (typically large) bounded range of times as
    determined by the current time, [now t], and the [level_bits] and [alarm_precision]
    arguments supplied to [create].  Various functions raise if they are supplied a time
    smaller than [now t] or [>= alarm_upper_bound t].  This situation likely indicates a
    misconfiguration of the [level_bits] and/or [alarm_precision].  Here are some examples
    of the duration [alarm_upper_bound t - now t] for 32-bit and 64-bit machines using the
    default [level_bits].

    {v
      | word size | # intervals | alarm_precision | duration |
      |-----------+-------------+-----------------+----------|
      |        32 |        2^29 | millisecond     | 7 days   |
      |        64 |        2^61 | nanosecond      | 73 years |
    v}
*)

module type S = sig
  type 'a t with sexp_of

  type 'a timing_wheel = 'a t

  module Alarm : sig
    type 'a t with sexp_of

    (** [null ()] returns an alarm [t] such that [not (mem timing_wheel t)] for all
        [timing_wheel]s. *)
    val null : unit -> _ t

    (** All [Alarm] functions will raise if [not (Timing_wheel.mem timing_wheel t)]. *)
    val at    : 'a timing_wheel -> 'a t -> Time.t
    val key   : 'a timing_wheel -> 'a t -> int
    val value : 'a timing_wheel -> 'a t -> 'a
  end

  include Invariant.S1 with type 'a t := 'a t

  module Level_bits : sig
    (** The timing-wheel implementation uses an array of "levels", where level [i] is an
        array of length [2^b_i], where the [b_i] are the "level bits" specified via
        [Level_bits.create_exn [b_0, b_1; ...]].

        A timing wheel can handle approximately [2 ** num_bits t] intervals/keys beyond
        the current minimum time/key, where [num_bits t = b_0 + b_1 + ...].

        One can use a [Level_bits.t] to trade off run time and space usage of a timing
        wheel.  For a fixed [num_bits], as the number of levels increases, the length of
        the levels decreases and the timing wheel uses less space, but the constant factor
        for the running time of [add] and [increase_min_allowed_key] increases. *)
    type t with sexp

    include Invariant.S with type t := t

    (* [max_num_bits] is how many bits in an integer key the timing wheel can use, i.e.
       [Word_size.num_bits word_size - 3].  We subtract the 3 bits in the word that we
       won't use:

       - for the tag bit
       - for negative numbers
       - so we can do arithmetic around the bound without worrying about overflow *)
    val max_num_bits : int

    (** In [create_exn bits], it is an error if any of the [b_i] in [bits] has [b_i <= 0],
        or if the sum of the [b_i] in [bits] is greater than [max_num_bits]. *)
    val create_exn : int list -> t

    (** [default] returns the default value of [level_bits] used by [Timing_wheel.create]
        and [Timing_wheel.Priority_queue.create].  It varies based on the machine's word
        size.  Here are the the values and the amount of space used for the level arrays.

        | word | bits used | level_bits               | space used  |
        |------+-----------+--------------------------+-------------|
        |   32 |        29 | [10; 10; 9]              | < 4k words  |
        |   64 |        61 | [11; 10; 10; 10; 10; 10] | < 10k words |
    *)
    val default : Word_size.t -> t

    (** [num_bits t] is the sum of the [b_i] in [t]. *)
    val num_bits : t -> int

  end

  module Config : sig
    type t with sexp

    include Invariant.S with type t := t

    (** [create] raises if [alarm_precision <= 0]. *)
    val create
      :  ?alarm_precision:Time.Span.t
      -> ?level_bits:Level_bits.t
      -> unit
      -> t

    (** accessors *)
    val alarm_precision : t -> Time.Span.t
    val level_bits : t -> Level_bits.t

    (** [default] is [create ()]. *)
    val default : t

    (** [durations t] returns the durations of the levels in [t] *)
    val durations : t -> Time.Span.t list
  end

  (** [create ~config ~start] creates a new timing wheel with current time [start].

      For a fixed [level_bits], a smaller (i.e. more precise) [alarm_precision] decreases
      the representable range of times/keys and increases the constant factor for
      [advance_clock]. *)
  val create : config:Config.t -> start:Time.t -> 'a t

  (** Accessors *)
  val alarm_precision : _ t -> Time.Span.t
  val now             : _ t -> Time.t
  val start           : _ t -> Time.t

  (** One can think of a timing wheel as a set of alarms.  Here are various container
      functions along those lines. *)
  val is_empty : _ t -> bool
  val length : _ t -> int
  val iter : 'a t -> f:('a Alarm.t -> unit) -> unit

  (** [interval_num t time] returns the number of the interval that [time] is in, where
      [0] is the interval that starts at [start].  [interval_num] raises if [time] is too
      far in the past or future to represent.

      [now_interval_num t] equals [interval_num t (now t)]. *)
  val interval_num : _ t -> Time.t -> int
  val now_interval_num : _ t -> int

  (** [interval_num_start t n] is the start of the [n]'th interval in [t], i.e.:

      {[
        start t + n * alarm_precision t
      ]}

      [interval_start t time] is the start of the half-open interval containing [time],
      i.e.:

      {[
        interval_num_start t (interval_num t time)
      ]}

      [interval_start] raises in the same cases that [interval_num] does. *)
  val interval_num_start : _ t -> int    -> Time.t
  val interval_start     : _ t -> Time.t -> Time.t

  (** [advance_clock t ~to_ ~handle_fired] advances [t]'s clock to [to_].  It fires and
      removes all alarms [a] in [t] with [Time.(<) (Alarm.at a) (interval_start t to_)]
      applying [handle_fired] to each such [a].

      If [to_ <= now t], then [advance_clock] does nothing.

      [advance_clock] fails if [to_] is too far in the future to represent.

      Behavior is unspecified if [handle_fired] accesses [t] in any way other than
      [Alarm] functions. *)
  val advance_clock : 'a t -> to_:Time.t -> handle_fired:('a Alarm.t -> unit) -> unit

  (** [alarm_upper_bound t] returns the upper bound on an [at] that can be supplied to
      [add].  [alarm_upper_bound t] is not constant; its value increases as [now t]
      increases. *)
  val alarm_upper_bound : _ t -> Time.t

  (** [add t ~at a] adds a new value [a] to [t] and returns an alarm that can later be
      supplied to [remove] the alarm from [t].  [add] raises if [interval_num t at <
      now_interval_num t || at >= alarm_upper_bound t].

      [add_at_interval_num t ~at a] is equivalent to [add t ~at:(interval_num_start t at)
      a]. *)
  val add                 : 'a t -> at:Time.t -> 'a -> 'a Alarm.t
  val add_at_interval_num : 'a t -> at:int    -> 'a -> 'a Alarm.t

  val mem : 'a t -> 'a Alarm.t -> bool

  (** [remove t alarm] removes [alarm] from [t].  [remove] raises if [not (mem t
      alarm)]. *)
  val remove : 'a t -> 'a Alarm.t -> unit

  (** [clear t] removes all alarms from [t]. *)
  val clear : _ t -> unit

  (** [next_alarm_fires_at t] returns the minimum time to which the clock can be advanced
      such that an alarm will fire, or [None] if [t] has no alarms.  If
      [next_alarm_fires_at t = Some next], then for the minimum alarm time [min] that
      occurs in [t], it is guaranteed that: [next - alarm_precision t <= min < next]. *)
  val next_alarm_fires_at : _ t -> Time.t option


  (***********************************************************************************)
  (* The rest of this interface is not intended to be used with Timing_wheel, but is a
     separate data structure used to implement Timing_wheel, and may find use
     elsewhere. *)

  (** Timing wheel is implemented as a priority queue in which the keys are
      non-negative integers corresponding to the intervals of time.  The priority queue is
      unlike a typical priority queue in that rather than having a "delete min" operation,
      it has a nondecreasing minimum allowed key, which corresponds to the current time,
      and an [increase_min_allowed_key] operation, which implements [advance_clock].
      [increase_min_allowed_key] as a side effect removes all elements from the timing
      wheel whose key is smaller than the new minimum, which implements firing the alarms
      whose time has expired.

      Adding elements to and removing elements from a timing wheel takes constant time,
      unlike a heap-based priority queue which takes log(N), where N is the number of
      elements in the heap.  [increase_min_allowed_key] takes time proportional to the
      amount of increase in the min-allowed key, as compared to log(N) for a heap.  It is
      these performance differences that motivate the existence of timing wheels and make
      them a good choice for maintaing a set of alarms.  With a timing wheel, one can
      support any number of alarms paying constant overhead per alarm, while paying a
      small constant overhead per unit of time passed.

      As the minimum allowed key increases, the timing wheel does a lazy radix sort of the
      element keys, with level 0 handling the least significant [b_0] bits in a key, and
      each subsequent level [i] handling the next most significant [b_i] bits.  The levels
      hold increasingly larger ranges of keys, where the union of all the levels can hold
      any key from [min_allowed_key t] to [max_allowed_key t].  When a key is added to the
      timing wheel, it is added at the lowest possible level that can store the key.  As
      the minimum allowed key increases, timing-wheel elements move down levels until they
      reach level 0, and then are eventually removed.  *)
  module Priority_queue : sig

    type 'a t with sexp_of

    type 'a priority_queue = 'a t

    module Elt : sig
      (** An [Elt.t] represents an element that was added to a timing wheel. *)
      type 'a t with sexp_of

      val invariant : 'a priority_queue -> 'a Invariant.t -> 'a t Invariant.t

      val key   : 'a priority_queue -> 'a t -> int
      val value : 'a priority_queue -> 'a t -> 'a
    end

    include Invariant.S1 with type 'a t := 'a t

    (** [create ?level_bits ()] creates a new empty timing wheel, [t], with [length t = 0]
        and [min_allowed_key t = 0]. *)
    val create : ?level_bits:Level_bits.t -> unit -> 'a t

    (** [length t] returns the number of elements in the timing wheel. *)
    val length : _ t -> int

    (** [is_empty t] is [length t = 0] *)
    val is_empty : _ t -> bool

    (** To avoid issues with arithmetic overflow, the implementation restricts keys to
        being between [0] and [max_representable_key], where:

        {[
          max_representable_key = 1 lsl Level_bits.max_num_bits - 1
        ]}

        This is different from [max_allowed_key t], which gives the maximum key that can
        currently be stored in [t].  The maximum allowed key is never larger than the
        maximum representable key, but may be smaller. *)
    val max_representable_key : int

    (** [min_allowed_key t] is the minimum key that can be stored in [t].  This only
        indicates the possibility; there need not be an element [elt] in [t] with [Elt.key
        elt = min_allowed_key t].  This is not the same as the "min_key" operation in a
        typical priority queue.

        [min_allowed_key t] can increase over time, via calls to
        [increase_min_allowed_key].  It is guaranteed that [min_allowed_key t <=
        max_representable_key]. *)
    val min_allowed_key : _ t -> int

    (** [max_allowed_key t] is the maximum allowed key that can be stored in [t].  As
        [min_allowed_key] increases, so does [max_allowed_key]; however it is not the case
        that [max_allowed_key t - min_allowed_key t] is a constant.  It is guaranteed that
        [max_allowed_key t >= min (max_representable_key, min_allowed_key t + 2^B - 1],
        where [B] is the sum of the b_i in [level_bits].  It is also guaranteed that
        [max_allowed_key t <= max_representable_key]. *)
    val max_allowed_key : _ t -> int

    (** [min_elt t] returns an element in [t] that has the minimum key, if [t] is
        nonempty.  [min_elt] takes time proportional to the size of the timing-wheel data
        structure in the worst case.  It is implemented via a linear search.

        [min_key t] returns the key of [min_elt t], if any. *)
    val min_elt : 'a t -> 'a Elt.t option
    val min_key :  _ t ->      int option

    (** [add t ~key value] adds a new value to [t] and returns an element that can later
        be supplied to [remove] the element from [t].  [add] raises if [key <
        min_allowed_key t || key > max_allowed_key t]. *)
    val add : 'a t -> key:int -> 'a -> 'a Elt.t

    (** [remove t elt] removes [elt] from [t].  It is an error if [elt] is not currently
        in [t], and this error may or may not be detected. *)
    val remove : 'a t -> 'a Elt.t -> unit

    (** [clear t] removes all elts from [t]. *)
    val clear : _ t -> unit

    val mem : 'a t -> 'a Elt.t -> bool

    (** [increase_min_allowed_key t ~key ~handle_removed] increases the minimum allowed
        key in [t] to [key], and removes all elements with keys less than [key], applying
        [handle_removed] to each element that is removed.  If [key <= min_allowed_key t],
        then [increase_min_allowed_key] does nothing.  Otherwise, if
        [increase_min_allowed_key] returns successfully, [min_allowed_key t = key].

        [increase_min_allowed_key] raises if [key > max_representable_key].

        [increase_min_allowed_key] takes time proportional to [key - min_allowed_key t],
        although possibly less time.

        Behavior is unspecified if [handle_removed] accesses [t] in any way other than
        [Elt] functions. *)
    val increase_min_allowed_key
      : 'a t -> key:int -> handle_removed:('a Elt.t -> unit) -> unit

    val iter : 'a t -> f:('a Elt.t -> unit) -> unit
  end

end

module type Timing_wheel = sig

  module type S = S

  include S

  module Debug (M : S) : sig
    include S

    val check_invariant : bool ref
    val show_messages : bool ref
  end
end
