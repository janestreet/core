(**
   Deprecated version of [Schedule], which defines a type for schedules like "every 5 min
   after the hour" or "every weekday at 3pm."
*)
(** {2 Overview}

    This version of Schedule has been superceded by V5.  In normal circumstances this
    would just be a version bump to the Stable type, but the newer version of Schedule
    has more expressive power and there are schedules that can't be expressed or
    serialized using the V4 serializer available in this deprecated module.  Programs
    are encouraged to upgrade as quickly as reasonably possible.

    A [Schedule.t] describes a (potentially repeating) schedule by selecting
    a subset of all seconds using the set operations in [t].  For example:

    - every 5 min after the hour : {v Mins [ 5 ] v}

    - 9am to 10am every day :
    {v Between (Time.Ofday.create ~hr:9 (), Time.Ofday.create ~hr:10 ()) v}

    - Every weekday at 3pm:
    {v And
        [ Weekdays [ Mon; Tue; Wed; Thu; Fri ]
        ; At [ Time.Ofday.create ~hr:15 () ] ] v}

    - On the 15th of every month at midnight:
    {v And
        [ Days [ 15 ]
        ; At [ Time.Ofday.start_of_day ] ] v}

    - 9:30 am on weekends, and 5 am on weekdays
    {v Or
        [ And
            [ Weekdays [ Sat; Sun ]
            ; At [ Time.Ofday.create ~hr:9 ~min:30 () ] ]
        ; And
            [ Weekdays [ Mon; Tue; Wed; Thu; Fri ]
            ; At [ Time.Ofday.create ~hr:5 () ] ]
        ] v}
*)

(** {2 Zones and Tags}

    On top of this selection language there are two labeling branches of the variant that
    are important.

    [In_zone (zone, t)] expresses that all of t should be evaluated relative to the time
    zone given.

    [Tag (tag, t)] tags anything matching t with [tag].

    Combining these we can express something complex like the on-call groups across three
    offices:

    {[
      let weekdays         = Weekdays Day_of_week.weekdays in
      let working_hours    = Between Time.Ofday.((create ~hr:8 (), create ~hr:18 ())) in
      let working_schedule = And [ weekdays; working_hours ] in
      let offices =
        let (!!) = Time.Zone.find_exn in
        Location.Abbrev.([
          tot, !!"America/New_York"
        ; hkg, !!"Asia/Hong_Kong"
        ; ldn, !!"Europe/London" ])
      in
      List.map offices ~f:(fun (office, zone) ->
        In_zone (zone, Tag (office, working_schedule)))
    ]}

    after which we can use the [tags] function to extract the groups on call at any moment.
*)

(** {2 Daylight Savings Time}

    Schedules are expressed in terms of wall clock time, and as such have interesting
    behavior around daylight savings time boundaries.  There are two circumstances that
    might affect a schedule.  The first is a repeated time, which occurs when time jumps
    back (e.g. 2:30 may happen twice in one day).  The second is a skipped time, which
    occurs when time jumps forward by an hour.

    In both cases [Schedule] does the naive thing.  If the time happens twice and is
    included in the schedule it is included twice.  If it never happens [Schedule] makes
    no special attempt to artificially include it.
*)

(** {2 Interface} *)

open! Import
open Import_time

(** these phantom types are concrete and exposed to help the compiler understand
    that zoned and unzoned cannot be the same type (which it could not know if they
    were abstract), which helps it infer the injectivity of the type [t] below.
*)
type zoned   = Zoned   [@@deriving compare]
type unzoned = Unzoned [@@deriving compare]

(**
   {ul {- [In_zone]: see the discussion under Zones and Tags above}
   {- [Tag]: see the discussion under Zones and Tags above}
   {- [And], [Or], [Not]: correspond to the set operations intersection, union, and
   complement.}
   {- [If_then_else (A, B, C)]: corresponds to (A && B) || (NOT A && C), useful for
   dealing with schedules that change during certain times of the year (holidays, etc.)}
   {- [At]: the exact times given on every day}
   {- [Shift]: shifts an entire schedule forward or backwards by a known span. For
   example:
   {ul {- [Shift ((sec 3.), Secs[10]) = Secs [13]]}
   {- [Shift ((sec (-3.)), Secs[10]) = Secs [7]]}}}
   {- [Between]: the contiguous range between the start and end times given on every day}
   {- [Secs]: the exact seconds given during every hour of every day}
   {- [Mins]: all seconds in the minutes given during every hour of every day}
   {- [Hours]: all seconds in the hours given on every day}
   {- [Weekdays]: all seconds in the days given on every week}
   {- [Days]: all seconds in the days given, every month}
   {- [Weeks]: all seconds in the weeks given (numbered ISO 8601), every year}
   {- [Months]: all seconds in the months given, every year}
   {- [On]: all seconds in the exact dates given}
   {- [Before]: all seconds before the given boundary (inclusive or exclusive)}
   {- [After]: all seconds after the given boundary (inclusive or exclusive)}
   {- [Always]: the set of all seconds}
   {- [Never]: the empty set}}

   ['a] indicates whether the schedule currently has an established zone.

   ['b] is the type of the tag used in this schedule. In many cases it can be
   unspecified.  See [tags] for more.

   [Between (a, b)] is the empty set if a > b.

   Items that take [int list]s silently ignore [int]s outside of the viable
   range. E.g. [Days [32]] will never occur. *)

module Inclusive_exclusive : sig
  type t =
    | Inclusive
    | Exclusive
  [@@deriving compare]
end

type ('a, 'b) t =
  | In_zone       : Time.Zone.t * (unzoned, 'b) t                     -> (zoned, 'b) t
  | Tag           : 'b * ('a, 'b) t                                   -> ('a, 'b) t
  | And           : ('a, 'b) t list                                   -> ('a, 'b) t
  | Or            : ('a, 'b) t list                                   -> ('a, 'b) t
  | Not           : ('a, 'b) t                                        -> ('a, 'b) t
  | If_then_else  : (('a, 'b) t * ('a, 'b) t * ('a, 'b) t)            -> ('a, 'b) t
  | Shift         : Time.Span.t * ('a, 'b) t                          -> ('a, 'b) t
  | Between       : (Inclusive_exclusive.t * Time.Ofday.t)
                    * (Inclusive_exclusive.t * Time.Ofday.t)        -> (unzoned, 'b) t
  | At            : Time.Ofday.t list                                 -> (unzoned, 'b) t
  | Secs          : int list                                          -> (unzoned, 'b) t
  | Mins          : int list                                          -> (unzoned, 'b) t
  | Hours         : int list                                          -> (unzoned, 'b) t
  | Weekdays      : Day_of_week.t list                                -> (unzoned, 'b) t
  | Days          : int list                                          -> (unzoned, 'b) t
  | Weeks         : int list                                          -> (unzoned, 'b) t
  | Months        : Month.t list                                      -> (unzoned, 'b) t
  | On            : Date.t list                                       -> (unzoned, 'b) t
  | Before        : (Inclusive_exclusive.t * (Date.t * Time.Ofday.t)) -> (unzoned, 'b) t
  | After         : (Inclusive_exclusive.t * (Date.t * Time.Ofday.t)) -> (unzoned, 'b) t
  | Always        : ('a, 'b) t
  | Never         : ('a, 'b) t
[@@deriving compare]

type 'b zoned_t = (zoned, 'b) t [@@deriving sexp_of]

(** Return a string suitable for debugging purposes. *)
val to_string_zoned : (zoned, 'b) t -> string_of_tag:('b -> string) -> string

module Stable : sig
  module V4 : sig
    include Stable1 with type 'b t = (zoned, 'b) t

    val flag
      :  ?flag_name:string
      -> ?default:'b t
      -> ?doc:string
      -> (module Sexpable.S with type t = 'b)
      -> unit
      -> 'b t Command.Spec.param
  end
end

(** [includes t time] is true if the second represented by [time] falls within the
    schedule [t]. *)
val includes : (zoned, 'b) t -> Time.t -> bool

(** [ tags t time = `Not_included ] iff [not (includes t time)]. Otherwise, [ tags t time
    = `Included lst ], where [lst] includes all tags of a schedule such that [includes t'
    time] is true where [t'] is a tagged branch of the schedule. E.g. for some [t] equal
    to [Tag some_tag t'], [tags t time] will return [some_tag] if and only if [includes t'
    time] returns true.  For a more interesting use case, consider the per-office on-call
    schedule example given in the beginning of this module. Note that a schdeule may have
    no tags, and therefore, [lst] can be empty.
*)
val tags
  :  (zoned, 'tag) t
  -> Time.t
  -> [ `Not_included | `Included of 'tag list ]

val all_tags
  :  (zoned, 'tag) t
  -> tag_comparator:('tag, 'cmp) Comparator.t
  -> ('tag, 'cmp) Set.t

(** [fold_tags t ~init ~f time] is nearly behaviorally equivalent to (but more efficient
    than) [List.fold ~init ~f (tags t time)], with the exception that it returns [None] if
    [includes t time] is false. It is important that [f] be pure, as its results may be
    discarded. *)
val fold_tags
  :  (zoned, 'tag) t
  -> init:'m
  -> f:('m -> 'tag -> 'm)
  -> Time.t
  -> 'm option

val map_tags
  :  ('a, 'b) t
  -> f:('b -> 'c)
  -> ('a, 'c) t

(** Return a sequence of schedule changes over time that will never end.

    If your schedules ends, you will continue to receive `No_change_until_at_least
    with increasing times forever.

    The return type indicates whether [includes t start_time] is true and
    delivers a sequence of subsequent changes over time.

    The times returned by the sequence are strictly increasing and never less
    than [start_time].  That is, [`No_change_until_at_least x] can never be
    followed by [`Enter x], only by (at least) [`Enter (x + 1s)].

    if [emit] is set to [Transitions_and_tag_changes] then all changes in tags
    will be present in the resulting sequence.  Otherwise only the tags in effect
    when a schedule is entered are available.

    The [`In_range | `Out_of_range] flag in [`No_change_until_at_least]
    indicates whether the covered range is entirely within, or outside of the
    time covered by the schedule and is only there to help with bookkeeping for
    the caller.  [`In_range | `Out_of_range] will never disagree with what
    could be inferred from the [`Enter] and [`Leave] events.

    The sequence takes care to do only a small amount of work between each
    element, so that pulling the next element of the sequence is always cheap.
    This is the primary motivation behind including [`No_change_until_at_least].

    The [Time.t] returned by `No_change_until_at_least is guaranteed to be a reasonable
    amount of time in the future (at least 1 hour).
*)

module Event : sig
  type no_change =
    [ `No_change_until_at_least of [ `In_range | `Out_of_range ] * Time.t
    ] [@@deriving sexp_of, compare]

  type 'tag transition =
    [ `Enter of Time.t * 'tag list
    | `Leave of Time.t
    ] [@@deriving sexp_of, compare]

  type 'tag tag_change =
    [ `Change_tags of Time.t * 'tag list
    ] [@@deriving sexp_of, compare]

  val to_time : [< no_change | 'tag transition | 'tag tag_change] -> Time.t
end

(** in [Transitions_and_tag_changes] equality for the tag type must be given *)
type ('tag, 'a) emit =
  | Transitions                 : ('tag, [ Event.no_change | 'tag Event.transition ]) emit
  | Transitions_and_tag_changes : ('tag -> 'tag -> bool)
    -> ('tag, [ Event.no_change | 'tag Event.transition | 'tag Event.tag_change]) emit

val to_endless_sequence
  :  (zoned, 'tag) t
  -> start_time:Time.t
  -> emit:(('tag, 'a) emit)
  -> [ `Started_in_range     of 'tag list * 'a Sequence.t
     | `Started_out_of_range of 'a Sequence.t ]

(** [next_enter_between t start end] The given [start] [end] range is inclusive on both
    ends.  This function is useful for one-off events during the run of a program.
    If you want to track changes to a schedule over time it is better to call
    [to_endless_sequence] *)
val next_enter_between
  :  (zoned, 'tag) t
  -> Time.t
  -> Time.t
  -> Time.t option

(** as [next_enter_between] but for leave events *)
val next_leave_between
  :  (zoned, 'tag) t
  -> Time.t
  -> Time.t
  -> Time.t option
