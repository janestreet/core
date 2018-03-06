(** A module for representing absolute points in time, independent of time zone.

    Note that on 32bit architecture, most functions will raise when used on time
    outside the range [1901-12-13 20:45:52 - 2038-01-19 03:14:07].
*)

open! Import
open Core_kernel.Core_kernel_private

module type S = sig
  module Time0 : Time0_intf.S
  module Time  : Time_intf.S with module Time := Time0

  module Zone  : sig
    include module type of (struct include Time.Zone end)

    include Core_zone.Extend_zone with type t := t
  end

  module Ofday : sig
    include (module type of struct include Time.Ofday end)

    module Zoned : sig
      (** Sexps look like "(12:01 nyc)"

          Two [t]'s may or may not correspond to the same times depending on which date
          they're evaluated. *)
      type t [@@deriving bin_io, sexp, hash]

      include Pretty_printer.S    with type t := t
      include Stringable          with type t := t (** Strings look like "12:01 nyc" *)

      val create       : Time.Ofday.t -> Zone.t -> t
      val create_local : Time.Ofday.t ->           t

      val ofday : t -> Time.Ofday.t
      val zone  : t -> Zone.t

      val to_time : t -> Date.t -> Time.t

      module With_nonchronological_compare : sig
        (** It is possible to consistently compare [t]'s, but due to the complexities of
            time zones and daylight savings, the resulting ordering is not chronological.
            That is, [compare t1 t2 > 0] does not imply [t2] occurs after [t1] every day,
            or any day. *)
        type nonrec t = t [@@deriving bin_io, sexp, compare, hash]
      end
    end

    val now : zone:Zone.t -> t
  end

  (** A fully qualified point in time, independent of timezone. *)
  type t = Time.t [@@deriving bin_io, compare, hash, sexp, typerep]

  include (module type of Time
            with type   t     := t
             and module Zone  := Zone
             and module Ofday := Ofday)

  (** Sexp conversions use the local timezone by default. This can be overridden by
      calling [set_sexp_zone].

      The [{to,of}_string] functions in [Time] will produce times with time zone
      indications, but are generous in what they will read in. String/Sexp.t
      representations without time zone indications are assumed to be in the machine's
      local zone. *)
  include Identifiable.S
    with type   t                           := t
     and type   comparator_witness          := comparator_witness
     and module Replace_polymorphic_compare := Replace_polymorphic_compare

  val get_sexp_zone : unit -> Zone.t
  val set_sexp_zone : Zone.t -> unit

  include Robustly_comparable with type t := t

  (** [of_tm] converts a [Unix.tm] (mirroring a [struct tm] from the C stdlib) into a
      [Time.t].  Note that the [tm_wday], [tm_yday], and [tm_isdst] fields are ignored. *)
  val of_tm : Core_unix.tm -> zone:Zone.t -> t

  val to_string_fix_proto : [`Utc | `Local] -> t -> string
  val of_string_fix_proto : [`Utc | `Local] -> string -> t

  (** [of_string_abs s] is like [of_string], but demands that [s] indicate the timezone
      the time is expressed in. *)
  val of_string_abs : string -> t

  (** [of_string_gen ~if_no_timezone s] attempts to parse [s] to a [t].  If [s] doesn't
      supply a time zone [if_no_timezone] is consulted. *)
  val of_string_gen
    :  if_no_timezone:[ `Fail | `Local | `Use_this_one of Zone.t ]
    -> string
    -> t

  (** [t_of_sexp_abs sexp] as [t_of_sexp], but demands that [sexp] indicate the timezone
      the time is expressed in. *)
  val t_of_sexp_abs : Sexp.t -> t
  val sexp_of_t_abs : t -> zone:Zone.t -> Sexp.t

  (** {6 Miscellaneous} *)

  (** [pause span] sleeps for span time. *)
  val pause : Span.t -> unit

  (** [interruptible_pause span] sleeps for span time unless interrupted (e.g. by delivery
      of a signal), in which case the remaining unslept portion of time is returned. *)
  val interruptible_pause : Span.t -> [`Ok | `Remaining of Span.t]

  (** [pause_forever] sleeps indefinitely. *)
  val pause_forever : unit -> never_returns

  (** [format t fmt] formats the given time according to fmt, which follows the formatting
      rules given in 'man strftime'.  The time is output in the given timezone.

      {v
      %Y - year (4 digits)
      %y - year (2 digits)
      %m - month
      %d - day
      %H - hour
      %M - minute
      %S - second
    v}

      a common choice would be: %Y-%m-%d %H:%M:%S
  *)
  val format : t -> string -> zone:Zone.t -> string

  (** [parse string ~fmt ~zone] parses [string], according to [fmt], which follows the
      formatting rules given in 'man strptime'.  The time is assumed to be in the given
      timezone.

      {v
      %Y - year (4 digits)
      %y - year (2 digits)
      %m - month
      %d - day
      %H - hour
      %M - minute
      %S - second
    v}
  *)
  val parse : string -> fmt:string -> zone:Zone.t -> t

  module Exposed_for_tests : sig
    val ensure_colon_in_offset : string -> string
  end
end

module type Core_time = sig
  module type S = S

  module Make
      (Time0 : Time0_intf.S)
      (Time  : Time_intf.S with module Time := Time0) : S with module Time0 := Time0
                                                           and module Time  := Time
end

(** {1 Notes on time}

    This library replicates and extends the functionality of the standard Unix time
    handling functions (currently exposed in the Unix module, and indirectly through the
    Time module).

    Things you should know before delving into the mess of time...

    {2 Some general resources (summarized information also appears below) }

    {v
    general overview   - http://www.twinsun.com/tz/tz-link.htm
    zone abbreviations - http://blogs.msdn.com/oldnewthing/archive/2008/03/07/8080060.aspx
    leap seconds       - http://en.wikipedia.org/wiki/Leap_second
    epoch time         - http://en.wikipedia.org/wiki/Unix_time
    UTC/GMT time       - http://www.apparent-wind.com/gmt-explained.html
    TAI time           - http://en.wikipedia.org/wiki/International_Atomic_Time
    Almost every possible time measurement -
      http://www.ucolick.org/~sla/leapsecs/timescales.html
  v}

    {2 Standards for measuring time }

    - Epoch time/Unix time/Posix time: Defined as the number of seconds that have passed
      since midnight, January 1st, 1970 GMT.  However, under epoch time, a day is always
      86,400 seconds long, and a minute never contains more than 60 total seconds.  In other
      words, epoch time does not take leap seconds into account properly.  What a POSIX
      compliant system does during a leap second depends on the way in which its clock is
      managed.  It either ignores it, replays the second, or causes a second to last longer
      than a second (retards the second).  The important thing to remember is that however
      the transition is managed, all days start on an evenly divisible multiple of 86,400.
    - GMT/Greenwich Mean Time/Greenwich Civil Time: The time based on the movement of the
      sun relative to the meridian through the Old Greenwich Observatory (0 degrees).  The
      movement of the sun in this case is a "mean" movement of the sun to adjust for slight
      eccentricities in the rotation of the earth, as well as for the effect of the tilt of
      the earth on the visible speed of the sun across the sky at different times of the
      year.  GMT is often used synonymously with the term UTC (see below), but may also be
      used to refer to the time system described here, which differs from UTC (as of 2009)
      by ~1 second.
    - Standard Time: The time based on the adjusted (as in GMT) movement of the sun over a
      point on the earth that is not Greenwich.  Colloquially, the time in a time zone
      without accounting for any form of daylight savings time.
    - Wall Clock Time: The time as it appears on a clock on the wall in a given time zone.
      Essentially this is standard time with DST adjustments.
    - TAI: International atomic time.  The time based on a weighted average of the time kept
      by roughly 300 atomic clocks worldwide.  TAI is written using the same format as
      normal solar (also called civil) times, but is not based on, or adjusted for the
      apparent solar time.  Thus, as of 2009 TAI appears to be ahead of most other time
      systems by ~34 seconds when written out in date/time form (2004-09-17T00:00:32 TAI is
      2004-09-17T00:00:00 UTC)
    - UTC/Universal Coordinated Time: Often taken as just another term for GMT, UTC is
      actually TAI adjusted with leap seconds to keep it in line with apparent solar time.
      Each UTC day is not an exact number of seconds long (unlike TAI or epoch time), and
      every second is exactly one real second long (unlike GMT, which is based entirely on
      the apparent motion of the sun, meaning that seconds under GMT slowly get longer as
      the earth's rotation slows down).  Leap seconds are determined by the rotation of
      the earth, which is carefully measured by the International Earth Rotation Service
      in Paris, France using a combination of satellite and lunar laser ranging, very
      long baseline interferometry, and Navstar Global Positioning System (GPS) stations.
      This isn't important for using UTC, but is very cool.  UTC is not well defined before
      about 1960.
    - Windows File Time: The number of 100-nanosecond intervals that have elapsed since
      12:00 A.M. January 1, 1601, UTC.  This is great because UTC has no meaning in 1601
      (being based on atomic timekeeping technologies that didn't exist then), and also
      because 1601 predates the development of even reasonably accurate clocks of any sort.
      The reasoning behind the Windows epoch time choice is that "The Gregorian calendar
      operates on a 400-year cycle, and 1601 is the first year of the cycle that was
      active at the time Windows NT was being designed. In other words, it was chosen to
      make the math come out nicely."
      (http://blogs.msdn.com/oldnewthing/archive/2009/03/06/9461176.aspx)
    - VBScript (this is my favorite):
      http://blogs.msdn.com/ericlippert/archive/2003/09/16/eric-s-complete-guide-to-vt-date.aspx

    All of these systems start to exhibit problems as you go further back in time, partly
    because truly accurate timekeeping didn't make an appearance until roughly 1958, and
    partly because different parts of the world didn't actually have well defined time zones
    for a long time.  If you go back far enough, you run into the switch between the Julian
    (old) and the Gregorian calendar, which happened at different times in history in
    different places in the world.

    {2 How does a system determine what time zone it is in? }

    + Check to see if the TZ environment variable is set.  If it is, it can be set to one
    of three forms, two of which are rarely, if ever used see:

    http://www.opengroup.org/onlinepubs/000095399/basedefs/xbd_chap08.html

    for more information on the obscure forms.  The common form represents a relative path
    from the base /usr/share/zoneinfo/posix, and is generally in the form of a continent
    or country name paired with a city name (Europe/London, America/New_York).  This is
    used to load the specified file from disk, which contains a time zone database in zic
    format (man tzfile).

    + If TZ is not set, the system will try to read the file located at /etc/localtime,
    which must be a zic timezone database (and which is often just a symlink into
    /usr/share/zoneinfo/posix).
    + If /etc/localtime cannot be found, then the system is assumed to be in GMT.

    It's worth noting that under this system there is no place on the system to go to get
    the name of the file you are using (/etc/localtime may not be a link, and may just be a
    copy, or its own database not represented in /usr/share/zoneinfo).  Additionally, the
    names of the files in the system zoneinfo database follow an internal standard, and
    there is no established standard for naming timezones.  So even if you were using one of
    these files, and you did know its name, you cannot assume that that name matches any
    timezone specified by any other system or description.

    One common misconception about time zones is that the standard time zone abbreviations
    can be used.  For instance, EST surely refers to Eastern Standard Time.  This is
    unfortunately not true - CST can refer to China Central Time, Central Standard Time, or
    Cuba Summer Time for instance - and time zone libraries that appear to correctly parse
    times that use time zone abbreviations do so by using a heuristic that usually assumes
    you mean a time in the US or Europe, in that order.  Time zones also sometimes use two
    different abbreviations depending on whether the time in question is in standard time,
    or daylight savings time.  These abbreviations are kept in the timezone databases, which
    is how programs like date manage to output meaningful abbreviations, it is only reading
    in times with abbreviations that is poorly specified.

    This library contains a function that attempts to make an accurate determination of the
    machine timezone by testing the md5 sum of the currently referenced timezone file
    against all of the possible candidates in the system database.  It additionally makes
    some adjustments to return the more common timezone names since some files in the
    database are duplicated under several names.  It returns an option because of the
    problems mentioned above.

    {2 The problems with string time conversions }

    There are two cases where string time conversions are problematic, both related to
    daylight savings time.

    In the case where time jumps forward one hour, there are possible representations of
    times that never happened 2006-04-02T02:30:00 in the eastern U.S. never happened for
    instance, because the clock jumped forward one hour directly from 2 to 3.  Unix time
    zone libraries asked to convert one of these times will generally produce the epoch time
    that represents the time 1/2 hour after 2 am, which when converted back to a string
    representation will be T03:30:00.

    The second case is when the clocks are set back one hour, which causes one hour of time
    to happen twice.  Converting a string in this range without further specification into
    an epoch time is indeterminate since it could be referring to either of two times.  Unix
    libraries handle this by either allowing you to pass in a dst flag to the conversion
    function to specify which time you mean, or by using a heuristic to guess which time you
    meant.

    The existence of both cases make a strong argument for serializing all times in UTC,
    which doesn't suffer from these issues.
*)
