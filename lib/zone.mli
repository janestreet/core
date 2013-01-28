(*
  This library replicates and extends the functionality of the standard Unix time handling
  functions (currently exposed in the Unix module, and indirectly through the Time
  module).

  Things you should know before delving into the mess of time...

  # Some general resources (summarized information also appears below):
    general overview   - http://www.twinsun.com/tz/tz-link.htm
    zone abbreviations - http://blogs.msdn.com/oldnewthing/archive/2008/03/07/8080060.aspx
    leap seconds       - http://en.wikipedia.org/wiki/Leap_second
    epoch time         - http://en.wikipedia.org/wiki/Unix_time
    UTC/GMT time       - http://www.apparent-wind.com/gmt-explained.html
    TAI time           - http://en.wikipedia.org/wiki/International_Atomic_Time
    Almost every possible time measurement -
      http://www.ucolick.org/~sla/leapsecs/timescales.html

  # Standards for measuring time

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

  # How does a system determine what time zone it is in?

  1. Check to see if the TZ environment variable is set.  If it is, it can be set to one
  of three forms, two of which are rarely, if ever used see:
    http://www.opengroup.org/onlinepubs/000095399/basedefs/xbd_chap08.html
  for more information on the obscure forms.  The common form represents a relative path
  from the base /usr/share/zoneinfo/posix, and is generally in the form of a continent or
  country name paired with a city name (Europe/London, America/New_York).  This is used to
  load the specified file from disk, which contains a time zone database in zic format
  (man tzfile).
  2. If TZ is not set, the system will try to read the file located at /etc/localtime,
  which must be a zic timezone database (and which is often just a symlink into
  /usr/share/zoneinfo/posix).
  3. If /etc/localtime cannot be found, then the system is assumed to be in GMT.

  It's worth noting that under this system there is no place on the system to go to get
  the name of the file you are using (/etc/localtime may not be a link, and may just be a
  copy, or it's own database not represented in /usr/share/zoneinfo).  Additionally, the
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

  # The problems with string time conversions

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

open Std_internal

exception Unknown_zone of string
exception Invalid_file_format of string

(* bin_io and sexp representations of Zone.t are the name of the zone, and not the full
   data that is read from disk when Zone.find is called.  The full Zone.t is reconstructed
   on the receiving/reading side by reloading the zone file from disk.  Any zone name that
   is accepted by find is acceptable in the bin_io and sexp representations. *)
type t
include Sexpable   with type t := t
include Binable    with type t := t
include Stringable with type t := t

(* User friendly functions *)

(** [find name] looks up a [t] by its name and returns it.  [find] and [find_exn] also
    support the following zone aliases:
    - "hkg" -> Asia/Hong_Kong
    - "lon" -> Europe/London
    - "ldn" -> Europe/London
    - "nyc" -> America/New_York
    - "tyo" -> Asia/Tokyo *)
val find : string -> t option

(** [find_office office] a more type-safe interface for pulling timezones related to
    existing Jane Street offices/locations. *)
val find_office : [ `chi | `hkg | `ldn | `nyc ] -> t
val find_exn : string -> t

(** [machine_zone ?refresh ()] returns the machines zone (t).  It does this by first
    looking for a value in the environment variable "TZ", and loading the named zone if
    it is set.  If "TZ" is not set it reads /etc/localtime directly.

    The first call to machine_zone is cached, so there is no need to cache it locally.
    The cache can be bypassed and refreshed by setting ~refresh to true. *)
val machine_zone : ?refresh:bool -> unit -> t

(** [of_utc_offset offset] returns a timezone with a static UTC offset (given in
    hours). *)
val of_utc_offset : int -> t

(** [utc] the UTC time zone.  Included for convenience *)
val utc : t

(** [abbreviation zone t] returns t abbreviation name such as EDT, EST, JST of given
    [zone] at the time [t].  This string conversion is one-way only, and cannot reliably
    be turned back into a t *)
val abbreviation : t -> float -> string

(** [name zone] returns the name of the time zone *)
val name : t -> string

(* End user friendly functions - functions below are low level and generally should not be
   called by clients. *)

(** [init ()] pre-load all available time zones from disk, this function has no effect if
    it is called multiple times.  Time zones will otherwise be loaded at need from the
    disk on the first call to find/find_exn. *)
val init : unit -> unit

(** [digest t] return the MD5 digest of the file the t was created from (if any) *)
val digest : t -> string option

(** [to_utc_offset] returns the UTC offset of timezone [t], in seconds *)
val to_utc_offset : t -> int

(** [initialized_zones ()] returns a sorted list of time zone names that have been loaded
    from disk thus far. *)
val initialized_zones : unit -> (string * t) list

(* [shift_epoch_time zone [`Local | `UTC] time] Takes an epoch (aka "unix") time given
   either in local or in UTC (as indicated in the arguments) and shifts it according to
   the local time regime in force in zone.  That is, given a Local epoch time it will
   return the corresponding UTC timestamp and vice versa.  This function is low level, and
   is not intended to be called by most client code.  Use the high level functions
   provided in Time instead. *)
val shift_epoch_time : t -> [`Local | `UTC] -> float -> float

val pp : Format.formatter -> t -> unit

