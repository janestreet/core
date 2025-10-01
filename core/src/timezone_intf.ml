open! Import
open! Std_internal
open! Int.Replace_polymorphic_compare
module String = Base.String

module type Extend_zone = sig @@ portable
  type t [@@deriving sexp_grammar]

  include%template Identifiable.S [@mode local] [@modality portable] with type t := t

  include Diffable.S_atomic with type t := t

  (** [find name] looks up a [t] by its name and returns it. This also accepts some
      aliases, including:

      - chi -> America/Chicago
      - nyc -> America/New_York
      - hkg -> Asia/Hong_Kong
      - ldn -> Europe/London
      - lon -> Europe/London
      - tyo -> Asia/Tokyo
      - syd -> Australia/Sydney *)
  val find : string -> t option

  val find_exn : string -> t

  (** [local] is the machine's local timezone, as determined from the [TZ] environment
      variable or the [/etc/localtime] file. It is computed from the state of the process
      environment and on-disk tzdata database at some unspecified moment prior to its
      first use, so its value may be unpredictable if that state changes during program
      operation. Arguably, changing the timezone of a running program is a problematic
      operation anyway -- most people write code assuming the clock doesn't suddenly jump
      several hours without warning.

      Note that any function using this timezone can throw an exception if the [TZ]
      environment variable is misconfigured or if the appropriate timezone files can't be
      found because of the way the box is configured. We don't sprinkle [_exn] all over
      all the names in this module because such misconfiguration is quite rare. *)
  val local : t Lazy.t

  (** Like {!local}, but a [Portable_lazy.t]. *)
  val local_portable : t Portable_lazy.t

  (** [initialized_zones ()] returns a sorted list of time zone names that have been
      loaded from disk thus far. *)
  val initialized_zones : unit -> (string * t) list

  (** {3 Low-level functions}

      The functions below are lower level and should be used more rarely. *)

  (** [init ()] pre-load all available time zones from disk, this function has no effect
      if it is called multiple times. Time zones will otherwise be loaded at need from the
      disk on the first call to find/find_exn. *)
  val init : unit -> unit
end

module type Timezone = sig @@ portable
  (** Timezone handles parsing timezone data and create [Timezone.t] that can later be
      used to manipulate time using the [Time_float] and [Time_ns] modules.

      Timezone is currently only able to read the Timezone Database provided by
      {{:https://www.iana.org/time-zones} IANA}. It should work out of the box on Linux
      and macOS.

      {2 Where are the timezone data located ?}

      The location of the timezone files can be set using the environment variable
      [TZDIR]. If not set, [Timezone] will fallback to [/usr/share/zoneinfo/].

      {2 What is the local timezone ?}

      The local timezone can be set using the environment variable [TZ]. If not set,
      [Timezone] will fallback to [/etc/localtime]. In a JavaScript context, we
      automatically set the environment variable [TZ] to
      [Intl.DateTimeFormat().resolvedOptions().timeZone].

      {2 Compatibility with JavaScript.}

      The [Timezone] library can be used when constructing JavaScript applications with
      {{:https://github.com/ocsigen/js_of_ocaml/} Js_of_ocaml}.

      In the past, web browsers needed to be provided with a copy of the timezone
      database, but this is no longer required. *)

  module type Extend_zone = Extend_zone

  include Zone.S with type t = Zone.t and type Index.t = Zone.Index.t
  include Extend_zone with type t := t

  module Stable : sig
    module V1 : sig
      type nonrec t = t
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , hash
        , sexp
        , sexp_grammar
        , stable_witness]

      include Stringable.S with type t := t
      include Diffable.S with type t := t and type Diff.t = Diff.t
    end

    include Zone.S_stable with type t := t
  end

  (**/**)

  (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
  module Private : sig
    module Zone_cache : sig
      type zone := t

      type t =
        { mutable full : bool
        ; basedir : string
        ; table : Zone.t Hashtbl.M(String).t
        }

      module The_one_and_only : sig
        type k

        val mutex : k Mutex.t
        val capsule : (t, k) Capsule.Data.t
      end

      val clear : unit -> unit
      val init : unit -> unit
      val find : string -> zone option
    end
  end
end
