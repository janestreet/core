(** Send log messages via the Unix Syslog interface.

    Syslog is great for system daemons that log free-form human readable status messages
    or other debugging output, but not so great for archiving structured data.  Access to
    read Syslog's messages may also be restricted.  [syslogd]'s logs are also not
    necessarily kept forever.  For application level logging consider
    {!Core_extended.Std.Logger} instead. *)

open! Import

module Open_option : sig
  type t =
    | PID     (** Include PID with each message *)
    | CONS    (** Write directly to system console if there is an error
                  while sending to system logger *)
    | ODELAY  (** Delay opening of the connection until syslog is called *)
    | NDELAY  (** No delay opening connection to syslog daemon *)
    | NOWAIT  (** Do not wait for child processes while logging message *)
    | PERROR  (** Print to stderr as well *)
  [@@deriving sexp]
end

(** Types of messages *)
module Facility : sig
  type t =
    | KERN      (** Kernel messages *)
    | USER      (** Generic user-level message (default) *)
    | MAIL      (** Mail subsystem *)
    | DAEMON    (** System daemons without separate facility value *)
    | AUTH      (** Security/authorization messages (DEPRECATED, use AUTHPRIV) *)
    | SYSLOG    (** Messages generated internally by syslogd *)
    | LPR       (** Line printer subsystem *)
    | NEWS      (** USENET news subsystem *)
    | UUCP      (** UUCP subsystem *)
    | CRON      (** Clock daemon (cron and at) *)
    | AUTHPRIV  (** Security/authorization messages (private) *)
    | FTP       (** FTP daemon *)
    | LOCAL0
    | LOCAL1
    | LOCAL2
    | LOCAL3
    | LOCAL4
    | LOCAL5
    | LOCAL6
    | LOCAL7    (** LOCAL0-7 reserved for local use *)
  [@@deriving sexp]
end

module Level : sig
  type t =
    | EMERG                      (** System is unusable *)
    | ALERT                      (** Action must be taken immediately *)
    | CRIT                       (** Critical condition *)
    | ERR                        (** Error conditions *)
    | WARNING                    (** Warning conditions *)
    | NOTICE                     (** Normal, but significant, condition *)
    | INFO                       (** Informational message *)
    | DEBUG                      (** Debug-level message *)
  [@@deriving compare, enumerate, sexp]  (** [DEBUG] < [EMERG] *)

  include Stringable.S with type t := t
end

(** All levels in [allowed_levels] will be allowed, and additionally all ranging from
    [from_level] to [to_level] (inclusive). *)
val setlogmask
  :  ?allowed_levels : Level.t list  (** default is {!List.empty} *)
  -> ?from_level : Level.t           (** default is [DEBUG] *)
  -> ?to_level : Level.t             (** default is [EMERG] *)
  -> unit
  -> unit

(** {2 Logging functions} *)

(** [openlog ~id ~options ~facility ()] opens a connection to the system logger (possibly
    delayed) using prefixed identifier [id], [options], and [facility].

    WARNING: this function leaks the [id] argument, if provided.  There is no way around
    that if syslog is called in a multi-threaded environment!  Therefore it shouldn't be
    called too often.  What for, anyway?

    Calling [openlog] before [syslog] is optional.  If you forget, syslog will do it for
    you with the defaults. *)
val openlog
  :  ?id : string                   (** default is [Sys.argv.(0)] *)
  -> ?options : Open_option.t list  (** default is [[ODELAY]] *)
  -> ?facility : Facility.t         (** default is [USER] *)
  -> unit
  -> unit

(** [syslog ~facility ~level message] logs [message] using syslog with [facility] at
    [level]. *)
val syslog
  :  ?facility : Facility.t  (** default is [USER] *)
  -> ?level : Level.t        (** default is [INFO] *)
  -> string
  -> unit

(** [syslog_printf] acts like [syslog], but allows [printf]-style specification of the
    message. *)
val syslogf
  :  ?facility : Facility.t
  -> ?level : Level.t
  -> ('a, unit, string, unit) format4
  -> 'a

(** [closelog ()] closes the connection to the [syslog] daemon. *)
val closelog : unit -> unit
