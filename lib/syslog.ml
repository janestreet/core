open Format
open Sexplib.Std

module Open_option = struct
  type t =
    (* THESE MUST STAY IN THE SAME ORDER AS IN syslog_stubs.c!!! *)
    | PID | CONS | ODELAY | NDELAY | NOWAIT | PERROR
  with sexp
  external to_int : t -> int = "core_syslog_open_option_to_int"
  let collect_mask i t = to_int t lor i
  let mask ts = Core_list.fold ~f:collect_mask ~init:0 ts
end

module Facility = struct
  module T = struct
    type t =
      (* THESE MUST STAY IN THE SAME ORDER AS IN syslog_stubs.c!!! *)
      | KERN | USER | MAIL | DAEMON | AUTH | SYSLOG | LPR | NEWS
      | UUCP | CRON | AUTHPRIV | FTP
      | LOCAL0
      | LOCAL1
      | LOCAL2
      | LOCAL3
      | LOCAL4
      | LOCAL5
      | LOCAL6
      | LOCAL7
    with sexp
    external to_int : t -> int = "core_syslog_facility_to_int"
  end
  include T

  include Sexpable.To_stringable (T)
  TEST_UNIT = <:test_result< string >> ~expect:"KERN" (to_string KERN)
  TEST_UNIT = <:test_result< t >> ~expect:LOCAL7 (of_string "LOCAL7")
end

module Level = struct
  module T = struct
    type t =
      (* THESE MUST STAY IN THE SAME ORDER AS IN syslog_stubs.c!!! *)
      | EMERG | ALERT | CRIT | ERR | WARNING | NOTICE | INFO | DEBUG
    with sexp, enumerate, compare
    let compare a b = compare b a       (* listed in descending order *)
    TEST_UNIT = <:test_result< int >> ~expect:1 (compare EMERG DEBUG)
    external to_int : t -> int = "core_syslog_level_to_int"
    let collect_mask i t = to_int t lor i
    let mask ts = Core_list.fold ~f:collect_mask ~init:0 ts
  end
  include T

  include Sexpable.To_stringable (T)
  TEST_UNIT = <:test_result< string >> ~expect:"EMERG" (to_string EMERG)
  TEST_UNIT = <:test_result< t >> ~expect:DEBUG (of_string "DEBUG")
end

external core_syslog_openlog : string option -> int -> int -> unit = "core_syslog_openlog"
external core_syslog_syslog : int -> string -> unit = "core_syslog_syslog"
external core_syslog_closelog : unit -> unit = "core_syslog_closelog" "noalloc"
external core_syslog_setlogmask : int -> unit = "core_syslog_setlogmask" "noalloc"

let openlog ?id ?(options = []) ?(facility = Facility.USER) () =
  core_syslog_openlog id (Open_option.mask options) (Facility.to_int facility)

let syslog ?(facility = Facility.USER) ?(level = Level.INFO) ?(add_stderr = false)
      message =
  core_syslog_syslog (Level.to_int level lor Facility.to_int facility) message;
  if add_stderr then
    (eprintf "%s/%s: %s\n" (Facility.to_string facility) (Level.to_string level) message;
     flush stderr)

let syslogf ?facility ?level ?add_stderr format =
  ksprintf (fun message -> syslog ?facility ?level ?add_stderr message) format

let logmask_range ?(to_level = Level.EMERG) from_level =
  Core_list.fold Level.all ~init:0 ~f:(fun logmask level ->
    if Level.compare from_level    level < 1
    && Level.compare      level to_level < 1
    then Level.to_int level lor logmask
    else logmask)

let setlogmask ?(allowed_levels = []) ?(from_level = Level.DEBUG) ?to_level () =
  core_syslog_setlogmask
    (Level.mask allowed_levels lor logmask_range ?to_level from_level)

let closelog = core_syslog_closelog
