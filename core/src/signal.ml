open! Import

include (
  Int :
    sig
      type t = int [@@deriving bin_io]

      include Comparable.S with type t := t
      include Hashable.S with type t := t
    end)

let of_caml_int t = t
let to_caml_int t = t

type sys_behavior =
  [ `Continue (** Continue the process if it is currently stopped *)
  | `Dump_core (** Terminate the process and dump core *)
  | `Ignore (** Ignore the signal *)
  | `Stop (** Stop the process *)
  | `Terminate (** Terminate the process *)
  ]
[@@deriving sexp]

let equal (t : t) t' = t = t'

include struct
  (* Please keep in sync with the list for to_string/sys_behavior *)
  open Stdlib.Sys

  let abrt = sigabrt
  let alrm = sigalrm
  let bus = sigbus
  let chld = sigchld
  let cont = sigcont
  let fpe = sigfpe
  let hup = sighup
  let ill = sigill
  let int = sigint
  let kill = sigkill
  let pipe = sigpipe
  let poll = sigpoll
  let prof = sigprof
  let quit = sigquit
  let segv = sigsegv
  let stop = sigstop
  let sys = sigsys
  let term = sigterm
  let trap = sigtrap
  let tstp = sigtstp
  let ttin = sigttin
  let ttou = sigttou
  let urg = sigurg
  let usr1 = sigusr1
  let usr2 = sigusr2
  let vtalrm = sigvtalrm
  let xcpu = sigxcpu
  let xfsz = sigxfsz
  let zero = 0
end

exception Invalid_signal_mnemonic_or_number of string [@@deriving sexp]

let to_string_with_version, of_string, default_sys_behavior =
  let known =
    [ "sigabrt", abrt, `Dump_core, 1
    ; "sigalrm", alrm, `Terminate, 1
    ; "sigbus", bus, `Dump_core, 2
    ; "sigchld", chld, `Ignore, 1
    ; "sigcont", cont, `Continue, 1
    ; "sigfpe", fpe, `Dump_core, 1
    ; "sighup", hup, `Terminate, 1
    ; "sigill", ill, `Dump_core, 1
    ; "sigint", int, `Terminate, 1
    ; "sigkill", kill, `Terminate, 1
    ; "sigpipe", pipe, `Terminate, 1
    ; "sigpoll", poll, `Terminate, 2
    ; "sigprof", prof, `Terminate, 1
    ; "sigquit", quit, `Dump_core, 1
    ; "sigsegv", segv, `Dump_core, 1
    ; "sigstop", stop, `Stop, 1
    ; "sigsys", sys, `Dump_core, 2
    ; "sigterm", term, `Terminate, 1
    ; "sigtrap", trap, `Dump_core, 2
    ; "sigtstp", tstp, `Stop, 1
    ; "sigttin", ttin, `Stop, 1
    ; "sigttou", ttou, `Stop, 1
    ; "sigurg", urg, `Ignore, 2
    ; "sigusr1", usr1, `Terminate, 1
    ; "sigusr2", usr2, `Terminate, 1
    ; "sigvtalrm", vtalrm, `Terminate, 1
    ; "sigxcpu", xcpu, `Dump_core, 2
    ; "sigxfsz", xfsz, `Dump_core, 2
    ; "sigzero", zero, `Ignore, 1
    ]
  in
  let name_and_version_by_t = Int.Table.create ~size:1 () in
  let t_by_name = String.Table.create ~size:1 () in
  let behavior_by_t = Int.Table.create ~size:1 () in
  List.iter known ~f:(fun (name, t, behavior, stable_version) ->
    Hashtbl.set name_and_version_by_t ~key:t ~data:(name, stable_version);
    Hashtbl.set t_by_name ~key:name ~data:t;
    Hashtbl.set behavior_by_t ~key:t ~data:behavior);
  (* For unknown signal numbers, [to_string] returns a meaningful
     string, while [default_sys_behavior] has to raise an exception
     because we don't know what the right answer is. *)
  let to_string_with_version t ~version:requested_version =
    match Hashtbl.find name_and_version_by_t t with
    | Some (string, needed_version) when requested_version >= needed_version -> string
    | _ -> "<unknown signal " ^ Int.to_string t ^ ">"
  in
  let of_string s =
    let s = String.lowercase (String.strip s) in
    match Hashtbl.find t_by_name s with
    | Some sn -> sn
    | None ->
      if String.is_prefix s ~prefix:"<unknown signal "
      then (
        try Int.of_string (String.slice s 16 ~-1) with
        | _ -> raise (Invalid_signal_mnemonic_or_number s))
      else raise (Invalid_signal_mnemonic_or_number s)
  in
  let default_sys_behavior t =
    match Hashtbl.find behavior_by_t t with
    | None ->
      raise
        (Invalid_argument
           ("Signal.default_sys_behavior: unknown signal " ^ Int.to_string t))
    | Some behavior -> behavior
  in
  to_string_with_version, of_string, default_sys_behavior
;;

exception Expected_atom of Sexp.t [@@deriving sexp]

let sexp_of_t_with_version t ~version = Sexp.Atom (to_string_with_version t ~version)
let to_string s = to_string_with_version s ~version:2
let sexp_of_t t = sexp_of_t_with_version t ~version:2

let t_of_sexp s =
  match s with
  | Sexp.Atom s -> of_string s
  | _ -> raise (Expected_atom s)
;;

type pid_spec = [ `Use_Signal_unix ]
type sigprocmask_command = [ `Use_Signal_unix ]

let can_send_to = `Use_Signal_unix
let of_system_int = `Use_Signal_unix
let send = `Use_Signal_unix
let send_exn = `Use_Signal_unix
let send_i = `Use_Signal_unix
let sexp_of_pid_spec = `Use_Signal_unix
let sigpending = `Use_Signal_unix
let sigprocmask = `Use_Signal_unix
let sigsuspend = `Use_Signal_unix
let to_system_int = `Use_Signal_unix

module Expert = struct
  type behavior =
    [ `Default
    | `Ignore
    | `Handle of (t -> unit[@sexp.opaque])
    ]
  [@@deriving sexp_of]

  module Behavior = struct
    let of_caml = function
      | Stdlib.Sys.Signal_default -> `Default
      | Signal_ignore -> `Ignore
      | Signal_handle f -> `Handle f
    ;;

    let to_caml = function
      | `Default -> Stdlib.Sys.Signal_default
      | `Ignore -> Signal_ignore
      | `Handle f -> Signal_handle (fun t -> Exn.handle_uncaught_and_exit (fun () -> f t))
    ;;
  end

  let signal t behavior =
    Behavior.of_caml (Stdlib.Sys.signal t (Behavior.to_caml behavior))
  ;;

  let set t behavior = ignore (signal t behavior : behavior)
  let handle t f = set t (`Handle f)
end

open Expert

let handle_default t = set t `Default
let ignore t = set t `Ignore

module Stable = struct
  module V2 = struct
    type nonrec t = t [@@deriving bin_io, compare]

    let t_of_sexp = t_of_sexp
    let sexp_of_t t = sexp_of_t_with_version t ~version:2
  end

  module V1 = struct
    type nonrec t = t [@@deriving bin_io, compare]

    let t_of_sexp = t_of_sexp
    let sexp_of_t t = sexp_of_t_with_version t ~version:1
  end
end
