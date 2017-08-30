open! Import

include (Int : sig
           type t = int [@@deriving bin_io]

           include Comparable.S with type t := t
           include Hashable  .S with type t := t
         end)

external ml_caml_to_nonportable_signal_number : int -> int =
  "ml_caml_to_nonportable_signal_number"

external ml_nonportable_to_caml_signal_number : int -> int =
  "ml_nonportable_to_caml_signal_number"

let of_system_int t = ml_nonportable_to_caml_signal_number t
let to_system_int t = ml_caml_to_nonportable_signal_number t

let of_caml_int t = t
let to_caml_int t = t

type sys_behavior = [
  | `Continue (** Continue the process if it is currently stopped *)
  | `Dump_core (** Terminate the process and dump core *)
  | `Ignore (** Ignore the signal *)
  | `Stop  (** Stop the process *)
  | `Terminate  (** Terminate the process *)
] [@@deriving sexp]

let equal (t : t) t' = (t = t')

include struct
  (* Please keep in sync with the list for to_string/sys_behavior *)
  open Sys
  let abrt   = sigabrt
  let alrm   = sigalrm
  let bus    = sigbus
  let chld   = sigchld
  let cont   = sigcont
  let fpe    = sigfpe
  let hup    = sighup
  let ill    = sigill
  let int    = sigint
  let kill   = sigkill
  let pipe   = sigpipe
  let poll   = sigpoll
  let prof   = sigprof
  let quit   = sigquit
  let segv   = sigsegv
  let stop   = sigstop
  let sys    = sigsys
  let term   = sigterm
  let trap   = sigtrap
  let tstp   = sigtstp
  let ttin   = sigttin
  let ttou   = sigttou
  let urg    = sigurg
  let usr1   = sigusr1
  let usr2   = sigusr2
  let vtalrm = sigvtalrm
  let xcpu   = sigxcpu
  let xfsz   = sigxfsz
  let zero   = 0
end

exception Invalid_signal_mnemonic_or_number of string [@@deriving sexp]

let to_string_with_version, of_string, default_sys_behavior =
  let known =
    [
      ("sigabrt",   abrt,   `Dump_core, 1);
      ("sigalrm",   alrm,   `Terminate, 1);
      ("sigbus",    bus,    `Dump_core, 2);
      ("sigchld",   chld,   `Ignore,    1);
      ("sigcont",   cont,   `Continue,  1);
      ("sigfpe",    fpe,    `Dump_core, 1);
      ("sighup",    hup,    `Terminate, 1);
      ("sigill",    ill,    `Dump_core, 1);
      ("sigint",    int,    `Terminate, 1);
      ("sigkill",   kill,   `Terminate, 1);
      ("sigpipe",   pipe,   `Terminate, 1);
      ("sigpoll",   poll,   `Terminate, 2);
      ("sigprof",   prof,   `Terminate, 1);
      ("sigquit",   quit,   `Dump_core, 1);
      ("sigsegv",   segv,   `Dump_core, 1);
      ("sigstop",   stop,   `Stop,      1);
      ("sigsys",    sys,    `Dump_core, 2);
      ("sigterm",   term,   `Terminate, 1);
      ("sigtrap",   trap,   `Dump_core, 2);
      ("sigtstp",   tstp,   `Stop,      1);
      ("sigttin",   ttin,   `Stop,      1);
      ("sigttou",   ttou,   `Stop,      1);
      ("sigurg",    urg,    `Ignore,    2);
      ("sigusr1",   usr1,   `Terminate, 1);
      ("sigusr2",   usr2,   `Terminate, 1);
      ("sigvtalrm", vtalrm, `Terminate, 1);
      ("sigxcpu",   xcpu,   `Dump_core, 2);
      ("sigxfsz",   xfsz,   `Dump_core, 2);
      ("sigzero",   zero,   `Ignore,    1);
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
      if String.is_prefix s ~prefix:"<unknown signal " then
        try Int.of_string (String.slice s 16 ~-1)
        with _ -> raise (Invalid_signal_mnemonic_or_number s)
      else raise (Invalid_signal_mnemonic_or_number s)
  in
  let default_sys_behavior t =
    match Hashtbl.find behavior_by_t t with
    | None ->
      raise (Invalid_argument ("Signal.default_sys_behavior: unknown signal " ^
                               Int.to_string t))
    | Some behavior -> behavior
  in
  to_string_with_version, of_string, default_sys_behavior
;;

exception Expected_atom of Sexp.t [@@deriving sexp]

let sexp_of_t_with_version t ~version = Sexp.Atom (to_string_with_version t ~version)

let to_string s = to_string_with_version s ~version:2

let sexp_of_t t = sexp_of_t_with_version t ~version:1

let t_of_sexp s =
  match s with
  | Sexp.Atom s -> of_string s
  | _ -> raise (Expected_atom s)
;;

type pid_spec = [ `Pid of Pid.t | `My_group | `Group of Pid.t ] [@@deriving sexp_of]

let pid_spec_to_int = function
  | `Pid pid -> Pid.to_int pid
  | `My_group -> 0
  | `Group pid -> ~- (Pid.to_int pid)
;;

let pid_spec_to_string p = Int.to_string (pid_spec_to_int p)

let send signal pid_spec =
  try UnixLabels.kill ~pid:(pid_spec_to_int pid_spec) ~signal; `Ok
  with Unix.Unix_error (Unix.ESRCH, _, _) -> `No_such_process
;;

let send_i t pid_spec =
  match send t pid_spec with
  | `Ok | `No_such_process -> ()
;;

let send_exn t pid_spec =
  match send t pid_spec with
  | `Ok -> ()
  | `No_such_process ->
    failwithf "Signal.send_exn %s pid:%s" (to_string t)
      (pid_spec_to_string pid_spec) ()
;;

module Expert = struct

  type behavior = [ `Default | `Ignore | `Handle of t -> unit ]

  module Behavior = struct

    let of_caml = function
      | Sys.Signal_default -> `Default
      | Sys.Signal_ignore -> `Ignore
      | Sys.Signal_handle f -> `Handle f

    let to_caml = function
      | `Default -> Sys.Signal_default
      | `Ignore -> Sys.Signal_ignore
      | `Handle f ->
        Sys.Signal_handle (fun t -> Exn.handle_uncaught_and_exit (fun () -> f t))
    ;;
  end

  let signal t behavior =
    Behavior.of_caml (Sys.signal t (Behavior.to_caml behavior))
  ;;

  let set t behavior = ignore (signal t behavior)

  let handle t f = set t (`Handle f)

end

open Expert

let handle_default t = set t `Default
let ignore         t = set t `Ignore

type sigprocmask_command = [ `Set | `Block | `Unblock ]

let sigprocmask mode sigs =
  let mode =
    match mode with
    | `Block -> Unix.SIG_BLOCK
    | `Unblock -> Unix.SIG_UNBLOCK
    | `Set -> Unix.SIG_SETMASK
  in
  Unix.sigprocmask mode sigs
;;

let sigpending = Unix.sigpending
let sigsuspend = Unix.sigsuspend

let can_send_to pid =
  try
    send_exn zero (`Pid pid);
    true
  with
  | _ -> false
;;

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

let%test_unit _ =
  [%test_eq: string] (to_string bus) "sigbus";
  [%test_eq: string] (Sexp.to_string (sexp_of_t bus)) {|"<unknown signal -22>"|};
  [%test_eq: string] (Sexp.to_string (Stable.V1.sexp_of_t bus)) {|"<unknown signal -22>"|};
  [%test_eq: string] (Sexp.to_string (Stable.V2.sexp_of_t bus)) "sigbus";
;;
