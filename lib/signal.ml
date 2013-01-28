open Sexplib.Std

module Int = Core_int
module List = Core_list
module Hashtbl = Core_hashtbl
module String = Core_string

let failwithf = Core_printf.failwithf

type t = int

include (Int : sig
  include Comparable.S with type t := t
  include Hashable.S with type t := t
  include Sexpable.S with type t := t
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
] with sexp

let equal (t : t) t' = (t = t')

include struct
  (* Please keep in sync with the list for to_string/sys_behavior *)
  open Sys
  let abrt = sigabrt
  let alrm = sigalrm
  let chld = sigchld
  let cont = sigcont
  let fpe = sigfpe
  let hup = sighup
  let ill = sigill
  let int = sigint
  let kill = sigkill
  let pipe = sigpipe
  let prof = sigprof
  let quit = sigquit
  let segv = sigsegv
  let stop = sigstop
  let term = sigterm
  let tstp = sigtstp
  let ttin = sigttin
  let ttou = sigttou
  let usr1 = sigusr1
  let usr2 = sigusr2
  let vtalrm = sigvtalrm
  let zero = 0
end

exception Invalid_signal_mnemonic_or_number of string with sexp

let to_string, of_string, default_sys_behavior =
  let known =
    [
      ("abrt", abrt, `Dump_core);
      ("alrm", alrm, `Terminate);
      ("chld", chld, `Ignore);
      ("cont", cont, `Continue);
      ("fpe", fpe, `Dump_core);
      ("hup", hup, `Terminate);
      ("ill", ill, `Dump_core);
      ("int", int, `Terminate);
      ("kill", kill, `Terminate);
      ("pipe", pipe, `Terminate);
      ("prof", prof, `Terminate);
      ("quit", quit, `Dump_core);
      ("segv", segv, `Dump_core);
      ("stop", stop, `Stop);
      ("term", term, `Terminate);
      ("tstp", tstp, `Stop);
      ("ttin", ttin, `Stop);
      ("ttou", ttou, `Stop);
      ("usr1", usr1, `Terminate);
      ("usr2", usr2, `Terminate);
      ("vtalrm", vtalrm, `Terminate);
      ("<zero>", zero, `Ignore);
    ]
  in
  let str_tbl = Int.Table.create ~size:1 () in
  let int_tbl = Core_string.Table.create ~size:1 () in
  let behavior_tbl = Int.Table.create ~size:1 () in
  List.iter known ~f:(fun (name, s, behavior) ->
    Hashtbl.replace str_tbl ~key:s ~data:("sig" ^ name);
    Hashtbl.replace int_tbl ~key:name ~data:s;
    Hashtbl.replace behavior_tbl ~key:s ~data:behavior);
  (* For unknown signal numbers, [to_string] returns a meaningful
     string, while [default_sys_behavior] has to raise an exception
     because we don't know what the right answer is. *)
  let to_string s =
    match Hashtbl.find str_tbl s with
    | None -> "<unknown signal " ^ Int.to_string s ^ ">"
    | Some string -> string
  in
  let of_string s =
    let s = Core_string.lowercase (Core_string.strip s) in
    match Hashtbl.find int_tbl s with
    | Some sn -> sn
    | None ->
      if Core_string.is_prefix s ~prefix:"<unknown signal " then
        try Int.of_string (Core_string.slice s 16 ~-1)
        with _ -> raise (Invalid_signal_mnemonic_or_number s)
      else raise (Invalid_signal_mnemonic_or_number s)
  in
  let default_sys_behavior s =
    match Hashtbl.find behavior_tbl s with
    | None ->
        raise (Invalid_argument ("Signal.default_sys_behavior: unknown signal " ^
  Int.to_string s))
    | Some behavior -> behavior
  in
  to_string, of_string, default_sys_behavior
;;

exception Expected_atom of Sexplib.Sexp.t with sexp

let sexp_of_t t = Sexplib.Sexp.Atom (to_string t)

let t_of_sexp s =
  match s with
  | Sexplib.Sexp.Atom s -> of_string s
  | _ -> raise (Expected_atom s)
;;

type pid_spec = [ `Pid of Pid.t | `My_group | `Group of Pid.t ] ;;

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

type behavior = [ `Default | `Ignore | `Handle of t -> unit ]

module Behavior = struct
  type t = behavior

  let of_caml = function
    | Sys.Signal_default -> `Default
    | Sys.Signal_ignore -> `Ignore
    | Sys.Signal_handle f -> `Handle f

  let to_caml = function
    | `Default -> Sys.Signal_default
    | `Ignore -> Sys.Signal_ignore
    | `Handle f -> Sys.Signal_handle f
end

let signal t behavior =
  Behavior.of_caml (Sys.signal t (Behavior.to_caml behavior))
;;

let set t behavior = ignore (signal t behavior)

let handle t f = set t (`Handle f)
let handle_default t = set t `Default
let ignore t = set t `Ignore

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
