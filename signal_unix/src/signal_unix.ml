open! Core
open! Import
open Core.Signal

external ml_caml_to_nonportable_signal_number : t -> int =
  "ml_caml_to_nonportable_signal_number"

external ml_nonportable_to_caml_signal_number : int -> t =
  "ml_nonportable_to_caml_signal_number"

let of_system_int t = ml_nonportable_to_caml_signal_number t
let to_system_int t = ml_caml_to_nonportable_signal_number t

type pid_spec = [ `Pid of Pid.t | `My_group | `Group of Pid.t ] [@@deriving sexp_of]

let pid_spec_to_int = function
  | `Pid pid -> Pid.to_int pid
  | `My_group -> 0
  | `Group pid -> ~- (Pid.to_int pid)
;;

let pid_spec_to_string p = Int.to_string (pid_spec_to_int p)

let send t pid_spec =
  try UnixLabels.kill ~pid:(pid_spec_to_int pid_spec) ~signal:(to_caml_int t); `Ok
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
    failwithf "Signal_unix.send_exn %s pid:%s" (to_string t)
      (pid_spec_to_string pid_spec) ()
;;

type sigprocmask_command = [ `Set | `Block | `Unblock ]

let sigprocmask mode sigs =
  let mode =
    match mode with
    | `Block -> Unix.SIG_BLOCK
    | `Unblock -> Unix.SIG_UNBLOCK
    | `Set -> Unix.SIG_SETMASK
  in
  Unix.sigprocmask mode (sigs |> List.map ~f:to_caml_int) |> List.map ~f:of_caml_int
;;

let sigpending () = Unix.sigpending () |> List.map ~f:of_caml_int
let sigsuspend ts = Unix.sigsuspend (ts |> List.map ~f:to_caml_int)

let can_send_to pid =
  try
    send_exn zero (`Pid pid);
    true
  with
  | _ -> false
;;
