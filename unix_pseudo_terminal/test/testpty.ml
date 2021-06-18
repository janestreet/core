open Core
open Poly
module Unix = Core_unix

module Pty = Unix_pseudo_terminal

let run_parent fdm fds =
  let msg = "Input: " in
  let buflen = 512 in
  let buf = Bytes.create buflen in
  let rec loop () =
    let _ = eprintf "%s%!" msg in
    let rlen = Unix.read Unix.stdin ~len:buflen ~buf in
    let s = if rlen < buflen then Bytes.subo buf ~len:rlen else buf in
    let _:int = Unix.write fdm ~buf:s in
    let rlen = Unix.read fdm ~len:buflen ~buf in
    let s = if rlen < buflen then Bytes.To_string.subo buf ~len:rlen else Bytes.to_string buf in
    let _ = eprintf "Master: %s%!" s in
    loop ()
  in
  Unix.close fds;
  loop ()
;;

let run_child fdm fds =
  let buflen = 512 in
  let buf = Bytes.create buflen in
  let rec loop () =
    let rlen = Unix.read fds ~len:buflen ~buf in
    let s = if rlen < buflen then Bytes.To_string.subo buf ~len:rlen else Bytes.to_string buf in
    let s = String.rstrip s ~drop:(fun c -> c = '\n') in
    eprintf "Slave received [%s]\n%!" s;
    loop ();
  in
  Unix.close fdm;
  let term_orig = Unix.Terminal_io.tcgetattr fds in
  let term_new =
    { term_orig with
      c_ignbrk = false
    ; c_brkint = false
    ; c_parmrk = false
    ; c_istrip = false
    ; c_inlcr = false
    ; c_igncr = false
    ; c_icrnl = false
    ; c_ixon = false
    ; c_opost = false
    ; c_echo = false
    ; c_echonl = false
    ; c_icanon = false
    ; c_isig = false
    ; c_csize = 8
    ; c_parenb = false
    }
  in
  let () = Unix.Terminal_io.tcsetattr term_new fds ~mode:Unix.Terminal_io.TCSANOW in
  Unix.dup2 ~src:fds ~dst:Unix.stderr ();
  Unix.dup2 ~src:fds ~dst:Unix.stdout ();
  Unix.dup2 ~src:fds ~dst:Unix.stdin ();
  loop ()
;;

let () =
  let posix_openpt = Or_error.ok_exn Pty.posix_openpt in
  let grantpt = Or_error.ok_exn Pty.grantpt in
  let unlockpt = Or_error.ok_exn Pty.unlockpt in
  let ptsname = Or_error.ok_exn Pty.ptsname in
  let fdm = posix_openpt [ O_RDWR ] in
  let () = grantpt fdm in
  let () = unlockpt fdm in
  let ptsname = ptsname fdm in
  eprintf "ptsname...%s\n%!" ptsname;
  let fds = Unix.openfile ptsname ~mode:[ Unix.O_RDWR ] in
  match Unix.fork () with
  | `In_the_parent _pid -> run_parent fdm fds
  | `In_the_child -> run_child fdm fds
