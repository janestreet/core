(* This module provides support for daemonizing a process.  It provides flexibility
   as to where the standard file descriptors (stdin, stdout and stderr) are connected
   after daemonization has occurred.
*)

module Fd_redirection : sig
  type t = [
  | `Dev_null
  | `Do_not_redirect
  | `File_append of string
  | `File_truncate of string
  ]
end

(** [daemonize] makes the executing process a daemon.

    See Chapter 13 of Advanced Programming in the UNIX Environment Second Edition by
    Stephens and Rago for more details.

    The optional arguments have defaults as per [daemonize_wait], below.

    By default, output sent to stdout and stderr after daemonization will be silently
    eaten.  This behaviour may be adjusted by using [redirect_stdout] and
    [redirect_stderr].  See the documentation for [daemonize_wait] below.

    @raise Failure if fork was unsuccessful.
*)
val daemonize
  :  ?redirect_stdout : Fd_redirection.t
  -> ?redirect_stderr : Fd_redirection.t
  -> ?cd : string
  -> ?umask : int
  -> unit
  -> unit

(** [daemonize_wait] makes the executing process a daemon, but delays full detachment
    from the calling shell/process until the returned "release" closure is called.

    Any output to stdout/stderr before the "release" closure is called will get
    sent out normally.  After "release" is called, stdin is connected to /dev/null,
    and stdout and stderr are connected as specified by [redirect_stdout] and
    [redirect_stderr].  The default is the usual behaviour whereby both of these
    descriptors are connected to /dev/null.

    Note that calling [release] will adjust SIGPIPE handling, so you should not rely on
    the delivery of this signal during this time.

    [daemonize_wait] allows you to daemonize and then start async, but still have
    stdout/stderr go to the controlling terminal during startup.  By default, when you
    [daemonize], toplevel exceptions during startup would get sent to /dev/null.  With
    [daemonize_wait], toplevel exceptions can go to the terminal until you call [release].

    @raise Failure if fork was unsuccessful.
*)
val daemonize_wait
  :  ?redirect_stdout : Fd_redirection.t  (* default redirect to /dev/null *)
  -> ?redirect_stderr : Fd_redirection.t  (* default redirect to /dev/null *)
  -> ?cd : string  (* default / *)
  -> ?umask : int  (* default zero *)
  -> unit
  -> (unit -> unit) Staged.t
