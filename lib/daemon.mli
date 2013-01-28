(* Describes how you would like to redirect stdio *)

type redirect_fds = [
| `Dev_null (* default *)
| `Do_not_redirect
| `File_append of string
| `File_truncate of string
]

(** [daemonize ?(redirect_stdout=`Dev_null) ?(redirect_stderr=`Dev_null)
    ?(cd = "/") ?umask=[0] ()] makes the current
    executing process a daemon, and dups "/dev/null", redirect_stdout, redirect_stderr
    to stdin/stdout/stderr. redirect_stdout and redirect_stderr default to /dev/null.
    See Chapter 13 of Advanced Programming in the UNIX Environment
    Second Edition by Stephens and Rago for more details.

    @raise Failure if fork was unsuccessful.
*)
val daemonize :
  ?redirect_stdout : redirect_fds
  -> ?redirect_stderr : redirect_fds
  -> ?cd : string
  -> ?umask : int
  -> unit
  -> unit

(** [daemonize_wait ?(cd = "/") ?(umask=0) ()] makes the executing process a
    daemon, but delays full detachment from the calling shell/process until
    the returned "release" closure is called.

    Any output to stdout/stderr before the "release" closure is called will get
    sent out normally.  After "release" is called, "/dev/null", redirect_stdout, and
    redirect_stderr gets dup'd to stdin/stdout/stderr.
    redirect_stdout and redirect_stderr default to /dev/null.

    Note that calling [release] will adjust SIGPIPE handling, so you should not rely on
    the delivery of this signal during this time.

    [daemonize_wait] allows you to daemonize and then start async, but still have
    stdout/stderr go to the controlling terminal during startup.  By default, when you
    [daemonize], toplevel exceptions during startup would get sent to /dev/null.  With
    [daemonize_wait], toplevel exceptions can go to the terminal until you call [release].

    @raise Failure if fork was unsuccessful.
*)
val daemonize_wait :
  ?redirect_stdout : redirect_fds
  -> ?redirect_stderr : redirect_fds
  -> ?cd : string
  -> ?umask : int
  -> unit
  -> (unit -> unit)
