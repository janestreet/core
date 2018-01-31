(** This module provides support for daemonizing a process.  It provides flexibility as to
    where the standard file descriptors (stdin, stdout and stderr) are connected after
    daemonization has occurred. *)

open! Import

module Fd_redirection : sig
  type t = [
    | `Dev_null
    | `Dev_null_skip_regular_files (** Redirect to /dev/null unless already redirected to
                                       a regular file. *)
    | `Do_not_redirect
    | `File_append of string
    | `File_truncate of string
  ]
end

(** [daemonize] makes the executing process a daemon.

    The optional arguments have defaults as per [daemonize_wait], below.

    By default, output sent to stdout and stderr after daemonization will be silently
    eaten.  This behaviour may be adjusted by using [redirect_stdout] and
    [redirect_stderr].  See the documentation for [daemonize_wait] below.

    See [daemonize_wait] for a description of [allow_threads_to_have_been_created].

    Raises [Failure] if fork was unsuccessful. *)
val daemonize
  :  ?redirect_stdout : Fd_redirection.t
  -> ?redirect_stderr : Fd_redirection.t
  -> ?cd : string
  -> ?umask : int (** defaults to use existing umask *)
  -> ?allow_threads_to_have_been_created : bool (** defaults to false *)
  -> unit
  -> unit

(** [daemonize_wait] makes the executing process a daemon, but delays full detachment from
    the calling shell/process until the returned "release" closure is called.

    Any output to stdout/stderr before the "release" closure is called will get
    sent out normally. After "release" is called, stdin is connected to /dev/null,
    and stdout and stderr are connected as specified by [redirect_stdout] and
    [redirect_stderr]. The default is the usual behavior whereby both of these
    descriptors are connected to /dev/null. [daemonize_wait], however, will not
    redirect stdout/stderr to /dev/null if they are already redirected to a regular
    file by default, i.e., default redirection is [`Dev_null_skip_regular_files]. This
    is to preserve behavior from earlier versions.)

    Note that calling [release] will adjust SIGPIPE handling, so you should not rely on
    the delivery of this signal during this time.

    [daemonize_wait] allows you to daemonize and then start asynchronously, but still have
    stdout/stderr go to the controlling terminal during startup. By default, when you
    [daemonize], toplevel exceptions during startup would get sent to /dev/null. With
    [daemonize_wait], toplevel exceptions can go to the terminal until you call [release].

    Forking (especially to daemonize) when running multiple threads is tricky and
    generally a mistake. [daemonize] and [daemonize_wait] check that the current number of
    threads is not greater than expected. [daemonize_wait] and [daemonize] also check that
    threads have not been created, which is more conservative than the actual requirement
    that multiple threads are not running. Using
    [~allow_threads_to_have_been_created:true] bypasses that check. This is useful if
    Async was previously running, and therefore threads have been created, but has since
    been shut down. On non-Linux platforms, using
    [~allow_threads_to_have_been_created:true] eliminates the protection [daemonize] and
    [daemonize_wait] have regarding threads.

    Raises [Failure] if forking was unsuccessful. *)
val daemonize_wait
  :  ?redirect_stdout : Fd_redirection.t  (** default `Dev_null_skip_regular_files *)
  -> ?redirect_stderr : Fd_redirection.t  (** default `Dev_null_skip_regular_files *)
  -> ?cd : string  (** default / *)
  -> ?umask : int  (** defaults to use existing umask *)
  -> ?allow_threads_to_have_been_created : bool (** defaults to false *)
  -> unit
  -> (unit -> unit) Staged.t
