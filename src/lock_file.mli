(** Mutual exclusion between processes using flock and lockf.  A file is considered locked
    only if both of these mechanisms work.

    These locks are OS-level and as such are local to the machine and will not work across
    computers even if they mount the same directory.
*)

open! Import
open Import_time

(** [create ?close_on_exec ?message path] tries to create a file at [path] containing the
    text [message], which defaults to the pid of the locking process.  It returns true on
    success, false on failure.

    Note: there is no way to release the lock or the fd created inside!  It will only be
    released when the process dies. If [close_on_exec] is [false], then the lock will not
    be released until children created via fork and exec also terminate. If not specified,
    [close_on_exec=true].

    Note that by default, the lock file is not cleaned up for you when the process
    exits. If you pass [unlink_on_exit:true], an [at_exit] handler will be set up to
    remove the lock file on program termination.

    The lock file is created with mode 664, so will not be world-writable even with
    umask 0. *)
val create
  :  ?message : string
  -> ?close_on_exec : bool (** defaults to true *)
  -> ?unlink_on_exit : bool (** defaults to false *)
  -> string
  -> bool

(** [create_exn ?message path] is like [create] except that it throws an exception on
    failure instead of returning a boolean value. *)
val create_exn
  :  ?message : string
  -> ?close_on_exec : bool (** defaults to true *)
  -> ?unlink_on_exit : bool (** defaults to false *)
  -> string
  -> unit

(** [blocking_create t] tries to create the lock. If another process holds the lock this
    function will wait until it is released or until [timeout] expires. *)
val blocking_create
  :  ?timeout : Time.Span.t (** defaults to wait indefinitely *)
  -> ?message : string
  -> ?close_on_exec : bool (** defaults to true *)
  -> ?unlink_on_exit : bool (** defaults to false *)
  -> string
  -> unit

(** [is_locked path] returns [true] when the file at [path] exists and is locked, [false]
    otherwise. Requires write permission for the lock file. *)
val is_locked : string -> bool

(** [get_pid path] reads the lock file at [path] and returns the pid in the file.  Returns
    [None] if the file cannot be read, or if the file contains a message that is not an
    int. *)
val get_pid : string -> Pid.t option

(** An implementation-neutral NFS lock file scheme that relies on the atomicity of link
    over NFS.  Rather than relying on a working traditional advisory lock system over NFS,
    we create a hard link between the file given to the [create] call and a new file
    <filename>.nfs_lock.  This link call is atomic (in that it succeeds or fails) across
    all systems that have the same filesystem mounted.  The link file must be cleaned up
    on program exit (normally accomplished by an [at_exit] handler, but see caveats
    below).

    There are a few caveats compared to local file locks:

    - These calls require the locker to have write access to the directory containing the
      file being locked.

    - Unlike a normal flock call the lock may not be removed when the calling program
      exits (in particular if it is killed with SIGKILL).

    - NFS lock files are non-standard and difficult to reason about.  This implementation
      strives to strike a balance between safety and utility in the common case:
      {ul
      {li one program per machine}
      {li one shared user running the program}
      }

    Use cases outside of this may push on/break assumptions used for easy lock
    cleanup/taking and may lead to double-taking the lock.  If you have such an odd use
    case you should test it carefully/consider a different locking mechanism.
*)
module Nfs : sig
  (** [create ?message path] tries to create and lock the file at [path] by creating a
      hard link to [path].nfs_lock. The contents of [path] will be replaced with a sexp
      containing the caller's hostname and pid, and the optional [message].

      Efforts will be made to release this lock when the calling program exits. But there
      is no guarantee that this will occur under some types of program crash. If the
      program crashes without removing the lock file an attempt will be made to clean up
      on restart by checking the hostname and pid stored in the lockfile.
  *)
  val create : ?message : string -> string -> unit Or_error.t

  (** [create_exn ?message path] is like [create], but throws an exception when it fails
      to obtain the lock. *)
  val create_exn : ?message : string -> string -> unit

  (** [blocking_create ?message path] is like [create], but sleeps for a short while
      between lock attempts and does not return until it succeeds or [timeout] expires.
      Timeout defaults to wait indefinitely. *)
  val blocking_create : ?timeout : Time.Span.t -> ?message : string -> string -> unit

  (** [critical_section ?message ~timeout path ~f] wraps function [f] (including
      exceptions escaping it) by first locking (using {!blocking_create}) and then
      unlocking the given lock file. *)
  val critical_section
    : ?message : string -> string -> timeout : Time.Span.t -> f : (unit -> 'a) -> 'a

  (** [get_hostname_and_pid path] reads the lock file at [path] and returns the hostname
      and path in the file.  Returns [None] if the file cannot be read. *)
  val get_hostname_and_pid : string -> (string * Pid.t) option

  (** [get_message path] reads the lock file at [path] and returns the message in the
      file.  Returns [None] if the file cannot be read. *)
  val get_message : string -> string option

  (** [unlock_exn path] unlocks [path] if [path] was locked from the same host and the pid
      in the file is either the current pid or not the pid of a running process.

      It will raise if for some reason the lock at the given path cannot be unlocked, for
      example if the lock is taken by somebody else that is still alive on the same box,
      or taken by a process on a different host, or if there are Unix permissions issues,
      etc.

      This function should be used only by programs that need to release their lock before
      exiting. If releasing the lock can or should wait till the end of the running
      process, do not call this function -- this library already takes care of releasing
      at exit all the locks taken. *)
  val unlock_exn : string -> unit
  val unlock     : string -> unit Or_error.t
end
