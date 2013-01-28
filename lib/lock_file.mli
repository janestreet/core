(** Mutual exclusion between processes using flock and lockf.  A file is
    considered locked if either of these mechanisms works.

    These locks are OS-level but are Local (will not work across computers
    even if they mount the same directory).
*)

open Std_internal

(** [create ?close_on_exec ?message path] tries to create a file at [path] containing the
    text [message], which defaults to the pid of the locking process.  It returns true on
    success, false on failure.  Note: there is no way to release the lock or the fd
    created inside!  It will only be released when the process dies. If close_on_exec is
    false, then the lock will not be released until children created via fork and exec
    also terminate. If not specified, close_on_exec=true.  Note that by default, the lock
    file is not cleaned up for you when the process exits. If you pass
    [unlink_on_exit:true], an at_exit handler will be set up to remove the lock-file on
    program termination. *)
val create
  :  ?message : string
  -> ?close_on_exec : bool (* defaults to true *)
  -> ?unlink_on_exit : bool (* defaults to false *)
  -> string
  -> bool

(** [create_exn ?message path] is like [create] except that it throws an exception on
    failure instead of returning a boolean value *)
val create_exn
  :  ?message : string
  -> ?close_on_exec : bool (* defaults to true *)
  -> ?unlink_on_exit : bool (* defaults to false *)
  -> string
  -> unit

(** [blocking_create t] tries to create the lock. If another process holds the lock this
    function will wait until it is released. *)
val blocking_create
  :  ?message : string
  -> ?close_on_exec : bool (* defaults to true *)
  -> ?unlink_on_exit : bool (* defaults to false *)
  -> string
  -> unit

(** [is_locked path] returns true when the file at [path] exists and is locked, false
    otherwise. *)
val is_locked : string -> bool

(** An implementation neutral NFS lock file scheme that relies on the atomicity of link
    over NFS (see NFS Illustrated, atomicity for more information).  Rather than relying
    on a working traditional advisory lock system over NFS we create a hard link between
    the file given to the create call and a new file <filename>.nfs_lock.  This link call
    is atomic (in that it succeeds or fails) across all systems that have the same
    filesystem mounted.  The link file must be cleaned up on program exit (normally
    accomplished by an at_exit handler, but see caveats below).

    There are a few caveats compared to local file locks:

    - These calls require the locker to have write access to the directory containing the
      file being locked.

    - Unlike a normal flock call the lock may not be removed when the calling program
      exits (in particular if it is killed with SIGKILL).
*)
module Nfs : sig
  (** [lock ?message path] tries to create and lock the file at [path] by creating a hard
      link to [path].nfs_lock.  The contents of path will be replaced with [message],
      which will be the caller's hostname:pid by default.

      Efforts will be made to release this lock when the calling program exits.  But there
      is no guarantee that this will occur under some types of program crash *)
  val create : ?message : string -> string -> bool

  (** [create_exn ?message path] like create, but throws an exception when it fails to
      obtain the lock *)
  val create_exn : ?message : string -> string -> unit

  (** [blocking_create ?message path] like create, but sleeps for 1 second between lock
      attempts and does not return until it succeeds *)
  val blocking_create : ?message : string -> string -> unit

  (** [critical_section ?message path ~f] wrap function [f] (including exceptions escaping
      it) by first locking (using {!create_exn}) and then unlocking the given lock
      file. *)
  val critical_section : ?message : string -> string -> f : (unit -> 'a) -> 'a

  (** [get_hostname_and_pid path] reads the lock file at [path] and returns the hostname
      and path in the file if it can be parsed. *)
  val get_hostname_and_pid : string -> (string * int) option
end

