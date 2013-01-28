(** Mutual exclusion between processes using flock and lockf.  A file is
    considered locked if either of these mechanisms works.

    These locks are OS-level but are Local (will not work across computers
    even if they mount the same directory).
*)

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
  -> ?close_on_exec : bool
  -> ?unlink_on_exit : bool
  -> string
  -> bool

(** [create_exn ?message path] is like [create] except that it throws an exception on
    failure instead of returning a boolean value *)
val create_exn
  :  ?message : string
  -> ?close_on_exec : bool
  -> ?unlink_on_exit : bool
  -> string
  -> unit

(** [blocking_create t] tries to create the lock. If another process holds the lock this
    function will wait until it is released. *)
val blocking_create
  :  ?message : string
  -> ?close_on_exec : bool
  -> ?unlink_on_exit : bool
  -> string
  -> unit

(** [is_locked path] returns true when the file at [path] exists and is locked, false
    otherwise. *)
val is_locked : string -> bool

(** an implementation neutral NFS lock file scheme that relies on the atomicity of link
    and rename over NFS (see NFS Illustrated, atomicity for more information).  There are
    a few caveats compared to local file locks:

    - These calls require the locker to have write access to the directory containing the
    file being locked.

    - Unlike a normal flock call the lock will not be removed when the calling program
    exits, so unlock must be called.

    - There is no protection provided to prevent a different caller from calling
    unlock. *)
module Nfs : sig
  (** [lock ?message path] tries to lock the file at [path] by creating two new files
      [path].nfs_lock and [path].nfs_lock.msg.  [path].nfs_lock will be a hard link to
      [path] and [path].nfs_lock.msg will contain [message] (caller's hostname and pid by
      default).  This lock WILL NOT be released when the calling program exits.  You MUST
      call unlock. *)
  val create : ?message : string -> string -> bool

  (** [lock_exn ?message path] like lock, but throws an exception when it fails to obtain
      the lock *)
  val create_exn : ?message : string -> string -> unit

  (** [lock_blocking ?message path] like lock, but sleeps for 1 second between lock
      attempts and does not return until it succeeds *)
  val blocking_create : ?message : string -> string -> unit

  (** [unlock path] unlocks a file locked by some version of lock.  There is no protection
      provided to stop you from unlocking a file you have not locked *)
  val unlock : string -> unit

  (** [critical_section ?message path ~f] wrap function [f] (including exceptions escaping
      it) by first locking (using {!create_exn}) and then unlocking the given lock
      file. *)
  val critical_section : ?message : string -> string -> f : (unit -> 'a) -> 'a
end
