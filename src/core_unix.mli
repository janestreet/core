(** This file is a modified version of unixLabels.mli from the OCaml distribution. *)

open! Import

(** File descriptor. *)
module File_descr : sig
  type t = Unix.file_descr [@@deriving bin_io, sexp]

  include Equal     .S with type t := t
  include Hashable  .S with type t := t
  include Stringable.S with type t := t

  val of_int : int -> t
  val to_int : t -> int
end

(** {6 Error report} *)

type error =
  Unix.error =
    E2BIG
  | EACCES
  | EAGAIN
  | EBADF
  | EBUSY
  | ECHILD
  | EDEADLK
  | EDOM
  | EEXIST
  | EFAULT
  | EFBIG
  | EINTR
  | EINVAL
  | EIO
  | EISDIR
  | EMFILE
  | EMLINK
  | ENAMETOOLONG
  | ENFILE
  | ENODEV
  | ENOENT
  | ENOEXEC
  | ENOLCK
  | ENOMEM
  | ENOSPC
  | ENOSYS
  | ENOTDIR
  | ENOTEMPTY
  | ENOTTY
  | ENXIO
  | EPERM
  | EPIPE
  | ERANGE
  | EROFS
  | ESPIPE
  | ESRCH
  | EXDEV

  | EWOULDBLOCK
  | EINPROGRESS
  | EALREADY
  | ENOTSOCK
  | EDESTADDRREQ
  | EMSGSIZE
  | EPROTOTYPE
  | ENOPROTOOPT
  | EPROTONOSUPPORT
  | ESOCKTNOSUPPORT
  | EOPNOTSUPP
  | EPFNOSUPPORT
  | EAFNOSUPPORT
  | EADDRINUSE
  | EADDRNOTAVAIL
  | ENETDOWN
  | ENETUNREACH
  | ENETRESET
  | ECONNABORTED
  | ECONNRESET
  | ENOBUFS
  | EISCONN
  | ENOTCONN
  | ESHUTDOWN
  | ETOOMANYREFS
  | ETIMEDOUT
  | ECONNREFUSED
  | EHOSTDOWN
  | EHOSTUNREACH
  | ELOOP
  | EOVERFLOW

  | EUNKNOWNERR of int
[@@deprecated "[since 2016-10] use [Unix.Error.t] instead"]

val sexp_of_error : Unix.error -> Sexp.t
[@@deprecated "[since 2016-10] use [Unix.Error.t] instead"]
val error_of_sexp : Sexp.t -> Unix.error
[@@deprecated "[since 2016-10] use [Unix.Error.t] instead"]

module Error : sig
  (** The type of error codes.  Errors defined in the POSIX standard and additional
      errors, mostly BSD.  All other errors are mapped to [EUNKNOWNERR]. *)
  type t = Unix.error =
    | E2BIG               (** Argument list too long *)
    | EACCES              (** Permission denied *)
    | EAGAIN              (** Resource temporarily unavailable; try again *)
    | EBADF               (** Bad file descriptor *)
    | EBUSY               (** Resource unavailable *)
    | ECHILD              (** No child process *)
    | EDEADLK             (** Resource deadlock would occur *)
    | EDOM                (** Domain error for math functions, etc. *)
    | EEXIST              (** File exists *)
    | EFAULT              (** Bad address *)
    | EFBIG               (** File too large *)
    | EINTR               (** Function interrupted by signal *)
    | EINVAL              (** Invalid argument *)
    | EIO                 (** Hardware I/O error *)
    | EISDIR              (** Is a directory *)
    | EMFILE              (** Too many open files by the process *)
    | EMLINK              (** Too many links *)
    | ENAMETOOLONG        (** Filename too long *)
    | ENFILE              (** Too many open files in the system *)
    | ENODEV              (** No such device *)
    | ENOENT              (** No such file or directory *)
    | ENOEXEC             (** Not an executable file *)
    | ENOLCK              (** No locks available *)
    | ENOMEM              (** Not enough memory *)
    | ENOSPC              (** No space left on device *)
    | ENOSYS              (** Function not supported *)
    | ENOTDIR             (** Not a directory *)
    | ENOTEMPTY           (** Directory not empty *)
    | ENOTTY              (** Inappropriate I/O control operation *)
    | ENXIO               (** No such device or address *)
    | EPERM               (** Operation not permitted *)
    | EPIPE               (** Broken pipe *)
    | ERANGE              (** Result too large *)
    | EROFS               (** Read-only file system *)
    | ESPIPE              (** Invalid seek e.g. on a pipe *)
    | ESRCH               (** No such process *)
    | EXDEV               (** Invalid link *)

    | EWOULDBLOCK         (** Operation would block *)
    | EINPROGRESS         (** Operation now in progress *)
    | EALREADY            (** Operation already in progress *)
    | ENOTSOCK            (** Socket operation on non-socket *)
    | EDESTADDRREQ        (** Destination address required *)
    | EMSGSIZE            (** Message too long *)
    | EPROTOTYPE          (** Protocol wrong type for socket *)
    | ENOPROTOOPT         (** Protocol not available *)
    | EPROTONOSUPPORT     (** Protocol not supported *)
    | ESOCKTNOSUPPORT     (** Socket type not supported *)
    | EOPNOTSUPP          (** Operation not supported on socket *)
    | EPFNOSUPPORT        (** Protocol family not supported *)
    | EAFNOSUPPORT        (** Address family not supported by protocol family *)
    | EADDRINUSE          (** Address already in use *)
    | EADDRNOTAVAIL       (** Can't assign requested address *)
    | ENETDOWN            (** Network is down *)
    | ENETUNREACH         (** Network is unreachable *)
    | ENETRESET           (** Network dropped connection on reset *)
    | ECONNABORTED        (** Software caused connection abort *)
    | ECONNRESET          (** Connection reset by peer *)
    | ENOBUFS             (** No buffer space available *)
    | EISCONN             (** Socket is already connected *)
    | ENOTCONN            (** Socket is not connected *)
    | ESHUTDOWN           (** Can't send after socket shutdown *)
    | ETOOMANYREFS        (** Too many references: can't splice *)
    | ETIMEDOUT           (** Connection timed out *)
    | ECONNREFUSED        (** Connection refused *)
    | EHOSTDOWN           (** Host is down *)
    | EHOSTUNREACH        (** No route to host *)
    | ELOOP               (** Too many levels of symbolic links *)
    | EOVERFLOW           (** File size or position not representable *)

    | EUNKNOWNERR of int  (** Unknown error *)
  [@@deriving sexp]

  val of_system_int : errno:int -> t

  (** Return a string describing the given error code. *)
  val message : t -> string
end

(** Raised by the system calls below when an error is encountered.
    The first component is the error code; the second component
    is the function name; the third component is the string parameter
    to the function, if it has one, or the empty string otherwise. *)
exception Unix_error of Error.t * string * string

module Syscall_result :
  module type of Syscall_result with type 'a t = 'a Syscall_result.t

(** @raise Unix_error with a given errno, function name and argument *)
external unix_error : int -> string -> string -> _ = "unix_error_stub"

val error_message : Error.t -> string
[@@deprecated "[since 2016-10] use [Unix.Error.message] instead"]

(** [handle_unix_error f] runs [f ()] and returns the result.  If the exception
    [Unix_error] is raised, it prints a message describing the error and exits with code
    2. *)
val handle_unix_error : (unit -> 'a) -> 'a

(** [retry_until_no_eintr f] returns [f ()] unless [f ()] fails with [EINTR]; in which
    case [f ()] is run again until it raises a different error or returns a value. *)
val retry_until_no_eintr : (unit -> 'a) -> 'a

(** {6 Access to the process environment}

    If you're looking for [getenv], that's in the Sys module.
*)

(** Return the process environment, as an array of strings
    with the format ``variable=value''. *)
val environment : unit -> string array

(** [Unix.putenv ~key ~data] sets the value associated to a
    variable in the process environment.
    [key] is the name of the environment variable,
    and [data] its new associated value. *)
val putenv : key : string -> data : string -> unit

(**
   [unsetenv name] deletes the variable [name] from the environment.

   EINVAL [name] contained an ’=’ or an '\000' character.
*)
val unsetenv : string -> unit

(** {6 Process handling} *)

(** The termination status of a process. *)
module Exit : sig
  type error = [ `Exit_non_zero of int ] [@@deriving compare, sexp]

  type t = (unit, error) Result.t [@@deriving compare, sexp]

  val to_string_hum : t -> string

  val code : t -> int
  val of_code : int -> t
  val or_error : t -> unit Or_error.t
end

module Exit_or_signal : sig
  type error = [ Exit.error | `Signal of Signal.t ] [@@deriving compare, sexp]

  type t = (unit, error) Result.t [@@deriving compare, sexp]

  (** [of_unix] assumes that any signal numbers in the incoming value are O'Caml internal
      signal numbers. *)
  val of_unix : Unix.process_status -> t

  val to_string_hum : t -> string
  val or_error : t -> unit Or_error.t
end

module Exit_or_signal_or_stop : sig
  type error = [ Exit_or_signal.error | `Stop of Signal.t ] [@@deriving sexp]

  type t = (unit, error) Result.t [@@deriving sexp]

  (** [of_unix] assumes that any signal numbers in the incoming value are O'Caml internal
      signal numbers. *)
  val of_unix : Unix.process_status -> t

  val to_string_hum : t -> string
  val or_error : t -> unit Or_error.t
end

(** [env] is used to control the environment of a child process, and can take three forms.
    [`Replace_raw] replaces the entire environment with strings in the Unix style, like
    ["VARIABLE_NAME=value"].  [`Replace] has the same effect as [`Replace_raw], but using
    bindings represented as ["VARIABLE_NAME", "value"].  [`Extend] adds entries to the
    existing environment rather than replacing the whole environment.

    If [env] contains multiple bindings for the same variable, the last takes precedence.
    In the case of [`Extend], bindings in [env] take precedence over the existing
    environment. *)
type env = [ `Replace of (string * string) list
           | `Extend of (string * string) list
           | `Replace_raw of string list
           ]
[@@deriving sexp]

(** [exec ~prog ~argv ?search_path ?env] execs [prog] with [argv].  If [use_path = true]
    (the default) and [prog] doesn't contain a slash, then [exec] searches the [PATH]
    environment variable for [prog].  If [env] is supplied, it determines the environment
    when [prog] is executed.

    The first element in [argv] should be the program itself; the correct way to call
    [exec] is:

    {[    exec ~prog ~argv:[ prog; arg1; arg2; ...] ()    ]} *)
val exec
  :  prog:string
  -> argv:string list
  -> ?use_path:bool  (** default is [true] *)
  -> ?env:env
  -> unit
  -> never_returns

(** [fork_exec ~prog ~argv ?use_path ?env ()] forks and execs [prog] with [argv] in the
    child process, returning the child PID to the parent. As in [exec], the first element
    in [argv] should be the program itself. *)
val fork_exec
  :  prog:string
  -> argv:string list
  -> ?use_path:bool  (** default is [true] *)
  -> ?env:env
  -> unit
  -> Pid.t

(** [fork ()] forks a new process.  The return value indicates whether we are continuing
    in the child or the parent, and if the parent, includes the child's process id. *)
val fork : unit -> [ `In_the_child | `In_the_parent of Pid.t ]

(** [wait{,_nohang,_untraced,_nohang_untraced} ?restart wait_on] is a family of functions
    that wait on a process to exit (normally or via a signal) or be stopped by a signal
    (if [untraced] is used).  The [wait_on] argument specifies which processes to wait on.
    The [nohang] variants return [None] immediately if no such process exists.  If
    [nohang] is not used, [waitpid] will block until one of the desired processes exits.

    The non-nohang variants have a [restart] flag with (default true) that causes the
    system call to be retried upon EAGAIN|EINTR.  The nohang variants do not have this
    flag because they don't block. *)


type wait_on =
  [ `Any
  | `Group of Pid.t
  | `My_group
  | `Pid of Pid.t
  ]
[@@deriving sexp]


val wait
  : ?restart:bool (** defaults to true *) -> wait_on ->  Pid.t * Exit_or_signal.t
val wait_nohang
  :                                          wait_on -> (Pid.t * Exit_or_signal .t       ) option
val wait_untraced
  : ?restart:bool (** defaults to true *) -> wait_on ->  Pid.t * Exit_or_signal_or_stop.t
val wait_nohang_untraced
  :                                          wait_on -> (Pid.t * Exit_or_signal_or_stop.t) option

(** [waitpid pid] waits for child process [pid] to terminate, and returns its exit status.
    [waitpid_exn] is like [waitpid], except it only returns if the child exits with status
    zero, and raises if the child terminates in any other way. *)
val waitpid : Pid.t -> Exit_or_signal.t
val waitpid_exn : Pid.t -> unit

(** Execute the given command, wait until it terminates, and return
    its termination status. The string is interpreted by the shell
    [/bin/sh] and therefore can contain redirections, quotes, variables,
    etc. The result [WEXITED 127] indicates that the shell couldn't
    be executed. *)
val system : string -> Exit_or_signal.t

(** Return the pid of the process. *)
val getpid : unit -> Pid.t

(** Return the pid of the parent process. *)
val getppid : unit -> Pid.t option

(** Return the pid of the parent process, if you're really sure
    you're never going to be the init process.
*)
val getppid_exn : unit -> Pid.t

module Thread_id : sig
  type t [@@deriving sexp_of, bin_io]
  include Comparable.S with type t := t
  val to_int : t -> int
end

(** Get the numeric ID of the current thread, e.g. for identifying it in top(1). *)
val gettid : (unit -> Thread_id.t) Or_error.t

(** Change the process priority. The integer argument is added to the
    ``nice'' value. (Higher values of the ``nice'' value mean
    lower priorities.) Return the new nice value. *)
val nice : int -> int

(** {6 Basic file input/output} *)

(** The abstract type of file descriptors. *)

(** File descriptor for standard input.*)
val stdin : File_descr.t

(** File descriptor for standard output.*)
val stdout : File_descr.t

(** File descriptor for standard standard error. *)
val stderr : File_descr.t

(** The flags to {!UnixLabels.openfile}. *)

type open_flag =
  Unix.open_flag =
    O_RDONLY        (** Open for reading *)
  | O_WRONLY        (** Open for writing *)
  | O_RDWR          (** Open for reading and writing *)
  | O_NONBLOCK      (** Open in non-blocking mode *)
  | O_APPEND        (** Open for append *)
  | O_CREAT         (** Create if nonexistent *)
  | O_TRUNC         (** Truncate to 0 length if existing *)
  | O_EXCL          (** Fail if existing *)
  | O_NOCTTY        (** Don't make this dev a controlling tty *)
  | O_DSYNC         (** Writes complete as `Synchronised I/O data integrity completion' *)
  | O_SYNC          (** Writes complete as `Synchronised I/O file integrity completion' *)
  | O_RSYNC         (** Reads complete as writes (depending on O_SYNC/O_DSYNC) *)
  | O_SHARE_DELETE  (** Windows only: allow the file to be deleted while still open *)
  | O_CLOEXEC       (** Set the close-on-exec flag on the descriptor returned by {!openfile} *)
  | O_KEEPEXEC [@if ocaml_version >= (4, 05, 0)]

(** We can't use [with sexp] because pa_sexp inserts two copies of the [val] specs, which
    leads to a spurious "unused" warning. *)
val open_flag_of_sexp : Sexp.t -> open_flag
val sexp_of_open_flag : open_flag -> Sexp.t

(** file access rights *)
type file_perm = int [@@deriving sexp]

(** Open the named file with the given flags. Third argument is the permissions to give to
    the file if it is created. Return a file descriptor on the named file. Default
    permissions 0o644. *)
val openfile : ?perm:file_perm -> mode:open_flag list -> string -> File_descr.t

module Open_flags : sig
  (** [Open_flags.t] represents the flags associated with a file descriptor in the
      open-file-descriptor table.  It deals with the same thing as OCaml's [open_flag]
      type; however, it uses Core's [Flags] approach and the underlying integer bitmask
      representation, and so interoperates more smoothly with C. *)
  type t [@@deriving sexp_of]

  include Flags.S with type t := t

  (** access mode.

      These three flags are not individual bits like flags usually are.  The access mode
      is represented by the lower two bits of the [Open_flags.t].  A particular
      [Open_flags.t] should include exactly one access mode.  Combining different
      [Open_flags.t]'s using flags operations (e.g [+]) is only sensible if they have the
      same access mode. *)
  val rdonly   : t
  val wronly   : t
  val rdwr     : t

  (** creation *)
  val creat    : t
  val excl     : t
  val noctty   : t
  val trunc    : t

  val append   : t
  val nonblock : t

  val dsync    : t
  val sync     : t
  val rsync    : t

  (** [can_read t] iff [t] has [rdonly] or [rdwr] *)
  val can_read  : t -> bool

  (** [can_read t] iff [t] has [wronly] or [rdwr] *)
  val can_write : t -> bool
end

(** [fcntl_getfl fd] gets the current flags for [fd] from the open-file-descriptor table
    via the system call [fcntl(fd, F_GETFL)].  See "man fcntl". *)
val fcntl_getfl : File_descr.t -> Open_flags.t

(** [fcntl_setfl fd flags] sets the flags for [fd] in the open-file-descriptor table via
    the system call [fcntl(fd, F_SETFL, flags)].  See "man fcntl".  As per the Linux man
    page, on Linux this only allows [append] and [nonblock] to be set. *)
val fcntl_setfl : File_descr.t -> Open_flags.t -> unit

(** Close a file descriptor. *)
val close : ?restart:bool (** defaults to false *) -> File_descr.t -> unit

(** [with_file file ~mode ~perm ~f] opens [file], and applies [f] to the resulting file
    descriptor.  When [f] finishes (or raises), [with_file] closes the descriptor and
    returns the result of [f] (or raises). *)
val with_file
  :  ?perm:file_perm
  -> string
  -> mode:open_flag list
  -> f:(File_descr.t -> 'a)
  -> 'a

(** [read fd buff ofs len] reads [len] characters from descriptor
    [fd], storing them in string [buff], starting at position [ofs]
    in string [buff]. Return the number of characters actually read. *)
val read
  :  ?restart:bool (** defaults to false *)
  -> ?pos:int
  -> ?len:int
  -> File_descr.t
  -> buf:Bytes.t
  -> int

(** [write fd buff ofs len] writes [len] characters to descriptor
    [fd], taking them from bytes [buff], starting at position [ofs]
    in bytes [buff]. Return the number of characters actually
    written.

    When an error is reported some characters might have already been
    written.  Use [single_write] instead to ensure that this is not the
    case.

    WARNING: write is an interruptible call and has no way to handle
    EINTR properly. You should most probably be using single write.
*)
val write : ?pos:int -> ?len:int -> File_descr.t -> buf:Bytes.t -> int

(** Same as [write] but with a string buffer. *)
val write_substring : ?pos:int -> ?len:int -> File_descr.t -> buf:string -> int

(** Same as [write] but ensures that all errors are reported and
    that no character has ever been written when an error is reported. *)
val single_write
  :  ?restart:bool (** defaults to false *)
  -> ?pos:int
  -> ?len:int
  -> File_descr.t
  -> buf:Bytes.t
  -> int

(** Same as [single_write] but with a string buffer. *)
val single_write_substring
  :  ?restart:bool (** defaults to false *)
  -> ?pos:int
  -> ?len:int
  -> File_descr.t
  -> buf:string
  -> int

(** {6 Interfacing with the standard input/output library} *)


(** Create an input channel reading from the given descriptor.
    The channel is initially in binary mode; use
    [set_binary_mode_in ic false] if text mode is desired. *)
val in_channel_of_descr : File_descr.t -> In_channel.t

(** Create an output channel writing on the given descriptor.
    The channel is initially in binary mode; use
    [set_binary_mode_out oc false] if text mode is desired. *)
val out_channel_of_descr : File_descr.t -> Out_channel.t

(** Return the descriptor corresponding to an input channel. *)
val descr_of_in_channel : In_channel.t -> File_descr.t

(** Return the descriptor corresponding to an output channel. *)
val descr_of_out_channel : Out_channel.t -> File_descr.t


(** {6 Seeking and truncating} *)

(** POSITIONING modes for {!UnixLabels.lseek}. *)
type seek_command =
  Unix.seek_command =
    SEEK_SET (** indicates positions relative to the beginning of the file *)
  | SEEK_CUR (** indicates positions relative to the current position *)
  | SEEK_END (** indicates positions relative to the end of the file *)
[@@deriving sexp]

(** Set the current position for a file descriptor *)
val lseek : File_descr.t -> int64 -> mode:seek_command -> int64

(** Truncates the named file to the given size. *)
val truncate : string -> len:int64 -> unit

(** Truncates the file corresponding to the given descriptor
    to the given size. *)
val ftruncate : File_descr.t -> len:int64 -> unit

(** {6 File statistics} *)

type file_kind =
  Unix.file_kind =
    S_REG                   (** Regular file *)
  | S_DIR                   (** Directory *)
  | S_CHR                   (** Character device *)
  | S_BLK                   (** Block device *)
  | S_LNK                   (** Symbolic link *)
  | S_FIFO                  (** Named pipe *)
  | S_SOCK                  (** Socket *)
[@@deriving sexp]

(** The informations returned by the {!UnixLabels.stat} calls.  The times are [float]
    number of seconds since the epoch; we don't use [Time.t] because [Time] depends on
    [Unix], so the fix isn't so trivial.  Same for [Native_file.stats] below. *)
type stats =
  Unix.LargeFile.stats =
  { st_dev : int;                       (** Device number *)
    st_ino : int;                       (** Inode number *)
    st_kind : file_kind;                (** Kind of the file *)
    st_perm : file_perm;                (** Access rights *)
    st_nlink : int;                     (** Number of links *)
    st_uid : int;                       (** User id of the owner *)
    st_gid : int;                       (** Group ID of the file's group *)
    st_rdev : int;                      (** Device minor number *)
    st_size : int64;                    (** Size in bytes *)
    st_atime : float;                   (** Last access time *)
    st_mtime : float;                   (** Last modification time *)
    st_ctime : float                    (** Last status change time *)
  } [@@deriving sexp]

(** Return the information for the named file. *)
val stat : string -> stats

(** Same as {!UnixLabels.stat}, but in case the file is a symbolic link,
    return the information for the link itself. *)
val lstat : string -> stats

(** Return the information for the file associated with the given
    descriptor. *)
val fstat : File_descr.t -> stats

(** This sub-module provides the normal OCaml Unix functions that deal with file size
    using native ints.  These are here because, in general, you should be using 64bit
    file operations so that large files aren't an issue.  If you have a real need to
    use potentially 31bit file operations (and you should be dubious of such a need) you
    can open this module *)
module Native_file : sig
  (** The informations returned by the {!UnixLabels.stat} calls. *)
  type stats = Unix.stats =
    { st_dev : int;                       (** Device number *)
      st_ino : int;                       (** Inode number *)
      st_kind : file_kind;                (** Kind of the file *)
      st_perm : file_perm;                (** Access rights *)
      st_nlink : int;                     (** Number of links *)
      st_uid : int;                       (** User id of the owner *)
      st_gid : int;                       (** Group ID of the file's group *)
      st_rdev : int;                      (** Device minor number *)
      st_size : int;                      (** Size in bytes *)
      st_atime : float;                   (** Last access time *)
      st_mtime : float;                   (** Last modification time *)
      st_ctime : float                    (** Last status change time *)
    }
  [@@deriving sexp]

  (** Return the information for the named file. *)
  val stat : string -> stats

  (** Same as {!UnixLabels.stat}, but in case the file is a symbolic link,
      return the information for the link itself. *)
  val lstat : string -> stats

  (** Return the information for the file associated with the given
      descriptor. *)
  val fstat : File_descr.t -> stats

  val lseek : File_descr.t -> int -> mode:seek_command -> int
  val truncate : string -> len:int -> unit
  val ftruncate : File_descr.t -> len:int -> unit
end

(** {6 Locking} *)


(** Commands for {!lockf}. *)
type lock_command =
  Unix.lock_command =
    F_ULOCK  (** Unlock a region *)
  | F_LOCK   (** Lock a region for writing, and block if already locked *)
  | F_TLOCK  (** Lock a region for writing, or fail if already locked *)
  | F_TEST   (** Test a region for other process locks *)
  | F_RLOCK  (** Lock a region for reading, and block if already locked *)
  | F_TRLOCK (** Lock a region for reading, or fail if already locked *)
[@@deriving sexp]

(** [lockf fd cmd size] place a lock on a file_descr that prevents any other process from
 * calling lockf successfully on the same file.  Due to a limitation in the current
 * implementation the length will be converted to a native int, potentially throwing an
 * exception if it is too large. *)
val lockf : File_descr.t -> mode:lock_command -> len:Int64.t -> unit

module Flock_command : sig
  type t

  val lock_shared : t
  val lock_exclusive : t
  val unlock : t
end

(** [flock fd cmd] places or releases a lock on the fd as per the flock C call of the same
    name. *)
val flock : File_descr.t -> Flock_command.t -> bool

(** Return [true] if the given file descriptor refers to a terminal or
    console window, [false] otherwise. *)
val isatty : File_descr.t -> bool

(** {6 Operations on file names} *)


(** Removes the named file *)
val unlink : string -> unit

(** Removes the named file or directory *)
val remove : string -> unit

(** [rename old new] changes the name of a file from [old] to [new]. *)
val rename : src:string -> dst:string -> unit

(** [link ?force ~target ~link_name ()] creates a hard link named [link_name]
    to the file named [target].  If [force] is true, an existing entry in
    place of [link_name] will be unlinked.  This unlinking may raise a Unix
    error, e.g. if the entry is a directory. *)
val link
  :  ?force:bool (** defaults to false *)
  -> target : string
  -> link_name : string
  -> unit
  -> unit

(** {6 File permissions and ownership} *)

(** Change the permissions of the named file. *)
val chmod : string -> perm:file_perm -> unit

(** Change the permissions of an opened file. *)
val fchmod : File_descr.t -> perm:file_perm -> unit

(** Change the owner uid and owner gid of the named file. *)
val chown : string -> uid:int -> gid:int -> unit

(** Change the owner uid and owner gid of an opened file. *)
val fchown : File_descr.t -> uid:int -> gid:int -> unit

(** Set the process creation mask, and return the previous mask. *)
val umask : int -> int

(** Check that the process has the given permissions over the named file. *)
val access
  :  string
  -> [ `Read | `Write | `Exec | `Exists ] list
  -> (unit, exn) Result.t

val access_exn
  :  string
  -> [ `Read | `Write | `Exec | `Exists ] list
  -> unit

(** {6 Operations on file descriptors} *)

(** Return a new file descriptor referencing the same file as
    the given descriptor. *)
val dup : File_descr.t -> File_descr.t

(** [dup2 ~src ~dst] duplicates [src] to [dst], closing [dst] if already
    opened. *)
val dup2 : src:File_descr.t -> dst:File_descr.t -> unit

(** Set the ``non-blocking'' flag on the given descriptor.
    When the non-blocking flag is set, reading on a descriptor
    on which there is temporarily no data available raises the
    [EAGAIN] or [EWOULDBLOCK] error instead of blocking;
    writing on a descriptor on which there is temporarily no room
    for writing also raises [EAGAIN] or [EWOULDBLOCK]. *)
val set_nonblock : File_descr.t -> unit

(** Clear the ``non-blocking'' flag on the given descriptor.
    See {!UnixLabels.set_nonblock}.*)
val clear_nonblock : File_descr.t -> unit

(** Set the ``close-on-exec'' flag on the given descriptor.
    A descriptor with the close-on-exec flag is automatically
    closed when the current process starts another program with
    one of the [exec] functions. *)
val set_close_on_exec : File_descr.t -> unit

(** Clear the ``close-on-exec'' flag on the given descriptor.
    See {!UnixLabels.set_close_on_exec}.*)
val clear_close_on_exec : File_descr.t -> unit

(** {6 Directories} *)

(** Create a directory.  The permissions of the created directory are (perm & ~umask &
    0777).  The default perm is 0777. *)
val mkdir : ?perm:file_perm -> string -> unit

(** Create a directory recursively.  The permissions of the created directory are
    those granted by [mkdir ~perm]. *)
val mkdir_p : ?perm:file_perm -> string -> unit

(** Remove an empty directory. *)
val rmdir : string -> unit

(** Change the process working directory. *)
val chdir : string -> unit

(** Return the name of the current working directory. *)
val getcwd : unit -> string

(** Change the process root directory. *)
val chroot : string -> unit

(** The type of descriptors over opened directories. *)
type dir_handle = Unix.dir_handle

(** Open a descriptor on a directory *)
val opendir : ?restart:bool (** defaults to false *) -> string -> dir_handle

(** Return the next entry in a directory.  Returns [None] when the end of the directory
    has been reached. *)
val readdir_opt : dir_handle -> string option

(** Same as [readdir_opt] except that it signals the end of the directory by raising
    [End_of_file]. *)
val readdir : dir_handle -> string
[@@deprecated "[since 2016-08] use [readdir_opt] instead"]

(** Reposition the descriptor to the beginning of the directory *)
val rewinddir : dir_handle -> unit

(** Close a directory descriptor. *)
val closedir : dir_handle -> unit

(** {6 Pipes and redirections} *)

(** Create a pipe. The first component of the result is opened
    for reading, that's the exit to the pipe. The second component is
    opened for writing, that's the entrance to the pipe. *)
val pipe : unit -> File_descr.t * File_descr.t

(** Create a named pipe with the given permissions. *)
val mkfifo : string -> perm:file_perm -> unit


(** {6 High-level process and redirection management} *)

module Process_info : sig
  type t =
    { pid: Pid.t;
      stdin: File_descr.t;
      stdout: File_descr.t;
      stderr: File_descr.t
    }
  [@@deriving sexp]
end

(** Low-level process *)

(** [create_process ~prog ~args] forks a new process that executes the program [prog] with
    arguments [args].  The function returns the pid of the process along with file
    descriptors attached to stdin, stdout, and stderr of the new process.  The executable
    file [prog] is searched for in the path.  The new process has the same environment as
    the current process.  Unlike in [execve] the program name is automatically passed as
    the first argument. *)
val create_process : prog : string -> args : string list -> Process_info.t

(** [create_process_env ~prog ~args ~env] as [create_process], but takes an additional
    parameter that extends or replaces the current environment.  No effort is made to
    ensure that the keys passed in as env are unique, so if an environment variable is set
    twice the second version will override the first. *)
val create_process_env
  :  ?working_dir : string
  -> prog : string
  -> args : string list
  -> env : env
  -> unit
  -> Process_info.t

(** High-level pipe and process management. These functions
    (with {!UnixLabels.open_process_out} and {!UnixLabels.open_process})
    run the given command in parallel with the program,
    and return channels connected to the standard input and/or
    the standard output of the command. The command is interpreted
    by the shell [/bin/sh] (cf. [system]). Warning: writes on channels
    are buffered, hence be careful to call {!Pervasives.flush} at the right times
    to ensure correct synchronization. *)
val open_process_in : string -> In_channel.t

(** See {!UnixLabels.open_process_in}. *)
val open_process_out : string -> Out_channel.t

(** See {!UnixLabels.open_process_in}. *)
val open_process : string -> In_channel.t * Out_channel.t

(** Similar to {!UnixLabels.open_process}, but the second argument specifies
    the environment passed to the command.  The result is a triple
    of channels connected to the standard output, standard input,
    and standard error of the command. *)
module Process_channels : sig
  type t =
    { stdin : Out_channel.t;
      stdout : In_channel.t;
      stderr : In_channel.t;
    }
end

val open_process_full : string -> env:string array -> Process_channels.t

(** [close_process_*] raises [Unix_error] if, for example, the file descriptor has already
    been closed. *)

(** Close channels opened by {!UnixLabels.open_process_in},
    wait for the associated command to terminate,
    and return its termination status.
*)
val close_process_in : In_channel.t -> Exit_or_signal.t

(** Close channels opened by {!UnixLabels.open_process_out},
    wait for the associated command to terminate,
    and return its termination status.
*)
val close_process_out : Out_channel.t -> Exit_or_signal.t

(** Close channels opened by {!UnixLabels.open_process},
    wait for the associated command to terminate,
    and return its termination status. *)
val close_process : In_channel.t * Out_channel.t -> Exit_or_signal.t

(** Close channels opened by {!UnixLabels.open_process_full},
    wait for the associated command to terminate,
    and return its termination status. *)
val close_process_full : Process_channels.t -> Exit_or_signal.t

(** {6 Symbolic links} *)

(** [symlink source dest] creates the file [dest] as a symbolic link
    to the file [source].
    On Windows, this has the semantics using [stat] as described at:
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html
*)
val symlink : src:string -> dst:string -> unit

(** Read the contents of a link. *)
val readlink : string -> string

(** {6 Polling} *)

module Select_fds : sig
  type t =
    { read   : File_descr.t list;
      write  : File_descr.t list;
      except : File_descr.t list;
    }
  [@@deriving sexp_of]

  val empty : t
end

type select_timeout = [ `Never | `Immediately | `After of Core_kernel.Time_ns.Span.t ]
[@@deriving sexp_of]

(** Wait until some input/output operations become possible on some channels.  The three
    list arguments are a set of descriptors to check for reading, for writing, or for
    exceptional conditions.  [~timeout] is the maximal timeout.  The result is composed of
    three sets of descriptors: those ready for reading, ready for writing, and over which
    an exceptional condition is pending.

    Setting restart to true means that we want [select] to restart automatically on EINTR
    (instead of propagating the exception)... *)
val select
  :  ?restart : bool  (** defaults to [false] *)
  -> read     : File_descr.t list
  -> write    : File_descr.t list
  -> except   : File_descr.t list
  -> timeout  : select_timeout
  -> unit
  -> Select_fds.t

(** Wait until a non-ignored, non-blocked signal is delivered. *)
val pause : unit -> unit

(** {6 Time functions} *)

(** The execution times (CPU times) of a process. *)
type process_times =
  Unix.process_times =
  { tms_utime : float;          (** User time for the process *)
    tms_stime : float;          (** System time for the process *)
    tms_cutime : float;         (** User time for the children processes *)
    tms_cstime : float;         (** System time for the children processes *)
  }
[@@deriving sexp]

module Clock : sig
  (** Supported clocks. See clock_gettime(3) man pages for semantics. *)
  type t =
    | Realtime
    | Monotonic
    | Process_cpu
    | Process_thread

  (** Return the resolution of the given clock in nanoseconds. *)
  val getres : (t -> Int63.t) Or_error.t

  (** Return the current time of the given clock since 00:00:00 UTC, Jan. 1,
      1970, in nanoseconds. *)
  val gettime : (t -> Int63.t) Or_error.t
end

(** The type representing wallclock time and calendar date. *)
type tm =
  Unix.tm =
  { tm_sec : int;                       (** Seconds 0..59 *)
    tm_min : int;                       (** Minutes 0..59 *)
    tm_hour : int;                      (** Hours 0..23 *)
    tm_mday : int;                      (** Day of month 1..31 *)
    tm_mon : int;                       (** Month of year 0..11 *)
    tm_year : int;                      (** Year - 1900 *)
    tm_wday : int;                      (** Day of week (Sunday is 0) *)
    tm_yday : int;                      (** Day of year 0..365 *)
    tm_isdst : bool;                    (** Daylight time savings in effect *)
  }
[@@deriving sexp]

(** Return the current time since 00:00:00 GMT, Jan. 1, 1970, in seconds. *)
val time : unit -> float

(** Same as {!time} above, but with resolution better than 1 second. *)
val gettimeofday : unit -> float

(** Convert a time in seconds, as returned by {!UnixLabels.time}, into a date and
    a time. Assumes UTC. *)
val gmtime : float -> tm

(** Convert a UTC time in a tm record to a time in seconds *)
val timegm : tm -> float

(** Convert a time in seconds, as returned by {!UnixLabels.time}, into a date and
    a time. Assumes the local time zone. *)
val localtime : float -> tm

(** Convert a date and time, specified by the [tm] argument, into
    a time in seconds, as returned by {!UnixLabels.time}. Also return a normalized
    copy of the given [tm] record, with the [tm_wday], [tm_yday],
    and [tm_isdst] fields recomputed from the other fields.
    The [tm] argument is interpreted in the local time zone. *)
val mktime : tm -> float * tm

(** Convert a date and time, specified by the [tm] argument, into a formatted string.
    See 'man strftime' for format options. *)
val strftime : tm -> string -> string

(** Given a format string, convert a corresponding string to a date and time
    See 'man strptime' for format options. *)
val strptime : fmt:string -> string -> Unix.tm

(** Schedule a [SIGALRM] signal after the given number of seconds. *)
val alarm : int -> int

(** Stop execution for the given number of seconds. *)
val sleep : int -> unit

(** [nanosleep f] delays execution of the program for at least [f] seconds.  The function
    can return earlier if a signal has been delivered, in which case the number of seconds
    left is returned.  Any other failure raises an exception. *)
val nanosleep : float -> float

(** Return the execution times of the process. *)
val times : unit -> process_times

(** Set the last access time (second arg) and last modification time
    (third arg) for a file. Times are expressed in seconds from
    00:00:00 GMT, Jan. 1, 1970. *)
val utimes : string -> access:float -> modif:float -> unit

(** The three kinds of interval timers. *)
type interval_timer =
  Unix.interval_timer =
    ITIMER_REAL
  (** decrements in real time, and sends the signal [SIGALRM] when expired.*)
  | ITIMER_VIRTUAL
  (**  decrements in process virtual time, and sends [SIGVTALRM] when expired. *)
  | ITIMER_PROF
  (** (for profiling) decrements both when the process
      is running and when the system is running on behalf of the
      process; it sends [SIGPROF] when expired. *)
[@@deriving sexp]

(** The type describing the status of an interval timer *)
type interval_timer_status =
  Unix.interval_timer_status =
  { it_interval : float;          (** Period *)
    it_value : float;             (** Current value of the timer *)
  }
[@@deriving sexp]

(** Return the current status of the given interval timer. *)
val getitimer : interval_timer -> interval_timer_status

(** [setitimer t s] sets the interval timer [t] and returns
    its previous status. The [s] argument is interpreted as follows:
    [s.it_value], if nonzero, is the time to the next timer expiration;
    [s.it_interval], if nonzero, specifies a value to
    be used in reloading it_value when the timer expires.
    Setting [s.it_value] to zero disable the timer.
    Setting [s.it_interval] to zero causes the timer to be disabled
    after its next expiration. *)
val setitimer : interval_timer -> interval_timer_status -> interval_timer_status


(** {6 User id, group id}
    It's highly recommended to read the straight unix docs on these functions for more
    color. You can get that info from man pages or
    http://www.opengroup.org/onlinepubs/000095399/functions/setuid.html
*)


(** Return the user id of the user executing the process. *)
val getuid : unit -> int

(** Return the effective user id under which the process runs. *)
val geteuid : unit -> int

(** Sets the real user id and effective user id for the process. Only use this when
    superuser. To setuid as an ordinary user, see Core_extended.Unix.seteuid. *)
val setuid : int -> unit

(** Return the group id of the user executing the process. *)
val getgid : unit -> int

(** Return the effective group id under which the process runs. *)
val getegid : unit -> int

(** Set the real group id and effective group id for the process. *)
val setgid : int -> unit

(** Structure of entries in the [passwd] database *)
module Passwd : sig
  type t =
    { name : string;
      passwd : string;
      uid : int;
      gid : int;
      gecos : string;
      dir : string;
      shell : string;
    }
  [@@deriving compare, sexp]

  val getbyname : string -> t option
  val getbyname_exn : string -> t

  val getbyuid : int -> t option
  val getbyuid_exn : int -> t

  (** [getpwents] is a thread-safe wrapper over the low-level passwd database functions.
      The order in which the results are returned is not deterministic. *)
  val getpwents : unit -> t list

  module Low_level : sig
    (** These functions may not be thread safe.
        Use [getpwents], above, if possible. *)

    val setpwent : unit -> unit
    val getpwent : unit -> t option
    val getpwent_exn : unit -> t
    val endpwent : unit -> unit
  end
end

(** Structure of entries in the [groups] database. *)
module Group : sig
  type t =
    { name : string;
      passwd : string;
      gid : int;
      mem : string array;
    }
  [@@deriving sexp_of]

  val getbyname : string -> t option
  val getbyname_exn : string -> t
  val getbygid : int -> t option
  val getbygid_exn : int -> t
end

(** Return the login name of the user executing the process. *)
val getlogin : unit -> string

module Protocol_family : sig
  type t = [ `Unix | `Inet | `Inet6 ] [@@deriving bin_io, sexp]
end

(** {6 Internet addresses} *)

module Inet_addr : sig
  type t = Unix.inet_addr [@@deriving bin_io, compare, hash, sexp_of]

  (** [t_of_sexp] is deprecated because it used to block to do a DNS lookup, and we don't
      want a sexp converter to do that.  As we transition away, one can use
      [Blocking_sexp], which has the old behavior. *)
  val t_of_sexp : Sexp.t -> t
  [@@deprecated "[since 2015-10] Replace [t] by [Blocking_sexp.t]"]

  (** [Blocking_sexp] performs DNS lookup to resolve hostnames to IP addresses. *)
  module Blocking_sexp : sig
    type t = Unix.inet_addr [@@deriving bin_io, compare, hash, sexp]
  end

  include Comparable.S with type t := t

  (** Conversion from the printable representation of an Internet address to its internal
      representation.  The argument string consists of 4 numbers separated by periods
      ([XXX.YYY.ZZZ.TTT]) for IPv4 addresses, and up to 8 numbers separated by colons for
      IPv6 addresses.  Raise [Failure] when given a string that does not match these
      formats. *)
  val of_string : string -> t

  (** Call [of_string] and if that fails, use [Host.getbyname]. *)
  val of_string_or_getbyname : string -> t

  (** Return the printable representation of the given Internet address.  See [of_string]
      for a description of the printable representation. *)
  val to_string : t -> string

  (** A special address, for use only with [bind], representing all the Internet addresses
      that the host machine possesses. *)
  val bind_any       : t
  val bind_any_inet6 : t

  (** Special addresses representing the host machine. *)
  val localhost       : t (** [127.0.0.1] *)

  val localhost_inet6 : t (** ([::1]) *)

  (** Some things (like the kernel) report addresses as hex or decimal strings.
      Provide conversion functions. *)
  val inet4_addr_of_int32     : Int32.t -> t

  (** [inet4_addr_to_int32_exn t = 0l] when [t = Inet_addr.of_string ("0.0.0.0")].
      An exception is raised if [t] is a not an IPv4 address. *)
  val inet4_addr_to_int32_exn : t -> Int32.t

  val inet4_addr_of_int63     : Int63.t -> t
  val inet4_addr_to_int63_exn : t -> Int63.t

  module Stable : sig
    module V1 : Stable with type t = t
  end
end

(** A representation of CIDR netmasks (e.g. "192.168.0.0/24") and functions to match if a
    given address is inside the range or not.  Only IPv4 addresses are supported.  Values
    are always normalized so the base address is the lowest IP address in the range, so
    for example [to_string (of_string "192.168.1.101/24") = "192.168.1.0/24"].
*)
module Cidr : sig
  type t [@@deriving sexp, bin_io]

  (** [of_string] Generates a Cidr.t based on a string like ["10.0.0.0/8"].  Addresses are
      not expanded, i.e. ["10/8"] is invalid. *)
  include Identifiable.S with type t := t
  include Invariant.S    with type t := t

  val create : base_address:Inet_addr.t -> bits:int -> t

  (** Accessors.
      - [base_address 192.168.0.0/24 = 192.168.0.0]
      - [bits         192.168.0.0/24 = 24]. *)
  val base_address : t -> Inet_addr.t
  val bits         : t -> int

  (** Generate a sequence of all addresses in the block. *)
  val all_matching_addresses : t -> Inet_addr.t Sequence.t

  (** IPv4 multicast address can be represented by the CIDR prefix 224.0.0.0/4,
      (i.e. addresses from 224.0.0.0 to 239.255.255.255, inclusive) *)
  val multicast : t

  (** Is the given address inside the given Cidr.t?  Note that the broadcast and network
      addresses are considered valid so [does_match 10.0.0.0/8 10.0.0.0] is true. *)
  val does_match : t -> Inet_addr.t -> bool

  (** Return the netmask corresponding to the number of network bits in the CIDR.
      For example, the netmask for a CIDR with 24 network bits (e.g. 1.2.3.0/24)
      is 255.255.255.0. *)
  val netmask_of_bits : t -> Inet_addr.t

  (** [is_subset t1 ~of:t2] is true iff the set of IP addresses specified by [t1] is a
      subset of those specified by [t2].

      If [is_subset t1 ~of_:t2], then [does_match t1 x] implies [does_match t2 x].

      If [does_match t1 x] and [does_match t2 x], then either [is_subset t1 ~of_:t2] or
      [is_subset t2 ~of_:t1] (or both). *)
  val is_subset : t -> of_:t -> bool

  module Stable : sig
    module V1 : Stable_comparable.V1
      with type t = t
      with type comparator_witness = comparator_witness
  end
end

(** {6 Sockets} *)

(** The type of socket domains. *)
type socket_domain = Unix.socket_domain =
  | PF_UNIX                             (** Unix domain *)
  | PF_INET                             (** Internet domain *)
  | PF_INET6                            (** Internet domain (IPv6) *)
[@@deriving sexp, bin_io]

(** The type of socket kinds, specifying the semantics of
    communications. *)
type socket_type = Unix.socket_type =
  | SOCK_STREAM                         (** Stream socket *)
  | SOCK_DGRAM                          (** Datagram socket *)
  | SOCK_RAW                            (** Raw socket *)
  | SOCK_SEQPACKET                      (** Sequenced packets socket *)
[@@deriving sexp, bin_io]

(** The type of socket addresses. [ADDR_UNIX name] is a socket address in the Unix domain;
    [name] is a file name in the file system. [ADDR_INET(addr,port)] is a socket address
    in the Internet domain; [addr] is the Internet address of the machine, and [port] is
    the port number. *)
type sockaddr = Unix.sockaddr =
  | ADDR_UNIX of string
  | ADDR_INET of Inet_addr.t * int
[@@deriving bin_io, sexp_of]

val sockaddr_of_sexp : Sexp.t -> sockaddr
[@@deprecated "[since 2015-10] Replace [sockaddr] by [sockaddr_blocking_sexp]"]

(** [sockaddr_blocking_sexp] is like [sockaddr], with [of_sexp] that performs DNS lookup
    to resolve [Inet_addr.t]. *)
type sockaddr_blocking_sexp = sockaddr [@@deriving bin_io, sexp]

(** Return the socket domain adequate for the given socket address. *)
val domain_of_sockaddr : sockaddr -> socket_domain

(** Create a new socket in the given domain, and with the
    given kind. The third argument is the protocol type; 0 selects
    the default protocol for that kind of sockets. *)
val socket : domain:socket_domain -> kind:socket_type -> protocol:int -> File_descr.t

(** Create a pair of unnamed sockets, connected together. *)
val socketpair
  :  domain:socket_domain
  -> kind:socket_type
  -> protocol:int
  -> File_descr.t * File_descr.t

(** Accept connections on the given socket. The returned descriptor
    is a socket connected to the client; the returned address is
    the address of the connecting client. *)
val accept : File_descr.t -> File_descr.t * sockaddr

(** Bind a socket to an address. *)
val bind : File_descr.t -> addr:sockaddr -> unit

(** Connect a socket to an address. *)
val connect : File_descr.t -> addr:sockaddr -> unit

(** Set up a socket for receiving connection requests. The integer argument is the number
    of pending requests that will be established and queued for {!accept}.  Depending on
    operating system, version, and configuration, subsequent connections may be refused
    actively (as with [RST]), ignored, or effectively established and queued anyway.

    Because handling of excess connections varies, it is most robust for applications to
    accept and close excess connections if they can.  To be sure the client receives an
    [RST] rather than an orderly shutdown, you can [setsockopt_optint file_descr SO_LINGER
    (Some 0)] before closing.

    In Linux, for example, the system configuration parameters [tcp_max_syn_backlog],
    [tcp_abort_on_overflow], and [syncookies] can all affect connection queuing
    behavior. *)
val listen : File_descr.t -> backlog:int -> unit

(** The type of commands for [shutdown]. *)
type shutdown_command =
  Unix.shutdown_command =
    SHUTDOWN_RECEIVE                    (** Close for receiving *)
  | SHUTDOWN_SEND                       (** Close for sending *)
  | SHUTDOWN_ALL                        (** Close both *)
[@@deriving sexp]

(** Shutdown a socket connection. [SHUTDOWN_SEND] as second argument
    causes reads on the other end of the connection to return
    an end-of-file condition.
    [SHUTDOWN_RECEIVE] causes writes on the other end of the connection
    to return a closed pipe condition ([SIGPIPE] signal). *)
val shutdown : File_descr.t -> mode:shutdown_command -> unit

(** Return the address of the given socket. *)
val getsockname : File_descr.t -> sockaddr

(** Return the address of the host connected to the given socket. *)
val getpeername : File_descr.t -> sockaddr

(** The flags for {!UnixLabels.recv},  {!UnixLabels.recvfrom},
    {!UnixLabels.send} and {!UnixLabels.sendto}. *)
type msg_flag = Unix.msg_flag =
    MSG_OOB
  | MSG_DONTROUTE
  | MSG_PEEK
[@@deriving sexp]

(** Receive data from a connected socket. *)
val recv
  :  File_descr.t
  -> buf:Bytes.t
  -> pos:int
  -> len:int
  -> mode:msg_flag list
  -> int

(** Receive data from an unconnected socket. *)
val recvfrom
  :  File_descr.t
  -> buf:Bytes.t
  -> pos:int
  -> len:int
  -> mode:msg_flag list
  -> int * sockaddr

(** Send data over a connected socket. *)
val send
  : File_descr.t -> buf:Bytes.t -> pos:int -> len:int -> mode:msg_flag list -> int

(** Same as [send] but with a string buffer. *)
val send_substring
  : File_descr.t -> buf:string -> pos:int -> len:int -> mode:msg_flag list -> int

(** Send data over an unconnected socket. *)
val sendto
  :  File_descr.t
  -> buf:Bytes.t
  -> pos:int
  -> len:int
  -> mode:msg_flag list
  -> addr:sockaddr
  -> int

(** Same as [sendto] but with a string buffer. *)
val sendto_substring
  :  File_descr.t
  -> buf:string
  -> pos:int
  -> len:int
  -> mode:msg_flag list
  -> addr:sockaddr
  -> int

(** {6 Socket options} *)


(** The socket options that can be consulted with {!UnixLabels.getsockopt}
    and modified with {!UnixLabels.setsockopt}.  These options have a boolean
    ([true]/[false]) value. *)
type socket_bool_option =
    SO_DEBUG               (** Record debugging information *)
  | SO_BROADCAST           (** Permit sending of broadcast messages *)
  | SO_REUSEADDR           (** Allow reuse of local addresses for bind *)
  | SO_KEEPALIVE           (** Keep connection active *)
  | SO_DONTROUTE           (** Bypass the standard routing algorithms *)
  | SO_OOBINLINE           (** Leave out-of-band data in line *)
  | SO_ACCEPTCONN          (** Report whether socket listening is enabled *)
  | TCP_NODELAY            (** Control the Nagle algorithm for TCP sockets *)
  | IPV6_ONLY              (** Forbid binding an IPv6 socket to an IPv4 address *)
[@@deriving sexp]

(** The socket options that can be consulted with {!UnixLabels.getsockopt_int}
    and modified with {!UnixLabels.setsockopt_int}.  These options have an
    integer value. *)
type socket_int_option =
    SO_SNDBUF              (** Size of send buffer *)
  | SO_RCVBUF              (** Size of received buffer *)
  | SO_ERROR               (** Report the error status and clear it *)
  | SO_TYPE                (** Report the socket type *)
  | SO_RCVLOWAT            (** Minimum number of bytes to process for input operations *)
  | SO_SNDLOWAT            (** Minimum number of bytes to process for output operations *)
[@@deriving sexp]

(** The socket options that can be consulted with {!UnixLabels.getsockopt_optint}
    and modified with {!UnixLabels.setsockopt_optint}.  These options have a
    value of type [int option], with [None] meaning ``disabled''. *)
type socket_optint_option =
    SO_LINGER              (** Whether to linger on closed connections
                               with sexp                      that have data present, and for how long
                               (in seconds) *)

(** The socket options that can be consulted with {!UnixLabels.getsockopt_float}
    and modified with {!UnixLabels.setsockopt_float}.  These options have a
    floating-point value representing a time in seconds.
    The value 0 means infinite timeout. *)
type socket_float_option =
    SO_RCVTIMEO            (** Timeout for input operations *)
  | SO_SNDTIMEO            (** Timeout for output operations *)
[@@deriving sexp]

(** Return the current status of a boolean-valued option
    in the given socket. *)
val getsockopt : File_descr.t -> socket_bool_option -> bool

(** Set or clear a boolean-valued option in the given socket. *)
val setsockopt : File_descr.t -> socket_bool_option -> bool -> unit

(** Same as {!UnixLabels.getsockopt} for an integer-valued socket option. *)
val getsockopt_int : File_descr.t -> socket_int_option -> int

(** Same as {!UnixLabels.setsockopt} for an integer-valued socket option. *)
val setsockopt_int : File_descr.t -> socket_int_option -> int -> unit

(** Same as {!UnixLabels.getsockopt} for a socket option whose value is an [int option]. *)
val getsockopt_optint : File_descr.t -> socket_optint_option -> int option

(** Same as {!UnixLabels.setsockopt} for a socket option whose value is an [int option]. *)
val setsockopt_optint : File_descr.t -> socket_optint_option -> int option -> unit

(** Same as {!UnixLabels.getsockopt} for a socket option whose value is a floating-point
    number. *)
val getsockopt_float : File_descr.t -> socket_float_option -> float

(** Same as {!UnixLabels.setsockopt} for a socket option whose value is a floating-point
    number. *)
val setsockopt_float
  : File_descr.t -> socket_float_option -> float -> unit


(** {6 High-level network connection functions} *)


(** Connect to a server at the given address.
    Return a pair of buffered channels connected to the server.
    Remember to call {!Pervasives.flush} on the output channel at the right times
    to ensure correct synchronization. *)
val open_connection : sockaddr -> In_channel.t * Out_channel.t

(** ``Shut down'' a connection established with {!UnixLabels.open_connection};
    that is, transmit an end-of-file condition to the server reading
    on the other side of the connection. *)
val shutdown_connection : In_channel.t -> unit

(** Establish a server on the given address.
    The function given as first argument is called for each connection
    with two buffered channels connected to the client. A new process
    is created for each connection. The function {!UnixLabels.establish_server}
    never returns normally. *)
val establish_server
  : (In_channel.t -> Out_channel.t -> unit) -> addr:sockaddr -> unit


(** {6 Host and protocol databases} *)

(** Return the name of the local host. *)
val gethostname : unit -> string

module Host : sig
  (** Structure of entries in the [hosts] database. *)
  type t =
    { name : string;
      aliases : string array;
      family : Protocol_family.t;
      addresses : Inet_addr.t array;
    }
  [@@deriving sexp_of]

  (** Find an entry in [hosts] with the given name.

      NOTE: This function is not thread safe with certain versions of winbind using "wins"
      name resolution. *)
  val getbyname : string -> t option
  val getbyname_exn : string -> t

  (** Find an entry in [hosts] with the given address. *)
  val getbyaddr : Inet_addr.t -> t option
  val getbyaddr_exn : Inet_addr.t -> t

  val have_address_in_common : t -> t -> bool
end

module Protocol : sig
  (** Structure of entries in the [protocols] database. *)
  type t =
    { name : string;
      aliases : string array;
      proto : int;
    }
  [@@deriving sexp]

  (** Find an entry in [protocols] with the given name. *)
  val getbyname : string -> t option
  val getbyname_exn : string -> t

  (** Find an entry in [protocols] with the given protocol number. *)
  val getbynumber : int -> t option
  val getbynumber_exn : int -> t
end

module Service : sig
  (** Structure of entries in the [services] database. *)
  type t =
    { name : string;
      aliases : string array;
      port : int;
      proto : string;
    }
  [@@deriving sexp]

  (** Find an entry in [services] with the given name. *)
  val getbyname : string -> protocol:string -> t option
  val getbyname_exn : string -> protocol:string -> t

  (** Find an entry in [services] with the given service number. *)
  val getbyport : int -> protocol:string -> t option
  val getbyport_exn : int -> protocol:string -> t
end

(** Address information returned by {!Unix.getaddrinfo}. *)
type addr_info =
  { ai_family    : socket_domain; (** Socket domain *)
    ai_socktype  : socket_type;   (** Socket type *)
    ai_protocol  : int;           (** Socket protocol number *)
    ai_addr      : sockaddr;      (** Address *)
    ai_canonname : string         (** Canonical host name *)
  }
[@@deriving sexp_of]

(** [addr_info_blocking_sexp] is like [addr_info], with [of_sexp] that performs DNS lookup
    to resolve [Inet_addr.t]. *)
type addr_info_blocking_sexp = addr_info [@@deriving sexp]

(** Options to {!Unix.getaddrinfo}. *)
type getaddrinfo_option =
    AI_FAMILY of socket_domain          (** Impose the given socket domain *)
  | AI_SOCKTYPE of socket_type          (** Impose the given socket type *)
  | AI_PROTOCOL of int                  (** Impose the given protocol  *)
  | AI_NUMERICHOST                      (** Do not call name resolver,
                                            expect numeric IP address *)
  | AI_CANONNAME                        (** Fill the [ai_canonname] field
                                            of the result *)
  | AI_PASSIVE                          (** Set address to ``any'' address
                                            for use with {!Unix.bind} *)
[@@deriving sexp]

(** [getaddrinfo host service opts] returns a list of {!Unix.addr_info}
    records describing socket parameters and addresses suitable for
    communicating with the given host and service.  The empty list is
    returned if the host or service names are unknown, or the constraints
    expressed in [opts] cannot be satisfied.

    [host] is either a host name or the string representation of an IP
    address.  [host] can be given as the empty string; in this case,
    the ``any'' address or the ``loopback'' address are used,
    depending whether [opts] contains [AI_PASSIVE].
    [service] is either a service name or the string representation of
    a port number.  [service] can be given as the empty string;
    in this case, the port field of the returned addresses is set to 0.
    [opts] is a possibly empty list of options that allows the caller
    to force a particular socket domain (e.g. IPv6 only, or IPv4 only)
    or a particular socket type (e.g. TCP only or UDP only). *)
val getaddrinfo : string -> string -> getaddrinfo_option list -> addr_info list

(** Host and service information returned by {!Unix.getnameinfo}. *)
type name_info = {
  ni_hostname : string; (** Name or IP address of host *)

  ni_service : string; (** Name of service or port number *)
}
[@@deriving sexp]

(** Options to {!Unix.getnameinfo}. *)
type getnameinfo_option =
    NI_NOFQDN            (** Do not qualify local host names *)
  | NI_NUMERICHOST       (** Always return host as IP address *)
  | NI_NAMEREQD          (** Fail if host name cannot be determined *)
  | NI_NUMERICSERV       (** Always return service as port number *)
  | NI_DGRAM             (** Consider the service as UDP-based
                             instead of the default TCP *)
[@@deriving sexp]

(** [getnameinfo addr opts] returns the host name and service name
    corresponding to the socket address [addr].  [opts] is a possibly
    empty list of options that governs how these names are obtained.
    Raise [Not_found] if an error occurs. *)
val getnameinfo : sockaddr -> getnameinfo_option list -> name_info



(** {6 Terminal interface} *)

(** The following functions implement the POSIX standard terminal
    interface. They provide control over asynchronous communication ports
    and pseudo-terminals. Refer to the [termios] man page for a complete
    description. *)

module Terminal_io : sig
  type t = Unix.terminal_io = {

    (*_ Input modes: *)
    mutable c_ignbrk : bool;  (** Ignore the break condition. *)
    mutable c_brkint : bool;  (** Signal interrupt on break condition. *)
    mutable c_ignpar : bool;  (** Ignore characters with parity errors. *)
    mutable c_parmrk : bool;  (** Mark parity errors. *)
    mutable c_inpck : bool;   (** Enable parity check on input. *)
    mutable c_istrip : bool;  (** Strip 8th bit on input characters. *)
    mutable c_inlcr : bool;   (** Map NL to CR on input. *)
    mutable c_igncr : bool;   (** Ignore CR on input. *)
    mutable c_icrnl : bool;   (** Map CR to NL on input. *)
    mutable c_ixon : bool;    (** Recognize XON/XOFF characters on input. *)
    mutable c_ixoff : bool;   (** Emit XON/XOFF chars to control input flow. *)

    (*_ Output modes: *)
    mutable c_opost : bool;   (** Enable output processing. *)

    (*_ Control modes: *)
    mutable c_obaud : int;    (** Output baud rate (0 means close connection).*)
    mutable c_ibaud : int;    (** Input baud rate. *)
    mutable c_csize : int;    (** Number of bits per character (5-8). *)
    mutable c_cstopb : int;   (** Number of stop bits (1-2). *)
    mutable c_cread : bool;   (** Reception is enabled. *)
    mutable c_parenb : bool;  (** Enable parity generation and detection. *)
    mutable c_parodd : bool;  (** Specify odd parity instead of even. *)
    mutable c_hupcl : bool;   (** Hang up on last close. *)
    mutable c_clocal : bool;  (** Ignore modem status lines. *)

    (*_ Local modes: *)
    mutable c_isig : bool;    (** Generate signal on INTR, QUIT, SUSP. *)
    mutable c_icanon : bool;  (** Enable canonical processing (line buffering and editing) *)
    mutable c_noflsh : bool;  (** Disable flush after INTR, QUIT, SUSP. *)
    mutable c_echo : bool;    (** Echo input characters. *)
    mutable c_echoe : bool;   (** Echo ERASE (to erase previous character). *)
    mutable c_echok : bool;   (** Echo KILL (to erase the current line). *)
    mutable c_echonl : bool;  (** Echo NL even if c_echo is not set. *)

    (*_ Control characters: *)
    mutable c_vintr : char;   (** Interrupt character (usually ctrl-C). *)
    mutable c_vquit : char;   (** Quit character (usually ctrl-\). *)
    mutable c_verase : char;  (** Erase character (usually DEL or ctrl-H). *)
    mutable c_vkill : char;   (** Kill line character (usually ctrl-U). *)
    mutable c_veof : char;    (** End-of-file character (usually ctrl-D). *)
    mutable c_veol : char;    (** Alternate end-of-line char. (usually none). *)
    mutable c_vmin : int;     (** Minimum number of characters to read before the read request is satisfied. *)
    mutable c_vtime : int;    (** Maximum read wait (in 0.1s units). *)
    mutable c_vstart : char;  (** Start character (usually ctrl-Q). *)
    mutable c_vstop : char;   (** Stop character (usually ctrl-S). *)
  } [@@deriving sexp_of]

  type setattr_when = Unix.setattr_when = TCSANOW | TCSADRAIN | TCSAFLUSH
  [@@deriving sexp_of]

  (** Return the status of the terminal referred to by the given
      file descriptor. *)
  val tcgetattr : File_descr.t -> t

  (** Set the status of the terminal referred to by the given
      file descriptor. The second argument indicates when the
      status change takes place: immediately ([TCSANOW]),
      when all pending output has been transmitted ([TCSADRAIN]),
      or after flushing all input that has been received but not
      read ([TCSAFLUSH]). [TCSADRAIN] is recommended when changing
      the output parameters; [TCSAFLUSH], when changing the input
      parameters. *)
  val tcsetattr : t -> File_descr.t -> mode:setattr_when -> unit

  (** Send a break condition on the given file descriptor.
      The second argument is the duration of the break, in 0.1s units;
      0 means standard duration (0.25s). *)
  val tcsendbreak : File_descr.t -> duration:int -> unit

  (** Waits until all output written on the given file descriptor
      has been transmitted. *)
  val tcdrain : File_descr.t -> unit

  type flush_queue =
    Unix.flush_queue =
      TCIFLUSH
    | TCOFLUSH
    | TCIOFLUSH
  [@@deriving sexp]

  (** Discard data written on the given file descriptor but not yet
      transmitted, or data received but not yet read, depending on the
      second argument: [TCIFLUSH] flushes data received but not read,
      [TCOFLUSH] flushes data written but not transmitted, and
      [TCIOFLUSH] flushes both. *)
  val tcflush : File_descr.t -> mode:flush_queue -> unit

  type flow_action
    = Unix.flow_action =
      | TCOOFF
      | TCOON
      | TCIOFF
      | TCION
  [@@deriving sexp]

  (** Suspend or restart reception or transmission of data on
      the given file descriptor, depending on the second argument:
      [TCOOFF] suspends output, [TCOON] restarts output,
      [TCIOFF] transmits a STOP character to suspend input,
      and [TCION] transmits a START character to restart input. *)
  val tcflow : File_descr.t -> mode:flow_action -> unit

  (** Put the calling process in a new session and detach it from
      its controlling terminal. *)
  val setsid : unit -> int
end


(** Get a sockaddr from a hostname or IP, and a port *)
val get_sockaddr : string -> int -> sockaddr

(** Set a timeout for a socket associated with an [In_channel.t] *)
val set_in_channel_timeout : In_channel.t -> float -> unit

(** Set a timeout for a socket associated with an [Out_channel.t] *)
val set_out_channel_timeout : Out_channel.t -> float -> unit

(** [exit_immediately exit_code] immediately calls the [exit] system call with the given
    exit code without performing any other actions (unlike Pervasives.exit).  Does not
    return. *)
val exit_immediately : int -> _

(** {2 Filesystem functions} *)

(** [mknod ?file_kind ?perm ?major ?minor path] creates a filesystem
    entry.  Note that only FIFO-entries are guaranteed to be supported
    across all platforms as required by the POSIX-standard.  On Linux
    directories and symbolic links cannot be created with this function.
    Use {!Unix.mkdir} and {!Unix.symlink} instead there respectively.

    @raise Invalid_argument if an unsupported file kind is used.
    @raise Unix_error if the system call fails.

    @param file_kind default = [S_REG] (= regular file)
    @param perm default = [0o600] (= read/write for user only)
    @param major default = [0]
    @param minor default = [0]
*)
val mknod
  :  ?file_kind : file_kind
  -> ?perm : int
  -> ?major : int
  -> ?minor : int
  -> string -> unit

(** {2 I/O vectors} *)

(** I/O-vectors for scatter/gather-operations *)
module IOVec : sig
  open Bigarray

  (** Representation of I/O-vectors.
      NOTE: DO NOT CHANGE THE MEMORY LAYOUT OF THIS TYPE!!!
      All C-functions in our bindings that handle I/O-vectors depend on it.
  *)
  type 'buf t = private
    {
      buf : 'buf;  (** Buffer holding the I/O-vector *)
      pos : int;  (** Position of I/O-vector in buffer *)
      len : int;  (** Length of I/O-vector in buffer *)
    }
  [@@deriving sexp]

  type 'buf kind  (** Kind of I/O-vector buffers *)

  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

  val string_kind : string kind
  val bigstring_kind : bigstring kind

  val empty : 'buf kind -> 'buf t
  (** [empty] the empty I/O-vector. *)

  val of_string : ?pos : int -> ?len : int -> string -> string t
  (** [of_string ?pos ?len str] @return an I/O-vector designated by
      position [pos] and length [len] in string [str].

      @raise Invalid_argument if designated substring out of bounds.

      @param pos default = 0
      @param len default = [String.length str - pos]
  *)

  val of_bigstring : ?pos : int -> ?len : int -> bigstring -> bigstring t
  (** [of_bigstring ?pos ?len bstr] @return an I/O-vector designated by
      position [pos] and length [len] in bigstring [bstr].

      @raise Invalid_argument if designated substring out of bounds.

      @param pos default = 0
      @param len default = [String.length str - pos]
  *)

  val drop : 'buf t -> int -> 'buf t
  (** [drop iovec n] drops [n] characters from [iovec].  @return resulting
      I/O-vector.

      @raise Failure if [n] is greater than length of [iovec].
  *)

  val max_iovecs : int Lazy.t
end


(** {2 I/O functions} *)

external dirfd : dir_handle -> File_descr.t = "unix_dirfd"

(** Extract a file descriptor from a directory handle. *)

external sync : unit -> unit = "unix_sync"
(** Synchronize all filesystem buffers with disk. *)

external fsync : File_descr.t -> unit = "unix_fsync"

(** Synchronize the kernel buffers of a given file descriptor with disk,
    but do not necessarily write file attributes. *)
external fdatasync : File_descr.t -> unit = "unix_fdatasync"

(** [readdir_ino_opt dh] return the next entry in a directory (([(filename, inode)]).  Returns
    [None] when the end of the directory has been reached. *)
val readdir_ino_opt : dir_handle -> (string * nativeint) option

(** Same as [readdir_ino_opt] except that it signals the end of the directory by raising
    [End_of_file]. *)
external readdir_ino
  : dir_handle -> string * nativeint = "unix_readdir_ino_stub"
[@@deprecated "[since 2016-08] use readdir_ino_opt instead"]

val read_assume_fd_is_nonblocking
  : File_descr.t -> ?pos : int -> ?len : int -> Bytes.t -> int
(** [read_assume_fd_is_nonblocking fd ?pos ?len buf] calls the system call
    [read] ASSUMING THAT IT IS NOT GOING TO BLOCK.  Reads at most [len]
    bytes into buffer [buf] starting at position [pos].  @return the
    number of bytes actually read.

    @raise Invalid_argument if buffer range out of bounds.
    @raise Unix_error on Unix-errors.

    @param pos = 0
    @param len = [String.length buf - pos]
*)

val write_assume_fd_is_nonblocking
  : File_descr.t -> ?pos : int -> ?len : int -> Bytes.t -> int
(** [write_assume_fd_is_nonblocking fd ?pos ?len buf] calls the system call
    [write] ASSUMING THAT IT IS NOT GOING TO BLOCK.  Writes at most [len]
    bytes from buffer [buf] starting at position [pos].  @return the
    number of bytes actually written.

    @raise Invalid_argument if buffer range out of bounds.
    @raise Unix_error on Unix-errors.

    @param pos = 0
    @param len = [String.length buf - pos]
*)

val writev_assume_fd_is_nonblocking
  : File_descr.t -> ?count : int -> string IOVec.t array -> int
(** [writev_assume_fd_is_nonblocking fd ?count iovecs] calls the system call
    [writev] ASSUMING THAT IT IS NOT GOING TO BLOCK using [count]
    I/O-vectors [iovecs].  @return the number of bytes actually written.

    @raise Invalid_argument if the designated ranges are invalid.
    @raise Unix_error on Unix-errors.
*)

val writev : File_descr.t -> ?count : int -> string IOVec.t array -> int
(** [writev fd ?count iovecs] like {!writev_assume_fd_is_nonblocking}, but does
    not require the descriptor to not block.  If you feel you have to
    use this function, you should probably have chosen I/O-vectors that
    build on bigstrings, because this function has to internally blit
    the I/O-vectors (ordinary OCaml strings) to intermediate buffers on
    the C-heap.

    @return the number of bytes actually written.

    @raise Invalid_argument if the designated ranges are invalid.
    @raise Unix_error on Unix-errors.
*)

external pselect
  :  File_descr.t list
    -> File_descr.t list
    -> File_descr.t list
    -> float
    -> int list
    -> File_descr.t list * File_descr.t list * File_descr.t list
  = "unix_pselect_stub"
(** [pselect rfds wfds efds timeout sigmask] like {!Core_unix.select} but
    also allows one to wait for the arrival of signals. *)

(** {2 Resource limits} *)
module RLimit : sig

  module Limit : sig
    type t = Limit of int64 | Infinity [@@deriving sexp]
    val min : t -> t -> t
    val max : t -> t -> t
  end

  type limit = Limit.t = Limit of int64 | Infinity [@@deriving sexp]

  type t = {
    cur : limit;  (** soft limit *)
    max : limit;  (** hard limit (ceiling for soft limit) *)
  } [@@deriving sexp]

  type resource [@@deriving sexp]

  val core_file_size       : resource
  val cpu_seconds          : resource
  val data_segment         : resource
  val file_size            : resource
  val num_file_descriptors : resource
  val stack                : resource
  val virtual_memory       : resource Or_error.t
  val nice                 : resource Or_error.t

  (** See man pages for "getrlimit" and "setrlimit" for details. *)

  val get : resource -> t
  val set : resource -> t -> unit
end

(** {2 Resource usage} -- For details, "man getrusage" *)
module Resource_usage : sig
  type t = {
    utime : float;
    stime : float;
    maxrss : int64;
    ixrss : int64;
    idrss : int64;
    isrss : int64;
    minflt : int64;
    majflt : int64;
    nswap : int64;
    inblock : int64;
    oublock : int64;
    msgsnd : int64;
    msgrcv : int64;
    nsignals : int64;
    nvcsw : int64;
    nivcsw : int64;
  }
  [@@deriving sexp, fields]

  val get : [`Self | `Children] -> t

  (** [add ru1 ru2] adds two rusage structures (e.g. your resource usage
      and your children's). *)
  val add : t -> t -> t
end

(** {2 System configuration} *)
type sysconf =
  | ARG_MAX
  | CHILD_MAX
  | HOST_NAME_MAX
  | LOGIN_NAME_MAX
  | OPEN_MAX
  | PAGESIZE
  | RE_DUP_MAX
  | STREAM_MAX
  | SYMLOOP_MAX
  | TTY_NAME_MAX
  | TZNAME_MAX
  | POSIX_VERSION
  | PHYS_PAGES
  | AVPHYS_PAGES
  | IOV_MAX
[@@deriving sexp]

(** Wrapper over [sysconf] function in C. *)
external sysconf : sysconf -> int64 option = "unix_sysconf"

val sysconf_exn : sysconf -> int64


(** {2 Temporary file and directory creation} *)

(** [mkstemp prefix] creates and opens a unique temporary file with [prefix],
    automatically appending a suffix of [.tmp.] followed by six random characters to make
    the name unique.  Unlike C's [mkstemp], [prefix] should not include six X's at the
    end.

    The file descriptor will have close-on-exec flag set if O_CLOEXEC flag is supported.

    @raise Unix_error on errors.
*)
val mkstemp : string -> string * File_descr.t

(** [mkdtemp prefix] creates a temporary directory with [prefix], automatically appending
    a suffix of [.tmp.] followed by six random characters to make the name unique.

    @raise Unix_error on errors.
    k*)
val mkdtemp : string -> string

(** {2 Signal handling} *)

(** Causes abnormal program termination unless the signal SIGABRT is
    caught and the signal handler does not return.  If the SIGABRT signal is
    blocked or ignored, the abort() function will still override it.
*)
external abort : unit -> _ = "unix_abort" [@@noalloc]

(** {2 User id, group id} *)

external initgroups : string -> int -> unit = "unix_initgroups"

(** [getgrouplist user group] returns the list of groups to which [user] belongs.
    See 'man getgrouplist'. *)
val getgrouplist : string -> int -> int array

(** Return the list of groups to which the user executing the process belongs. *)
val getgroups : unit -> int array

(** {2 Globbing and shell expansion} *)

(** no system calls involved *)
val fnmatch
  :  ?flags : [ `No_escape
              | `Pathname
              | `Period
              | `File_name
              | `Leading_dir
              | `Casefold
              ] list
  -> pat : string
  -> string
  -> bool

(** See man page for wordexp. *)
val wordexp
  : (?flags : [ `No_cmd | `Show_err | `Undef ] list -> string -> string array) Or_error.t

(** {2 System information} *)

module Utsname : sig
  type t [@@deriving sexp, compare]

  val sysname : t -> string
  val nodename : t -> string
  val release : t -> string
  val version : t -> string
  val machine : t -> string
end

(** See man page for uname. *)
val uname : unit -> Utsname.t

(** {2 Additional IP functionality} *)

(** [if_indextoname ifindex] If [ifindex] is an interface index, then
    the function returns the interface name.  Otherwise, it raises
    [Unix_error]. *)
val if_indextoname : int -> string

(** [mcast_join ?ifname sock addr] join a multicast group at [addr] with socket [sock],
    from source at [source] if specified, optionally using network interface [ifname].

    @param ifname default = any interface
*)
val mcast_join
  :  ?ifname : string
  -> ?source : Inet_addr.t
  -> File_descr.t
  -> sockaddr
  -> unit

(** [mcast_leave ?ifname sock addr] leaves a multicast group at [addr] with socket [sock],
    optionally using network interface [ifname].

    @param ifname default = any interface
*)
val mcast_leave : ?ifname : string -> File_descr.t -> sockaddr -> unit



(** [get_mcast_ttl sock] reads the time-to-live value of outgoing multicast packets for
    socket [sock]. *)
val get_mcast_ttl : File_descr.t -> int

(** [set_mcast_ttl sock ttl] sets the time-to-live value of outgoing multicast packets for
    socket [sock] to [ttl]. *)
val set_mcast_ttl : File_descr.t -> int -> unit

(** [get_mcast_loop sock] reads the boolean argument that determines whether sent
    multicast packets are looped back to local sockets. *)
val get_mcast_loop : File_descr.t -> bool

(** [set_mcast_loop sock loop] sets the boolean argument that determines whether sent
    multicast packets are looped back to local sockets. *)
val set_mcast_loop : File_descr.t -> bool -> unit

(** [set_mcast_ifname sock "eth0"] sets outgoing multicast traffic on IPv4 UDP socket
    [sock] to go out through interface [eth0].

    This uses [setsockopt] with [IP_MULTICAST_IF] and applies to multicast traffic.  For
    non-multicast applications, see {!Linux_ext.bind_to_interface}. *)
val set_mcast_ifname : File_descr.t -> string -> unit

module Scheduler : sig
  module Policy : sig
    type t = [ `Fifo | `Round_robin | `Other ] [@@deriving sexp]
  end

  (** See [man sched_setscheduler].

      The [priority] supplied here is *not* the nice value of a process.  It is the
      "static" priority (1 .. 99) used in conjunction with real-time processes.  If you
      want to set the nice value of a normal process, use [Linux_ext.setpriority]
      or [Core_unix.nice]. *)
  val set : pid:Pid.t option -> policy : Policy.t -> priority : int -> unit
end

module Priority : sig
  external nice : int -> int = "unix_nice"
end

(** For keeping your memory in RAM, i.e. preventing it from being swapped out. *)
module Mman : sig
  module Mcl_flags : sig
    type t = Current | Future [@@deriving sexp]
  end

  (** Lock all pages in this process's virtual address space into physical memory. See [man
      mlockall] for more details. *)
  val mlockall : Mcl_flags.t list -> unit

  (** Unlock previously locked pages. See [man munlockall]. *)
  val munlockall : unit -> unit
end

module Syslog : module type of Syslog

(** A network interface on the local machine.  See [man getifaddrs]. *)
module Ifaddr : sig
  module Broadcast_or_destination : sig
    type t =
      | Broadcast   of Inet_addr.t
      | Destination of Inet_addr.t
    [@@deriving sexp_of]
  end

  module Flag : sig
    type t =
      | Allmulti
      | Automedia
      | Broadcast
      | Debug
      | Dynamic
      | Loopback
      | Master
      | Multicast
      | Noarp
      | Notrailers
      | Pointopoint
      | Portsel
      | Promisc
      | Running
      | Slave
      | Up
    [@@deriving enumerate, sexp_of]

    include Comparable.S with type t := t
  end

  module Family : sig
    type t = Packet | Inet4 | Inet6 [@@deriving sexp_of]
  end

  type t =
    { name                     : string
    ; family                   : Family.t
    ; flags                    : Flag.Set.t
    ; address                  : Inet_addr.t                option
    ; netmask                  : Inet_addr.t                option
    ; broadcast_or_destination : Broadcast_or_destination.t option
    }
  [@@deriving fields, sexp_of]
end

val getifaddrs : unit -> Ifaddr.t list


module Stable : sig
  module Inet_addr = Inet_addr.Stable
  module Cidr = Cidr.Stable
end

