open Core_kernel.Std
open Core_unix

(** Interface to Linux-specific system calls *)

(** {2 sysinfo} *)

module Sysinfo : sig
  (** Result of sysinfo syscall (man 2 sysinfo) *)
  type t =
    { uptime : Span.t;  (** time since boot *)
      load1 : int;  (** load average over the last minute *)
      load5 : int;  (** load average over the last 5 minutes*)
      load15 : int;  (** load average over the last 15 minutes *)
      total_ram : int;  (** total usable main memory *)
      free_ram : int;  (** available memory size *)
      shared_ram : int;  (** amount of shared memory *)
      buffer_ram : int;  (** memory used by buffers *)
      total_swap : int;  (** total swap page size *)
      free_swap : int;  (** available swap space *)
      procs : int;  (** number of current processes *)
      totalhigh : int;  (** Total high memory size *)
      freehigh : int;  (** Available high memory size *)
      mem_unit : int;  (** Memory unit size in bytes *)
    } with sexp, bin_io

  val sysinfo : (unit -> t) Or_error.t
end

(** {2 Filesystem functions} *)

(** [sendfile ?pos ?len ~fd sock] sends mmap-able data from file
    descriptor [fd] to socket [sock] using offset [pos] and length [len].
    @return the number of characters actually written.

    NOTE: if the returned value is unequal to what was requested (=
    the initial size of the data by default), the system call may have
    been interrupted by a signal, the source file may have been truncated
    during operation, or a timeout occurred on the socket during sending.
    It is currently impossible to find out which of the events above
    happened.  Calling {!sendfile} several times on the same descriptor
    that only partially accepted data due to a timeout will eventually
    lead to the unix error [EAGAIN].

    @raise Unix_error on Unix-errors.

    @param default pos = 0
    @param default len = length of data (file) associated with descriptor [fd]
*)
val sendfile
  : (?pos : int
     -> ?len : int
     -> fd : File_descr.t
     -> File_descr.t
     -> int) Or_error.t

(** {2 Non-portable TCP-functionality} *)

type tcp_bool_option = TCP_CORK with sexp, bin_io

(** [gettcpopt_bool sock opt] @return the current value of the boolean
    TCP socket option [opt] for socket [sock]. *)
val gettcpopt_bool : (File_descr.t -> tcp_bool_option -> bool) Or_error.t

(** [settcpopt_bool sock opt v] sets the current value of the boolean
    TCP socket option [opt] for socket [sock] to value [v]. *)
val settcpopt_bool
  : (File_descr.t -> tcp_bool_option -> bool -> unit) Or_error.t

(** [send_nonblocking_no_sigpipe sock ?pos ?len buf] tries to do a
    nonblocking send on socket [sock] given buffer [buf], offset [pos]
    and length [len].  Prevents [SIGPIPE], i.e. raise a Unix-error
    in that case immediately.  @return [Some bytes_written] or [None]
    if the operation would have blocked.

    @param pos default = 0
    @param len default = [String.length buf - pos]

    @raise Invalid_argument if the designated buffer range is invalid.
    @raise Unix_error on Unix-errors.
*)
val send_nonblocking_no_sigpipe
  : (File_descr.t
     -> ?pos : int
     -> ?len : int
     -> string
     -> int option) Or_error.t

(** [send_no_sigpipe sock ?pos ?len buf] tries to do a
    blocking send on socket [sock] given buffer [buf], offset [pos]
    and length [len].  Prevents [SIGPIPE], i.e. raise a Unix-error in
    that case immediately.  @return the number of bytes written.

    @param pos default = 0
    @param len default = [String.length buf - pos]

    @raise Invalid_argument if the designated buffer range is invalid.
    @raise Unix_error on Unix-errors.
*)
val send_no_sigpipe
  : (File_descr.t -> ?pos : int -> ?len : int -> string -> int) Or_error.t

(** [sendmsg_nonblocking_no_sigpipe sock ?count iovecs] tries to do
    a nonblocking send on socket [sock] using [count] I/O-vectors
    [iovecs].  Prevents [SIGPIPE], i.e. raises a Unix-error in that
    case immediately.  @return [Some bytes_written] or [None] if the
    operation would have blocked.

    @raise Invalid_argument if the designated ranges are invalid.
    @raise Unix_error on Unix-errors.
*)
val sendmsg_nonblocking_no_sigpipe
  : (File_descr.t
     -> ?count : int
     -> string IOVec.t array
     -> int option) Or_error.t

(** {2 Clock functions} *)

module Clock : sig
  type t

  (* All these functions can raise Unix_error. *)

  (* returns the CPU-clock associated with the thread *)
  val get : (Thread.t -> t) Or_error.t

  val get_time : (t -> Span.t) Or_error.t

  val set_time : (t -> Span.t -> unit) Or_error.t

  val get_resolution : (t -> Span.t) Or_error.t

  (** [get_process_clock] the clock measuring the CPU-time of a process. *)
  val get_process_clock : (unit -> t) Or_error.t

  (** [get_thread_clock] the clock measuring the CPU-time of the current thread. *)
  val get_thread_clock : (unit -> t) Or_error.t
end

(** {2 Timerfd functions} *)

module Timerfd : sig
  (** Clock used to mark the progress of a timer. *)
  module Clock : sig
    type t with bin_io, compare, sexp

    (** Settable system-wide clock. *)
    val realtime : t

    (** Nonsettable clock.  It is not affected by manual changes to the system time. *)
    val monotonic : t
  end

  module Flags : sig
    type t with sexp_of

    include Flags.S with type t := t

    val nonblock : t (** [TFD_NONBLOCK] *)
    val cloexec  : t (** [TFD_CLOEXEC]  *)
  end

  type t = private File_descr.t with bin_io, compare, sexp

  val to_file_descr : t -> File_descr.t

  (** [create ?flags clock] creates a new timer file descriptor.  With Linux 2.6.26 or
      earlier [flags] must be empty. *)
  val create : (?flags:Flags.t -> Clock.t -> t) Or_error.t

  (** [set t when] sets [t] to fire once, at the time specified by [when]. *)
  val set : t -> [ `At of Time.t | `After of Span.t ] -> unit

  (** [set_repeating ?initial t interval] sets [t] to fire every [interval] starting at
      [when]. *)
  val set_repeating
    :  ?initial:[ `At of Time.t | `After of Span.t ] (** default is [`After interval] *)
    -> t
    -> Span.t
    -> unit

  (** [clear t] causes [t] to not fire any more. *)
  val clear : t -> unit

  type repeat = { fire_after : Span.t; interval : Span.t }

  (** [get t] returns the current state of the timer [t]. *)
  val get
    :  t
    -> [ `Not_armed
       | `Fire_after of Span.t
       | `Repeat of repeat
       ]
end

(** {2 Parent death notifications} *)

(** [pr_set_pdeathsig s] sets the signal [s] to be sent to the executing
    process when its parent dies.  NOTE: the parent may have died
    before or while executing this system call.  To make sure that you
    do not miss this event, you should call {!getppid} to get
    the parent process id after this system call.  If the parent has
    died, the returned parent PID will be 1, i.e. the init process will
    have adopted the child.  You should then either send the signal to
    yourself using Unix.kill, or execute an appropriate handler. *)
val pr_set_pdeathsig : (Signal.t -> unit) Or_error.t

(** [pr_get_pdeathsig ()] get the signal that will be sent to the
    currently executing process when its parent dies. *)
val pr_get_pdeathsig : (unit -> Signal.t) Or_error.t


(** {2 Task name} *)

(** [pr_set_name_first16 name] sets the name of the executing thread to [name].  Only
    the first 16 bytes in [name] will be used, the rest is ignored. *)
val pr_set_name_first16 : (string -> unit) Or_error.t

(** [pr_get_name ()] gets the name of the executing thread.  The name is
    at most 16 bytes long. *)
val pr_get_name : (unit -> string) Or_error.t


(** {2 Pathname resolution} *)

(** [file_descr_realpath fd] @return the canonicalized absolute
    pathname of the file associated with file descriptor [fd].

    @raise Unix_error on errors.
*)
val file_descr_realpath : (File_descr.t -> string) Or_error.t

(** [out_channel_realpath oc] @return the canonicalized absolute
    pathname of the file associated with output channel [oc].

    @raise Unix_error on errors.
*)
val out_channel_realpath : (out_channel -> string) Or_error.t

(** [in_channel_realpath ic] @return the canonicalized absolute
    pathname of the file associated with input channel [ic].

    @raise Unix_error on errors.
*)
val in_channel_realpath : (in_channel -> string) Or_error.t

(** {2 Affinity} *)

(* Setting the CPU affinity causes a process to only run on the
   cores chosen.  You can find out how many cores a system has in
   /proc/cpuinfo.  This can be useful in two ways: first, it limits
   a process to a core so that it won't interfere with processes on
   other cores.  Second, you save time by not moving the process back
   and forth between CPUs, which sometimes invalidates their cache.
   See "man sched_setaffinity" for details. *)
val sched_setaffinity : (?pid : Pid.t -> cpuset : int list -> unit -> unit) Or_error.t


val sched_setaffinity_this_thread : (cpuset : int list -> unit) Or_error.t

(** [cores ()] @return the number of cores on the machine *)
val cores : (unit -> int) Or_error.t

(** [get_terminal_size ()] @return [(rows, cols)], the number of rows and
    columns of the terminal. *)
val get_terminal_size : (unit -> int * int) Or_error.t

module Priority : sig
  (* [Priority.t] is what is usually referred to as the "nice" value of a process.  It is
     also known as the "dynamic" priority.  It is used with normal (as opposed to
     real-time) processes that have static priority zero.  See [Unix.Scheduler.set] for
     setting the static priority. *)
  type t with sexp

  val equal : t -> t -> bool
  val of_int : int -> t
  val to_int : t -> int
  val incr : t -> t
  val decr : t -> t
end

(** Set the calling thread's priority in the linux scheduler *)
val setpriority : (Priority.t -> unit) Or_error.t

(** Get the calling thread's priority in the linux scheduler *)
val getpriority : (unit -> Priority.t) Or_error.t

(* [get_ipv4_address_for_interface "eth0"] returns the IP address
   assigned to eth0, or throws an exception if no IP address
   is configured. *)
val get_ipv4_address_for_interface : (string -> string) Or_error.t

(* [bind_to_interface fd (`Interface_name "eth0")] restricts packets from being
   received/sent on the given file descriptor [fd] on any interface other than "eth0".
   Use [bind_to_interface fd `Any] to allow traffic on any interface.  The bindings are
   not cumulative; you may only select one interface, or any.

   Not to be confused with a traditional BSD sockets API [bind()] call, this
   Linux-specific socket option ([SO_BINDTODEVICE]) is used for applications on
   multi-homed machines with specific security concerns.  For similar functionality when
   using multicast, see {!Core_unix.mcast_set_ifname}. *)
val bind_to_interface
  : (File_descr.t -> [ `Any | `Interface_name of string ] -> unit) Or_error.t

(** epoll() - a linux I/O multiplexer of the same family as select() or poll().  Its main
    differences are support for Edge or Level triggered notifications (We're using
    Level-triggered to emulate select) and much better scaling with the number of file
    descriptors.

    See the man pages for a full description of the epoll facility. *)
module Epoll : sig

  module Flags : sig
    (** An [Epoll.Flags.t] is an immutable set of flags for which one can register
        interest for a file descriptor.  It is implemented as a bitmask, and so all
        operations (+, -, etc.) are constant time with no allocation.

        [sexp_of_t] produces a human-readable list of bits, e.g. "(in out)". *)
    type t with sexp_of

    include Flags.S with type t := t

    (* The names of the flags match the man pages.  E.g. [in_] = "EPOLLIN", [out] =
       "EPOLLOUT", etc. *)
    val none    : t (* Associated fd is readable                      *)
    val in_     : t (* Associated fd is readable                      *)
    val out     : t (* Associated fd is writable                      *)
    (* val rdhup   : t (\* Event flag For detecting tcp half-close        *\) *)
    val pri     : t (* Urgent data available                          *)
    val err     : t (* Error condition (always on, no need to set it) *)
    val hup     : t (* Hang up happened (always on)                   *)
    val et      : t (* Edge Triggered behavior (see man page)         *)
    val oneshot : t (* one-shot behavior for the associated fd        *)
  end

  (** An [Epoll.t] maintains a map from [File_descr.t] to [Flags.t], where the domain is
      the set of file descriptors that one is interested in, and the flags associated with
      each file descriptor specify the types of events one is interested in being notified
      about for that file descriptor.  Our implementation maintains a user-level table
      equivalent to the kernel epoll set, so that [sexp_of_t] produces useful
      human-readable information, and so that we can present our standard table
      interface.

      An [Epoll.t] also has a buffer that is used to store the set of ready fds returned
      by calling [wait]. *)
  type t with sexp_of

  val invariant : t -> unit

  (** [create ~num_file_descrs] creates a new epoll set able to watch file descriptors in
      \[0, num_file_descrs).  Additionally, the set allocates space for reading the ready
      events when [wait] returns, allowing for up to [max_ready_events] to be returned in
      a single call to [wait]. *)
  val create  : (num_file_descrs:int -> max_ready_events:int -> t) Or_error.t

  val close : t -> unit

  (** map operations *)
  val find     : t -> File_descr.t -> Flags.t option
  val find_exn : t -> File_descr.t -> Flags.t
  val set      : t -> File_descr.t -> Flags.t -> unit
  val remove   : t -> File_descr.t -> unit
  val iter     : t -> f:(File_descr.t -> Flags.t -> unit) -> unit

  (** [wait t ~timeout] blocks until at least one file descriptor in [t] is ready for one
      of the events it is being watched for, or [timeout] passes.  [wait] side effects [t]
      by storing the ready set in it.  One can subsequently access the ready set by
      calling [iter_ready] or [fold_ready].

      The [timeout] has a granularity of one millisecond.  [wait] rounds up the [timeout]
      to the next millisecond.  E.g. a [timeout] of one microsecond will be rounded up
      to one millisecond.

      Note that this method should not be considered thread safe.  There is mutable state
      in t that will be changed by invocations to wait that cannot be prevented by mutexes
      around [wait]. *)
  val wait
    :  t
    -> timeout:[ `Never | `Immediately | `After of Span.t ]
    -> [ `Ok | `Timeout ]

  (** [iter_ready] and [fold_ready] iterate over the ready set computed by the last
      call to [wait]. *)
  val iter_ready : t -> f:(File_descr.t -> Flags.t -> unit) -> unit
  val fold_ready : t -> init:'a -> f:('a -> File_descr.t -> Flags.t -> 'a) -> 'a

  (* pwait -> with the specified sigmask, analogous to pselect *)
  (* val pwait   : t -> timeout:Span.t -> int list -> [ `Ok of Ready_fds.t | `Timeout ] *)
end
