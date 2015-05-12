open Core_kernel.Std

module File_descr = Core_unix.File_descr

module Sysinfo0 = struct
  type t =
    { uptime : Span.t;
      load1 : int;
      load5 : int;
      load15 : int;
      total_ram : int;
      free_ram : int;
      shared_ram : int;
      buffer_ram : int;
      total_swap : int;
      free_swap : int;
      procs : int;
      totalhigh : int;
      freehigh : int;
      mem_unit : int;
    }
  with bin_io, sexp
end

(* If you update this type, you also must update linux_tcpopt_bool, in the C stubs. (And
   do make sure you get the order correct) *)
type tcp_bool_option = TCP_CORK with sexp, bin_io

INCLUDE "core_config.mlh"

IFDEF POSIX_TIMERS THEN
module Clock = struct
  type t

  (* These functions should be in Unix, but due to the dependency on Time,
     this is not possible (cyclic dependency). *)
  external get_time : t -> float = "unix_clock_gettime"
  let get_time t = Span.of_float (get_time t)

  external set_time : t -> float -> unit = "unix_clock_settime"
  let set_time t s = set_time t (Span.to_float s)

  external get_resolution : t -> float = "unix_clock_getres"
  let get_resolution t = Span.of_float (get_resolution t)

  external get_process_clock : unit -> t = "unix_clock_process_cputime_id_stub"

  external get_thread_clock : unit -> t = "unix_clock_thread_cputime_id_stub"

  IFDEF THREAD_CPUTIME THEN
  external get : Thread.t -> t = "unix_pthread_getcpuclockid"

  let get               = Ok get
  ELSE
  let get               = Or_error.unimplemented "Linux_ext.Clock.get"
  ENDIF
  let get_time          = Ok get_time
  let set_time          = Ok set_time
  let get_resolution    = Ok get_resolution
  let get_process_clock = Ok get_process_clock
  let get_thread_clock  = Ok get_thread_clock

end

ELSE

module Clock = struct
  type t

  let get               = Or_error.unimplemented "Linux_ext.Clock.get"
  let get_time          = Or_error.unimplemented "Linux_ext.Clock.get_time"
  let set_time          = Or_error.unimplemented "Linux_ext.Clock.set_time"
  let get_resolution    = Or_error.unimplemented "Linux_ext.Clock.get_resolution"
  let get_process_clock = Or_error.unimplemented "Linux_ext.Clock.get_process_clock"
  let get_thread_clock  = Or_error.unimplemented "Linux_ext.Clock.get_thread_clock"
end

ENDIF

IFDEF TIMERFD THEN

module Timerfd = struct
  module Clock : sig
    type t with bin_io, compare, sexp
    val realtime : t
    val monotonic : t
  end = struct
    type t = Int63.t with bin_io, compare, sexp

    external realtime : unit -> Int63.t = "linux_timerfd_CLOCK_REALTIME"
    let realtime = realtime ()

    external monotonic : unit -> Int63.t = "linux_timerfd_CLOCK_MONOTONIC"
    let monotonic = monotonic ()
  end

  module Flags = struct
    external nonblock : unit -> Int63.t = "linux_timerfd_TFD_NONBLOCK"
    let nonblock = nonblock ()

    external cloexec  : unit -> Int63.t = "linux_timerfd_TFD_CLOEXEC"
    let cloexec = cloexec ()

    include Flags.Make (struct
      let allow_intersecting = false
      let should_print_error = true
      let remove_zero_flags = false
      let known =
        List.rev
          [ nonblock, "nonblock";
            cloexec,  "cloexec";
          ]
    end)
  end

  type t = File_descr.t with bin_io, compare, sexp

  let to_file_descr t = t

  external timerfd_create : Clock.t -> Flags.t -> int = "linux_timerfd_create"

  (* At Jane Street, we link with [--wrap timerfd_create] so that we can use
     our own wrapper around [timerfd_create].  This allows us to compile an executable on
     a machine that has timerfd (e.g. CentOS 6) but then run the executable on a machine
     that does not (e.g. CentOS 5), but that has our wrapper library.  We set up our
     wrapper so that when running on a machine that doesn't have it, [timerfd_create]
     raises ENOSYS. *)
  let create =
    let create ?(flags = Flags.empty) clock =
      File_descr.of_int (timerfd_create clock flags)
    in
    match Result.try_with (fun () -> create Clock.realtime) with
    | Ok t -> (Unix.close t; Ok create)
    | Error (Unix.Unix_error (Unix.ENOSYS, _, _)) ->
      Or_error.unimplemented "Linux_ext.Timerfd.create"
    | Error _ ->
      (* [timerfd_create] is implemented but fails with the arguments we used above.
         [create] might still be usable with different arguments, so we expose it here. *)
      Ok create
  ;;

  external timerfd_settime : t -> bool -> float -> float -> unit = "linux_timerfd_settime"

  let settime ~initial ~interval t =
    let absolute, initial =
      match initial with
      | `At t    -> (true,  Time.to_float t)
      | `After s -> (false, Span.to_sec s)
    in
    let interval = Span.to_sec interval in
    timerfd_settime t absolute initial interval;
  ;;

  let set t when_ = settime t ~initial:when_ ~interval:Span.zero

  let set_repeating ?initial  t interval =
    settime t ~initial:(Option.value initial ~default:(`After interval)) ~interval
  ;;

  let clear t = settime t ~initial:(`After Span.zero) ~interval:Span.zero

  module Spec = struct
    type t =
      { fire_after : float;
        interval : float;
      }
  end

  type repeat = { fire_after : Span.t; interval : Span.t }

  external timerfd_gettime : t -> Spec.t = "linux_timerfd_gettime"

  let get t =
    let { Spec. fire_after; interval } = timerfd_gettime t in
    let fire_after = Span.of_sec fire_after in
    let interval = Span.of_sec interval in
    if Span.equal interval Span.zero then
      if Span.equal fire_after Span.zero
      then `Not_armed
      else `Fire_after fire_after
    else
      `Repeat { fire_after; interval }
  ;;

  TEST_MODULE "Linux_ext.Timerfd" = struct

    TEST_UNIT =
      match create with
      | Error _ -> ()
      | Ok create ->
        let t = create Clock.realtime in
        assert (get t = `Not_armed);
        set t (`After Span.minute);
        assert (match get t with
                | `Fire_after span -> Span.(<=) span Span.minute
                | _ -> false);
        let span = Span.scale Span.minute 2. in
        set_repeating t ~initial:(`After Span.minute) span;
        assert (match get t with
                | `Repeat { fire_after; interval } ->
                  Span.(<=) fire_after Span.minute && Span.equal interval span
                | _ ->
                  false);
    ;;
  end
end

ELSE

module Timerfd = struct
  module Clock = struct
    type t = unit with bin_io, compare, sexp
    let realtime = ()
    let monotonic = ()
  end

  module Flags = struct
    let nonblock = Int63.of_int 0o4000
    let cloexec  = Int63.of_int 0o2000000

    include Flags.Make (struct
      let allow_intersecting = false
      let should_print_error = true
      let remove_zero_flags = false

      let known =
        List.rev
          [ nonblock, "nonblock";
            cloexec,  "cloexec";
          ]
    end)
  end

  type t = File_descr.t with bin_io, compare, sexp

  let to_file_descr t = t

  type repeat = { fire_after : Span.t; interval : Span.t }

  let create = Or_error.unimplemented "Linux_ext.Timerfd.create"

  let set _                      _ = assert false
  let set_repeating ?initial:_ _ _ = assert false
  let clear                      _ = assert false
  let get                        _ = assert false
end

ENDIF

(* We use [Int63] rather than [Int] because these flags use 32 bits. *)

module Epoll_flags (Flag_values : sig
  val in_     : Int63.t
  val out     : Int63.t
  (* val rdhup   : Int63.t *)
  val pri     : Int63.t
  val err     : Int63.t
  val hup     : Int63.t
  val et      : Int63.t
  val oneshot : Int63.t
end) = struct
  let none = Int63.zero

  include Flag_values

  include Flags.Make (struct
    let allow_intersecting = false
    let should_print_error = true
    let remove_zero_flags = false
    let known =
      [ in_, "in";
        out, "out";
        (* rdhup, "rdhup"; *)
        pri, "pri";
        err, "err";
        hup, "hup";
        et, "et";
        oneshot, "oneshot";
      ]
    ;;
  end)

end

module Priority : sig
  type t with sexp

  val equal : t -> t -> bool
  val of_int : int -> t
  val to_int : t -> int
  val incr : t -> t
  val decr : t -> t
end = struct
  type t = int with sexp

  let of_int t = t
  let to_int t = t

  let incr t = t - 1

  let decr t = t + 1

  let equal (t : t) t' = t = t'
end

IFDEF LINUX_EXT THEN

type file_descr = Core_unix.File_descr.t

external sendfile
  : sock : file_descr -> fd : file_descr -> pos : int -> len : int -> int
    = "linux_sendfile_stub"
;;

let sendfile ?(pos = 0) ?len ~fd sock =
  let len =
    match len with
    | Some len -> len
    | None -> (Unix.fstat fd).Unix.st_size - pos
  in
  sendfile ~sock ~fd ~pos ~len

(* Raw result of sysinfo syscall *)
module Raw_sysinfo = struct
  type t = {
    uptime : int;
    load1 : int;
    load5 : int;
    load15 : int;
    total_ram : int;
    free_ram : int;
    shared_ram : int;
    buffer_ram : int;
    total_swap : int;
    free_swap : int;
    procs : int;
    totalhigh : int;
    freehigh : int;
    mem_unit : int;
  }
end

module Sysinfo = struct
  include Sysinfo0

  external raw_sysinfo : unit -> Raw_sysinfo.t = "linux_sysinfo"

  let sysinfo = Ok (fun () ->
    let raw = raw_sysinfo () in
    {
      uptime = Span.of_int_sec raw.Raw_sysinfo.uptime;
      load1 = raw.Raw_sysinfo.load1;
      load5 = raw.Raw_sysinfo.load5;
      load15 = raw.Raw_sysinfo.load15;
      total_ram = raw.Raw_sysinfo.total_ram;
      free_ram = raw.Raw_sysinfo.free_ram;
      shared_ram = raw.Raw_sysinfo.shared_ram;
      buffer_ram = raw.Raw_sysinfo.buffer_ram;
      total_swap = raw.Raw_sysinfo.total_swap;
      free_swap = raw.Raw_sysinfo.free_swap;
      procs = raw.Raw_sysinfo.procs;
      totalhigh = raw.Raw_sysinfo.totalhigh;
      freehigh = raw.Raw_sysinfo.freehigh;
      mem_unit = raw.Raw_sysinfo.mem_unit;
    })
end

external gettcpopt_bool
  : file_descr -> tcp_bool_option -> bool = "linux_gettcpopt_bool_stub"

external settcpopt_bool
  : file_descr -> tcp_bool_option -> bool -> unit = "linux_settcpopt_bool_stub"

external unsafe_send_nonblocking_no_sigpipe
  : file_descr -> pos : int -> len : int -> string -> int
  = "linux_send_nonblocking_no_sigpipe_stub"

let unsafe_send_nonblocking_no_sigpipe fd ~pos ~len buf =
  let res = unsafe_send_nonblocking_no_sigpipe fd ~pos ~len buf in
  if res = -1 then None
  else Some res

external unsafe_send_no_sigpipe
  : file_descr -> pos : int -> len : int -> string -> int
  = "linux_send_no_sigpipe_stub"

let check_send_args ?pos ?len buf =
  let str_len = String.length buf in
  let pos =
    match pos with
    | None -> 0
    | Some pos ->
        if pos < 0 then invalid_arg "send_nonblocking_no_sigpipe: pos < 0";
        if pos > str_len then
          invalid_arg "send_nonblocking_no_sigpipe: pos > str_len";
        pos
  in
  let len =
    match len with
    | None -> str_len - pos
    | Some len ->
        if len < 0 then invalid_arg "send_nonblocking_no_sigpipe: pos < 0";
        if pos + len > str_len then
          invalid_arg "send_nonblocking_no_sigpipe: pos + len > str_len";
        len
  in
  (pos, len)

let send_nonblocking_no_sigpipe sock ?pos ?len buf =
  let (pos, len) = check_send_args ?pos ?len buf in
  unsafe_send_nonblocking_no_sigpipe sock ~pos ~len buf

let send_no_sigpipe sock ?pos ?len buf =
  let (pos, len) = check_send_args ?pos ?len buf in
  unsafe_send_no_sigpipe sock ~pos ~len buf

external unsafe_sendmsg_nonblocking_no_sigpipe
  : file_descr -> string Core_unix.IOVec.t array -> int -> int
  = "linux_sendmsg_nonblocking_no_sigpipe_stub"

let unsafe_sendmsg_nonblocking_no_sigpipe fd iovecs count =
  let res = unsafe_sendmsg_nonblocking_no_sigpipe fd iovecs count in
  if res = -1 then None
  else Some res

let sendmsg_nonblocking_no_sigpipe sock ?count iovecs =
  let count =
    match count with
    | None -> Array.length iovecs
    | Some count ->
        if count < 0 then
          invalid_arg "sendmsg_nonblocking_no_sigpipe: count < 0";
        let n_iovecs = Array.length iovecs in
        if count > n_iovecs then
          invalid_arg "sendmsg_nonblocking_no_sigpipe: count > n_iovecs";
        count
  in
  unsafe_sendmsg_nonblocking_no_sigpipe sock iovecs count

external pr_set_pdeathsig : Signal.t -> unit = "linux_pr_set_pdeathsig_stub"
external pr_get_pdeathsig : unit -> Signal.t = "linux_pr_get_pdeathsig_stub"

external pr_set_name_first16 : string -> unit = "linux_pr_set_name"
external pr_get_name : unit -> string = "linux_pr_get_name"

let file_descr_realpath fd =
  Core_filename.realpath ("/proc/self/fd/" ^ File_descr.to_string fd)

let out_channel_realpath oc = file_descr_realpath (Unix.descr_of_out_channel oc)
let in_channel_realpath ic = file_descr_realpath (Unix.descr_of_in_channel ic)

external raw_sched_setaffinity
  : pid : int -> cpuset : int list -> unit = "linux_sched_setaffinity"

let sched_setaffinity ?pid ~cpuset () =
  let pid = match pid with None -> 0 | Some pid -> Pid.to_int pid in
  raw_sched_setaffinity ~pid ~cpuset
;;

(* defined in unix_stubs.c *)
external gettid : unit -> int = "unix_gettid"

external setpriority : Priority.t -> unit = "linux_setpriority"

external getpriority : unit -> Priority.t = "linux_getpriority"

let sched_setaffinity_this_thread ~cpuset =
  sched_setaffinity ~pid:(Pid.of_int (gettid ())) ~cpuset ()
;;

let cores =
  Memo.unit (fun () ->
    let num_cores =
      In_channel.with_file "/proc/cpuinfo" ~f:In_channel.input_lines
      |! List.fold_left ~init:0 ~f:(fun count line ->
        count +
          (match String.lsplit2 ~on:':' line with
          | None -> 0
          | Some (label, _) ->
            if String.(=) (String.rstrip label) "processor" then 1
            else 0))
    in
    if num_cores > 0 then num_cores
    else failwith "Linux_ext.cores: failed to parse /proc/cpuinfo")

TEST = cores () > 0
TEST = cores () < 100000 (* 99,999 cores ought to be enough for anybody *)

external get_terminal_size : unit -> int * int = "linux_get_terminal_size"

external get_ipv4_address_for_interface : string -> string =
  "linux_get_ipv4_address_for_interface" ;;

TEST "lo interface addr is 127.0.0.1" =
  (* This could false positive if the test box is misconfigured *)
  get_ipv4_address_for_interface "lo" = "127.0.0.1"

(* The C-stub is a simple pass-through of the linux SO_BINDTODEVICE semantics, wherein an
   empty string removes any binding *)
external bind_to_interface' : File_descr.t -> string -> unit =
  "linux_bind_to_interface" ;;

let bind_to_interface fd ifname =
  let name =
    match ifname with
    | `Interface_name name -> name
    | `Any -> ""
  in
  bind_to_interface' fd name
;;

module Epoll = struct

  external flag_epollin      : unit -> Int63.t  = "linux_epoll_EPOLLIN_flag"
  external flag_epollout     : unit -> Int63.t  = "linux_epoll_EPOLLOUT_flag"
  (* external flag_epollrdhup   : unit -> Int63.t  = "linux_epoll_EPOLLRDHUP_flag" *)
  external flag_epollpri     : unit -> Int63.t  = "linux_epoll_EPOLLPRI_flag"
  external flag_epollerr     : unit -> Int63.t  = "linux_epoll_EPOLLERR_flag"
  external flag_epollhup     : unit -> Int63.t  = "linux_epoll_EPOLLHUP_flag"
  external flag_epollet      : unit -> Int63.t  = "linux_epoll_EPOLLET_flag"
  external flag_epolloneshot : unit -> Int63.t  = "linux_epoll_EPOLLONESHOT_flag"

  module Flags = Epoll_flags (struct
    let in_     = flag_epollin ()
    let out     = flag_epollout ()
    (* let rdhup   = flag_epollrdhup () *)
    let pri     = flag_epollpri ()
    let err     = flag_epollerr ()
    let hup     = flag_epollhup ()
    let et      = flag_epollet ()
    let oneshot = flag_epolloneshot ()
  end)

  external sizeof_epoll_event : unit -> int   = "linux_sizeof_epoll_event" "noalloc"

  external epoll_create : int -> File_descr.t = "linux_epoll_create"

  (* Some justification for the below interface: Unlike select() and poll(), epoll() fills
     in an array of ready events, analogous to a read() call where you pass in a buffer to
     be filled.

     Since this is at the core of the I/O loop, we'd like to avoid reallocating that
     buffer on every call to poll.  We're allocating the array on the ocaml side (as a
     Bigstring), then iterating through it in-place, reducing allocation, copies, and any
     intermediate lists.  For very high message rates and many fds this could be a very
     beneficial. *)
  type ready_events = Bigstring.t

  external epoll_readyfd
    : ready_events -> int -> File_descr.t = "linux_epoll_readyfd" "noalloc"

IFDEF ARCH_SIXTYFOUR THEN
  external epoll_readyflags
    : ready_events -> int -> Flags.t = "linux_epoll_readyflags" "noalloc"
ELSE
  external epoll_readyflags
    : ready_events -> int -> Flags.t = "linux_epoll_readyflags"
ENDIF

  external epoll_ctl_add
    : File_descr.t -> File_descr.t -> Flags.t -> unit
      = "linux_epoll_ctl_add"

  external epoll_ctl_mod
    : File_descr.t -> File_descr.t -> Flags.t -> unit
      = "linux_epoll_ctl_mod"

  external epoll_ctl_del
    : File_descr.t -> File_descr.t -> unit
      = "linux_epoll_ctl_del"

  module Table = Bounded_int_table

  module T = struct
    type 'a t =
      { epollfd : File_descr.t;
        (* [flags_by_fd] has one entry for each file-descr in the epoll set, and stores
           the epoll flags that the kernel's epoll set currently has for that
           file-descr.  Keeping our own representation of the kernel data structure is
           useful for debugging, since the information appears in a human-readable way
           in [sexp_of_t]'s output.  It also allows us to hide the distinction between
           [epoll_ctl_add] and [epoll_ctl_mod], since we know which to use based on
           whether the file descriptor is already being watched. *)
        flags_by_fd : (File_descr.t, Flags.t) Table.t;
        max_ready_events : int;
        (* [num_ready_events] holds the number of ready events in [ready_events], as
           determined by the last call to [wait]. *)
        mutable num_ready_events : int;
        ready_events : 'a;
      }
    with fields, sexp_of
  end

  open T

  type in_use = ready_events T.t

  module Pretty = struct
    type ready_event =
      { file_descr : File_descr.t;
        flags : Flags.t;
      }
    with sexp_of

    type ready_events = ready_event array with sexp_of

    type t = ready_events T.t with sexp_of
  end

  let to_pretty t =
    { t with
      ready_events =
        Array.init t.num_ready_events ~f:(fun i ->
          { Pretty.
            file_descr = epoll_readyfd t.ready_events i;
            flags = epoll_readyflags t.ready_events i;
          });
    }
  ;;

  let sexp_of_in_use t = Pretty.sexp_of_t (to_pretty t)

  type t = [ `Closed | `In_use of in_use ] ref with sexp_of

  let close t =
    match !t with
    | `Closed -> ()
    | `In_use { epollfd; _ } ->
      t := `Closed;
      Unix.close epollfd;
  ;;

  let invariant t : unit =
    match !t with
    | `Closed -> ()
    | `In_use t ->
      try
        let check f field = f (Field.get field t) in
        Fields.iter
          ~epollfd:ignore
          ~flags_by_fd:(check (Table.invariant ignore ignore))
          ~max_ready_events:(check (fun max_ready_events -> assert (max_ready_events > 0)))
          ~num_ready_events:(check (fun num_ready -> assert (num_ready >= 0)))
          ~ready_events:ignore
      with exn ->
        failwiths "Epoll.invariant failed" (exn, t) <:sexp_of< exn * in_use >>
  ;;

  let create ~num_file_descrs ~max_ready_events =
    if max_ready_events < 0 then
      failwiths "Epoll.create got nonpositive max_ready_events" max_ready_events
        (<:sexp_of< int >>);
    ref (`In_use
            { epollfd = epoll_create max_ready_events;
              flags_by_fd =
                Table.create
                  ~num_keys:num_file_descrs
                  ~key_to_int:File_descr.to_int
                  ~sexp_of_key:File_descr.sexp_of_t
                  ();
              max_ready_events;
              num_ready_events = 0;
              ready_events = Bigstring.create (sizeof_epoll_event () * max_ready_events);
            })
  ;;

  let use t ~f =
    match !t with
    | `Closed -> failwith "attempt to use closed epoll set"
    | `In_use r -> f r
  ;;

  let find t file_descr = use t ~f:(fun t -> Table.find t.flags_by_fd file_descr)

  let find_exn t file_descr = use t ~f:(fun t -> Table.find_exn t.flags_by_fd file_descr)

  let iter t ~f =
    use t ~f:(fun t ->
      Table.iter t.flags_by_fd ~f:(fun ~key:file_descr ~data:flags ->
        f file_descr flags))
  ;;

  let set t fd flags =
    use t ~f:(fun t ->
      let already_present = Table.mem t.flags_by_fd fd in
      Table.set t.flags_by_fd ~key:fd ~data:flags;
      if already_present
      then epoll_ctl_mod t.epollfd fd flags
      else epoll_ctl_add t.epollfd fd flags);
  ;;

  let remove t fd =
    use t ~f:(fun t ->
      if Table.mem t.flags_by_fd fd then epoll_ctl_del t.epollfd fd;
      Table.remove t.flags_by_fd fd)
  ;;

  external epoll_wait : File_descr.t -> ready_events -> int -> int = "linux_epoll_wait"

  let wait t ~timeout =
    let timeout =
      (* From the epoll man page:

         | Specifying a timeout of -1 makes epoll_wait() wait indefinitely, while
         | specifying a timeout equal to zero makes epoll_wait() to return immediately
         | even if no events are available (return code equal to zero). *)
      match timeout with
      | `Never -> -1
      | `Immediately -> 0
      | `After span ->
        if Span.(<) span Span.zero then
          raise (Unix.Unix_error (Unix.EINVAL, "negative timeout", Span.to_string span));
        (* We round up to ensure that we are guaranteed that the timeout has passed when
           we wake up.  If we rounded down, then when the user requests a positive
           sub-millisecond timeout, we would use a timout of zero, and return immediately.
           This caused async to spin, repeatedly requesting slightly smaller timeouts. *)
        Float.iround_exn ~dir:`Up (Span.to_ms span)
    in
    use t ~f:(fun t ->
      (* We clear [num_ready_events] because [epoll_wait] will invalidate [ready_events],
         and we don't want another thread to observe [t] and see junk. *)
      t.num_ready_events <- 0;
      t.num_ready_events <- epoll_wait t.epollfd t.ready_events timeout;
      if t.num_ready_events = 0 then `Timeout else `Ok)
  ;;

  let fold_ready t ~init ~f =
    use t ~f:(fun t ->
      let ac = ref init in
      for i = 0 to t.num_ready_events - 1 do
        ac := f !ac (epoll_readyfd t.ready_events i) (epoll_readyflags t.ready_events i)
      done;
      !ac)
  ;;

  let iter_ready t ~f = fold_ready t ~init:() ~f:(fun () fd flags -> f fd flags)

(* external epoll_pwait
 *   : File_descr.t -> Events_buffer.raw -> int -> int list -> int
 *   = "linux_epoll_pwait"
 *
 * let pwait t ~timeout sigs =
 *   let millis = Float.iround_exn ~dir:`Zero ( Span.to_ms timeout ) in
 *   let num_ready = epoll_pwait t.epollfd t.events millis sigs in
 *   if num_ready = 0 then `Timeout
 *   else `Ok { Ready_fds.num_ready ; events = t.events }
 * ;; *)

let create = Ok create
end


(* Epoll unit test included here for some example usage. Creates 2 sockets,
   adds them to an epoll set, sends data to one of them and calls Epoll.wait.
   The test passes if the resulting Ready_fds set has 1 ready fd, matching
   the one we sent to, with read, !write, and !error. *)
TEST_MODULE = struct

  module Unix = Core_unix
  module Flags = Epoll.Flags

  let udp_listener ~port =
    let sock = Unix.socket ~domain:Unix.PF_INET ~kind:Unix.SOCK_DGRAM ~protocol:0 in
    let iaddr = Unix.ADDR_INET (Unix.Inet_addr.localhost, port) in
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    Unix.bind sock ~addr:iaddr;
    sock
  ;;

  let send s buf ~port =
    let addr = Unix.ADDR_INET (Unix.Inet_addr.localhost, port) in
    let len  = String.length buf in
    Unix.sendto s ~buf ~pos:0 ~len ~mode:[] ~addr
  ;;

  let with_epoll ~f =
    protectx ~finally:Epoll.close ~f
      ((Or_error.ok_exn Epoll.create) ~num_file_descrs:1024 ~max_ready_events:256)

  TEST_UNIT "epoll errors" = with_epoll ~f:(fun t ->
    let tmp = "temporary-file-for-testing-epoll" in
    let fd = Unix.openfile tmp ~mode:[Unix.O_CREAT; Unix.O_WRONLY] in
    (* Epoll does not support ordinary files, and so should fail if you ask it to watch
       one. *)
    assert (Result.is_error (Result.try_with (fun () -> Epoll.set t fd Flags.none)));
    Unix.close fd;
    Unix.unlink tmp)
  ;;

  TEST_UNIT "epoll test" = with_epoll ~f:(fun epset ->
    let timeout = Span.of_sec 0.1 in
    let sock1 = udp_listener ~port:7070 in
    let sock2 = udp_listener ~port:7071 in
    Epoll.set epset sock1 Flags.in_;
    Epoll.set epset sock2 Flags.in_;
    let _sent = send sock2 "TEST" ~port:7070 in
    begin match Epoll.wait epset ~timeout:(`After timeout) with
    | `Timeout -> assert false
    | `Ok ->
      let ready =
        Epoll.fold_ready epset ~init:[] ~f:(fun ac fd flags ->
          if flags = Flags.in_ then fd :: ac else ac)
      in
      (* Explanation of the test:
         1) I create two udp sockets, sock1 listening on 7070 and sock2, on 7071
         2) These two sockets are both added to epoll for read notification
         3) I send a packet, _using_ sock2 to sock1 (who is listening on 7070)
         4) epoll_wait should return, with [ sock1 ] ready to be read.
      *)
      match ready with
      | [ sock ] when sock = sock1 -> ()
      | [_] -> failwith  "wrong socket is ready"
      | xs  -> failwithf "%d sockets are ready" (List.length xs) ()
    end)
  ;;
end


let cores                          = Ok cores
let file_descr_realpath            = Ok file_descr_realpath
let get_ipv4_address_for_interface = Ok get_ipv4_address_for_interface
let bind_to_interface              = Ok bind_to_interface
let get_terminal_size              = Ok get_terminal_size
let gettcpopt_bool                 = Ok gettcpopt_bool
let setpriority                    = Ok setpriority
let getpriority                    = Ok getpriority
let in_channel_realpath            = Ok in_channel_realpath
let out_channel_realpath           = Ok out_channel_realpath
let pr_get_name                    = Ok pr_get_name
let pr_get_pdeathsig               = Ok pr_get_pdeathsig
let pr_set_name_first16            = Ok pr_set_name_first16
let pr_set_pdeathsig               = Ok pr_set_pdeathsig
let sched_setaffinity              = Ok sched_setaffinity
let sched_setaffinity_this_thread  = Ok sched_setaffinity_this_thread
let send_no_sigpipe                = Ok send_no_sigpipe
let send_nonblocking_no_sigpipe    = Ok send_nonblocking_no_sigpipe
let sendfile                       = Ok sendfile
let sendmsg_nonblocking_no_sigpipe = Ok sendmsg_nonblocking_no_sigpipe
let settcpopt_bool                 = Ok settcpopt_bool

ELSE

module Sysinfo = struct
  include Sysinfo0

  let sysinfo = Or_error.unimplemented "Linux_ext.Sysinfo.sysinfo"
end

let u = Or_error.unimplemented
let cores                          = u "Linux_ext.cores"
let file_descr_realpath            = u "Linux_ext.file_descr_realpath"
let get_ipv4_address_for_interface = u "Linux_ext.get_ipv4_address_for_interface"
let bind_to_interface              = u "Linux_ext.bind_to_interface"
let get_terminal_size              = u "Linux_ext.get_terminal_size"
let gettcpopt_bool                 = u "Linux_ext.gettcpopt_bool"
let setpriority                    = u "Linux_ext.setpriority"
let getpriority                    = u "Linux_ext.getpriority"
let in_channel_realpath            = u "Linux_ext.in_channel_realpath"
let out_channel_realpath           = u "Linux_ext.out_channel_realpath"
let pr_get_name                    = u "Linux_ext.pr_get_name"
let pr_get_pdeathsig               = u "Linux_ext.pr_get_pdeathsig"
let pr_set_name_first16            = u "Linux_ext.pr_set_name_first16"
let pr_set_pdeathsig               = u "Linux_ext.pr_set_pdeathsig"
let sched_setaffinity              = u "Linux_ext.sched_setaffinity"
let sched_setaffinity_this_thread  = u "Linux_ext.sched_setaffinity_this_thread"
let send_no_sigpipe                = u "Linux_ext.send_no_sigpipe"
let send_nonblocking_no_sigpipe    = u "Linux_ext.send_nonblocking_no_sigpipe"
let sendfile                       = u "Linux_ext.sendfile"
let sendmsg_nonblocking_no_sigpipe = u "Linux_ext.sendmsg_nonblocking_no_sigpipe"
let settcpopt_bool                 = u "Linux_ext.settcpopt_bool"

module Epoll = struct
  module Flags = Epoll_flags (struct
    let in_     = Int63.of_int (1 lsl 0)
    let out     = Int63.of_int (1 lsl 1)
    (* let rdhup   = Int63.of_int (1 lsl 2) *)
    let pri     = Int63.of_int (1 lsl 3)
    let err     = Int63.of_int (1 lsl 4)
    let hup     = Int63.of_int (1 lsl 5)
    let et      = Int63.of_int (1 lsl 6)
    let oneshot = Int63.of_int (1 lsl 7)
  end)

  type t = [ `Epoll_is_not_implemented ] with sexp_of
  let create = Or_error.unimplemented "Linux_ext.Epoll.create"

  let invariant _               = assert false

  let close _                   = assert false
  let find _ _                  = assert false
  let find_exn _ _              = assert false
  let set _ _ _                 = assert false
  let remove _ _                = assert false
  let iter _ ~f:_               = assert false
  let wait _ ~timeout:_         = assert false
  let iter_ready _ ~f:_         = assert false
  let fold_ready _ ~init:_ ~f:_ = assert false

  (* let pwait _ ~timeout:_ _      = assert false *)
end

ENDIF
