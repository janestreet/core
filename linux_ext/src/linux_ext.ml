open! Core

module File_descr = Unix.File_descr
module Syscall_result = Unix.Syscall_result

module Sysinfo0 = struct
  type t =
    { uptime : Time.Span.t;
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
  [@@deriving bin_io, sexp]
end

(* If you update this type, you also must update linux_tcpopt_bool, in the C stubs. (And
   do make sure you get the order correct) *)
type tcp_bool_option = TCP_CORK | TCP_QUICKACK [@@deriving sexp, bin_io]

module Bound_to_interface = struct
  type t = Any | Only of string [@@deriving sexp_of]
end

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
  type t [@@deriving sexp]

  val equal : t -> t -> bool
  val of_int : int -> t
  val to_int : t -> int
  val incr : t -> t
  val decr : t -> t
end = struct
  type t = int [@@deriving sexp]

  let of_int t = t
  let to_int t = t

  let incr t = t - 1

  let decr t = t + 1

  let equal (t : t) t' = t = t'
end

module Peer_credentials = struct
  (* C code depends on the layout of the type *)
  type t =
    { pid : Pid.t
    ; uid : int
    ; gid : int
    }
  [@@deriving sexp_of]
end

(* These module contains definitions that get used when the necessary features are not
   enabled. We put these somewhere where they'll always be compiled, to prevent them from
   getting out of sync with the real implementations. *)
module Null_toplevel = struct
  module Sysinfo = struct
    include Sysinfo0

    let sysinfo = Or_error.unimplemented "Linux_ext.Sysinfo.sysinfo"
  end

  let u = Or_error.unimplemented
  let cores                          = u "Linux_ext.cores"
  let file_descr_realpath            = u "Linux_ext.file_descr_realpath"
  let get_ipv4_address_for_interface = u "Linux_ext.get_ipv4_address_for_interface"
  let bind_to_interface              = u "Linux_ext.bind_to_interface"
  let get_bind_to_interface          = u "Linux_ext.get_bind_to_interface"
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
  let sched_getaffinity              = u "Linux_ext.sched_getaffinity"
  let sched_setaffinity_this_thread  = u "Linux_ext.sched_setaffinity_this_thread"
  let send_no_sigpipe                = u "Linux_ext.send_no_sigpipe"
  let send_nonblocking_no_sigpipe    = u "Linux_ext.send_nonblocking_no_sigpipe"
  let sendfile                       = u "Linux_ext.sendfile"
  let sendmsg_nonblocking_no_sigpipe = u "Linux_ext.sendmsg_nonblocking_no_sigpipe"
  let settcpopt_bool                 = u "Linux_ext.settcpopt_bool"
  let peer_credentials               = u "Linux_ext.peer_credentials"

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

    type t = [ `Epoll_is_not_implemented ] [@@deriving sexp_of]
    let create = Or_error.unimplemented "Linux_ext.Epoll.create"
    let close _ = assert false

    let invariant _               = assert false

    let find _ _                  = assert false
    let find_exn _ _              = assert false
    let set _ _ _                 = assert false
    let remove _ _                = assert false
    let iter _ ~f:_               = assert false
    let wait _ ~timeout:_         = assert false
    let wait_timeout_after _ _    = assert false
    let iter_ready _ ~f:_         = assert false
    let fold_ready _ ~init:_ ~f:_ = assert false

    module Expert = struct
      let clear_ready _           = assert false
    end

    (* let pwait _ ~timeout:_ _      = assert false *)
  end
end
module Null : Linux_ext_intf.S = struct
  type nonrec tcp_bool_option = tcp_bool_option =
      TCP_CORK | TCP_QUICKACK
  [@@deriving sexp, bin_io]

  module Bound_to_interface = struct
    type t = Bound_to_interface.t = Any | Only of string
    [@@deriving sexp_of]
  end

  module Peer_credentials = Peer_credentials
  module Priority = Priority

  module Clock = struct
    type t

    let get               = Or_error.unimplemented "Linux_ext.Clock.get"
    let get_time          = Or_error.unimplemented "Linux_ext.Clock.get_time"
    let set_time          = Or_error.unimplemented "Linux_ext.Clock.set_time"
    let get_resolution    = Or_error.unimplemented "Linux_ext.Clock.get_resolution"
    let get_process_clock = Or_error.unimplemented "Linux_ext.Clock.get_process_clock"
    let get_thread_clock  = Or_error.unimplemented "Linux_ext.Clock.get_thread_clock"
  end

  module Eventfd = struct
    type t = File_descr.t [@@deriving compare, sexp_of]

    module Flags = struct
      (* These (and flags below) are in octal to match the system header file
         <bits/eventfd.h> *)
      let nonblock  = Int63.of_int 0o4000
      let cloexec   = Int63.of_int 0o2000000
      let semaphore = Int63.of_int 0o1

      include Flags.Make(struct
          let allow_intersecting = true
          let should_print_error = true
          let remove_zero_flags = false

          let known =
            [ nonblock , "nonblock"
            ; cloexec  , "cloexec"
            ; semaphore, "semaphore" ]
        end)
    end

    let create = Or_error.unimplemented "Linux_ext.Eventfd.create"
    let read _ = assert false
    let write _ = assert false

    let to_file_descr t = t
  end

  module Timerfd = struct
    module Clock = struct
      type t = unit [@@deriving bin_io, compare, sexp]
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

    type t = File_descr.t [@@deriving compare, sexp_of]

    let to_file_descr t = t

    type repeat =
      { fire_after : Time_ns.Span.t
      ; interval   : Time_ns.Span.t
      }

    let create = Or_error.unimplemented "Linux_ext.Timerfd.create"

    let set_at _                 _ = assert false
    let set_after _              _ = assert false
    let set_repeating ?after:_ _ _ = assert false
    let clear                    _ = assert false
    let get                      _ = assert false

    module Private = struct
      let unsafe_timerfd_settime _ = assert false
    end
  end

  module Extended_file_attributes = struct
    module Get_attr_result = struct
      type t =
        | Ok of string
        | ENOATTR
        | ERANGE
        | ENOTSUP
      [@@deriving sexp_of]
    end

    let getxattr = Or_error.unimplemented "Linux_ext.Extended_file_attributes.getxattr"

    module Set_attr_result = struct
      type t =
        | Ok
        | EEXIST
        | ENOATTR
        | ENOTSUP
      [@@deriving sexp_of]
    end

    let setxattr = Or_error.unimplemented "Linux_ext.Extended_file_attributes.setxattr"
  end

  include Null_toplevel
end

[%%import "config.h"]

[%%ifdef JSC_POSIX_TIMERS]
module Clock = struct
  type t

  (* These functions should be in Unix, but due to the dependency on Time,
     this is not possible (cyclic dependency). *)
  external get_time : t -> float = "core_unix_clock_gettime"
  let get_time t = Time.Span.of_sec (get_time t)

  external set_time : t -> float -> unit = "core_unix_clock_settime"
  let set_time t s = set_time t (Time.Span.to_sec s)

  external get_resolution : t -> float = "core_unix_clock_getres"
  let get_resolution t = Time.Span.of_sec (get_resolution t)

  external get_process_clock : unit -> t = "core_unix_clock_process_cputime_id_stub"

  external get_thread_clock : unit -> t = "core_unix_clock_thread_cputime_id_stub"

  [%%ifdef JSC_THREAD_CPUTIME]
  external get : Thread.t -> t = "core_unix_pthread_getcpuclockid"

  let get               = Ok get
  [%%else]
  let get               = Or_error.unimplemented "Linux_ext.Clock.get"
  [%%endif]
  let get_time          = Ok get_time
  let set_time          = Ok set_time
  let get_resolution    = Ok get_resolution
  let get_process_clock = Ok get_process_clock
  let get_thread_clock  = Ok get_thread_clock

end

[%%else]
module Clock = Null.Clock
[%%endif]


[%%ifdef JSC_TIMERFD]

module Timerfd = struct
  module Clock : sig
    type t [@@deriving bin_io, compare, sexp]
    val realtime : t
    val monotonic : t
  end = struct
    type t = Int63.t [@@deriving bin_io, compare, sexp]

    external realtime : unit -> Int63.t = "core_linux_timerfd_CLOCK_REALTIME"
    let realtime = realtime ()

    external monotonic : unit -> Int63.t = "core_linux_timerfd_CLOCK_MONOTONIC"
    let monotonic = monotonic ()
  end

  module Flags = struct
    external nonblock : unit -> Int63.t = "core_linux_timerfd_TFD_NONBLOCK"
    let nonblock = nonblock ()

    external cloexec  : unit -> Int63.t = "core_linux_timerfd_TFD_CLOEXEC"
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

  type t = File_descr.t [@@deriving compare, sexp_of]

  let to_file_descr t = t

  external timerfd_create : Clock.t -> Flags.t -> int = "core_linux_timerfd_create"

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
    | Error (Unix.Unix_error (ENOSYS, _, _)) ->
      Or_error.unimplemented "Linux_ext.Timerfd.create"
    | Error _ ->
      (* [timerfd_create] is implemented but fails with the arguments we used above.
         [create] might still be usable with different arguments, so we expose it
         here. *)
      Ok create
  ;;

  external unsafe_timerfd_settime
    :  t
    -> bool
    -> initial  : Int63.t
    -> interval : Int63.t
    -> Syscall_result.Unit.t
    = "core_linux_timerfd_settime" [@@noalloc]

  let timerfd_settime t ~absolute ~initial ~interval =
    (* We could accept [interval < 0] or [initial < 0 when absolute], but then the
       conversions to timespecs in the C code become tedious and [timerfd_setttime] fails
       when it gets anything negative anyway. *)
    if Int63.O.( initial < zero || interval < zero )
    then raise_s
           [%sexp
             "timerfd_settime got invalid parameters (initial < 0 or interval < 0).",
             { timerfd  = (t : t)
             ; initial  = (initial  : Int63.t)
             ; interval = (interval : Int63.t)
             }
           ];
    unsafe_timerfd_settime t absolute ~initial ~interval
    |> Syscall_result.Unit.ok_or_unix_error_exn ~syscall_name:"timerfd_settime"
  ;;

  let initial_of_span span =
    Time_ns.Span.to_int63_ns
      (if Time_ns.Span.( <= ) span Time_ns.Span.zero
       then Time_ns.Span.nanosecond
       else span)
  ;;

  let set_at t at =
    if Time_ns.( <= ) at Time_ns.epoch
    then failwiths ~here:[%here] "Timerfd.set_at got time before epoch" at [%sexp_of: Time_ns.t];
    timerfd_settime t
      ~absolute:true
      ~initial:(Time_ns.to_int63_ns_since_epoch at)
      ~interval:Int63.zero

  let set_after t span =
    timerfd_settime t
      ~absolute:false
      ~initial:(initial_of_span span)
      ~interval:Int63.zero
  ;;

  let set_repeating ?after t interval =
    if Time_ns.Span.( <= ) interval Time_ns.Span.zero
    then failwiths ~here:[%here] "Timerfd.set_repeating got invalid interval" interval
           [%sexp_of: Time_ns.Span.t];
    let interval = Time_ns.Span.to_int63_ns interval in
    timerfd_settime t
      ~absolute:false
      ~initial:(Option.value_map after ~f:initial_of_span ~default:interval)
      ~interval
  ;;

  let clear t =
    timerfd_settime t ~absolute:false ~initial:Int63.zero ~interval:Int63.zero

  type repeat =
    { fire_after : Time_ns.Span.t
    ; interval   : Time_ns.Span.t
    }

  external timerfd_gettime : t -> repeat = "core_linux_timerfd_gettime"

  let get t =
    let spec = timerfd_gettime t in
    if Time_ns.Span.equal spec.interval Time_ns.Span.zero
    then
      if Time_ns.Span.equal spec.fire_after Time_ns.Span.zero
      then `Not_armed
      else `Fire_after spec.fire_after
    else
      `Repeat spec
  ;;

  module Private = struct
    let unsafe_timerfd_settime = unsafe_timerfd_settime
  end
end

[%%else]
module Timerfd = Null.Timerfd
[%%endif]

[%%ifdef JSC_LINUX_EXT]

type file_descr = Unix.File_descr.t

module Eventfd = struct
  module Flags = struct
    external cloexec   : unit -> Int63.t = "core_linux_eventfd_EFD_CLOEXEC"
    external nonblock  : unit -> Int63.t = "core_linux_eventfd_EFD_NONBLOCK"
    external semaphore : unit -> Int63.t = "core_linux_eventfd_EFD_SEMAPHORE"

    let cloexec   = cloexec ()
    let nonblock  = nonblock ()
    let semaphore = semaphore ()

    let known =
      [ cloexec  , "cloexec"
      ; nonblock , "nonblock"
      ; semaphore, "semaphore" ]

    include Flags.Make (struct
        let allow_intersecting = true
        let should_print_error = true
        let known = known
        let remove_zero_flags = false
      end)
  end

  type t = File_descr.t [@@deriving compare, sexp_of]

  external create : Int32.t -> Flags.t -> t  = "core_linux_eventfd"
  external read   : t -> Int64.t             = "core_linux_eventfd_read"
  external write  : t -> Int64.t -> unit     = "core_linux_eventfd_write"

  let create =
    let create ?(flags=Flags.empty) init = create init flags in
    Or_error.return create

  let to_file_descr t = t
end

external sendfile
  : sock : file_descr -> fd : file_descr -> pos : int -> len : int -> int
  = "core_linux_sendfile_stub"
;;

let sendfile ?(pos = 0) ?len ~fd sock =
  let len =
    match len with
    | Some len -> len
    | None -> Int64.to_int_exn (Int64.( - ) (Unix.fstat fd).st_size (Int64.of_int pos))
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

  external raw_sysinfo : unit -> Raw_sysinfo.t = "core_linux_sysinfo"

  let sysinfo = Ok (fun () ->
    let raw = raw_sysinfo () in
    {
      uptime = Time.Span.of_int_sec raw.Raw_sysinfo.uptime;
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
  : file_descr -> tcp_bool_option -> bool = "core_linux_gettcpopt_bool_stub"

external settcpopt_bool
  : file_descr -> tcp_bool_option -> bool -> unit = "core_linux_settcpopt_bool_stub"

external peer_credentials : file_descr -> Peer_credentials.t = "core_linux_peer_credentials"

external unsafe_send_nonblocking_no_sigpipe
  : file_descr -> pos : int -> len : int -> Bytes.t -> int
  = "core_linux_send_nonblocking_no_sigpipe_stub"

let unsafe_send_nonblocking_no_sigpipe fd ~pos ~len buf =
  let res = unsafe_send_nonblocking_no_sigpipe fd ~pos ~len buf in
  if res = -1 then None
  else Some res

external unsafe_send_no_sigpipe
  : file_descr -> pos : int -> len : int -> Bytes.t -> int
  = "core_linux_send_no_sigpipe_stub"

let check_send_args ?pos ?len buf =
  let str_len = Bytes.length buf in
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
  : file_descr -> string Unix.IOVec.t array -> int -> int
  = "core_linux_sendmsg_nonblocking_no_sigpipe_stub"

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

external pr_set_pdeathsig : Signal.t -> unit = "core_linux_pr_set_pdeathsig_stub"
external pr_get_pdeathsig : unit -> Signal.t = "core_linux_pr_get_pdeathsig_stub"

external pr_set_name_first16 : string -> unit = "core_linux_pr_set_name"
external pr_get_name : unit -> string = "core_linux_pr_get_name"

let file_descr_realpath fd =
  Filename.realpath ("/proc/self/fd/" ^ File_descr.to_string fd)

let out_channel_realpath oc = file_descr_realpath (Unix.descr_of_out_channel oc)
let in_channel_realpath ic = file_descr_realpath (Unix.descr_of_in_channel ic)

external raw_sched_setaffinity
  : pid : int -> cpuset : int list -> unit = "core_linux_sched_setaffinity"

let sched_setaffinity ?pid ~cpuset () =
  let pid = match pid with None -> 0 | Some pid -> Pid.to_int pid in
  raw_sched_setaffinity ~pid ~cpuset
;;

external raw_sched_getaffinity
  : pid : int -> int list = "core_linux_sched_getaffinity"

let sched_getaffinity ?pid () =
  let pid = match pid with None -> 0 | Some pid -> Pid.to_int pid in
  raw_sched_getaffinity ~pid
;;

(* defined in unix_stubs.c *)
external gettid : unit -> int = "core_unix_gettid"

external setpriority : Priority.t -> unit = "core_linux_setpriority"

external getpriority : unit -> Priority.t = "core_linux_getpriority"

let sched_setaffinity_this_thread ~cpuset =
  sched_setaffinity ~pid:(Pid.of_int (gettid ())) ~cpuset ()
;;

let cores =
  Memo.unit (fun () ->
    let num_cores =
      In_channel.with_file "/proc/cpuinfo" ~f:In_channel.input_lines
      |> List.fold_left ~init:0 ~f:(fun count line ->
        count +
        (match String.lsplit2 ~on:':' line with
         | None -> 0
         | Some (label, _) ->
           if String.(=) (String.rstrip label) "processor" then 1
           else 0))
    in
    if num_cores > 0 then num_cores
    else failwith "Linux_ext.cores: failed to parse /proc/cpuinfo")

external get_terminal_size : File_descr.t -> int * int = "core_linux_get_terminal_size"

let get_terminal_size = function
  | `Fd fd -> get_terminal_size fd
  | `Controlling ->
    protectx
      (Unix.openfile "/dev/tty" ~mode:[ O_RDWR ] ~perm:0)
      ~finally:Unix.close
      ~f:get_terminal_size

external get_ipv4_address_for_interface : string -> string =
  "core_linux_get_ipv4_address_for_interface" ;;

(* The C-stub is a simple pass-through of the linux SO_BINDTODEVICE semantics, wherein an
   empty string removes any binding *)
external bind_to_interface' : File_descr.t -> string -> unit =
  "core_linux_bind_to_interface" ;;

let bind_to_interface fd ifname =
  let name =
    match ifname with
    | Bound_to_interface.Only name -> name
    | Bound_to_interface.Any -> ""
  in
  bind_to_interface' fd name
;;

external get_bind_to_interface' : File_descr.t -> string =
  "core_linux_get_bind_to_interface" ;;

let get_bind_to_interface fd =
  match get_bind_to_interface' fd with
  | "" -> Bound_to_interface.Any
  | name -> Bound_to_interface.Only name

module Epoll = struct

  external flag_epollin      : unit -> Int63.t  = "core_linux_epoll_EPOLLIN_flag"
  external flag_epollout     : unit -> Int63.t  = "core_linux_epoll_EPOLLOUT_flag"
  (* external flag_epollrdhup   : unit -> Int63.t  = "core_linux_epoll_EPOLLRDHUP_flag" *)
  external flag_epollpri     : unit -> Int63.t  = "core_linux_epoll_EPOLLPRI_flag"
  external flag_epollerr     : unit -> Int63.t  = "core_linux_epoll_EPOLLERR_flag"
  external flag_epollhup     : unit -> Int63.t  = "core_linux_epoll_EPOLLHUP_flag"
  external flag_epollet      : unit -> Int63.t  = "core_linux_epoll_EPOLLET_flag"
  external flag_epolloneshot : unit -> Int63.t  = "core_linux_epoll_EPOLLONESHOT_flag"

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

  external epoll_create : unit -> File_descr.t = "core_linux_epoll_create"

  (* Some justification for the below interface: Unlike select() and poll(), epoll() fills
     in an array of ready events, analogous to a read() call where you pass in a buffer to
     be filled.

     Since this is at the core of the I/O loop, we'd like to avoid reallocating that
     buffer on every call to poll.  We're allocating the array on the ocaml side (as a
     Bigstring), then iterating through it in-place, reducing allocation, copies, and any
     intermediate lists.  For very high message rates and many fds this could be a very
     beneficial. *)
  type ready_events = Bigstring.t

  external epoll_sizeof_epoll_event
    : unit -> int = "core_linux_epoll_sizeof_epoll_event" [@@noalloc]

  external epoll_offsetof_readyfd
    : unit -> int = "core_linux_epoll_offsetof_readyfd" [@@noalloc]

  external epoll_offsetof_readyflags
    : unit -> int = "core_linux_epoll_offsetof_readyflags" [@@noalloc]

  let sizeof_epoll_event  = epoll_sizeof_epoll_event ()
  let offsetof_readyfd    = epoll_offsetof_readyfd ()
  let offsetof_readyflags = epoll_offsetof_readyflags ()

  external epoll_ctl_add
    : File_descr.t -> File_descr.t -> Flags.t -> unit
    = "core_linux_epoll_ctl_add"

  external epoll_ctl_mod
    : File_descr.t -> File_descr.t -> Flags.t -> unit
    = "core_linux_epoll_ctl_mod"

  external epoll_ctl_del
    : File_descr.t -> File_descr.t -> unit
    = "core_linux_epoll_ctl_del"

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
    [@@deriving fields, sexp_of]
  end

  open T

  let epoll_readyfd t i =
    Bigstring.unsafe_get_int32_le
      t
      ~pos:(i*sizeof_epoll_event + offsetof_readyfd)
    |> File_descr.of_int
  ;;

  let epoll_readyflags t i =
    Bigstring.unsafe_get_int32_le
      t
      ~pos:(i*sizeof_epoll_event + offsetof_readyflags)
    |> Flags.of_int
  ;;

  type in_use = ready_events T.t

  module Pretty = struct
    type ready_event =
      { file_descr : File_descr.t;
        flags : Flags.t;
      }
    [@@deriving sexp_of]

    type ready_events = ready_event array [@@deriving sexp_of]

    type t = ready_events T.t [@@deriving sexp_of]
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

  type t = [ `Closed | `In_use of in_use ] ref [@@deriving sexp_of]

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
        failwiths ~here:[%here] "Epoll.invariant failed" (exn, t) [%sexp_of: exn * in_use]
  ;;

  let create ~num_file_descrs ~max_ready_events =
    if max_ready_events < 0 then
      failwiths ~here:[%here] "Epoll.create got nonpositive max_ready_events" max_ready_events
        ([%sexp_of: int]);
    ref (`In_use
           { epollfd = epoll_create ();
             flags_by_fd =
               Table.create
                 ~num_keys:num_file_descrs
                 ~key_to_int:File_descr.to_int
                 ~sexp_of_key:File_descr.sexp_of_t
                 ();
             max_ready_events;
             num_ready_events = 0;
             ready_events = Bigstring.create (sizeof_epoll_event * max_ready_events);
           })
  ;;

  let in_use_exn t =
    match !t with
    | `Closed -> failwith "attempt to use closed epoll set"
    | `In_use r -> r
  ;;

  let find t file_descr =
    let t = in_use_exn t in
    Table.find t.flags_by_fd file_descr
  ;;

  let find_exn t file_descr =
    let t = in_use_exn t in
    Table.find_exn t.flags_by_fd file_descr
  ;;

  let iter t ~f =
    let t = in_use_exn t in
    Table.iteri t.flags_by_fd ~f:(fun ~key:file_descr ~data:flags ->
      f file_descr flags)
  ;;

  let set t fd flags =
    let t = in_use_exn t in
    let already_present = Table.mem t.flags_by_fd fd in
    Table.set t.flags_by_fd ~key:fd ~data:flags;
    if already_present
    then epoll_ctl_mod t.epollfd fd flags
    else epoll_ctl_add t.epollfd fd flags
  ;;

  let remove t fd =
    let t = in_use_exn t in
    if Table.mem t.flags_by_fd fd then epoll_ctl_del t.epollfd fd;
    Table.remove t.flags_by_fd fd
  ;;

  external epoll_wait : File_descr.t -> ready_events -> int -> int = "core_linux_epoll_wait"

  let wait_internal t ~timeout_ms =
    let t = in_use_exn t in
    (* We clear [num_ready_events] because [epoll_wait] will invalidate [ready_events],
       and we don't want another thread to observe [t] and see junk. *)
    t.num_ready_events <- 0;
    t.num_ready_events <- epoll_wait t.epollfd t.ready_events timeout_ms;
    if t.num_ready_events = 0 then `Timeout else `Ok
  ;;

  let wait_timeout_after t span =
    let timeout_ms =
      if Time_ns.Span.( <= ) span Time_ns.Span.zero
      then 0
      else
        (* For positive timeouts, we use a minimum timeout of one millisecond, to ensure
           that we are guaranteed that the timeout has passed when we wake up.  If we
           allowed a positive sub-millisecond timeout, we would round down and end up
           using a timeout of zero, causing [wait_internal] to return immediately.  Such
           behaviour has been seen to cause Async to spin, repeatedly requesting slightly
           smaller timeouts. *)
        let span = Time_ns.Span.max span Time_ns.Span.millisecond in
        Int63.to_int_exn
          Time_ns.Span.(div
                          (span + of_int63_ns (Int63.of_int 500_000))
                          (of_int63_ns (Int63.of_int 1_000_000)))
    in
    assert (timeout_ms >= 0);
    wait_internal t ~timeout_ms
  ;;

  let wait t ~timeout =
    (* From the epoll man page:

       | Specifying a timeout of -1 makes epoll_wait() wait indefinitely, while
       | specifying a timeout equal to zero makes epoll_wait() to return immediately
       | even if no events are available (return code equal to zero). *)
    match timeout with
    | `Never       -> wait_internal      t ~timeout_ms:(-1)
    | `Immediately -> wait_internal      t ~timeout_ms:0
    | `After span  -> wait_timeout_after t span
  ;;

  let fold_ready t ~init ~f =
    let t = in_use_exn t in
    let ac = ref init in
    for i = 0 to t.num_ready_events - 1 do
      ac := f !ac (epoll_readyfd t.ready_events i) (epoll_readyflags t.ready_events i)
    done;
    !ac
  ;;

  let iter_ready t ~f =
    let t = in_use_exn t in
    for i = 0 to t.num_ready_events - 1 do
      f (epoll_readyfd t.ready_events i) (epoll_readyflags t.ready_events i)
    done
  ;;

  module Expert = struct
    let clear_ready t =
      let t = in_use_exn t in
      t.num_ready_events <- 0
    ;;
  end

  (* external epoll_pwait
   *   : File_descr.t -> Events_buffer.raw -> int -> int list -> int
   *   = "core_linux_epoll_pwait"
   *
   * let pwait t ~timeout sigs =
   *   let millis = Float.iround_exn ~dir:`Zero ( Span.to_ms timeout ) in
   *   let num_ready = epoll_pwait t.epollfd t.events millis sigs in
   *   if num_ready = 0 then `Timeout
   *   else `Ok { Ready_fds.num_ready ; events = t.events }
   * ;; *)

  let create = Ok create
end

let cores                          = Ok cores
let file_descr_realpath            = Ok file_descr_realpath
let get_ipv4_address_for_interface = Ok get_ipv4_address_for_interface
let bind_to_interface              = Ok bind_to_interface
let get_bind_to_interface          = Ok get_bind_to_interface
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
let sched_getaffinity              = Ok sched_getaffinity
let sched_setaffinity_this_thread  = Ok sched_setaffinity_this_thread
let send_no_sigpipe                = Ok send_no_sigpipe
let send_nonblocking_no_sigpipe    = Ok send_nonblocking_no_sigpipe
let sendfile                       = Ok sendfile
let sendmsg_nonblocking_no_sigpipe = Ok sendmsg_nonblocking_no_sigpipe
let settcpopt_bool                 = Ok settcpopt_bool
let peer_credentials               = Ok peer_credentials

module Extended_file_attributes = struct
  module Flags = struct
    external only_create : unit -> Int63.t = "core_linux_xattr_XATTR_CREATE_flag"
    external only_replace : unit -> Int63.t = "core_linux_xattr_XATTR_REPLACE_flag"
    let set = Int63.zero
  end

  module Get_attr_result = struct
    type t =
      | Ok of string
      | ENOATTR
      | ERANGE
      | ENOTSUP
    [@@deriving sexp_of]
  end

  module Set_attr_result = struct
    type t =
      | Ok
      | EEXIST
      | ENOATTR
      | ENOTSUP
    [@@deriving sexp_of]
  end

  external getxattr : string -> string -> Get_attr_result.t = "core_linux_getxattr"
  external setxattr : string -> string -> string -> Int63.t -> Set_attr_result.t = "core_linux_setxattr"

  let getxattr ~path ~name =
    getxattr path name
  ;;

  let setxattr ?(how = `Set) ~path ~name ~value () =
    let flags =
      match how with
      | `Set -> Flags.set
      | `Create -> Flags.only_create ()
      | `Replace -> Flags.only_replace ()
    in
    setxattr path name value flags
  ;;

  let getxattr = Ok getxattr
  let setxattr = Ok setxattr
end

[%%else]
include Null_toplevel
module Eventfd = Null.Eventfd
module Extended_file_attributes = Null.Extended_file_attributes
[%%endif]
