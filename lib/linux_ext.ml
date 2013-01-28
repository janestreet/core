open Unix
open Std_internal

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

module Clock0 = struct
  type t
end

module Clock_unimplemented = struct
  include Clock0
  let get               = unimplemented "Linux_ext.Clock.get"
  let get_time          = unimplemented "Linux_ext.Clock.get_time"
  let set_time          = unimplemented "Linux_ext.Clock.set_time"
  let get_resolution    = unimplemented "Linux_ext.Clock.get_resolution"
  let get_process_clock = unimplemented "Linux_ext.Clock.get_process_clock"
  let get_thread_clock  = unimplemented "Linux_ext.Clock.get_thread_clock"
end

(* If you update this type, you also must update linux_tcpopt_bool, in the C stubs. (And
   do make sure you get the order correct) *)
type tcp_bool_option = TCP_CORK with sexp, bin_io

INCLUDE "config.mlh"
IFDEF LINUX_EXT THEN

external sendfile :
  sock : file_descr -> fd : file_descr -> pos : int -> len : int -> int
    = "linux_sendfile_stub"
;;

let sendfile ?(pos = 0) ?len ~fd sock =
  let len =
    match len with
    | Some len -> len
    | None -> (fstat fd).st_size - pos
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

external gettcpopt_bool :
  file_descr -> tcp_bool_option -> bool = "linux_gettcpopt_bool_stub"

external settcpopt_bool :
  file_descr -> tcp_bool_option -> bool -> unit = "linux_settcpopt_bool_stub"

external unsafe_send_nonblocking_no_sigpipe :
  file_descr -> pos : int -> len : int -> string -> int
  = "linux_send_nonblocking_no_sigpipe_stub"

let unsafe_send_nonblocking_no_sigpipe fd ~pos ~len buf =
  let res = unsafe_send_nonblocking_no_sigpipe fd ~pos ~len buf in
  if res = -1 then None
  else Some res

external unsafe_send_no_sigpipe :
  file_descr -> pos : int -> len : int -> string -> int
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

external unsafe_sendmsg_nonblocking_no_sigpipe :
  file_descr -> string Core_unix.IOVec.t array -> int -> int
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

IFDEF POSIX_TIMERS THEN
module Clock = struct
  include Clock0

  (* These functions should be in Unix, but due to the dependency on Time,
     this is not possible (cyclic dependency). *)
  external get_time : t -> float = "unix_clock_gettime"
  let get_time t = Span.of_float (get_time t)

  external set_time : t -> float -> unit = "unix_clock_settime"
  let set_time t s = set_time t (Span.to_float s)

  external get_resolution : t -> float = "unix_clock_getres"
  let get_resolution t = Span.of_float (get_resolution t)

  external get : Thread.t -> t = "unix_pthread_getcpuclockid"

  external get_process_clock : unit -> t = "unix_clock_process_cputime_id_stub"

  external get_thread_clock : unit -> t = "unix_clock_thread_cputime_id_stub"

  let get               = Ok get
  let get_time          = Ok get_time
  let set_time          = Ok set_time
  let get_resolution    = Ok get_resolution
  let get_process_clock = Ok get_process_clock
  let get_thread_clock  = Ok get_thread_clock

end
ELSE
module Clock = Clock_unimplemented
ENDIF

external pr_set_pdeathsig : Signal.t -> unit = "linux_pr_set_pdeathsig_stub"
external pr_get_pdeathsig : unit -> Signal.t = "linux_pr_get_pdeathsig_stub"

external pr_set_name_first16 : string -> unit = "linux_pr_set_name"
external pr_get_name : unit -> string = "linux_pr_get_name"

let file_descr_realpath fd =
  Core_filename.realpath ("/proc/self/fd/" ^ Core_unix.File_descr.to_string fd)

let out_channel_realpath oc = file_descr_realpath (descr_of_out_channel oc)
let in_channel_realpath ic = file_descr_realpath (descr_of_in_channel ic)

external raw_sched_setaffinity :
  pid : int -> cpuset : int list -> unit = "linux_sched_setaffinity"

let sched_setaffinity ?pid ~cpuset () =
  let pid = match pid with None -> 0 | Some pid -> Pid.to_int pid in
  raw_sched_setaffinity ~pid ~cpuset
;;

external gettid : unit -> int = "linux_ext_gettid"

let sched_setaffinity_this_thread ~cpuset =
  sched_setaffinity ~pid:(Pid.of_int (gettid ())) ~cpuset ()
;;

let cores =
  Memo.unit (fun () ->
    Exn.protectx
      (open_in "/proc/cpuinfo")
      ~finally:close_in_noerr
      ~f:(fun ic ->
        let rec loop count =
          let line =
            try Some (input_line ic) with
            | End_of_file -> None
          in
          match line with
          | None -> count
          | Some line ->
              match Core_string.lsplit2 ~on:':' line with
              | None -> loop count
              | Some (label, _) ->
                  if Core_string.(=) (Core_string.rstrip label) "processor" then
                    loop (count + 1)
                  else
                    loop count
        in
        let num_cores = loop 0 in
        if num_cores > 0 then num_cores
        else failwith "failed to parse /proc/cpuinfo"
      )
  )
;;

external get_terminal_size : unit -> int * int = "linux_get_terminal_size"

external get_ipv4_address_for_interface : string -> string =
  "linux_get_ipv4_address_for_interface" ;;

let cores                          = Ok cores
let file_descr_realpath            = Ok file_descr_realpath
let get_ipv4_address_for_interface = Ok get_ipv4_address_for_interface
let get_terminal_size              = Ok get_terminal_size
let gettcpopt_bool                 = Ok gettcpopt_bool
let gettid                         = Ok gettid
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

  let sysinfo = unimplemented "Linux_ext.Sysinfo.sysinfo"
end

module Clock = Clock_unimplemented

let cores                          = unimplemented "Linux_ext.cores"
let file_descr_realpath            = unimplemented "Linux_ext.file_descr_realpath"
let get_ipv4_address_for_interface = unimplemented "Linux_ext.get_ipv4_address_for_interface"
let get_terminal_size              = unimplemented "Linux_ext.get_terminal_size"
let gettcpopt_bool                 = unimplemented "Linux_ext.gettcpopt_bool"
let gettid                         = unimplemented "Linux_ext.gettid"
let in_channel_realpath            = unimplemented "Linux_ext.in_channel_realpath"
let out_channel_realpath           = unimplemented "Linux_ext.out_channel_realpath"
let pr_get_name                    = unimplemented "Linux_ext.pr_get_name"
let pr_get_pdeathsig               = unimplemented "Linux_ext.pr_get_pdeathsig"
let pr_set_name_first16            = unimplemented "Linux_ext.pr_set_name_first16"
let pr_set_pdeathsig               = unimplemented "Linux_ext.pr_set_pdeathsig"
let sched_setaffinity              = unimplemented "Linux_ext.sched_setaffinity"
let sched_setaffinity_this_thread  = unimplemented "Linux_ext.sched_setaffinity_this_thread"
let send_no_sigpipe                = unimplemented "Linux_ext.send_no_sigpipe"
let send_nonblocking_no_sigpipe    = unimplemented "Linux_ext.send_nonblocking_no_sigpipe"
let sendfile                       = unimplemented "Linux_ext.sendfile"
let sendmsg_nonblocking_no_sigpipe = unimplemented "Linux_ext.sendmsg_nonblocking_no_sigpipe"
let settcpopt_bool                 = unimplemented "Linux_ext.settcpopt_bool"

ENDIF
