(** [Core] is an extension of {{!Core_kernel}[Core_kernel]} with Unix APIs. The unmodified
    libraries can be found there.

    In particular, [Core] has comprehensive implementation of times (see [Time],
    [Time_ns], [Timing_wheel_float], and [Schedule_v5]), where some details are
    platform-specific (like timing wheels based on floats).

    Some modules are mere extensions of those existing in [Core_kernel], like [Bigbuffer],
    [Bigstring], [Caml], [Time], and [Md5], where what's added is handlers for reading
    from or writing to Unix sockets and file descriptors, or support for floating-point
    numbers. Other modules are entirely new, like:

    - [Command], a richly featured tool for creating command-line programs.
    - [Daemon], for daemonizing processes.
    - [Iobuf], which lets you use contiguous ranges of bytes for I/O purposes.
    - [Lock_file], for managing OS-level locks.
    - [Crc], for cyclic redundancy checks.
    - [Linux_ext], providing a wrapper around Linux-specific system calls.
    - [Mac_address], for managing MAC addresses.
    - [Signal], for handling Unix signals like SIGHUP and SIGKILL.

    A few modules in Core don't have any platform-specific functionality but haven't yet
    been ported to Core_kernel for technical reasons (like a dependency on [Time], which
    until recently was only in Core):

    - [Interval]
    - [Squeue]
    - [Uuid]
*)
(**/**)
include Core_kernel.Core_kernel_private.Std_kernel
(**/**)

module Bigbuffer              = Bigbuffer
module Bigstring              = Bigstring
module Bigstring_marshal      = Bigstring_marshal
module Caml                   = Caml
module Command                = Command
module Condition              = Core_condition
module Core_stable            = Stable
module Crc                    = Crc
module Daemon                 = Daemon
module Date                   = Core_date
module Filename               = Core_filename
module Interval               = Interval
module Interval_intf          = Interval_intf
module Iobuf                  = Iobuf
module Iobuf_debug            = Iobuf_debug
module Iobuf_intf             = Iobuf_intf
module Linux_ext              = Linux_ext
module Lock_file              = Lock_file
module Mac_address            = Mac_address
module Md5                    = Md5
module Digest                 = Md5 [@@ocaml.deprecated "[since 2017-05] Use Md5 instead."]
module Mutex                  = Core_mutex
module Nano_mutex             = Nano_mutex
module Piecewise_linear       = Piecewise_linear
module Process_env            = Process_env
module Schedule_v4_deprecated = Schedule_v4_deprecated
module Schedule_v5            = Schedule_v5
module Signal                 = Signal
module Squeue                 = Squeue
module Sys                    = Core_sys
module Thread                 = Core_thread
module Time                   = Core_time_float
module Time_common            = Time_common
module Time_ns            = Core_time_ns
module Time_stamp_counter     = Time_stamp_counter
module Timing_wheel_float     = Timing_wheel_float
module Unix                   = Core_unix
module User_and_group         = User_and_group
module Uuid                   = Uuid
module Version_util           = Version_util
module Weak_hashtbl           = Weak_hashtbl

(* Can't go in Common for circular-reference reasons *)
let sec = Time.Span.of_sec
let ( ^/ ) = Core_filename.concat

(* See [Core_kernel.Std_kernel] for the reason that we perform top-level side effects in
   the [Std] modules. *)
let () =
  Am_running_inline_test.initialize_module ()
;;
