(** [Core] is an extension of {{!Core_kernel}[Core_kernel]} with Unix APIs. The unmodified
    libraries can be found there.

    In particular, [Core] has comprehensive implementation of times ([Time] and
    [Time_ns]), where some details are platform-specific.

    Some modules are mere extensions of those existing in [Core_kernel], like [Bigstring],
    [Caml], [Time], and [Md5], where what's added is handlers for reading from or writing
    to Unix sockets and file descriptors, or support for floating-point numbers. Other
    modules are entirely new, like:

    - [Command], a richly featured tool for creating command-line programs.
    - [Iobuf], which lets you use contiguous ranges of bytes for I/O purposes.
    - [Linux_ext], providing a wrapper around Linux-specific system calls.
    - [Signal], for handling Unix signals like SIGHUP and SIGKILL.

    A few modules in Core don't have any platform-specific functionality but haven't yet
    been ported to Core_kernel for technical reasons (like a dependency on [Time], which
    until recently was only in Core):

    - [Interval]
*)
(**/**)

include Core_kernel.Core_kernel_private.Std_kernel
(**/**)

module Bigstring              = Bigstring
module Caml                   = Caml
module Command                = Command
module Core_stable            = Stable
module Date                   = Core_date
module Filename               = Core_filename
module Interval               = Interval
module Interval_intf          = Interval_intf
module Iobuf                  = Iobuf
module Iobuf_debug            = Iobuf_debug
module Iobuf_intf             = Iobuf_intf
module Linux_ext              = Linux_ext
module Digest                 = Md5 [@@ocaml.deprecated "[since 2017-05] Use Md5 instead."]
(* When we moved [Mutex] out of [Core], we added this declaration of [Mutex] to prevent a
   mistake in which code that used to use [Core.Mutex] is unintentionally and silently
   switched to the stdlib's [Mutex] module. *)
module Mutex = struct end [@@deprecated "[since 2019-02] Use [Error_checking_mutex]"]
module Signal                 = Signal
module Sys                    = Core_sys
module Thread                 = Core_thread
module Time                   = Core_time_float
module Time_common            = Time_common
module Time_ns                = Core_time_ns
module Unix                   = Core_unix
module Version_util           = Version_util

(* Can't go in Common for circular-reference reasons *)
let sec = Time.Span.of_sec
let ( ^/ ) = Core_filename.concat

module Core_private = struct
  module Core_zone = Core_zone
end
