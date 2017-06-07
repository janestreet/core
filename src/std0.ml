include Core_kernel.Core_kernel_private.Std_kernel

let ( ^/ ) = Core_filename.concat

module Bigbuffer          = Bigbuffer
module Bigstring          = Bigstring
module Bigstring_marshal  = Bigstring_marshal
module Caml               = Caml
module Command            = Command
module Condition          = Core_condition
module Core_stable        = Stable
module Crc                = Crc
module Daemon             = Daemon
module Date               = Core_date
module Filename           = Core_filename
module Interval           = Interval
module Iobuf              = Iobuf
module Iobuf_debug        = Iobuf_debug
module Iobuf_intf         = Iobuf_intf
module Linux_ext          = Linux_ext
module Lock_file          = Lock_file
module Mac_address        = Mac_address
module Mutex              = Core_mutex
module Nano_mutex         = Nano_mutex
module Piecewise_linear   = Piecewise_linear
module Process_env        = Process_env
module Schedule           = Schedule
module Signal             = Signal
module Squeue             = Squeue
module Sys                = Core_sys
module Thread             = Core_thread
module Time               = Core_time_float
module Time_common        = Time_common
module Time_ns            = Core_time_ns
module Time_stamp_counter = Time_stamp_counter
module Timing_wheel_float = Timing_wheel_float
module Unix               = Core_unix
module User_and_group     = User_and_group
module Uuid               = Uuid
module Version_util       = Version_util
module Weak_hashtbl       = Weak_hashtbl

(* Can't go in Common for circular-reference reasons *)
let sec = Time.Span.of_sec

(* See [Core_kernel.Std_kernel] for the reason that we perform top-level side effects in
   the [Std] modules. *)
let () =
  Am_running_inline_test.initialize_module ()
;;
