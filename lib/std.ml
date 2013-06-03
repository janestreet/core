include Core_kernel.Std_kernel

module Time = struct
  module Zone = Zone
  module Span = Span

  module Ofday = struct
    include Ofday

    (* can't be defined in Ofday directly because it would create a circular reference *)
    let now () = snd (Time.to_local_date_ofday (Time.now ()))
  end

  module Date = struct
    include Date

    let of_time time = Time.to_local_date time
    let today () = of_time (Time.now ())
    let format date pat =
      let time = Time.of_local_date_ofday date Ofday.start_of_day in
      Time.format time pat
  end

  include Time
end

(* Included here instead of in common because time depends on common *)
include Time.Date.Export

(* Can't go in Common for circular-reference reasons *)
let sec = Time.Span.of_sec

let ( ^/ ) = Core_filename.concat

module Caml = Caml
module Command = Command
module Backtrace = Backtrace
module Bigbuffer = Bigbuffer
module Bigstring = Bigstring
module Bigstring_marshal = Bigstring_marshal
module Linux_ext = Linux_ext
module Condition = Core_condition
module Crc = Crc
module Date = Time.Date
module Daemon = Daemon
module Process_env = Process_env
module Filename = Core_filename
module Gc = Core_gc
module Interval = Interval
module Iobuf = Iobuf
module Lock_file = Lock_file
module Mutex = Core_mutex
module Nano_mutex = Nano_mutex
module Piecewise_linear = Piecewise_linear
module Signal = Signal
module Squeue = Squeue
module Sys = Core_sys
module Thread = Core_thread
module Timer = Timer
module Unix = Core_unix
module User_and_group = User_and_group
module Uuid = Uuid
INCLUDE "version_defaults.mlh"
IFDEF BUILD_VERSION_UTIL THEN
  module Version_util = Version_util
ENDIF
module Zone = Zone

let _squelch_unused_module_warning_ = ()

(* These checks are outside modules rather than in them because we want to check a
   property of the module as it is exported in Core.Std, and so we need to feed the entire
   module to the functor. *)
TEST_MODULE = struct

  module Check = Core_kernel.Std.Comparable.Check_sexp_conversion

  include Check (struct
    include Time
    let examples = [ epoch ]
  end)

  include Check (struct
    include Time.Ofday
    let examples = [ start_of_day ]
  end)

  include Check (struct
    include Time.Span
    let examples = [ of_sec 13. ]
  end)

  include Check (struct
    include Core_kernel.Std.Month
    let examples = all
  end)

end
