(* We list the modules we want to export here and follow the convention of opening
   Core.Std instead of Core. *)

include Std_internal

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
    let yesterday () = Date.add_days (today ()) (-1)
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

module Agnostic_mutex = Agnostic_mutex
module Commutative_group = Commutative_group
module Arg = Core_arg
module Backtrace = Backtrace
module Bag = Bag
module Bigbuffer = Bigbuffer
module Bigstring = Bigstring
module Bigsubstring = Bigsubstring
module Bin_prot = Core_bin_prot

module Binable = struct
  include Binable

  (* [of_string] and [to_string] can't go in binable.ml due to a cyclic dependency. *)
  let of_string m string = of_bigstring m (Bigstring.of_string string)

  let to_string m t = Bigstring.to_string (to_bigstring m t)
end

module Linux_ext = Linux_ext
module Bigstring_marshal = Bigstring_marshal
module Binary_packing = Binary_packing
module Blang = Blang
module Bounded_int_table = Bounded_int_table
module Bucket = Bucket
module Byte_units = Byte_units
module Caml = Caml
module Comparable = Comparable
module Condition = Core_condition
module Container = Container
module Crc = Crc
module Date = Time.Date
module Daemon = Daemon
module Default = Default
module Dequeue = Dequeue
module Doubly_linked = Doubly_linked
module Error = Error
module Exn = Exn
module Float = Float
module Float_intf = Float_intf
module Force_once = Force_once
module Fqueue = Fqueue
module Filename = Core_filename
module Floatable = Floatable
module Fn = Fn
module Gc = Core_gc
module Hash_queue = Hash_queue
module Hash_heap = Hash_heap
module Hash_set = Hash_set
module Hashable = Hashable
module Heap = Heap
module Host_and_port = Host_and_port
module Identifiable = Identifiable
module In_channel = In_channel
module Int63 = Core_int63
module Int_intf = Int_intf
module Int_set = Int_set
module Interfaces = Interfaces
module Interval = Interval
module Field = Core_field
module Lock_file = Lock_file
module Memo = Memo
module Monad = Monad
module Month = Month
module Mutex = Core_mutex
module Nano_mutex = Nano_mutex
module No_polymorphic_compare = No_polymorphic_compare
module Ofday = Time.Ofday
module Only_in_test = Only_in_test
module Option = Option
module Or_error = Or_error
module Out_channel = Out_channel
module Pid = Pid
module Piecewise_linear = Piecewise_linear
module Polymorphic_compare = Polymorphic_compare
module Pretty_printer = Pretty_printer
module Printexc = Core_printexc
module Printf = Core_printf
module Quickcheck = Quickcheck
module Result = Result
module Robustly_comparable = Robustly_comparable
module Set_once = Set_once
module Sexpable = Sexpable
module Sexp_maybe = Core_sexp.Sexp_maybe
module Signal = Signal
module Space_safe_tuple2 = Space_safe_tuple.T2
module Space_safe_tuple3 = Space_safe_tuple.T3
module Squeue = Squeue
module Staged = Staged
module Stringable = Stringable
module String_id = String_id
module Substring = Substring
module Substring_intf = Substring_intf
module Thread = Core_thread
module Thread_safe_queue = Thread_safe_queue
module Timer = Timer
module Tuple = Tuple
module Tuple2 = Tuple.T2
module Tuple3 = Tuple.T3
module Union_find = Union_find
module Unique_id : Unique_id_intf.S = Unique_id
module Unique_id_intf = Unique_id_intf
module Unit = Unit
module Univ = Univ
module Unix = Core_unix
module Unpack_buffer = Unpack_buffer
module Weekday = Weekday
module Word_size = Word_size
module Zone = Zone

module type Unique_id = Unique_id_intf.Id

include T



