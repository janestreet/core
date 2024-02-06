module Unit_test = Stable_unit_test.Make

module type Stable = Stable_module_types.S0
module type Stable_without_comparator = Stable_module_types.S0_without_comparator
module type Stable1 = Stable_module_types.S1
module type Stable2 = Stable_module_types.S2
module type Stable3 = Stable_module_types.S3
module type Stable4 = Stable_module_types.S4
module type Stable_with_witness = Stable_module_types.With_stable_witness.S0
module type Stable_int63able_with_witness = Stable_int63able.With_stable_witness.S

module type Stable_without_comparator_with_witness =
  Stable_module_types.With_stable_witness.S0_without_comparator

module type Stable1_with_witness = Stable_module_types.With_stable_witness.S1
module type Stable2_with_witness = Stable_module_types.With_stable_witness.S2
module type Stable3_with_witness = Stable_module_types.With_stable_witness.S3
module type Stable4_with_witness = Stable_module_types.With_stable_witness.S4

module Make_stable = Make_stable
include Stable_internal
module Hashable = Hashable.Stable
module Bigstring = Bigstring.Stable
module Binable = Binable.Stable
module Blang = Blang.Stable
module Bool = Bool.Stable
module Byte_units = Byte_units.Stable
module Bytes = Bytes.Stable
module Char = Char.Stable
module Comparable = Comparable.Stable
module Comparator = Comparator.Stable
module Date = Date.Stable
module Day_of_week = Day_of_week.Stable
module Md5 = Md5.Stable
module Either = Either.Stable
module Error = Error.Stable
module Fdeque = Fdeque.Stable
module Filename = Filename.Stable
module Float = Float.Stable
module Float_with_finite_only_serialization = Float_with_finite_only_serialization.Stable
module Fqueue = Fqueue.Stable
module Gc = Gc.Stable
module Host_and_port = Host_and_port.Stable
module Info = Info.Stable
module Int = Int.Stable
module Int63 = Int63.Stable
module Lazy = Lazy.Stable
module List = List.Stable
module Map = Map.Stable
module Maybe_bound = Maybe_bound.Stable
module Month = Month.Stable
module Nothing = Nothing.Stable
module Option = Option.Stable
module Or_error = Or_error.Stable
module Percent = Percent.Stable
module Perms = Perms.Stable
module Pid = Pid.Stable
module Queue = Queue.Stable
module Result = Result.Stable
module Set = Set.Stable
module Sexp = Sexp.Stable
module Sexpable = Sexpable.Stable
module Source_code_position = Source_code_position.Stable
module String = String.Stable
module String_id = String_id.Stable
module Time_ns = Time_ns.Stable

module Time = Time_float.Stable
[@@deprecated "[since 2021-11] Use [Time_float] or [Time_float_unix] instead"]

module Time_float = Time_float.Stable
module Uchar = Uchar.Stable
module Unit = Unit.Stable
module Unix = struct end [@@deprecated "[since 2021-02] Use [Core_unix.Stable]"]
include Perms.Export
include Ppx_compare_lib.Builtin
include Base.Exported_for_specific_uses.Globalize
include Import.Not_found
