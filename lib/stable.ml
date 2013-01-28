(* This module collects various Stable sub-modules from Core.  A Stable module is one that
   defines types for which it is guaranteed that the serializations will never change,
   so they can be safely used in protocols.  Within each Stable module there are one or
   more sub-modules of the form Vn where n is a version number.  If it is necessary to
   make changes that would change the serialization, a new Vn module will be minted to
   reflect this change. *)

module Unit_test = Stable_unit_test.Make

(* miscellaneous unit tests that can't be put in their respective .mls due to circular
   dependencies *)
module Unit_tests = struct
  TEST_MODULE "Result.V1" = Unit_test(Result.Stable.V1_stable_unit_test)
end

include Stable_internal

include Stable_containers
module Blang     = Blang    .Stable
module Date      = Date     .Stable
module Interval  = Interval .Stable
module Month     = Month    .Stable
module Ofday     = Ofday    .Stable
module Result    = Result   .Stable
module Span      = Span     .Stable
module String_id = String_id.Stable
module Time      = Time     .Stable
module Weekday   = Weekday  .Stable
module Zone      = Zone     .Stable


