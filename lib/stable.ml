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


