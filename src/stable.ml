include (Core_kernel.Stable : module type of Core_kernel.Stable
         with module Time_ns := Core_kernel.Stable.Time_ns)

module Interval       = Interval      .Stable
module Mac_address    = Mac_address   .Stable
module Schedule       = Schedule      .Stable
module Time           = Time          .Stable
module Time_ns        = Time_ns       .Stable
module Unix           = Core_unix     .Stable
module User_and_group = User_and_group.Stable
module Uuid           = Uuid          .Stable
module Zone           = Zone          .Stable
