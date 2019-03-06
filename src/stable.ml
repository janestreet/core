include (Core_kernel.Core_kernel_stable : module type of Core_kernel.Core_kernel_stable
         with module Time_ns := Core_kernel.Core_kernel_stable.Time_ns
         with module Time    := Core_kernel.Core_kernel_stable.Time)

module Interval               = Interval               .Stable
module Time                   = Core_time_float        .Stable
module Time_ns                = Core_time_ns           .Stable
module Unix                   = Core_unix              .Stable
