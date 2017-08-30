include (Core_kernel.Core_kernel_stable : module type of Core_kernel.Core_kernel_stable
         with module Time_ns := Core_kernel.Core_kernel_stable.Time_ns
         with module Time    := Core_kernel.Core_kernel_stable.Time)

module Interval               = Interval               .Stable
module Mac_address            = Mac_address            .Stable
module Schedule_v4_deprecated = Schedule_v4_deprecated .Stable
module Schedule_v5            = Schedule_v5            .Stable
module Time                   = Core_time_float        .Stable
module Time_ns                = Core_time_ns           .Stable
module Unix                   = Core_unix              .Stable
module User_and_group         = User_and_group         .Stable
module Uuid                   = Uuid                   .Stable
