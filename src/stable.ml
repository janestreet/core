include Core_kernel.Stable

module Date           = Date          .Stable
module Interval       = Interval      .Stable
module Ofday          = struct
  include Ofday.Stable
  module Zoned = Ofday.Zoned.Stable
end

module Schedule       = Schedule      .Stable
module Span           = Span          .Stable
module Time           = Time          .Stable
module Uuid           = Uuid          .Stable
module User_and_group = User_and_group.Stable
module Zone           = Zone          .Stable

module Time_ns = struct
  include Time_ns.Stable
  module Option = Time_ns.Option.Stable
  module Span = struct
    include Time_ns.Span.Stable
    module Option = Time_ns.Span.Option.Stable
  end
  module Ofday = struct
    include Time_ns.Ofday.Stable
    module Option = Time_ns.Ofday.Option.Stable
  end
end

module Unix = struct
  module Inet_addr = Core_unix.Inet_addr.Stable
end
