open! Import
open Import_time

module Unix = Core_unix

module Command = Core_kernel.Command

include (Command
         : (module type of struct include Command end
           with module Shape := Command.Shape
           with module Deprecated := Command.Deprecated))

module Path = Private.Path

module For_unix = Private.For_unix (struct
    module Signal = Signal
    module Thread = Core_thread
    module Time = Time
    module Unix = struct
      include Unix
      let unsafe_getenv = Core_sys.unsafe_getenv
    end
    module Version_util = Version_util
  end)

let run = For_unix.run
let shape = For_unix.shape

module Deprecated = struct
  include Command.Deprecated
  let run = For_unix.deprecated_run
end

module Shape = struct
  include Command.Shape
  let help_text = For_unix.help_for_shape
end
