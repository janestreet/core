open! Import
open! Std_internal
include Command

module Arg_type = struct
  include Arg_type

  module Export = struct
    include Export

    let date = create Date.of_string
    let percent = create Percent.of_string
    let host_and_port = create Host_and_port.of_string
  end
end

module Param = struct
  include (
    Param :
      sig
        include module type of Param with module Arg_type := Param.Arg_type
      end)

  module Arg_type = Arg_type
  include Arg_type.Export
end

module Spec = struct
  include (
    Spec :
      sig
        include module type of Spec with module Arg_type := Spec.Arg_type
      end)

  module Arg_type = Arg_type
  include Arg_type.Export
end

module Let_syntax = struct
  include Let_syntax

  module Let_syntax = struct
    include Let_syntax
    module Open_on_rhs = Param
  end
end
