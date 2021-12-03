open! Import

include module type of struct
  include Base.Fn
end

include module type of struct
  include Deprecate_pipe_bang
end
