open! Import

include module type of struct
  include Base.Stack
end

include Binable.S1 with type 'a t := 'a t
