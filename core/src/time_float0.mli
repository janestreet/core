open! Import

module Span : module type of struct
  include Span_float
end
with module Stable := Span_float.Stable

module Ofday : module type of struct
  include Ofday_float
end
with module Stable := Ofday_float.Stable

include
  Time0_intf.S
  with type underlying = float
   and module Span := Span
   and module Ofday := Ofday

module Stable : sig
  module Span = Span_float.Stable
  module Ofday = Ofday_float.Stable
end
