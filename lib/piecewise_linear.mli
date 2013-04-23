open Std_internal

(** piece-wise linear interpolation from float-like types to float *)

open Piecewise_linear_intf

module type S = S
module type S_invertible = S_invertible

module Make (Key : Key) (Value : Value) : S
  with type key = Key.t
  with type value = Value.t

module Make_invertible (Key : Key) (Value : Value) : S_invertible
  with type key = Key.t
  with type value = Value.t

module Time  : S with type key = Time.t  with type value = float
module Ofday : S with type key = Ofday.t with type value = float
module Span  : S with type key = Span.t  with type value = float
module Float : S with type key = float   with type value = float
module Int   : S with type key = int     with type value = float

