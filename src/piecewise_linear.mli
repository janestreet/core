(** Piece-wise linear interpolation from float-like types to float. *)

open Core_kernel.Std

open Piecewise_linear_intf

(** This type constructor is how we expose, for instance, that [Make(K)(V).t] and
    [Stable.V1.Make(K)(V).t] are the same type (as long as [Stable.V1] is current).
    Likewise, if [K0.t = K1.t], then [Make(K0)(V).t = Make(K1)(V).t]. *)
type ('key, 'value) t_

module type S = S with type ('k, 'v) t_ := ('k, 'v) t_

module Make (Key : Float_like) (Value : Float_like) : S
  with type key = Key.t
  with type value = Value.t

type ('key, 'value) t_invertible

module type S_invertible = S_invertible with type ('k, 'v) t_ := ('k, 'v) t_invertible

module Make_invertible (Key : Float_like) (Value : Float_like) : S_invertible
  with type key = Key.t
  with type value = Value.t

module Time  : S with type key = Time.t  with type value = float
module Ofday : S with type key = Ofday.t with type value = float
module Span  : S with type key = Span.t  with type value = float
module Float : S with type key = float   with type value = float
module Int   : S with type key = int     with type value = float

(** Note that applications of the following functors are only as stable
    as [Key] and [Value]. *)
module Stable : sig
  module V1 : sig
    type nonrec ('key, 'value) t_ = ('key, 'value) t_

    module Make (Key : Float_like) (Value : Float_like) : sig
      type t = (Key.t, Value.t) t_ with bin_io, compare, sexp
    end

    type nonrec ('key, 'value) t_invertible = ('key, 'value) t_invertible

    module Make_invertible (Key : Float_like) (Value : Float_like) : sig
      type t = (Key.t, Value.t) t_invertible with bin_io, compare, sexp
    end
  end
end
