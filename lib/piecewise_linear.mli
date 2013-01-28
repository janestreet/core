open Std_internal

(** piece-wise linear interpolation from float-like types to float *)

(* todo: add to_float/of_float for y values for more generality? *)

module type Key = sig
  type t with bin_io, sexp
  include Floatable with type t := t
end

module type S = sig
  type key
  type t with bin_io, sexp

  val create : (key * float) list -> (t, string) Result.t
  val get : t -> key -> float
  val to_knots : t -> (key * float) list
end

module Make (Key : Key) : S with type key = Key.t

module Time : S with type key = Time.t
module Ofday : S with type key = Ofday.t
module Span : S with type key = Span.t
module Float : S with type key = float
module Int : S with type key = int
