(* This is factored out of float.ml in order to break a module dependency cycle.  *)

module type S = sig
  (* note: this is not the same as epsilon_float, rather it is intended to
     be a tolerance on human-entered floats *)
  val epsilon : float
  include Robustly_comparable.S with type t := float
end

module Make(T : sig val epsilon : float end) : S
