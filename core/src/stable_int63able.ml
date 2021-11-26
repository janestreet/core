open! Import

module type S = sig
  include Stable_module_types.S0

  (** [to_int63] and [of_int63_exn] encode [t] for use in wire protocols; they are
      intended to avoid allocation on 64-bit machines and should be implemented
      efficiently.  [of_int63_exn (to_int63 t) = t] for all [t]; [of_int63_exn] raises for
      inputs not produced by [to_int63]. *)
  val to_int63 : t -> Int63.t

  val of_int63_exn : Int63.t -> t
end
