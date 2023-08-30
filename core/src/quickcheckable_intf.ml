open! Import

module type Conv = sig
  type quickcheckable
  type t

  val of_quickcheckable : quickcheckable -> t
  val to_quickcheckable : t -> quickcheckable
end

module type Conv1 = sig
  type 'a quickcheckable
  type 'a t

  val of_quickcheckable : 'a quickcheckable -> 'a t
  val to_quickcheckable : 'a t -> 'a quickcheckable
end

module type Conv_filtered = sig
  type quickcheckable
  type t

  val of_quickcheckable : quickcheckable -> t option
  val to_quickcheckable : t -> quickcheckable
end

module type Conv_filtered1 = sig
  type 'a quickcheckable
  type 'a t

  val of_quickcheckable : 'a quickcheckable -> 'a t option
  val to_quickcheckable : 'a t -> 'a quickcheckable
end

(** Provides functors for making a module quickcheckable with {!Quickcheck}. *)
module type Quickcheckable = sig
  module type Conv = Conv
  module type Conv1 = Conv1
  module type Conv_filtered = Conv_filtered
  module type Conv_filtered1 = Conv_filtered1
  module type S = Quickcheck.S
  module type S1 = Quickcheck.S1
  module type S2 = Quickcheck.S2
  module type S_int = Quickcheck.S_int

  module Of_quickcheckable
    (Quickcheckable : S)
    (Conv : Conv with type quickcheckable := Quickcheckable.t) : S with type t := Conv.t

  module Of_quickcheckable1
    (Quickcheckable : S1)
    (Conv : Conv1 with type 'a quickcheckable := 'a Quickcheckable.t) :
    S1 with type 'a t := 'a Conv.t

  module Of_quickcheckable_filtered
    (Quickcheckable : S)
    (Conv : Conv_filtered with type quickcheckable := Quickcheckable.t) :
    S with type t := Conv.t

  module Of_quickcheckable_filtered1
    (Quickcheckable : S1)
    (Conv : Conv_filtered1 with type 'a quickcheckable := 'a Quickcheckable.t) :
    S1 with type 'a t := 'a Conv.t
end
