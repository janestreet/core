open! Import

module Definitions = struct
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
end

(** Provides functors for making a module quickcheckable with {!Quickcheck}. *)
module type Quickcheckable = sig @@ portable
  include module type of struct
    include Quickcheck_intf.Definitions
    include Definitions
  end

  module%template.portable Of_quickcheckable
      (Quickcheckable : S)
      (Conv : Conv with type quickcheckable := Quickcheckable.t) : S with type t := Conv.t

  module%template.portable
    [@modality p] Of_quickcheckable1
      (Quickcheckable : S1
    [@modality p])
      (Conv : Conv1 with type 'a quickcheckable := 'a Quickcheckable.t) :
    S1 [@modality p] with type 'a t := 'a Conv.t

  module%template.portable Of_quickcheckable_filtered
      (Quickcheckable : S)
      (Conv : Conv_filtered with type quickcheckable := Quickcheckable.t) :
    S with type t := Conv.t

  module%template.portable
    [@modality p] Of_quickcheckable_filtered1
      (Quickcheckable : S1
    [@modality p])
      (Conv : Conv_filtered1 with type 'a quickcheckable := 'a Quickcheckable.t) :
    S1 [@modality p] with type 'a t := 'a Conv.t
end
