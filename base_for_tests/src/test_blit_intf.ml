(** Produce unit tests for blittable values. *)

open! Base
open! Blit

module Definitions = struct
  module type Elt1 = sig
    type 'a t

    val equal : bool t -> bool t -> bool

    (** [of_bool] is used to generate two distinct values of type [t], used in unit tests.
        It is required that [of_bool false <> of_bool true]. *)
    val of_bool : bool -> bool t
  end

  module type Elt = sig
    type t

    include Elt1 with type _ t := t
  end

  module type Sequence = sig
    type t
    type elt

    val create : len:int -> t
    val length : t -> int
    val get : t -> int -> elt
    val set : t -> int -> elt -> unit
  end

  module type Sequence1_phantom = sig
    type ('elt, 'phantom) t

    (** [Make1*] guarantees to only call [create_like ~len t] with [len > 0] if
        [length t > 0]. *)
    val length : (_, _) t -> int

    (** [create_bool], [get], and [set] are just used for unit tests. [elt] is needed for
        [Option_array]. *)

    type 'a elt

    val create_bool : len:int -> (bool, _) t
    val get : ('a, _) t -> int -> 'a elt
    val set : ('a, _) t -> int -> 'a elt -> unit
  end

  module type Sequence1 = sig
    type 'a t

    include Sequence1_phantom with type ('a, _) t := 'a t
  end
end

module type Test_blit = sig
  include module type of struct
    include Definitions
  end

  module Test
      (Elt : Elt)
      (Sequence : Sequence with type elt := Elt.t)
      (Tested : S with type t := Sequence.t) : sig end

  module Test_distinct
      (Elt : Elt)
      (Src : Sequence with type elt := Elt.t)
      (Dst : Sequence with type elt := Elt.t)
      (Tested : S_distinct with type src := Src.t with type dst := Dst.t) : sig end

  module Test1
      (Sequence : Sequence1 with type 'a elt := 'a)
      (Tested : S1 with type 'a t := 'a Sequence.t) : sig end

  module Test1_generic
      (Elt : Elt1)
      (Sequence : Sequence1 with type 'a elt := 'a Elt.t)
      (Tested : S1 with type 'a t := 'a Sequence.t) : sig end

  module Test1_phantom_distinct
      (Elt : Elt1)
      (Src : Sequence1_phantom with type 'a elt := 'a Elt.t)
      (Dst : Sequence1_phantom with type 'a elt := 'a Elt.t)
      (Tested : S1_phantom_distinct
                with type ('elt, 'phantom) src := ('elt, 'phantom) Src.t
                 and type ('elt, 'phantom) dst := ('elt, 'phantom) Dst.t) : sig end

  (** [Make_and_test] uses the [Blit.Make] functor and the [Test] functor. *)
  module%template.portable Make_and_test
      (Elt : Elt)
      (Sequence : sig
         include Sequence with type elt := Elt.t

         val unsafe_blit : (t, t) blit
       end) : S with type t := Sequence.t

  module Make_distinct_and_test
      (Elt : Elt)
      (Src : Sequence with type elt := Elt.t)
      (Dst : sig
         include Sequence with type elt := Elt.t

         val unsafe_blit : (Src.t, t) blit
       end) : S_distinct with type src := Src.t with type dst := Dst.t

  module Make1_and_test (Sequence : sig
      include Blit.Sequence1
      include Sequence1 with type 'a t := 'a t with type 'a elt := 'a
    end) : S1 with type 'a t := 'a Sequence.t

  module Make1_generic_and_test
      (Elt : Elt1)
      (Sequence : sig
         include Blit.Sequence1
         include Sequence1 with type 'a t := 'a t with type 'a elt := 'a Elt.t
       end) : S1 with type 'a t := 'a Sequence.t

  module Make1_phantom_distinct_and_test
      (Elt : Elt1)
      (Src : Sequence1_phantom with type 'a elt := 'a Elt.t)
      (Dst : sig
         include Sequence1_phantom with type 'a elt := 'a Elt.t

         val create_like : len:int -> ('elt, _) Src.t -> ('elt, _) t
         val unsafe_blit : (('elt, _) Src.t, ('elt, _) t) blit
       end) :
    S1_phantom_distinct
    with type ('elt, 'phantom) src := ('elt, 'phantom) Src.t
    with type ('elt, 'phantom) dst := ('elt, 'phantom) Dst.t
end
