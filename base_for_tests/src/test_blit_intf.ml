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
    val length : local_ t -> int
    val get : t -> int -> elt
    val set : t -> int -> elt -> unit
  end

  module type Sequence1_phantom2 = sig
    type ('elt, 'p1, 'p2) t

    (** [Make1*] guarantees to only call [create_like ~len t] with [len > 0] if
        [length t > 0]. *)
    val length : local_ (_, _, _) t -> int

    (** [create_bool], [get], and [set] are just used for unit tests. [elt] is needed for
        [Option_array]. *)

    type 'a elt

    val create_bool : len:int -> (bool, _, _) t
    val get : ('a, _, _) t -> int -> 'a elt
    val set : ('a, _, _) t -> int -> 'a elt -> unit
  end

  module type Sequence1 = sig
    type 'a t

    include Sequence1_phantom2 with type ('a, _, _) t := 'a t
  end
end

module type Test_blit = sig @@ portable
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

  module Test1_phantom2
      (Elt : Elt1)
      (Sequence : Sequence1_phantom2 with type 'a elt := 'a Elt.t)
      (Tested : S1_phantom2_distinct
                with type ('elt, 'p1, 'p2) src := ('elt, 'p1, 'p2) Sequence.t
                 and type ('elt, 'p1, 'p2) dst := ('elt, 'p1, 'p2) Sequence.t) : sig end

  module Test1_phantom2_distinct
      (Elt : Elt1)
      (Src : Sequence1_phantom2 with type 'a elt := 'a Elt.t)
      (Dst : Sequence1_phantom2 with type 'a elt := 'a Elt.t)
      (Tested : S1_phantom2_distinct
                with type ('elt, 'p1, 'p2) src := ('elt, 'p1, 'p2) Src.t
                 and type ('elt, 'p1, 'p2) dst := ('elt, 'p1, 'p2) Dst.t) : sig end

  (** [Make_and_test] uses the [Blit.Make] functor and the [Test] functor. *)
  module%template.portable Make_and_test
      (Elt : Elt)
      (Sequence : sig
         include Sequence with type elt := Elt.t

         val unsafe_blit : (t, t) blit
       end) : S with type t := Sequence.t

  module%template.portable Make_distinct_and_test
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

  module Make1_phantom2_and_test
      (Elt : Elt1)
      (Sequence : sig
         type (_, _, _) t

         val get : ('elt, _, _) t -> int -> 'elt Elt.t
         val set : ('elt, _, _) t -> int -> 'elt Elt.t -> unit
         val length : local_ (_, _, _) t -> int
         val create_bool : len:int -> (bool, _, _) t
         val create_like : len:int -> local_ ('elt, _, _) t -> ('elt, _, _) t
         val unsafe_blit : (('elt, _, _) t, ('elt, _, _) t) Blit.blit
       end) :
    S1_phantom2_distinct
    with type ('elt, 'p1, 'p2) src := ('elt, 'p1, 'p2) Sequence.t
    with type ('elt, 'p1, 'p2) dst := ('elt, 'p1, 'p2) Sequence.t

  module Make1_phantom2_distinct_and_test
      (Elt : Elt1)
      (Src : Sequence1_phantom2 with type 'a elt := 'a Elt.t)
      (Dst : sig
         include Sequence1_phantom2 with type 'a elt := 'a Elt.t

         val create_like : len:int -> local_ ('elt, _, _) Src.t -> ('elt, _, _) t
         val unsafe_blit : (('elt, _, _) Src.t, ('elt, _, _) t) blit
       end) :
    S1_phantom2_distinct
    with type ('elt, 'p1, 'p2) src := ('elt, 'p1, 'p2) Src.t
    with type ('elt, 'p1, 'p2) dst := ('elt, 'p1, 'p2) Dst.t
end
