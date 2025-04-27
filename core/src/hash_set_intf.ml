(** A mutable set of elements. *)

open! Import
module Binable = Binable0
open Base.Hash_set

module type M_quickcheck = sig
  type t [@@deriving compare, hash, quickcheck, sexp_of]
end

module type For_deriving = sig
  include For_deriving

  module type M_quickcheck = M_quickcheck

  val quickcheck_generator_m__t
    :  (module M_quickcheck with type t = 'key)
    -> 'key t Base_quickcheck.Generator.t

  val quickcheck_observer_m__t
    :  (module M_quickcheck with type t = 'key)
    -> 'key t Base_quickcheck.Observer.t

  val quickcheck_shrinker_m__t
    :  (module M_quickcheck with type t = 'key)
    -> 'key t Base_quickcheck.Shrinker.t
end

module type%template [@modality p = (portable, nonportable)] S_plain = sig @@ p
  type elt
  type 'a hash_set
  type t = elt hash_set [@@deriving equal, sexp_of]

  include
    Creators_generic
    with type 'a t := t
    with type 'a elt := elt
    with type ('a, 'z) create_options :=
      ('a, 'z) create_options_without_first_class_module

  module%template
    [@modality p' = (nonportable, p)] Provide_of_sexp
      (X : sig
           @@ p'
             type t [@@deriving of_sexp]
           end
           with type t := elt) : sig @@ p'
      type t [@@deriving of_sexp]
    end
    with type t := t

  module%template
    [@modality p' = (nonportable, p)] Provide_bin_io
      (X : sig
           @@ p'
             type t [@@deriving bin_io]
           end
           with type t := elt) : sig @@ p'
      type t [@@deriving bin_io]
    end
    with type t := t
end

module type%template [@modality p = (portable, nonportable)] S = sig @@ p
  include S_plain [@modality p]

  include sig
      type t [@@deriving of_sexp]
    end
    with type t := t
end

module type%template [@modality p = (portable, nonportable)] S_binable = sig @@ p
  include S [@modality p]
  include Binable.S with type t := t
end

module type%template [@modality p = (portable, nonportable)] S_stable = sig @@ p
  include S_binable [@modality p]

  val stable_witness : t Stable_witness.t
end

type ('key, 'z) create_options_with_hashable_required =
  ('key, unit, 'z) Hashtbl_intf.create_options_with_hashable

module type Hash_set = sig @@ portable
  type 'a t = 'a Base.Hash_set.t [@@deriving sexp_of]

  (** We use [[@@deriving sexp_of]] but not [[@@deriving sexp]] because we want people to
      be explicit about the hash and comparison functions used when creating hashtables.
      One can use [Hash_set.Poly.t], which does have [[@@deriving sexp]], to use
      polymorphic comparison and hashing. *)

  module Ok_or_absent = Ok_or_absent
  module Ok_or_duplicate = Ok_or_duplicate
  include Creators with type 'a t := 'a t
  include Accessors with type 'a t := 'a t with type 'a elt := 'a

  val hashable : 'key t -> 'key Hashtbl.Hashable.t

  module type Elt_plain = Hashtbl.Key_plain
  module type Elt = Hashtbl.Key
  module type Elt_binable = Hashtbl.Key_binable
  module type Elt_stable = Hashtbl.Key_stable

  module type%template [@modality p = (portable, nonportable)] S_plain =
    S_plain [@modality p] with type 'a hash_set := 'a t

  module type%template [@modality p = (portable, nonportable)] S =
    S [@modality p] with type 'a hash_set := 'a t

  module type%template [@modality p = (portable, nonportable)] S_binable =
    S_binable [@modality p] with type 'a hash_set := 'a t

  module type%template [@modality p = (portable, nonportable)] S_stable =
    S_stable [@modality p] with type 'a hash_set := 'a t

  module Using_hashable : sig
    include
      Creators_generic
      with type 'a t := 'a t
      with type 'a elt = 'a
      with type ('key, 'z) create_options :=
        ('key, 'z) create_options_with_hashable_required
  end

  (** A hash set that uses polymorphic comparison. *)
  module Poly : sig
    type nonrec 'a t = 'a t [@@deriving sexp]

    include
      Creators_generic
      with type 'a t := 'a t
      with type 'a elt = 'a
      with type ('key, 'z) create_options :=
        ('key, 'z) create_options_without_first_class_module

    include Accessors with type 'a t := 'a t with type 'a elt := 'a elt
  end

  module%template.portable [@modality p] Make_plain (Elt : Elt_plain) :
    S_plain [@modality p] with type elt = Elt.t

  module%template.portable [@modality p] Make (Elt : Elt) :
    S [@modality p] with type elt = Elt.t

  module%template.portable [@modality p] Make_binable (Elt : Elt_binable) :
    S_binable [@modality p] with type elt = Elt.t

  module%template.portable [@modality p] Make_stable (Elt : Elt_stable) :
    S_stable [@modality p] with type elt = Elt.t

  module%template.portable
    [@modality p] Make_plain_with_hashable (T : sig
      module Elt : Elt_plain

      val hashable : Elt.t Hashtbl.Hashable.t
    end) : S_plain [@modality p] with type elt = T.Elt.t

  module%template.portable
    [@modality p] Make_with_hashable (T : sig
      module Elt : Elt

      val hashable : Elt.t Hashtbl.Hashable.t
    end) : S [@modality p] with type elt = T.Elt.t

  module%template.portable
    [@modality p] Make_binable_with_hashable (T : sig
      module Elt : Elt_binable

      val hashable : Elt.t Hashtbl.Hashable.t
    end) : S_binable [@modality p] with type elt = T.Elt.t

  module%template.portable
    [@modality p] Make_stable_with_hashable (T : sig
      module Elt : Elt_stable

      val hashable : Elt.t Hashtbl.Hashable.t
    end) : S_stable [@modality p] with type elt = T.Elt.t

  include For_deriving with type 'a t := 'a t
end
