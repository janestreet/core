(** This module defines interfaces used in {{!Core.Set} [Set]}. See the {!Map} docs for a
    description of the design.

    This module defines module types
    [{Creators,Accessors}{0,1,2,_generic,_with_comparator}]. It uses check functors to
    ensure that each module type is an instance of the corresponding [_generic] one.

    We must treat [Creators] and [Accessors] separately, because we sometimes need to
    choose different instantiations of their [options]. In particular, [Set] itself
    matches [Creators2_with_comparator] but [Accessors2] (without comparator). *)

(*
   CRs and comments about [Set] functions do not belong in this file.  They belong next
   to the appropriate function in set.mli.
*)

open! Import
module Binable = Binable0
module Set = Base.Set
module Tree = Set.Using_comparator.Tree
module Container = Base.Container

[%%template
[@@@mode.default m = (local, global)]

module type Elt_plain = Set.Elt_plain [@mode m]

module type Elt = sig
  type t [@@deriving (compare [@mode m]), sexp]
end

module type Elt_binable = sig
  type t [@@deriving bin_io, (compare [@mode m]), sexp]
end]

module Elt_bin_io = struct
  module type S = sig
    type t [@@deriving bin_io]
    type comparator_witness

    val comparator : (t, comparator_witness) Comparator.t
  end

  type ('t, 'c) t = (module S with type t = 't and type comparator_witness = 'c)
end

module type For_deriving = sig
  include Base.Set.For_deriving
  module M = Base.Set.M

  (** The following [*bin*] functions support bin-io on base-style sets, e.g.:

      {[
        type t = Set.M(String).t [@@deriving bin_io]
      ]} *)

  val bin_shape_m__t : ('a, 'b) Elt_bin_io.t -> Bin_prot.Shape.t

  [%%template:
  [@@@mode.default m = (global, local)]

  val bin_size_m__t : ('a, 'b) Elt_bin_io.t -> (('a, 'b) t Bin_prot.Size.sizer[@mode m])

  val bin_write_m__t
    :  ('a, 'b) Elt_bin_io.t
    -> (('a, 'b) t Bin_prot.Write.writer[@mode m])]

  val bin_read_m__t : ('a, 'b) Elt_bin_io.t -> ('a, 'b) t Bin_prot.Read.reader
  val __bin_read_m__t__ : ('a, 'b) Elt_bin_io.t -> ('a, 'b) t Bin_prot.Read.vtag_reader

  (** The following [quickcheck*] functions support deriving quickcheck on base-style
      sets, e.g.:

      {[
        type t = Set.M(String).t [@@deriving quickcheck]
      ]} *)

  module type Quickcheck_generator_m = sig
    include Comparator.S

    val quickcheck_generator : t Quickcheck.Generator.t
  end

  module type Quickcheck_observer_m = sig
    include Comparator.S

    val quickcheck_observer : t Quickcheck.Observer.t
  end

  module type Quickcheck_shrinker_m = sig
    include Comparator.S

    val quickcheck_shrinker : t Quickcheck.Shrinker.t
  end

  val quickcheck_generator_m__t
    :  (module Quickcheck_generator_m with type t = 'a and type comparator_witness = 'cmp)
    -> ('a, 'cmp) t Quickcheck.Generator.t

  val quickcheck_observer_m__t
    :  (module Quickcheck_observer_m with type t = 'a and type comparator_witness = 'cmp)
    -> ('a, 'cmp) t Quickcheck.Observer.t

  val quickcheck_shrinker_m__t
    :  (module Quickcheck_shrinker_m with type t = 'a and type comparator_witness = 'cmp)
    -> ('a, 'cmp) t Quickcheck.Shrinker.t
end

module type For_deriving_stable = sig
  type ('a, 'b) t

  module type Stable_witness_m = sig
    include Comparator.S

    val stable_witness : t Stable_witness.t
  end

  val stable_witness_m__t
    :  (module Stable_witness_m with type t = 'a and type comparator_witness = 'cmp)
    -> ('a, 'cmp) t Stable_witness.t
end

module Without_comparator = Set.Without_comparator
module With_comparator = Set.With_comparator
module With_first_class_module = Set.With_first_class_module
module Continue_or_stop = Container.Continue_or_stop
module Merge_to_sequence_element = Sequence.Merge_with_duplicates_element

module type Accessors_generic = sig
  include Set.Accessors_generic

  val to_map
    : ( 'a
        , 'cmp
        , ('a, 'cmp) t -> f:local_ ('a elt -> 'b) -> ('a elt, 'b, 'cmp cmp) Base.Map.t )
        access_options

  val quickcheck_observer
    :  'a elt Quickcheck.Observer.t
    -> ('a, 'cmp) t Quickcheck.Observer.t
end

module type Transformers_generic = sig
  include Set.Transformers_generic

  val quickcheck_shrinker
    : ( 'a
        , 'cmp
        , 'a elt Quickcheck.Shrinker.t -> ('a, 'cmp) t Quickcheck.Shrinker.t )
        access_options
end

module type Creators_generic = sig
  include Set.Creators_generic

  val of_hash_set : ('a, 'cmp, 'a elt Hash_set.t -> ('a, 'cmp) t) create_options
  val of_hashtbl_keys : ('a, 'cmp, ('a elt, _) Hashtbl.t -> ('a, 'cmp) t) create_options

  (** Never requires a comparator because it can get one from the input [Map.t]. *)
  val of_map_keys : ('a elt, _, 'cmp cmp) Base.Map.t -> ('a, 'cmp) t

  val quickcheck_generator
    : ( 'a
        , 'cmp
        , 'a elt Quickcheck.Generator.t -> ('a, 'cmp) t Quickcheck.Generator.t )
        create_options
end

module type Creators_and_accessors_generic = sig
  type ('elt, 'cmp) t
  type ('elt, 'cmp) tree
  type 'elt elt
  type 'cmp cmp
  type ('elt, 'cmp, 'fn) access_options

  include
    Accessors_generic
    with type ('a, 'b) t := ('a, 'b) t
    with type ('a, 'b) tree := ('a, 'b) tree
    with type 'a elt := 'a elt
    with type 'cmp cmp := 'cmp cmp
    with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) access_options

  include
    Transformers_generic
    with type ('a, 'b) t := ('a, 'b) t
    with type ('a, 'b) tree := ('a, 'b) tree
    with type 'a elt := 'a elt
    with type 'cmp cmp := 'cmp cmp
    with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) access_options

  include
    Creators_generic
    with type ('a, 'b) t := ('a, 'b) t
    with type ('a, 'b) tree := ('a, 'b) tree
    with type 'a elt := 'a elt
    with type 'cmp cmp := 'cmp cmp
end

module type S_plain_tree = sig
  module Elt : Comparator.S

  type t = (Elt.t, Elt.comparator_witness) Tree.t
  [@@deriving compare ~localize, equal ~localize, sexp_of]

  include
    Creators_generic
    with type ('a, 'b) set := ('a, 'b) Tree.t
    with type ('a, 'b) t := t
    with type ('a, 'b) tree := t
    with type 'a elt := Elt.t
    with type 'c cmp := Elt.comparator_witness
    with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) Without_comparator.t
end

[%%template
[@@@modality.default p = (portable, nonportable)]

module type S_plain = sig
  module Elt : sig
    type t [@@deriving sexp_of]

    include Comparator.S [@modality p] with type t := t
  end

  type t = (Elt.t, Elt.comparator_witness) Base.Set.t
  [@@deriving compare ~localize, equal ~localize, sexp_of]

  module Diff : sig
    type t = Elt.t Diffable.Set_diff.t [@@deriving sexp_of]

    include
      Diffable.Diff.S_plain
      with type t := t
       and type derived_on = (Elt.t, Elt.comparator_witness) Base.Set.t
  end

  include Diffable.S_plain with type t := t and module Diff := Diff

  include
    Creators_generic
    with type ('a, 'b) set := ('a, 'b) Set.t
    with type ('a, 'b) t := t
    with type ('a, 'b) tree := (Elt.t, Elt.comparator_witness) Tree.t
    with type 'a elt := Elt.t
    with type 'c cmp := Elt.comparator_witness
    with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) Without_comparator.t

  val quickcheck_observer : Elt.t Quickcheck.Observer.t -> t Quickcheck.Observer.t
  val quickcheck_shrinker : Elt.t Quickcheck.Shrinker.t -> t Quickcheck.Shrinker.t
end

module type S = sig
  module Elt : sig
    type t [@@deriving sexp]

    include Comparator.S [@modality p] with type t := t
  end

  module Diff : sig
    type t = Elt.t Diffable.Set_diff.t [@@deriving sexp]

    include
      Diffable.Diff.S_plain
      with type t := t
       and type derived_on = (Elt.t, Elt.comparator_witness) Base.Set.t
  end

  include S_plain [@modality p] with module Elt := Elt and module Diff := Diff
  include Sexpable.S with type t := t
end

module type S_binable = sig
  module Elt : sig
    type t [@@deriving sexp, bin_io]

    include Comparator.S [@modality p] with type t := t
  end

  module Diff : sig
    type t = Elt.t Diffable.Set_diff.t [@@deriving bin_io, sexp]

    include
      Diffable.Diff.S_plain
      with type t := t
       and type derived_on = (Elt.t, Elt.comparator_witness) Base.Set.t
  end

  include S [@modality p] with module Elt := Elt and module Diff := Diff
  include Binable.S [@mode local] with type t := t
end]
