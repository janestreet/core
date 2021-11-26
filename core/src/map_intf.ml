(** This module defines interfaces used in {{!Map}[Map]}. See those docs for a description
    of the design.

    This module defines module types
    [{Creators,Accessors}{1,2,3,_generic,_with_comparator}]. It uses check functors to
    ensure that each module type is an instance of the corresponding [_generic] one.

    We must treat [Creators] and [Accessors] separately, because we sometimes need to
    choose different instantiations of their [options]. In particular, [Map] itself
    matches [Creators3_with_comparator] but [Accessors3] (without comparator).
*)


open! Import
open T
module Binable = Binable0
module Map = Base.Map
module Or_duplicate = Map.Or_duplicate
module With_comparator = Map.With_comparator
module With_first_class_module = Map.With_first_class_module
module Without_comparator = Map.Without_comparator
module Tree = Map.Using_comparator.Tree

module type Key_plain = sig
  type t [@@deriving compare, sexp_of]
end

module type Key = sig
  type t [@@deriving compare, sexp]
end

module type Key_binable = sig
  type t [@@deriving bin_io, compare, sexp]
end

module type Key_hashable = sig
  type t [@@deriving compare, hash, sexp]
end

module type Key_binable_hashable = sig
  type t [@@deriving bin_io, compare, hash, sexp]
end

module Key_bin_io = struct
  module type S = sig
    type t [@@deriving bin_io]
    type comparator_witness

    val comparator : (t, comparator_witness) Comparator.t
  end

  type ('t, 'c) t = (module S with type t = 't and type comparator_witness = 'c)
end

module type Accessors_generic = sig
  include Map.Accessors_generic

  val key_set : ('k, 'cmp, ('k, _, 'cmp) t -> ('k key, 'cmp cmp) Base.Set.t) options

  val validate
    :  name:('k key -> string)
    -> 'v Validate.check
    -> ('k, 'v, _) t Validate.check

  val validatei
    :  name:('k key -> string)
    -> ('k key * 'v) Validate.check
    -> ('k, 'v, _) t Validate.check

  val quickcheck_observer
    :  'k key Quickcheck.Observer.t
    -> 'v Quickcheck.Observer.t
    -> ('k, 'v, 'cmp) t Quickcheck.Observer.t

  val quickcheck_shrinker
    : ( 'k
      , 'cmp
      , 'k key Quickcheck.Shrinker.t
      -> 'v Quickcheck.Shrinker.t
      -> ('k, 'v, 'cmp) t Quickcheck.Shrinker.t )
        options
end

module type Accessors1 = sig
  include Map.Accessors1

  val key_set : _ t -> (key, comparator_witness) Base.Set.t
  val validate : name:(key -> string) -> 'a Validate.check -> 'a t Validate.check
  val validatei : name:(key -> string) -> (key * 'a) Validate.check -> 'a t Validate.check

  val quickcheck_observer
    :  key Quickcheck.Observer.t
    -> 'v Quickcheck.Observer.t
    -> 'v t Quickcheck.Observer.t

  val quickcheck_shrinker
    :  key Quickcheck.Shrinker.t
    -> 'v Quickcheck.Shrinker.t
    -> 'v t Quickcheck.Shrinker.t
end

module type Accessors2 = sig
  include Map.Accessors2

  val key_set : ('k, 'v) t -> ('k, comparator_witness) Base.Set.t
  val validate : name:('a -> string) -> 'b Validate.check -> ('a, 'b) t Validate.check

  val validatei
    :  name:('a -> string)
    -> ('a * 'b) Validate.check
    -> ('a, 'b) t Validate.check

  val quickcheck_observer
    :  'k Quickcheck.Observer.t
    -> 'v Quickcheck.Observer.t
    -> ('k, 'v) t Quickcheck.Observer.t

  val quickcheck_shrinker
    :  'k Quickcheck.Shrinker.t
    -> 'v Quickcheck.Shrinker.t
    -> ('k, 'v) t Quickcheck.Shrinker.t
end

module type Accessors3 = sig
  include Map.Accessors3

  val key_set : ('k, _, 'cmp) t -> ('k, 'cmp) Base.Set.t
  val validate : name:('a -> string) -> 'b Validate.check -> ('a, 'b, _) t Validate.check

  val validatei
    :  name:('a -> string)
    -> ('a * 'b) Validate.check
    -> ('a, 'b, _) t Validate.check

  val quickcheck_observer
    :  'k Quickcheck.Observer.t
    -> 'v Quickcheck.Observer.t
    -> ('k, 'v, _) t Quickcheck.Observer.t

  val quickcheck_shrinker
    :  'k Quickcheck.Shrinker.t
    -> 'v Quickcheck.Shrinker.t
    -> ('k, 'v, _) t Quickcheck.Shrinker.t
end

module type Accessors3_with_comparator = sig
  include Map.Accessors3_with_comparator

  val key_set
    :  comparator:('k, 'cmp) Comparator.t
    -> ('k, _, 'cmp) t
    -> ('k, 'cmp) Base.Set.t

  val validate : name:('a -> string) -> 'b Validate.check -> ('a, 'b, _) t Validate.check

  val validatei
    :  name:('k -> string)
    -> ('k * 'v) Validate.check
    -> ('k, 'v, _) t Validate.check

  val quickcheck_observer
    :  'k Quickcheck.Observer.t
    -> 'v Quickcheck.Observer.t
    -> ('k, 'v, 'cmp) t Quickcheck.Observer.t

  val quickcheck_shrinker
    :  comparator:('k, 'cmp) Comparator.t
    -> 'k Quickcheck.Shrinker.t
    -> 'v Quickcheck.Shrinker.t
    -> ('k, 'v, 'cmp) t Quickcheck.Shrinker.t
end

(** Consistency checks (same as in [Container]). *)
module Check_accessors
    (T : T3)
    (Tree : T3)
    (Key : T1)
    (Cmp : T1)
    (Options : T3)
    (_ : Accessors_generic
     with type ('a, 'b, 'c) options := ('a, 'b, 'c) Options.t
     with type ('a, 'b, 'c) t := ('a, 'b, 'c) T.t
     with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree.t
     with type 'a key := 'a Key.t
     with type 'a cmp := 'a Cmp.t) =
struct end

module Check_accessors1 (M : Accessors1) =
  Check_accessors
    (struct
      type ('a, 'b, 'c) t = 'b M.t
    end)
    (struct
      type ('a, 'b, 'c) t = 'b M.tree
    end)
    (struct
      type 'a t = M.key
    end)
    (struct
      type 'a t = M.comparator_witness
    end)
    (Without_comparator)
    (M)

module Check_accessors2 (M : Accessors2) =
  Check_accessors
    (struct
      type ('a, 'b, 'c) t = ('a, 'b) M.t
    end)
    (struct
      type ('a, 'b, 'c) t = ('a, 'b) M.tree
    end)
    (struct
      type 'a t = 'a
    end)
    (struct
      type 'a t = M.comparator_witness
    end)
    (Without_comparator)
    (M)

module Check_accessors3 (M : Accessors3) =
  Check_accessors
    (struct
      type ('a, 'b, 'c) t = ('a, 'b, 'c) M.t
    end)
    (struct
      type ('a, 'b, 'c) t = ('a, 'b, 'c) M.tree
    end)
    (struct
      type 'a t = 'a
    end)
    (struct
      type 'a t = 'a
    end)
    (Without_comparator)
    (M)

module Check_accessors3_with_comparator (M : Accessors3_with_comparator) =
  Check_accessors
    (struct
      type ('a, 'b, 'c) t = ('a, 'b, 'c) M.t
    end)
    (struct
      type ('a, 'b, 'c) t = ('a, 'b, 'c) M.tree
    end)
    (struct
      type 'a t = 'a
    end)
    (struct
      type 'a t = 'a
    end)
    (With_comparator)
    (M)

module type Creators_generic = sig
  include Map.Creators_generic

  val of_hashtbl_exn : ('k, 'cmp, ('k key, 'v) Hashtbl.t -> ('k, 'v, 'cmp) t) options

  (** Never requires a comparator because it can get one from the input [Set.t]. *)
  val of_key_set : ('k key, 'cmp cmp) Base.Set.t -> f:('k key -> 'v) -> ('k, 'v, 'cmp) t

  val quickcheck_generator
    : ( 'k
      , 'cmp
      , 'k key Quickcheck.Generator.t
      -> 'v Quickcheck.Generator.t
      -> ('k, 'v, 'cmp) t Quickcheck.Generator.t )
        options
end

module type Creators1 = sig
  include Map.Creators1

  val of_hashtbl_exn : (key, 'a) Hashtbl.t -> 'a t
  val of_key_set : (key, comparator_witness) Base.Set.t -> f:(key -> 'v) -> 'v t

  val quickcheck_generator
    :  key Quickcheck.Generator.t
    -> 'a Quickcheck.Generator.t
    -> 'a t Quickcheck.Generator.t
end

module type Creators2 = sig
  include Map.Creators2

  val of_hashtbl_exn : ('a, 'b) Hashtbl.t -> ('a, 'b) t
  val of_key_set : ('a, comparator_witness) Base.Set.t -> f:('a -> 'b) -> ('a, 'b) t

  val quickcheck_generator
    :  'a Quickcheck.Generator.t
    -> 'b Quickcheck.Generator.t
    -> ('a, 'b) t Quickcheck.Generator.t
end

module type Creators3_with_comparator = sig
  include Map.Creators3_with_comparator

  val of_hashtbl_exn
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b) Hashtbl.t
    -> ('a, 'b, 'cmp) t

  val of_key_set : ('a, 'cmp) Base.Set.t -> f:('a -> 'b) -> ('a, 'b, 'cmp) t

  val quickcheck_generator
    :  comparator:('a, 'cmp) Comparator.t
    -> 'a Quickcheck.Generator.t
    -> 'b Quickcheck.Generator.t
    -> ('a, 'b, 'cmp) t Quickcheck.Generator.t
end

module Check_creators
    (T : T3)
    (Tree : T3)
    (Key : T1)
    (Cmp : T1)
    (Options : T3)
    (_ : Creators_generic
     with type ('a, 'b, 'c) options := ('a, 'b, 'c) Options.t
     with type ('a, 'b, 'c) t := ('a, 'b, 'c) T.t
     with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree.t
     with type 'a key := 'a Key.t
     with type 'a cmp := 'a Cmp.t) =
struct end

module Check_creators1 (M : Creators1) =
  Check_creators
    (struct
      type ('a, 'b, 'c) t = 'b M.t
    end)
    (struct
      type ('a, 'b, 'c) t = 'b M.tree
    end)
    (struct
      type 'a t = M.key
    end)
    (struct
      type 'a t = M.comparator_witness
    end)
    (Without_comparator)
    (M)

module Check_creators2 (M : Creators2) =
  Check_creators
    (struct
      type ('a, 'b, 'c) t = ('a, 'b) M.t
    end)
    (struct
      type ('a, 'b, 'c) t = ('a, 'b) M.tree
    end)
    (struct
      type 'a t = 'a
    end)
    (struct
      type 'a t = M.comparator_witness
    end)
    (Without_comparator)
    (M)

module Check_creators3_with_comparator (M : Creators3_with_comparator) =
  Check_creators
    (struct
      type ('a, 'b, 'c) t = ('a, 'b, 'c) M.t
    end)
    (struct
      type ('a, 'b, 'c) t = ('a, 'b, 'c) M.tree
    end)
    (struct
      type 'a t = 'a
    end)
    (struct
      type 'a t = 'a
    end)
    (With_comparator)
    (M)

module type Creators_and_accessors_generic = sig
  include Creators_generic

  include
    Accessors_generic
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
    with type 'a key := 'a key
    with type 'a cmp := 'a cmp
    with type ('a, 'b, 'c) options := ('a, 'b, 'c) options
end

module type Creators_and_accessors1 = sig
  include Creators1

  include
    Accessors1
    with type 'a t := 'a t
    with type 'a tree := 'a tree
    with type key := key
    with type comparator_witness := comparator_witness
end

module type Creators_and_accessors2 = sig
  include Creators2

  include
    Accessors2
    with type ('a, 'b) t := ('a, 'b) t
    with type ('a, 'b) tree := ('a, 'b) tree
    with type comparator_witness := comparator_witness
end

module type Creators_and_accessors3_with_comparator = sig
  include Creators3_with_comparator

  include
    Accessors3_with_comparator
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
end

module Make_S_plain_tree (Key : Comparator.S) = struct
  module type S = sig
    type 'a t = (Key.t, 'a, Key.comparator_witness) Tree.t [@@deriving sexp_of]

    include
      Creators_and_accessors1
      with type 'a t := 'a t
      with type 'a tree := 'a t
      with type key := Key.t
      with type comparator_witness := Key.comparator_witness

    module Provide_of_sexp
        (K : sig
           type t [@@deriving of_sexp]
         end
         with type t := Key.t) : sig
      type _ t [@@deriving of_sexp]
    end
    with type 'a t := 'a t
  end
end

module type S_plain = sig
  module Key : sig
    type t [@@deriving sexp_of]

    include Comparator.S with type t := t
  end

  type +'a t = (Key.t, 'a, Key.comparator_witness) Map.t [@@deriving compare, sexp_of]

  include
    Creators_and_accessors1
    with type 'a t := 'a t
    with type 'a tree := (Key.t, 'a, Key.comparator_witness) Tree.t
    with type key := Key.t
    with type comparator_witness := Key.comparator_witness

  module Provide_of_sexp
      (Key : sig
         type t [@@deriving of_sexp]
       end
       with type t := Key.t) : sig
    type _ t [@@deriving of_sexp]
  end
  with type 'a t := 'a t

  module Provide_bin_io
      (Key : sig
         type t [@@deriving bin_io]
       end
       with type t := Key.t) : Binable.S1 with type 'a t := 'a t

  module Provide_hash (Key : Hasher.S with type t := Key.t) : sig
    type 'a t [@@deriving hash]
  end
  with type 'a t := 'a t
end

module type S = sig
  module Key : sig
    type t [@@deriving sexp]

    include Comparator.S with type t := t
  end

  include S_plain with module Key := Key
  include Sexpable.S1 with type 'a t := 'a t
end

module type S_binable = sig
  module Key : sig
    type t [@@deriving bin_io, sexp]

    include Comparator.S with type t := t
  end

  include S with module Key := Key
  include Binable.S1 with type 'a t := 'a t
end

module type For_deriving = sig
  include Base.Map.For_deriving
  module M = Base.Map.M

  (** The following [*bin*] functions support bin-io on base-style maps,
      e.g.:

      {[ type t = int Map.M(String).t [@@deriving bin_io] ]} *)

  val bin_shape_m__t : ('a, 'c) Key_bin_io.t -> Bin_prot.Shape.t -> Bin_prot.Shape.t

  val bin_size_m__t
    :  ('a, 'c) Key_bin_io.t
    -> 'b Bin_prot.Size.sizer
    -> ('a, 'b, 'c) t Bin_prot.Size.sizer

  val bin_write_m__t
    :  ('a, 'c) Key_bin_io.t
    -> 'b Bin_prot.Write.writer
    -> ('a, 'b, 'c) t Bin_prot.Write.writer

  val bin_read_m__t
    :  ('a, 'c) Key_bin_io.t
    -> 'b Bin_prot.Read.reader
    -> ('a, 'b, 'c) t Bin_prot.Read.reader

  val __bin_read_m__t__
    :  ('a, 'c) Key_bin_io.t
    -> 'b Bin_prot.Read.reader
    -> (int -> ('a, 'b, 'c) t) Bin_prot.Read.reader

  (** The following [quickcheck*] functions support deriving quickcheck on base-style maps,
      e.g.:

      {[ type t = int Map.M(String).t [@@deriving quickcheck] ]} *)

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
    :  (module Quickcheck_generator_m with type t = 'k and type comparator_witness = 'cmp)
    -> 'v Quickcheck.Generator.t
    -> ('k, 'v, 'cmp) t Quickcheck.Generator.t

  val quickcheck_observer_m__t
    :  (module Quickcheck_observer_m with type t = 'k and type comparator_witness = 'cmp)
    -> 'v Quickcheck.Observer.t
    -> ('k, 'v, 'cmp) t Quickcheck.Observer.t

  val quickcheck_shrinker_m__t
    :  (module Quickcheck_shrinker_m with type t = 'k and type comparator_witness = 'cmp)
    -> 'v Quickcheck.Shrinker.t
    -> ('k, 'v, 'cmp) t Quickcheck.Shrinker.t
end
