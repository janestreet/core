(** Hashtbl is a reimplementation of the standard
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/MoreLabels.Hashtbl.html}
      [MoreLabels.Hashtbl]}.
    Its worst case time complexity is O(log(N)) for lookups and additions, unlike the
    standard [MoreLabels.Hashtbl], which is O(N).

    A hash table is implemented as an array of AVL trees (see [Avltree]). If
    [growth_allowed] (default true) is false then [size] is the final size of the array;
    the table can always hold more elements than [size], but they will all go into tree
    nodes. If it is true (default) then the array will double in size when the number of
    elements in the table reaches twice the size of the array. When this happens, all
    existing elements will be reinserted, which can take a long time. If you care about
    latency, set [size] and [growth_allowed=false] if possible.

    In most cases, functions passed as arguments to hash table accessors must not mutate
    the hash table while it is being accessed, as this will result in an exception. For
    example, [iter] and [change] take a function [f] which must not modify [t]. In a few
    cases, mutation is allowed, such as in [Hashtbl.find_and_call], where all access to
    [t] is finished before the [~if_found] and [~if_not_found] arguments are invoked.

    We have three kinds of hash table modules:
    - [Hashtbl]
    - [Hashtbl.Poly]
    - [Key.Table] (a class of similar modules)

    There are three kinds of hash-table functions:
    - creation from nothing ([create], [of_alist])
    - sexp converters ([t_of_sexp], [sexp_of_t], and [bin_io] too)
    - accessors and mappers ([fold], [mem], [find], [map], [filter_map], ...)

    Here is a table showing what classes of functions are available in each kind of
    hash-table module:
    {v
                   creation     sexp-conv   accessors
    Hashtbl                                   X
    Hashtbl.Poly      X           X
    Key.Table         X           X           X'
    v}

    The entry marked with [X'] is there for historical reasons, and may be eliminated at
    some point. The upshot is that one should use [Hashtbl] for accessors, [Hashtbl.Poly]
    for hash-table creation and sexp conversion using polymorphic compare/hash, and
    [Key.Table] for hash-table creation and sexp conversion using [Key.compare] and
    [Key.hash].

    {2:usage Usage}

    For many students of OCaml, using hashtables is complicated by the functors. Here are
    a few tips:

    {ol
     {- For a list of hashtable functions see {!Core.Hashtbl_intf.S}. }
     {- To create a hashtable with string keys use [String.Table]:
       {[
         let table = String.Table.create () ~size:4 in
         List.iter
           ~f:(fun (key, data) -> Hashtbl.set table ~key ~data)
           [ "A", 1; "B", 2; "C", 3 ];
         Hashtbl.find table "C"
       ]}

       Here 4 need only be a guess at the hashtable's future size. There are other similar
       pre-made hashtables, e.g., [Int63.Table] or [Host_and_port.Table].
    }
     {- To create a hashtable with a custom key type use Hashable:
       {[
         module Key = struct
           module T = struct
             type t = String.t * Int63.t [@@deriving compare, hash, sexp]
           end
           include T
           include Hashable.Make (T)
         end
         let table = Key.Table.create () ~size:4 in
         List.iter ~f:(fun (key, data) -> Hashtbl.set table ~key ~data)
           [ (("pi", Int63.zero), 3.14159);
             (("e", Int63.minus_one), 2.71828);
             (("Euler", Int63.one), 0.577215);
           ];
         Hashtbl.find table ("pi", Int63.zero)
       ]}

       Performance {i may} improve if you define [equal] and [hash] explicitly, e.g.:

       {[
         let equal (x, y) (x', y') = String.( = ) x x' && Int63.( = ) y y'
         let hash (x, y) = String.hash x + (Int63.hash y * 65599)
       ]}
    }
    } *)

open! Import
module Binable = Binable0
module Hashtbl = Base.Hashtbl

module type%template Key_plain = Hashtbl.Key.S [@mode m] [@@mode m = (local, global)]
module type Hashtbl_equality = Hashtbl.Hashtbl_equality

module Hashable = Base.Hashable
module Merge_into_action = Base.Hashtbl.Merge_into_action

[%%template
[@@@mode.default m = (local, global)]

module type Key = sig
  type t [@@deriving sexp]

  include Key_plain [@mode m] with type t := t
end

module type Key_binable = sig
  type t [@@deriving bin_io, sexp]

  include Key [@mode m] with type t := t
end

module type Key_stable = sig
  type t [@@deriving bin_io, sexp, stable_witness]

  include Key_binable [@mode m] with type t := t
end]

module type Creators = Hashtbl.Private.Creators_generic

module type Accessors = sig
  include Hashtbl.Accessors

  val validate : name:('a key -> string) -> 'b Validate.check -> ('a, 'b) t Validate.check
end

module type Multi = Hashtbl.Multi

type ('key, 'data, 'z) create_options_with_first_class_module =
  ('key, 'data, 'z) Hashtbl.create_options

type ('key, 'data, 'z) create_options_without_hashable =
  ('key, 'data, 'z) Hashtbl.Private.create_options_without_first_class_module

type ('key, 'data, 'z) create_options_with_hashable =
  ?growth_allowed:bool (** defaults to [true] *)
  -> ?size:int (** initial size -- default 0 *)
  -> hashable:'key Hashable.t
  -> 'z

module type%template M_quickcheck = sig
  type t [@@deriving (compare [@mode m]), hash, quickcheck, sexp_of]
end
[@@mode m = (local, global)]

module type For_deriving = sig
  include Base.Hashtbl.For_deriving

  module type%template M_quickcheck = M_quickcheck [@mode m] [@@mode m = (local, global)]

  val quickcheck_generator_m__t
    :  (module M_quickcheck with type t = 'k)
    -> 'v Base_quickcheck.Generator.t
    -> ('k, 'v) t Quickcheck.Generator.t

  val quickcheck_observer_m__t
    :  (module M_quickcheck with type t = 'k)
    -> 'v Quickcheck.Observer.t
    -> ('k, 'v) t Quickcheck.Observer.t

  val quickcheck_shrinker_m__t
    :  (module M_quickcheck with type t = 'k)
    -> 'v Quickcheck.Shrinker.t
    -> ('k, 'v) t Quickcheck.Shrinker.t

  val bin_shape_m__t
    :  (module Key_binable with type t = 'k)
    -> Bin_prot.Shape.t
    -> Bin_prot.Shape.t

  val bin_size_m__t
    :  (module Key_binable with type t = 'k)
    -> 'v Bin_prot.Size.sizer
    -> ('k, 'v) t Bin_prot.Size.sizer

  val bin_write_m__t
    :  (module Key_binable with type t = 'k)
    -> 'v Bin_prot.Write.writer
    -> ('k, 'v) t Bin_prot.Write.writer

  val bin_read_m__t
    :  (module Key_binable with type t = 'k)
    -> 'v Bin_prot.Read.reader
    -> ('k, 'v) t Bin_prot.Read.reader

  val __bin_read_m__t__
    :  (module Key_binable with type t = 'k)
    -> 'v Bin_prot.Read.reader
    -> ('k, 'v) t Bin_prot.Read.vtag_reader
end

module type S_plain = sig
  type key
  type ('a, 'b) hashtbl
  type 'b t = (key, 'b) hashtbl [@@deriving equal ~localize, sexp_of]
  type ('a, 'b) t_ = 'b t
  type 'a key_ = key

  val hashable : key Hashable.t

  include Invariant.S1 with type 'b t := 'b t

  include
    Creators
    with type ('a, 'b) t := ('a, 'b) t_
    with type 'a key := 'a key_
    with type ('key, 'data, 'z) create_options :=
      ('key, 'data, 'z) create_options_without_hashable
end

module type S = sig
  include S_plain

  include sig
      type _ t [@@deriving of_sexp]
    end
    with type 'a t := 'a t
end

module type S_binable = sig
  include S
  include Binable.S1 with type 'v t := 'v t
end

module type S_stable = sig
  include S_binable

  type nonrec 'a t = 'a t [@@deriving stable_witness]
end

module type%template
  [@modality p = (portable, nonportable)] Hashtbl_over_values = sig
  @@ p
  include Hashtbl.S_without_submodules (** @inline *)

  val validate : name:('a key -> string) -> 'b Validate.check -> ('a, 'b) t Validate.check

  module Using_hashable : sig
    include
      Creators
      with type ('a, 'b) t := ('a, 'b) t
      with type 'a key := 'a key
      with type ('a, 'b, 'z) create_options := ('a, 'b, 'z) create_options_with_hashable
  end

  module Poly : sig
    type nonrec ('a, 'b) t = ('a, 'b) t [@@deriving bin_io]

    include Hashtbl.S_poly with type ('a, 'b) t := ('a, 'b) t

    val validate
      :  name:('a key -> string)
      -> 'b Validate.check
      -> ('a, 'b) t Validate.check
  end

  [%%template:
  [@@@mode.default m = (local, global)]

  module type Key_plain = Key_plain [@mode m]
  module type Key = Key [@mode m]
  module type Key_binable = Key_binable [@mode m]
  module type Key_stable = Key_stable [@mode m]]

  module type S_plain = S_plain with type ('a, 'b) hashtbl = ('a, 'b) t
  module type S = S with type ('a, 'b) hashtbl = ('a, 'b) t
  module type S_binable = S_binable with type ('a, 'b) hashtbl = ('a, 'b) t
  module type S_stable = S_stable with type ('a, 'b) hashtbl = ('a, 'b) t

  module%template
    [@modality p = (p, nonportable)] Make_plain (Key : sig
    @@ p
      include Key_plain
    end) : sig
    @@ p
    include S_plain with type key = Key.t
  end

  module%template
    [@modality p = (p, nonportable)] Make (Key : sig
    @@ p
      include Key
    end) : sig
    @@ p
    include S with type key = Key.t
  end

  module%template
    [@modality p = (p, nonportable)] Make_binable (Key : sig
    @@ p
      include Key_binable
    end) : sig
    @@ p
    include S_binable with type key = Key.t
  end

  module%template
    [@modality p = (p, nonportable)] Make_stable (Key : sig
    @@ p
      include Key_stable
    end) : sig
    @@ p
    include S_stable with type key = Key.t
  end

  module%template
    [@modality p = (p, nonportable)] Make_plain_with_hashable (T : sig
    @@ p
      module Key : Key_plain

      val hashable : Key.t Hashable.t
    end) : sig
    @@ p
    include S_plain with type key = T.Key.t
  end

  module%template
    [@modality p = (p, nonportable)] Make_with_hashable (T : sig
    @@ p
      module Key : Key

      val hashable : Key.t Hashable.t
    end) : sig
    @@ p
    include S with type key = T.Key.t
  end

  module%template
    [@modality p = (p, nonportable)] Make_binable_with_hashable (T : sig
    @@ p
      module Key : Key_binable

      val hashable : Key.t Hashable.t
    end) : sig
    @@ p
    include S_binable with type key = T.Key.t
  end

  module%template
    [@modality p = (p, nonportable)] Make_stable_with_hashable (T : sig
    @@ p
      module Key : Key_stable

      val hashable : Key.t Hashable.t
    end) : sig
    @@ p
    include S_stable with type key = T.Key.t
  end

  module%template
    [@modality p = (p, nonportable)] Provide_of_sexp (Key : sig
    @@ p
      include Base.Hashtbl.M_of_sexp
    end) : sig @@ p
      type _ t [@@deriving of_sexp]
    end
    with type 'a t := (Key.t, 'a) t

  module%template
    [@modality p = (p, nonportable)] Provide_bin_io (Key : sig
    @@ p
      type t [@@deriving bin_io]

      include Key_plain with type t := t
    end) : sig @@ p
      type 'a t [@@deriving bin_io]
    end
    with type 'a t := (Key.t, 'a) t

  module M (K : T.T) : sig
    type nonrec 'v t = (K.t, 'v) t
  end

  module Hashable = Hashable
  module Merge_into_action = Merge_into_action

  val hashable : ('key, _) t -> 'key Hashable.t

  module type For_deriving = For_deriving

  include For_deriving with type ('a, 'b) t := ('a, 'b) t
end

module type%template [@modality p = (portable, nonportable)] Hashtbl = sig @@ p
  module type Non_value = Base.Hashtbl.Non_value

  include Non_value
  include Hashtbl_over_values [@modality p] with type ('a, 'b) t := ('a, 'b) t
  include Hashtbl_equality with type ('a, 'b) t := ('a, 'b) t
end
