(** Functors and signatures for dealing with modules for tuples. *)

open! Import

module Definitions = struct
  (*_ Defined here to circumvent shadowing of [Comparator] below. *)

  [%%template
  [@@@mode.default m = (local, global)]

  module type%template
    [@mode m] [@modality p = (portable, nonportable)] Comparable_plain_arg =
    Comparable.Using_comparator_arg [@mode m] [@modality p]

  module type Comparable_arg = sig
    type t : value_or_null [@@deriving (compare [@mode m]), sexp]
  end

  module type Comparable_sexpable = sig
    type t [@@deriving sexp]

    include Comparable.S [@mode m] with type t := t
  end

  module type Hashable_plain_arg = sig
    type t : value_or_null [@@deriving (compare [@mode m]), hash, sexp_of]
  end

  module type Hashable_arg = sig
    type t : value_or_null [@@deriving (compare [@mode m]), hash, sexp]
  end

  module type Hashable_sexpable = sig
    type t [@@deriving sexp]

    include Hashable.S [@mode m] with type t := t
  end]
end

module type Tuple = sig @@ portable
  include module type of struct
    include Definitions
  end

  (** Signature for a 2-tuple module *)
  module T2 : sig
    type ('a : value_or_null, 'b : value_or_null) t = 'a * 'b
    [@@deriving sexp, sexp_grammar, typerep]

    include%template
      Comparator.Derived2 [@modality portable] with type ('a, 'b) t := ('a, 'b) t

    val create : ('a : value_or_null) ('b : value_or_null). 'a -> 'b -> ('a, 'b) t

    val curry
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
      (('a, 'b) t -> 'c) -> 'a -> 'b -> 'c

    val uncurry
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
      ('a -> 'b -> 'c) -> ('a, 'b) t -> 'c

    val compare
      : ('a : value_or_null) ('b : value_or_null).
      cmp1:('a -> 'a -> int) -> cmp2:('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int

    val equal
      : ('a : value_or_null) ('b : value_or_null).
      eq1:('a -> 'a -> bool) -> eq2:('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool

    [%%if flambda_backend]

    external get1
      : ('a : value_or_null) ('b : value_or_null).
      (('a, 'b) t[@local_opt]) -> ('a[@local_opt])
      = "%field0_immut"

    external get2
      : ('a : value_or_null) ('b : value_or_null).
      (('a, 'b) t[@local_opt]) -> ('b[@local_opt])
      = "%field1_immut"

    [%%else]

    external get1
      : ('a : value_or_null) ('b : value_or_null).
      (('a, 'b) t[@local_opt]) -> ('a[@local_opt])
      = "%field0"

    external get2
      : ('a : value_or_null) ('b : value_or_null).
      (('a, 'b) t[@local_opt]) -> ('b[@local_opt])
      = "%field1"

    [%%endif]

    val map
      : ('a : value_or_null) ('b : value_or_null).
      ('a, 'a) t -> f:('a -> 'b) -> ('b, 'b) t

    val map_fst
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
      ('a, 'b) t -> f:('a -> 'c) -> ('c, 'b) t

    val map_snd
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
      ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t

    val map_both
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null)
        ('d : value_or_null).
      ('a, 'b) t -> f1:('a -> 'c) -> f2:('b -> 'd) -> ('c, 'd) t

    val map2
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
      ('a, 'a) t -> ('b, 'b) t -> f:('a -> 'b -> 'c) -> ('c, 'c) t

    val sort
      : ('a : value_or_null) ('a : value_or_null).
      ('a, 'a) t -> compare:('a -> 'a -> int) -> ('a, 'a) t

    val swap : ('a : value_or_null) ('b : value_or_null). ('a, 'b) t -> ('b, 'a) t
  end

  (** Signature for a 3-tuple module *)
  module T3 : sig
    type ('a : value_or_null, 'b : value_or_null, 'c : value_or_null) t = 'a * 'b * 'c
    [@@deriving sexp, sexp_grammar, typerep]

    val create
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
      'a -> 'b -> 'c -> ('a, 'b, 'c) t

    val curry
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null)
        ('d : value_or_null).
      (('a, 'b, 'c) t -> 'd) -> 'a -> 'b -> 'c -> 'd

    val uncurry
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null)
        ('d : value_or_null).
      ('a -> 'b -> 'c -> 'd) -> ('a, 'b, 'c) t -> 'd

    val equal
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
      eq1:('a -> 'a -> bool)
      -> eq2:('b -> 'b -> bool)
      -> eq3:('c -> 'c -> bool)
      -> ('a, 'b, 'c) t
      -> ('a, 'b, 'c) t
      -> bool

    val compare
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
      cmp1:('a -> 'a -> int)
      -> cmp2:('b -> 'b -> int)
      -> cmp3:('c -> 'c -> int)
      -> ('a, 'b, 'c) t
      -> ('a, 'b, 'c) t
      -> int

    [%%if flambda_backend]

    external get1
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
      (('a, 'b, 'c) t[@local_opt]) -> ('a[@local_opt])
      = "%field0_immut"

    external get2
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
      (('a, 'b, 'c) t[@local_opt]) -> ('b[@local_opt])
      = "%field1_immut"

    [%%else]

    external get1
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
      (('a, 'b, 'c) t[@local_opt]) -> ('a[@local_opt])
      = "%field0"

    external get2
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
      (('a, 'b, 'c) t[@local_opt]) -> ('b[@local_opt])
      = "%field1"

    [%%endif]

    val get3
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
      ('a, 'b, 'c) t -> 'c

    val map
      : ('a : value_or_null) ('b : value_or_null).
      ('a, 'a, 'a) t -> f:('a -> 'b) -> ('b, 'b, 'b) t

    val map_fst
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null)
        ('d : value_or_null).
      ('a, 'b, 'c) t -> f:('a -> 'd) -> ('d, 'b, 'c) t

    val map_snd
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null)
        ('d : value_or_null).
      ('a, 'b, 'c) t -> f:('b -> 'd) -> ('a, 'd, 'c) t

    val map_trd
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null)
        ('d : value_or_null).
      ('a, 'b, 'c) t -> f:('c -> 'd) -> ('a, 'b, 'd) t

    val map_all
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null)
        ('d : value_or_null) ('e : value_or_null) ('f : value_or_null).
      ('a, 'b, 'c) t -> f1:('a -> 'd) -> f2:('b -> 'e) -> f3:('c -> 'f) -> ('d, 'e, 'f) t

    val map2
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
      ('a, 'a, 'a) t -> ('b, 'b, 'b) t -> f:('a -> 'b -> 'c) -> ('c, 'c, 'c) t
  end

  (** These functors allow users to write:
      {[
        module Foo = struct
          include Tuple.Make (String) (Int)
          include Tuple.Comparator (String) (Int)
          include Tuple.Comparable (String) (Int)
          include Tuple.Hashable (String) (Int)
          include Tuple.Binable (String) (Int)
        end
      ]} *)

  module Make
      (T1 : sig
         type t : value_or_null
       end)
      (T2 : sig
         type t : value_or_null
       end) : sig
    type t = T1.t * T2.t
  end

  module Comparator (S1 : Comparator.S) (S2 : Comparator.S) :
    Comparator.S
    with type t = Make(S1)(S2).t
     and type comparator_witness =
      (S1.comparator_witness, S2.comparator_witness) T2.comparator_witness

  [%%template:
  [@@@mode.default m = (local, global)]

  module%template.portable
    [@modality p] Comparable_plain
      (S1 : Comparable_plain_arg
    [@mode m] [@modality p])
      (S2 : Comparable_plain_arg
    [@mode m] [@modality p]) : sig
    (*_ This type is introduced because older versions of OCaml do not support destructive
        substitutions with `type t1 = 'a t2`. *)

    type comparator_witness =
      (S1.comparator_witness, S2.comparator_witness) T2.comparator_witness

    include
      Comparable.S_plain
      [@mode m]
      with type t := Make(S1)(S2).t
      with type comparator_witness := comparator_witness
  end

  module%template.portable Comparable
      (S1 : Comparable_arg
    [@mode m])
      (S2 : Comparable_arg
    [@mode m]) : Comparable_sexpable [@mode m] with type t := Make(S1)(S2).t

  module%template.portable Hashable_plain
      (S1 : Hashable_plain_arg
    [@mode m])
      (S2 : Hashable_plain_arg
    [@mode m]) : Hashable.S_plain [@mode m] with type t := Make(S1)(S2).t

  (** {v
      The difference between [Hashable] and [Hashable_t] functors is that the former's
      result type doesn't contain type [t] and the latter does. Therefore, [Hashable] can't
      be used to combine two pairs into 4-tuple. but [Hashable_t] can. On the other hand
      result of [Hashable_t] cannot be combined with [Comparable].

      example:
      module Four_ints = Tuple.Hashable_t (Tuple.Hashable_t (Int)(Int))
      (Tuple.Hashable_t (Int)(Int))

      If instead we used [Hashable] compiler would complain that the input to outer functor
      doesn't have type [t].

      On the other hand:
      module Foo = struct
      type t = String.t * Int.t
      include Tuple.Comparable (String) (Int)
      include Tuple.Hashable (String) (Int)
      end

      If we used [Hashable_t] above, the compiler would complain that we have two types [t]
      defined.

      Unfortunately, it is not possible to define just one functor that could be used in
      both cases.
      v} *)
  module%template.portable Hashable
      (S1 : Hashable_arg
    [@mode m])
      (S2 : Hashable_arg
    [@mode m]) : Hashable_sexpable [@mode m] with type t := Make(S1)(S2).t

  module%template.portable Hashable_t
      (S1 : Hashable_arg
    [@mode m])
      (S2 : Hashable_arg
    [@mode m]) : Hashable_sexpable [@mode m] with type t = Make(S1)(S2).t

  module%template.portable Hasher
      (H1 : Hashable_arg
    [@mode m])
      (H2 : Hashable_arg
    [@mode m]) : Hashable_sexpable [@mode m] with type t := Make(H1)(H2).t]

  module%template.portable Sexpable
      (S1 : sig
         type t : value_or_null

         include Sexpable.S with type t := t
       end)
      (S2 : sig
         type t : value_or_null

         include Sexpable.S with type t := t
       end) : Sexpable.S with type t := Make(S1)(S2).t

  module%template.portable Binable
      (B1 : sig
         type t : value_or_null

         include Binable.S with type t := t
       end)
      (B2 : sig
         type t : value_or_null

         include Binable.S with type t := t
       end) : Binable.S with type t := Make(B1)(B2).t
end
