open! Import
include Tuple_intf.Definitions

module type T = sig
  type t
end

module Make (T1 : T) (T2 : T) = struct
  type t = T1.t * T2.t
end

module T2 = struct
  type ('a, 'b) t = 'a * 'b [@@deriving sexp, sexp_grammar, typerep]

  let create a b = a, b

  let curry f =
    ();
    fun a b -> f (a, b)
  ;;

  let uncurry f =
    ();
    fun (a, b) -> f a b
  ;;

  [%%if flambda_backend]

  external get1
    :  (('a, _) t[@local_opt])
    -> ('a[@local_opt])
    @@ portable
    = "%field0_immut"

  external get2
    :  ((_, 'a) t[@local_opt])
    -> ('a[@local_opt])
    @@ portable
    = "%field1_immut"

  [%%else]

  external get1 : (('a, _) t[@local_opt]) -> ('a[@local_opt]) @@ portable = "%field0"
  external get2 : ((_, 'a) t[@local_opt]) -> ('a[@local_opt]) @@ portable = "%field1"

  [%%endif]

  let map (x, y) ~f = f x, f y
  let map_fst (x, y) ~f = f x, y
  let map_snd (x, y) ~f = x, f y
  let map_both (x, y) ~f1 ~f2 = f1 x, f2 y
  let map2 (x1, y1) (x2, y2) ~f = f x1 x2, f y1 y2

  let compare ~cmp1 ~cmp2 (x, y) (x', y') =
    match cmp1 x x' with
    | 0 -> cmp2 y y'
    | i -> i
  ;;

  let equal ~eq1 ~eq2 (x, y) (x', y') = eq1 x x' && eq2 y y'
  let sort ((a, b) as t) ~compare = if compare a b <= 0 then t else b, a
  let swap (a, b) = b, a

  include%template Comparator.Derived2 [@modality portable] (struct
      type nonrec ('a, 'b) t = ('a, 'b) t [@@deriving sexp_of]

      let compare cmp1 cmp2 = compare ~cmp1 ~cmp2
    end)
end

module T3 = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c [@@deriving sexp, sexp_grammar, typerep]

  let create a b c = a, b, c

  let curry f =
    ();
    fun a b c -> f (a, b, c)
  ;;

  let uncurry f =
    ();
    fun (a, b, c) -> f a b c
  ;;

  let map (x, y, z) ~f = f x, f y, f z
  let map_fst (x, y, z) ~f = f x, y, z
  let map_snd (x, y, z) ~f = x, f y, z
  let map_trd (x, y, z) ~f = x, y, f z
  let map_all (x, y, z) ~f1 ~f2 ~f3 = f1 x, f2 y, f3 z
  let map2 (x1, y1, z1) (x2, y2, z2) ~f = f x1 x2, f y1 y2, f z1 z2

  [%%if flambda_backend]

  external get1
    :  (('a, _, _) t[@local_opt])
    -> ('a[@local_opt])
    @@ portable
    = "%field0_immut"

  external get2
    :  ((_, 'a, _) t[@local_opt])
    -> ('a[@local_opt])
    @@ portable
    = "%field1_immut"

  [%%else]

  external get1 : (('a, _, _) t[@local_opt]) -> ('a[@local_opt]) @@ portable = "%field0"
  external get2 : ((_, 'a, _) t[@local_opt]) -> ('a[@local_opt]) @@ portable = "%field1"

  [%%endif]

  (* There's no %field2....*)
  let get3 (_, _, a) = a

  (* lexicographic comparison  *)
  let compare ~cmp1 ~cmp2 ~cmp3 (x, y, z) (x', y', z') =
    let c1 = cmp1 x x' in
    if c1 <> 0
    then c1
    else (
      let c2 = cmp2 y y' in
      if c2 <> 0 then c2 else cmp3 z z')
  ;;

  let equal ~eq1 ~eq2 ~eq3 (x, y, z) (x', y', z') = eq1 x x' && eq2 y y' && eq3 z z'
end

module%template.portable Sexpable (S1 : Sexpable.S) (S2 : Sexpable.S) = struct
  type t = S1.t * S2.t [@@deriving sexp]
end

module%template.portable Binable (B1 : Binable.S) (B2 : Binable.S) = struct
  type t = B1.t * B2.t [@@deriving bin_io]
end

(* Redefinition to avoid shadowing Core.Comparator *)
module Comparator_ = Comparator

module Comparator (S1 : Comparator.S) (S2 : Comparator.S) = struct
  include Make (S1) (S2)

  type comparator_witness =
    (S1.comparator_witness, S2.comparator_witness) T2.comparator_witness

  let comparator = T2.comparator S1.comparator S2.comparator
end

[%%template
[@@@mode.default m = (local, global)]

module%template.portable
  [@modality p] Comparable_plain
    (S1 : Comparable_plain_arg
  [@mode m] [@modality p])
    (S2 : Comparable_plain_arg
  [@mode m] [@modality p]) =
struct
  module S1 = struct
    include S1

    let compare = [%eta2 Comparator_.compare comparator]
  end

  module S2 = struct
    include S2

    let compare = [%eta2 Comparator_.compare comparator]
  end

  module T = struct
    include Comparator (S1) (S2)

    let sexp_of_t = [%eta1 Comparator_.sexp_of_t comparator]

    let compare (s1, s2) (s1', s2') =
      match (S1.compare [@mode m]) s1 s1' with
      | 0 -> (S2.compare [@mode m]) s2 s2'
      | x -> x
    [@@mode m = (global, m)]
    ;;

    let _ = compare (* in the global case, we only need the comparator *)
  end

  include T
  include Comparable.Make_plain_using_comparator [@mode m] [@modality p] (T)
end

module%template.portable
  [@modality p] Comparable
    (S1 : Comparable_arg
  [@mode m])
    (S2 : Comparable_arg
  [@mode m]) =
struct
  module T = struct
    include Sexpable [@modality p] (S1) (S2)

    let compare (s1, s2) (s1', s2') =
      match (S1.compare [@mode m]) s1 s1' with
      | 0 -> (S2.compare [@mode m]) s2 s2'
      | x -> x
    [@@mode m = (global, m)]
    ;;
  end

  include T
  include Comparable.Make [@mode m] [@modality p] (T)
end

module%template.portable
  [@modality p] Hashable_plain
    (S1 : Hashable_plain_arg
  [@mode m])
    (S2 : Hashable_plain_arg
  [@mode m]) =
struct
  module T = struct
    type t = S1.t * S2.t [@@deriving (compare [@mode m]), hash, sexp_of]
  end

  include T
  include Hashable.Make_plain [@modality p] (T)
end

module%template.portable
  [@modality p] Hasher
    (H1 : Hashable_arg
  [@mode m])
    (H2 : Hashable_arg
  [@mode m]) =
struct
  module T = struct
    type t = H1.t * H2.t [@@deriving (compare [@mode m]), hash, sexp]
  end

  include T
  include Hashable.Make [@modality p] (T)
end

module%template.portable
  [@modality p] Hasher_sexpable_of_hashable_sexpable
    (S : Hashable_arg
  [@mode m]) : Hashable_arg [@mode m] with type t = S.t = struct
  include S

  let hash_fold_t state t = hash_fold_int state (hash t)
end

module%template.portable
  [@modality p] Hashable_t
    (S1 : Hashable_arg
  [@mode m])
    (S2 : Hashable_arg
  [@mode m]) =
  Hasher [@mode m] [@modality p]
    (Hasher_sexpable_of_hashable_sexpable [@mode m] [@modality p]
       (S1))
       (Hasher_sexpable_of_hashable_sexpable [@mode m] [@modality p] (S2))

module%template.portable [@modality p] Hashable = Hashable_t [@mode m] [@modality p]]
