open! Import

[%%template
[@@@kind_set.define ks = base_or_null]

module Stable = struct
  module V1 = struct
    include struct
      type ('a : k) t = ('a Base.Maybe_bound.t[@kind k]) =
        | Incl of 'a
        | Excl of 'a
        | Unbounded
      [@@kind k]
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , hash
        , sexp
        , sexp_grammar
        , stable_witness]
    end [@@kind k = ks]

    let map (x : 'a t) ~f : 'b t =
      match x with
      | Incl x -> Incl (f x)
      | Excl x -> Excl (f x)
      | Unbounded -> Unbounded
    ;;
  end
end

include Base.Maybe_bound

type ('a : k) t = ('a Base.Maybe_bound.t[@kind k]) =
  | Incl of 'a
  | Excl of 'a
  | Unbounded
[@@deriving
  bin_io ~localize
  , compare ~localize
  , equal ~localize
  , hash
  , quickcheck ~portable
  , sexp
  , sexp_grammar]
[@@kind k = ks]

let compare_one_sided
  (type a : k)
  ~side
  compare_a
  (t1 : (a t[@kind k]) @ m)
  (t2 : (a t[@kind k]) @ m)
  =
  match t1, t2 with
  | Unbounded, Unbounded -> 0
  | Unbounded, _ ->
    (match side with
     | `Lower -> -1
     | `Upper -> 1)
  | _, Unbounded ->
    (match side with
     | `Lower -> 1
     | `Upper -> -1)
  | Incl a1, Incl a2 -> compare_a a1 a2
  | Excl a1, Excl a2 -> compare_a a1 a2
  | Incl a1, Excl a2 ->
    let c = compare_a a1 a2 in
    if c = 0
    then (
      match side with
      | `Lower -> -1
      | `Upper -> 1)
    else c
  | Excl a1, Incl a2 ->
    let c = compare_a a1 a2 in
    if c = 0
    then (
      match side with
      | `Lower -> 1
      | `Upper -> -1)
    else c
[@@kind k = ks] [@@mode m = (local, global)]
;;

module As_lower_bound = struct
  [@@@kind.default k = ks]

  type nonrec ('a : k) t = ('a t[@kind k])
  [@@deriving bin_io, equal ~localize, hash, sexp, sexp_grammar]

  let compare compare_a t1 t2 =
    (compare_one_sided [@kind k] [@mode m]) ~side:`Lower compare_a t1 t2
  [@@mode m = (local, global)]
  ;;
end

module As_upper_bound = struct
  [@@@kind.default k = ks]

  type nonrec ('a : k) t = ('a t[@kind k])
  [@@deriving bin_io, equal ~localize, hash, sexp, sexp_grammar]

  let compare compare_a t1 t2 =
    (compare_one_sided [@kind k] [@mode m]) ~side:`Upper compare_a t1 t2
  [@@mode m = (local, global)]
  ;;
end]
