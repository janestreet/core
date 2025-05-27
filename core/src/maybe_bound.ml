open! Import

module Stable = struct
  module V1 = struct
    type 'a t = 'a Base.Maybe_bound.t =
      | Incl of 'a
      | Excl of 'a
      | Unbounded
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , hash
      , sexp
      , sexp_grammar
      , stable_witness]

    let map x ~f =
      match x with
      | Incl x -> Incl (f x)
      | Excl x -> Excl (f x)
      | Unbounded -> Unbounded
    ;;
  end
end

include Base.Maybe_bound

type 'a t = 'a Stable.V1.t =
  | Incl of 'a
  | Excl of 'a
  | Unbounded
[@@deriving
  bin_io ~localize
  , compare ~localize
  , equal ~localize
  , hash
  , quickcheck
  , sexp
  , sexp_grammar]

let%template compare_one_sided ~side compare_a (t1 @ m) (t2 @ m) =
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
[@@mode m = (local, global)]
;;

module As_lower_bound = struct
  type nonrec 'a t = 'a t [@@deriving bin_io, equal ~localize, hash, sexp, sexp_grammar]

  let%template compare compare_a t1 t2 =
    (compare_one_sided [@mode m]) ~side:`Lower compare_a t1 t2
  [@@mode m = (local, global)]
  ;;
end

module As_upper_bound = struct
  type nonrec 'a t = 'a t [@@deriving bin_io, equal ~localize, hash, sexp, sexp_grammar]

  let%template compare compare_a t1 t2 =
    (compare_one_sided [@mode m]) ~side:`Upper compare_a t1 t2
  [@@mode m = (local, global)]
  ;;
end
