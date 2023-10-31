open! Import

module Stable = struct
  module V1 = struct
    type 'a t = 'a Base.Maybe_bound.t =
      | Incl of 'a
      | Excl of 'a
      | Unbounded
    [@@deriving
      bin_io ~localize, compare, equal, hash, sexp, sexp_grammar, stable_witness]

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
[@@deriving bin_io ~localize, compare, equal, hash, quickcheck, sexp]

let compare_one_sided ~side compare_a t1 t2 =
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
;;

module As_lower_bound = struct
  type nonrec 'a t = 'a t [@@deriving bin_io, equal, hash, sexp]

  let compare compare_a t1 t2 = compare_one_sided ~side:`Lower compare_a t1 t2
end

module As_upper_bound = struct
  type nonrec 'a t = 'a t [@@deriving bin_io, equal, hash, sexp]

  let compare compare_a t1 t2 = compare_one_sided ~side:`Upper compare_a t1 t2
end
