open Sexplib

type ('a, 'unique_id) t =
  { compare : 'a -> 'a -> int;
    sexp_of_t : 'a -> Sexp.t;
  }
with fields

type ('a, 'unique_id) t_ = ('a, 'unique_id) t

module type Pre = sig
  type t with sexp
  val compare : t -> t -> int
end

module type Pre_binable = sig
  type t with bin_io, sexp
  val compare : t -> t -> int
end

module type S = sig
  include Pre
  type comparator
  val comparator : (t, comparator) t_
end

module type S_binable = sig
  include Pre_binable
  type comparator
  val comparator : (t, comparator) t_
end

module Make (M : Pre) = struct
  include M
  type comparator
  let comparator = let open M in { compare; sexp_of_t }
end

module Make_binable (M : Pre_binable) = struct
  include M
  type comparator
  let comparator = let open M in { compare; sexp_of_t }
end

module type S1 = sig
  type 'a t
  type comparator
  val comparator : ('a t, comparator) t_
end

module S_to_S1 (S : S) = struct
  type 'a t = S.t
  type comparator = S.comparator
  open S
  let comparator = comparator
  let compare = compare
  let sexp_of_t = sexp_of_t
end

module Make1 (M : sig
  type 'a t
  val compare : 'a t -> 'a t -> int
  val sexp_of_t : 'a t -> Sexp.t
end) = struct
  type comparator
  let comparator = let open M in { compare; sexp_of_t }
end

module Poly = struct
  type 'a t = 'a
  include Make1 (struct
    type 'a t = 'a
    let compare = Pervasives.compare
    let sexp_of_t _ = Sexp.Atom ""
  end)
end
