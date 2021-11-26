(** Extends {{!Base.Ordering}[Base.Ordering]}, intended to make code that matches on the
    result of a comparison more concise and easier to read.
*)

open! Import

type t = Base.Ordering.t =
  | Less
  | Equal
  | Greater
[@@deriving bin_io, compare, hash, sexp]

include module type of Base.Ordering with type t := t (** @inline *)
