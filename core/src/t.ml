(** Derived from [Base.T]. Used for matching bare signatures with just a type. *)

open! Import
include Base.T

module type T_bin = sig
  type t [@@deriving bin_io]
end
