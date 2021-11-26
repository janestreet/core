open Core


(* We just want to make sure all of this compiles. *)

module Set_tests = struct
  type t = Set.M(Int).t [@@deriving compare, hash, sexp]
end

module Map_tests = struct
  type t0 = float Map.M(Int).t [@@deriving compare, hash, sexp]
  type 'a t1 = 'a Map.M(Bool).t [@@deriving compare, hash, sexp]
end

module Hashtbl_tests = struct
  type t0 = float Hashtbl.M(Int).t [@@deriving sexp]
  type 'a t1 = 'a Hashtbl.M(Bool).t [@@deriving sexp]
end
