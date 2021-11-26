open! Import
open Std_internal
module Repr = Int63

module T : sig
  type t [@@deriving compare, hash, sexp_of] [@@immediate64]

  val to_string : t -> string
  val of_repr : Repr.t -> t
  val to_repr : t -> Repr.t
end = struct
  type t = Repr.t [@@deriving compare, hash]

  let of_repr = Fn.id
  let to_repr = Fn.id

  let to_string n =
    let open Repr in
    let kib = of_int 1024 in
    let mib = kib * kib in
    let gib = kib * mib in
    let n_abs = abs n in
    if n_abs < kib
    then sprintf "%dB" (to_int_exn n)
    else if n_abs < mib
    then sprintf "%gK" (to_float n /. to_float kib)
    else if n_abs < gib
    then sprintf "%gM" (to_float n /. to_float mib)
    else sprintf "%gG" (to_float n /. to_float gib)
  ;;

  let sexp_of_t n = Sexp.Atom (to_string n)
end

include T

let bytes_int_exn t = Repr.to_int_exn (to_repr t)
