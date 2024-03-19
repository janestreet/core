open! Import
open Std_internal
module Repr = Int63

module T : sig
  type t [@@deriving compare, hash, sexp_of, typerep] [@@immediate64]

  val to_string : t -> string
  val to_string_hum : t -> string
  val of_repr : Repr.t -> t
  val to_repr : t -> Repr.t
end = struct
  type t = Repr.t [@@deriving compare, hash, typerep]

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
    then sprintf "%.7gK" (to_float n /. to_float kib)
    else if n_abs < gib
    then sprintf "%.10gM" (to_float n /. to_float mib)
    else sprintf "%.13gG" (to_float n /. to_float gib)
  ;;

  let to_string_hum n =
    let open Repr in
    let kib = of_int 1024 in
    let mib = kib * kib in
    let gib = kib * mib in
    let tib = kib * gib in
    let pib = kib * tib in
    let n_abs = abs n in
    let f ~suffix ~size n =
      let f = to_float n /. to_float size in
      if Float.( >= ) f 999.5
      then sprintf "%.0f%s" f suffix
      else sprintf "%.3g%s" f suffix
    in
    if n_abs < kib
    then sprintf "%dB" (to_int_exn n)
    else if n_abs < mib
    then f ~suffix:"K" ~size:kib n
    else if n_abs < gib
    then f ~suffix:"M" ~size:mib n
    else if n_abs < tib
    then f ~suffix:"G" ~size:gib n
    else if n_abs < pib
    then f ~suffix:"T" ~size:tib n
    else f ~suffix:"P" ~size:pib n
  ;;

  let sexp_of_t n = Sexp.Atom (to_string n)
end

include T

let bytes_int_exn t = Repr.to_int_exn (to_repr t)
