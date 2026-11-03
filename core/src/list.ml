include List0 (** @inline *)

let zip_with_remainder =
  let rec zip_with_acc_and_remainder acc xs ys =
    match xs, ys with
    | [], [] -> rev acc, None
    | fst, [] -> rev acc, Some (Either.First fst)
    | [], snd -> rev acc, Some (Either.Second snd)
    | x :: xs, y :: ys -> zip_with_acc_and_remainder ((x, y) :: acc) xs ys
  in
  fun xs ys -> zip_with_acc_and_remainder [] xs ys
;;

module Wrapper = struct
  let wrap_list = Fn.id
  let unwrap = Fn.id
end

type sexp_thunk = unit -> Base.Sexp.t

let sexp_of_sexp_thunk x = x ()

exception Duplicate_found of sexp_thunk * Base.String.t
[@@deriving sexp ~nonportable__magic_unsafe_in_parallel_programs]

let exn_if_dup ~compare ?(context = "exn_if_dup") t ~to_sexp =
  let open Wrapper in
  match find_a_dup ~compare:(fun x y -> compare (unwrap x) (unwrap y)) (wrap_list t) with
  | None -> ()
  | Some dup ->
    let dup = unwrap dup in
    raise (Duplicate_found ((fun () -> to_sexp dup), context))
;;

let slice a start stop =
  Ordered_collection_common.slice ~length_fun:(length :> _ -> _) ~sub_fun:sub a start stop
;;

module Stable = struct
  module V1 = struct
    type nonrec 'a t = 'a t [@@deriving sexp, sexp_grammar, bin_io, compare, equal, hash]

    let stable_witness = List0.stable_witness [@@alert "-for_internal_use_only"]
  end
end
