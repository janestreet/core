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

type sexp_thunk = unit -> Base.Sexp.t

let sexp_of_sexp_thunk x = x ()

exception Duplicate_found of sexp_thunk * Base.String.t [@@deriving sexp]

let exn_if_dup ~compare ?(context = "exn_if_dup") t ~to_sexp =
  match find_a_dup ~compare t with
  | None -> ()
  | Some dup -> raise (Duplicate_found ((fun () -> to_sexp dup), context))
;;

exception Duplicate_found__portable of sexp_thunk * Base.String.t [@@deriving sexp]

let%template exn_if_dup ~compare ?(context = "exn_if_dup") t ~to_sexp =
  match
    find_a_dup
      ~compare:(fun x y -> compare (Modes.Portable.unwrap x) (Modes.Portable.unwrap y))
      (Modes.Portable.wrap_list t)
  with
  | None -> ()
  | Some { portable = dup } ->
    raise (Duplicate_found__portable ((fun () -> to_sexp dup), context))
[@@mode portable]
;;

let slice a start stop =
  Ordered_collection_common.slice ~length_fun:(length :> _ -> _) ~sub_fun:sub a start stop
;;

module Stable = struct
  module V1 = struct
    type%template nonrec 'a t = ('a t[@kind k])
    [@@kind k = (float64, bits32, bits64, word)]
    [@@deriving compare ~localize, equal ~localize]

    type nonrec 'a t = 'a t
    [@@deriving
      sexp, sexp_grammar, bin_io ~localize, compare ~localize, equal ~localize, hash]

    let stable_witness = List0.stable_witness [@@alert "-for_internal_use_only"]
  end
end
