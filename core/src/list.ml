include List0 (** @inline *)

(** [stable_dedup] Same as [dedup] but maintains the order of the list and doesn't allow
    compare function to be specified (otherwise, the implementation in terms of Set.t
    would hide a heavyweight functor instantiation at each call). *)
let stable_dedup = Set.Poly.stable_dedup_list

(* This function is staged to indicate that real work (the functor application) takes
   place after a partial application. *)
let stable_dedup_staged (type a) ~(compare : a -> a -> int)
  : (a list -> a list) Base.Staged.t
  =
  let module Set =
    Set.Make (struct
      type t = a

      let compare = compare

      (* [stable_dedup_list] never calls these *)
      let t_of_sexp _ = assert false
      let sexp_of_t _ = assert false
    end)
  in
  Base.Staged.stage Set.stable_dedup_list
;;

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

let slice a start stop =
  Ordered_collection_common.slice ~length_fun:length ~sub_fun:sub a start stop
;;

module Stable = struct
  module V1 = struct
    type nonrec 'a t = 'a t [@@deriving sexp, bin_io, compare]
  end
end
