module Hashtbl = Core_hashtbl
module List = StdLabels.List
open Sexplib
open Sexplib.Conv
open With_return
open Result.Export
open Hash_set_intf

module Hashable = Hashtbl.Hashable

type 'a t = ('a, unit) Hashtbl.t
type 'a hash_set = 'a t
type 'a elt = 'a

module type S         = S         with type 'a hash_set = 'a t
module type S_binable = S_binable with type 'a hash_set = 'a t

module Accessors = struct

  let clear = Hashtbl.clear
  let length = Hashtbl.length
  let mem = Hashtbl.mem

  let is_empty t = Hashtbl.is_empty t

  let find_map t ~f =
    with_return (fun r ->
      Hashtbl.iter t ~f:(fun ~key:elt ~data:_ ->
        match f elt with
        | None -> ()
        | Some _ as o -> r.return o);
      None)
  ;;

  let find t ~f = find_map t ~f:(fun a -> if f a then Some a else None)

  let add t k = Hashtbl.set t ~key:k ~data:()

  let strict_add t k =
    if mem t k then Or_error.error_string "element already exists"
    else begin
      Hashtbl.set t ~key:k ~data:();
      Result.Ok ()
    end
  ;;

  let strict_add_exn t k = Or_error.ok_exn (strict_add t k)

  let remove = Hashtbl.remove

  let strict_remove t k =
    if mem t k then begin
      remove t k;
      Result.Ok ()
    end else
      Or_error.error "element not in set" k (Hashtbl.sexp_of_key t)
  ;;

  let strict_remove_exn t k = Or_error.ok_exn (strict_remove t k)

  let fold t ~init ~f = Hashtbl.fold t ~init ~f:(fun ~key ~data:() acc -> f acc key)

  let count t ~f = Container.fold_count fold t ~f

  let iter t ~f = Hashtbl.iter t ~f:(fun ~key ~data:() -> f key)

  let to_list = Hashtbl.keys

  let sexp_of_t sexp_of_e t = sexp_of_list sexp_of_e (to_list t)

  let to_array t = Array.of_list (to_list t)

  let exists  t ~f =      Hashtbl.existsi t ~f:(fun ~key ~data:() ->      f key)
  let for_all t ~f = not (Hashtbl.existsi t ~f:(fun ~key ~data:() -> not (f key)))

  let equal t1 t2 = Hashtbl.equal t1 t2 (fun () () -> true)

  let copy t = Hashtbl.copy t

  let filter t ~f = Hashtbl.filteri t ~f:(fun ~key ~data:() -> f key)

  let diff t1 t2 = filter t1 ~f:(fun key -> not (Hashtbl.mem t2 key))

  let filter_inplace t ~f =
    let to_remove =
      fold t ~init:[] ~f:(fun ac x ->
        if f x then ac else x :: ac)
    in
    List.iter to_remove ~f:(fun x -> remove t x)
  ;;

  let of_hashtbl_keys hashtbl = Hashtbl.map hashtbl ~f:ignore
end

include Accessors

let create ?growth_allowed ?size ~hashable () =
  Hashtbl.create ?growth_allowed ?size ~hashable ()
;;

let of_list ?growth_allowed ?size ~hashable l =
  let size = match size with Some x -> x | None -> List.length l in
  let t = Hashtbl.create ?growth_allowed ~size ~hashable () in
  List.iter l ~f:(fun k -> add t k);
  t
;;

module Creators (Elt : sig
  type 'a t

  val hashable : 'a t Hashable.t
end) : sig

  type 'a t_ = 'a Elt.t t

  val t_of_sexp : (Sexp.t -> 'a Elt.t) -> Sexp.t -> 'a t_

  include Creators
    with type 'a t := 'a t_
    with type 'a elt := 'a Elt.t
    with type ('elt, 'z) create_options := ('elt, 'z) create_options_without_hashable

end = struct

  type 'a t_ = 'a Elt.t t

  let hashable = Elt.hashable

  let create ?growth_allowed ?size () = Hashtbl.create ?growth_allowed ~hashable ?size ()

  let of_list ?growth_allowed ?size l = of_list ?growth_allowed ?size ~hashable l

  let t_of_sexp e_of_sexp sexp =
    match sexp with
    | Sexp.Atom _ ->
      raise (Of_sexp_error (Failure "Hash_set.t_of_sexp requires a list", sexp))
    | Sexp.List list ->
      let t = create ~size:(List.length list) () in
      List.iter list ~f:(fun sexp ->
        let e = e_of_sexp sexp in
        match strict_add t e with
        | Ok () -> ()
        | Error _ ->
          raise (Of_sexp_error
                   (Error.to_exn
                      (Error.create "Hash_set.t_of_sexp got a duplicate element"
                         sexp Fn.id),
                    sexp)));
      t
  ;;

end

module Poly = struct

  type 'a t = 'a hash_set

  type 'a elt = 'a

  let hashable = Hashtbl.Poly.hashable

  include Creators (struct
    type 'a t = 'a
    let hashable = hashable
  end)

  include Accessors

  let sexp_of_t = sexp_of_t

end

module type Elt = Hashtbl.Key

module Make (Elt : Elt) = struct

  module T = Hashtbl.Make (Elt)

  type elt = Elt.t
  type 'a hash_set = 'a t
  type t = elt hash_set
  type 'a elt_ = elt

  include Creators (struct type 'a t = Elt.t let hashable = T.hashable end)

  let sexp_of_t t = Poly.sexp_of_t Elt.sexp_of_t t

  let t_of_sexp sexp = t_of_sexp Elt.t_of_sexp sexp

end

module Make_binable (Elt : sig
  include Elt
  include Binable.S with type t := t
end) = struct

  include Make (Elt)

  include Bin_prot.Utils.Make_iterable_binable (struct
    type t = elt hash_set
    type el = Elt.t with bin_io
    type acc = t
    let module_name = Some "Core.Hash_set"
    let length = length
    let iter = iter
    let init size = create ~size ()
    let insert acc v _i = add acc v; acc
    let finish t = t
  end)

end
