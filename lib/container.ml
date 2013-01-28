(* This file has generic signatures for container data structures, with standard functions
   (iter, fold, exists, for_all, ...) that one would expect to find in any container.  The
   idea is to include [Container.S0] or [Container.S1] in the signature for every
   container-like data structure (Array, List, String, ...) to ensure a consistent
   interface. *)

open T
open With_return

module type T = sig
  type 'a t
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
end

let fold_count fold t ~f = fold t ~init:0 ~f:(fun n a -> if f a then n + 1 else n)

module Make (T : T) = struct
  open T

  let fold = fold

  let count t ~f = fold_count fold t ~f

  let iter t ~f = fold t ~init:() ~f:(fun () a -> f a)

  let length c = fold c ~init:0 ~f:(fun acc _ -> acc + 1)

  let is_empty c =
    with_return (fun r ->
      iter c ~f:(fun _ -> r.return false);
      true)
  ;;

  let exists c ~f =
    with_return (fun r ->
      iter c ~f:(fun x -> if f x then r.return true);
      false)
  ;;

  let mem ?(equal = (=)) t a = exists t ~f:(equal a)

  let for_all c ~f =
    with_return (fun r ->
      iter c ~f:(fun x -> if not (f x) then r.return false);
      true)
  ;;

  let find_map t ~f =
    with_return (fun r ->
      iter t ~f:(fun x -> match f x with None -> () | Some _ as res -> r.return res);
      None)
  ;;

  let find c ~f =
    with_return (fun r ->
      iter c ~f:(fun x -> if f x then r.return (Some x));
      None)
  ;;

  let to_list c = List.rev (fold c ~init:[] ~f:(fun acc x -> x :: acc))

  let to_array c = Array.of_list (to_list c)
end

(* Signature for monomorphic container, e.g., string *)
module type S0 = sig
  type t
  type elt

  val mem : ?equal:(elt -> elt -> bool) -> t -> elt -> bool
  val length   : t -> int
  val is_empty : t -> bool
  val iter     : t -> f:(elt -> unit) -> unit
  val fold     : t -> init:'accum -> f:('accum -> elt -> 'accum) -> 'accum
  val exists   : t -> f:(elt -> bool) -> bool
  val for_all  : t -> f:(elt -> bool) -> bool
  val count    : t -> f:(elt -> bool) -> int
  val find     : t -> f:(elt -> bool) -> elt option
  val find_map : t -> f:(elt -> 'a option) -> 'a option
  val to_list  : t -> elt list
  val to_array : t -> elt array
(* val compare : t -> t -> cmp:(elt -> elt -> int) -> int *)
end

module type S0_phantom = sig
  type elt
  type 'a t
  val mem : ?equal:(elt -> elt -> bool) -> _ t -> elt -> bool
  val length   : _ t -> int
  val is_empty : _ t -> bool
  val iter     : _ t -> f:(elt -> unit) -> unit
  val fold     : _ t -> init:'accum -> f:('accum -> elt -> 'accum) -> 'accum
  val exists   : _ t -> f:(elt -> bool) -> bool
  val for_all  : _ t -> f:(elt -> bool) -> bool
  val count    : _ t -> f:(elt -> bool) -> int
  val find     : _ t -> f:(elt -> bool) -> elt option
  val find_map : _ t -> f:(elt -> 'a option) -> 'a option
  val to_list  : _ t -> elt list
  val to_array : _ t -> elt array
  (* val compare : _ t -> _ t -> cmp:(elt -> elt -> int) -> int *)
end

(* Signature for polymorphic container, e.g., 'a list or 'a array *)
module type S1 = sig
  type 'a t
  val mem : ?equal:('a -> 'a -> bool) -> 'a t -> 'a -> bool
  val length   : 'a t -> int
  val is_empty : 'a t -> bool
  val iter     : 'a t -> f:('a -> unit) -> unit
  val fold     : 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum
  val exists   : 'a t -> f:('a -> bool) -> bool
  val for_all  : 'a t -> f:('a -> bool) -> bool
  val count    : 'a t -> f:('a -> bool) -> int
  val find     : 'a t -> f:('a -> bool) -> 'a option
  val find_map : 'a t -> f:('a -> 'b option) -> 'b option
  val to_list  : 'a t -> 'a list
  val to_array : 'a t -> 'a array
  (* val compare : 'a t -> 'a t -> cmp:('a -> 'a -> int) -> int *)
end

module type S1_phantom = sig
  type ('a, +'phantom) t
  val mem : ?equal:('a -> 'a -> bool) -> ('a, _) t -> 'a -> bool
  val length   : ('a, _) t -> int
  val is_empty : ('a, _) t -> bool
  val iter     : ('a, _) t -> f:('a -> unit) -> unit
  val fold     : ('a, _) t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum
  val exists   : ('a, _) t -> f:('a -> bool) -> bool
  val for_all  : ('a, _) t -> f:('a -> bool) -> bool
  val count    : ('a, _) t -> f:('a -> bool) -> int
  val find     : ('a, _) t -> f:('a -> bool) -> 'a option
  val find_map : ('a, _) t -> f:('a -> 'b option) -> 'b option
  val to_list  : ('a, _) t -> 'a list
  val to_array : ('a, _) t -> 'a array
end

module type Generic = sig
  type 'a t
  type 'a elt
  val mem : ?equal:('a elt -> 'a elt -> bool) -> 'a t -> 'a elt -> bool
  val length   : _  t -> int
  val is_empty : _  t -> bool
  val iter     : 'a t -> f:('a elt -> unit) -> unit
  val fold     : 'a t -> init:'accum -> f:('accum -> 'a elt -> 'accum) -> 'accum
  val exists   : 'a t -> f:('a elt -> bool) -> bool
  val for_all  : 'a t -> f:('a elt -> bool) -> bool
  val count    : 'a t -> f:('a elt -> bool) -> int
  val find     : 'a t -> f:('a elt -> bool) -> 'a elt option
  val find_map : 'a t -> f:('a elt -> 'b option) -> 'b option
  val to_list  : 'a t -> 'a elt list
  val to_array : 'a t -> 'a elt array
  (* val compare : 'a t -> 'a t -> cmp:('a elt -> 'a elt -> int) -> int *)
end

module type Generic_phantom = sig
  type ('a, 'phantom) t
  type 'a elt
  val mem : ?equal:('a elt -> 'a elt -> bool) -> ('a, _) t -> 'a elt -> bool
  val length   : (_, _) t -> int
  val is_empty : (_, _) t -> bool
  val iter     : ('a, _) t -> f:('a elt -> unit) -> unit
  val fold     : ('a, _) t -> init:'accum -> f:('accum -> 'a elt -> 'accum) -> 'accum
  val exists   : ('a, _) t -> f:('a elt -> bool) -> bool
  val for_all  : ('a, _) t -> f:('a elt -> bool) -> bool
  val count    : ('a, _) t -> f:('a elt -> bool) -> int
  val find     : ('a, _) t -> f:('a elt -> bool) -> 'a elt option
  val find_map : ('a, _) t -> f:('a elt -> 'b option) -> 'b option
  val to_list  : ('a, _) t -> 'a elt list
  val to_array : ('a, _) t -> 'a elt array
  (* val compare : 'a t -> 'a t -> cmp:('a elt -> 'a elt -> int) -> int *)
end

(* The following functors exist as a consistency check among all the various [S?]
   interfaces.  They ensure that each particular [S?] is an instance of a more generic
   signature. *)
module Check (T : T1) (Elt : T1)
  (M : Generic with type 'a t := 'a T.t with type 'a elt := 'a Elt.t) = struct end

module Check_S0 (M : S0) =
  Check (struct type 'a t = M.t end) (struct type 'a t = M.elt end) (M)

module Check_S0_phantom (M : S0_phantom) =
  Check (struct type 'a t = 'a M.t end) (struct type 'a t = M.elt end) (M)

module Check_S1 (M : S1) =
  Check (struct type 'a t = 'a M.t end) (struct type 'a t = 'a end) (M)

type phantom

module Check_S1_phantom (M : S1_phantom) =
  Check (struct type 'a t = ('a, phantom) M.t end) (struct type 'a t = 'a end) (M)

