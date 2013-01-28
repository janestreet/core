open Sexplib
open T

module type Key = sig
  type t with sexp

  val compare : t -> t -> int

  (** Values returned by [hash] must be non-negative.  An exception will be raised in the
      case that [hash] returns a negative value. *)
  val hash : t -> int
end

module Hashable = struct
  type 'a t =
    { hash : 'a -> int;
      compare : 'a -> 'a -> int;
      sexp_of_t : 'a -> Sexp.t;
    }

  (* Copied from Inria hashtbl.ml *)
  external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"

  let hash x = hash_param 10 100 x

  let poly = { hash;
               compare;
               sexp_of_t = (fun _ -> Sexp.Atom "_");
             }

  let of_key (type a) k =
    let module Key = (val k : Key with type t = a) in
    { hash = Key.hash;
      compare = Key.compare;
      sexp_of_t = Key.sexp_of_t;
    }
  ;;

end

module type Hashable = sig
  type 'a t = 'a Hashable.t =
    { hash : 'a -> int;
      compare : 'a -> 'a -> int;
      sexp_of_t : 'a -> Sexp.t;
    }

  val poly : 'a t

  val of_key : (module Key with type t = 'a) -> 'a t

  val hash_param : int -> int -> 'a -> int

  val hash : 'a -> int
end

module type Accessors = sig
  type ('a, 'b) t
  type 'a key

  val sexp_of_key : ('a, _) t -> 'a key -> Sexp.t
  val clear : (_, _) t -> unit
  val copy : ('a, 'b) t -> ('a, 'b) t
  val invariant : (_, _) t -> unit
  val fold :
    ('a, 'b) t -> init:'c -> f:(key:'a key -> data:'b -> 'c -> 'c) -> 'c
  val iter : ('a, 'b) t -> f:(key:'a key -> data:'b -> unit) -> unit
  val existsi : ('a, 'b) t -> f:(key: 'a key -> data:'b -> bool) -> bool
  val exists : (_, 'b) t -> f:('b -> bool) -> bool
  val length : (_, _) t -> int
  val is_empty : (_, _) t -> bool
  val mem : ('a, _) t -> 'a key -> bool
  val remove : ('a, _) t -> 'a key -> unit

  (* [remove_one t key] if [key] is present in the table, and [data] is has at least two
     elements then replace [key] with [List.tl data], otherwise remove [key] *)
  val remove_one : ('a, _ list) t -> 'a key -> unit

  val replace : ('a, 'b) t -> key:'a key -> data:'b -> unit
  val set     : ('a, 'b) t -> key:'a key -> data:'b -> unit
  val add     : ('a, 'b) t -> key:'a key -> data:'b -> [ `Ok | `Duplicate ]
  val add_exn : ('a, 'b) t -> key:'a key -> data:'b -> unit

  (** [change t key f] updates the given table by changing the value stored under [key]
      according to [f], just like [Map.change] (see that for example). *)
  val change : ('a, 'b) t -> 'a key -> ('b option -> 'b option) -> unit

  (** [add_multi t ~key ~data] if [key] is present in the table then cons
     [data] on the list, otherwise add [key] with a single element list. *)
  val add_multi : ('a, 'b list) t -> key:'a key -> data:'b -> unit

  (** [remove_multi t key] updates the table, removing the head of the list bound to
      [key]. If the list has only one element (or is empty) then the binding is
      removed. *)
  val remove_multi : ('a, _ list) t -> 'a key -> unit

  (** [map t f] returns new table with bound values replaced by
      [f] applied to the bound values *)
  val map : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t

  (** like [map], but function takes both key and data as arguments *)
  val mapi : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'c) -> ('a, 'c) t

  (** returns new map with bound values filtered by f applied to the bound
      values *)
  val filter_map : ('a, 'b) t -> f:('b -> 'c option) -> ('a, 'c) t

  (** like [filter_map], but function takes both key and data as arguments*)
  val filter_mapi : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'c option) -> ('a, 'c) t

  val filter : ('a, 'b) t -> f:('b -> bool) -> ('a, 'b) t
  val filteri : ('a, 'b) t -> f:(key:'a key -> data:'b -> bool) -> ('a, 'b) t

  (** returns new maps with bound values partitioned by f applied to the bound values *)
  val partition_map
    :  ('a, 'b) t
    -> f:('b -> [`Fst of 'c | `Snd of 'd])
    -> ('a, 'c) t * ('a, 'd) t

  (** like [partition_map], but function takes both key and data as arguments*)
  val partition_mapi
    :  ('a, 'b) t
    -> f:(key:'a key -> data:'b -> [`Fst of 'c | `Snd of 'd])
    -> ('a, 'c) t * ('a, 'd) t

  val partition_tf : ('a, 'b) t -> f:('b -> bool) -> ('a, 'b) t * ('a, 'b) t
  val partitioni_tf : ('a, 'b) t -> f:(key:'a key -> data:'b -> bool) -> ('a, 'b) t * ('a, 'b) t

  (** [find_or_add t k ~default] returns the data associated with key k if it
      is in the table t, otherwise it lets d = default() and adds it to the
      table. *)
  val find_or_add : ('a, 'b) t -> 'a key -> default:(unit -> 'b) -> 'b

  (** [find t k] returns Some (the current binding) of k in t, or None if no
      such binding exists *)
  val find : ('a, 'b) t -> 'a key -> 'b option

  (** [find_exn t k] returns the current binding of k in t, or raises Not_found
      if no such binding exists.*)
  val find_exn : ('a, 'b) t -> 'a key -> 'b
  (** [iter_vals t ~f] is like iter, except it only supplies the value to f,
      not the key. *)
  val iter_vals : (_, 'b) t -> f:('b -> unit) -> unit

  (** Merge two hashtables.

      The result of [merge f h1 h2] has as keys the set of all [k] in the
      union of the sets of keys of [h1] and [h2] for which [d(k)] is not
      None, where:

      d(k) =
      - f ~key:k (Some d1) None
      if [k] in [h1] is to d1, and [h2] does not map [k];

      - f ~key:k None (Some d2)
      if [k] in [h2] is to d2, and [h1] does not map [k];

      - f ~key:k (Some d1) (Some d2)
      otherwise, where [k] in [h1] is to [d1] and [k] in [h2] is to [d2].

      Each key [k] is mapped to a single piece of data x, where [d(k)] = Some x. *)
  val merge
    :  ('k, 'a) t
    -> ('k, 'b) t
    -> f:(key:'k key -> [ `Left of 'a | `Right of 'b | `Both of 'a * 'b ] -> 'c option)
    -> ('k, 'c) t

  (** Merge one hashtable into another.

      After [merge_into f src dst], for every [key] in [src], [key] will be
      re-mapped in [dst] to [v] if [f ~key d1 (find dst key) = Some v].
  *)
  val merge_into
    :  f:(key:'a key -> 'b -> 'b option -> 'b option)
    -> src:('a, 'b) t
    -> dst:('a, 'b) t
    -> unit

  (** Returns the list of all keys for given hashtable. *)
  val keys : ('a, _) t -> 'a key list
  (** Returns the list of all data for given hashtable. *)
  val data : (_, 'b) t -> 'b list
  (** [filter_inplace t ~f] removes all the elements from [t] that don't satisfy [f]. *)
  val filter_inplace : (_, 'b) t -> f:('b -> bool) -> unit
  val filteri_inplace : ('a, 'b) t -> f:('a key -> 'b -> bool) -> unit

  val equal : ('a, 'b) t -> ('a, 'b) t -> ('b -> 'b -> bool) -> bool

  (** Returns the list of all (key,data) pairs for given hashtable. *)
  val to_alist : ('a, 'b) t -> ('a key * 'b) list

  val incr : ?by:int -> ('a, int) t -> 'a key -> unit
end

type ('key, 'z) create_options_without_hashable =
  ?growth_allowed:bool
  -> ?size:int (* initial size -- default 128 *)
  -> 'z

type ('key, 'z) create_options_with_hashable =
  ?growth_allowed:bool
  -> ?size:int (* initial size -- default 128 *)
  -> hashable:'key Hashable.t
  -> 'z

module type Creators = sig
  type ('a, 'b) t
  type 'a key
  type ('key, 'z) create_options

  val create : ('a key, unit -> ('a, 'b) t) create_options

  val of_alist
    :  ('a key,
        ('a key * 'b) list
        -> [ `Ok of ('a, 'b) t
           | `Duplicate_key of 'a key
           ]) create_options

  val of_alist_report_all_dups
    : ('a key,
       ('a key * 'b) list
       -> [ `Ok of ('a, 'b) t
          | `Duplicate_keys of 'a key list
          ]) create_options

  val of_alist_exn : ('a key, ('a key * 'b) list -> ('a, 'b) t) create_options

  val of_alist_multi : ('a key, ('a key * 'b) list -> ('a, 'b list) t) create_options


  (* create_mapped get_key get_data [x1,...,xn] =
     of_alist [get_key x1, get_data x1; ...; get_key xn, get_data xn] *)
  val create_mapped
    : ('a key,
       get_key:('r -> 'a key)
       -> get_data:('r -> 'b)
       -> 'r list
       -> [ `Ok of ('a, 'b) t
          | `Duplicate_keys of 'a key list ]) create_options

  (* create_with_key ~get_key [x1,...,xn] =
     of_alist [get_key x1, x1; ...; get_key xn, xn] *)
  val create_with_key
    : ('a key,
       get_key:('r -> 'a key)
       -> 'r list
       -> [ `Ok of ('a, 'r) t
          | `Duplicate_keys of 'a key list ]) create_options

  val create_with_key_exn
    : ('a key,
       get_key:('r -> 'a key)
       -> 'r list
       -> ('a, 'r) t) create_options

  val group
    : ('a key,
       get_key:('r -> 'a key)
       -> get_data:('r -> 'b)
       -> combine:('b -> 'b -> 'b)
       -> 'r list
       -> ('a, 'b) t) create_options
end

module type S = sig
  type key
  type ('a, 'b) hashtbl
  type 'b t = (key, 'b) hashtbl with sexp
  type ('a, 'b) t_ = 'b t
  type 'a key_ = key

  val hashable : key Hashable.t

  include Creators
    with type ('a, 'b) t := ('a, 'b) t_
    with type 'a key := 'a key_
    with type ('key, 'z) create_options := ('key, 'z) create_options_without_hashable

  include Accessors with type ('a, 'b) t := ('a, 'b) t_ with type 'a key := 'a key_

end

module type S_binable = sig
  include S
  include Binable.S1 with type 'v t := 'v t
end
