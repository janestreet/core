open Core.Std

type 'a t = 'a Date.Map.t with sexp, bin_io, compare

val add : 'a t -> key:Date.t -> data:'a -> 'a t

val add_multi : 'a list t -> key:Date.t -> data:'a -> 'a list t

val change : 'a t -> Date.t -> ('a option -> 'a option) -> 'a t

val compare_direct : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val data : 'a t -> 'a list

val empty : 'a t

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val exists : 'a t -> f:('a -> bool) -> bool

val filter : 'a t -> f:(key:Date.t -> data:'a -> bool) -> 'a t

val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

val filter_mapi : 'a t -> f:(key:Date.t -> data:'a -> 'b option) -> 'b t

val find : 'a t -> Date.t -> 'a option

val find_exn : 'a t -> Date.t -> 'a

val fold : 'a t -> init:'b -> f:(key:Date.t -> data:'a -> 'b -> 'b) -> 'b

val fold_range_inclusive :
  'a t
  -> min:Date.t
  -> max:Date.t
  -> init:'b
  -> f:(key:Date.t -> data:'a -> 'b -> 'b)
  -> 'b

val fold_right : 'a t -> init:'b -> f:(key:Date.t -> data:'a -> 'b -> 'b) -> 'b

val for_all : 'a t -> f:('a -> bool) -> bool

val is_empty : 'a t -> bool

val iter : 'a t -> f:(key:Date.t -> data:'a -> unit) -> unit

val keys : 'a t -> Date.t list

val length : 'a t -> int

val map : 'a t -> f:('a -> 'b) -> 'b t

val mapi : 'a t -> f:(key:Date.t -> data:'a -> 'b) -> 'b t

val max_elt : 'a t -> (Date.t * 'a) option

val max_elt_exn : 'a t -> Date.t * 'a

val mem : 'a t -> Date.t -> bool

val merge :
  'a t
  -> 'b t
  -> f:(key:Date.t -> [`Both of 'a * 'b | `Left of 'a | `Right of 'b] -> 'c option)
  -> 'c t

val min_elt : 'a t -> (Date.t * 'a) option

val min_elt_exn : 'a t -> Date.t * 'a

val next_key : 'a t -> Date.t -> (Date.t * 'a) option

val of_alist : (Date.t * 'a) list -> [`Duplicate_key of Date.t | `Ok of 'a t]

val of_alist_exn : (Date.t * 'a) list -> 'a t

val of_alist_fold : (Date.t * 'a) list -> init:'b -> f:('b -> 'a -> 'b) -> 'b t

val of_alist_multi : (Date.t * 'a) list -> 'a list t

val prev_key : 'a t -> Date.t -> (Date.t * 'a) option

val range_to_alist : 'a t -> min:Date.t -> max:Date.t -> (Date.t * 'a) list

val rank : 'a t -> Date.t -> int option

val remove : 'a t -> Date.t -> 'a t

val singleton : Date.t -> 'a -> 'a t

val to_alist : 'a t -> (Date.t * 'a) list

