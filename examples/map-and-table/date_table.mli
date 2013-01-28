open Core.Std

type 'a t = 'a Date.Table.t with sexp, bin_io

val add : 'b t -> key:Date.t -> data:'b -> [ `Duplicate | `Ok ]

val add_exn : 'b t -> key:Date.t -> data:'b -> unit

val add_multi : 'b list t -> key:Date.t -> data:'b -> unit

val change : 'b t -> Date.t -> ('b option -> 'b option) -> unit

val clear : 'a t -> unit

val copy : 'a t -> 'a t

val create : ?growth_allowed:bool -> ?size:int -> unit -> 'a t

val create_mapped :
  ?growth_allowed:bool
  -> ?size:int
  -> get_key:('a -> Date.t)
  -> get_data:('a -> 'b)
  -> 'a list
  -> [`Duplicate_keys of Date.t list | `Ok of 'b t]

val create_with_key_exn :
  ?growth_allowed:bool -> ?size:int -> get_key:('a -> Date.t) -> 'a list -> 'a t

val create_with_key :
  ?growth_allowed:bool
  -> ?size:int
  -> get_key:('a -> Date.t)
  -> 'a list
  -> [`Duplicate_keys of Date.t list | `Ok of 'a t]

val data : 'b t -> 'b list

val equal : 'b t -> 'b t -> ('b -> 'b -> bool) -> bool

val exists : 'b t -> f:('b -> bool) -> bool

val existsi : 'b t -> f:(key:Date.t -> data:'b -> bool) -> bool

val filter : 'b t -> f:('b -> bool) -> 'b t

val filteri : 'b t -> f:(key:Date.t -> data:'b -> bool) -> 'b t

val filteri_inplace : 'b t -> f:(Date.t -> 'b -> bool) -> unit

val filter_inplace : 'b t -> f:('b -> bool) -> unit

val filter_map : 'b t -> f:('b -> 'c option) -> 'c t

val filter_mapi : 'b t -> f:(key:Date.t -> data:'b -> 'c option) -> 'c t

val find : 'b t -> Date.t -> 'b option

val find_exn : 'b t -> Date.t -> 'b

val find_or_add : 'b t -> Date.t -> default:(unit -> 'b) -> 'b

val fold : 'a t -> init:'b -> f:(key:Date.t -> data:'a -> 'b -> 'b) -> 'b

val group :
  ?growth_allowed:bool
  -> ?size:int
  -> get_key:('a -> Date.t)
  -> get_data:('a -> 'b)
  -> combine:('b -> 'b -> 'b)
  -> 'a list
  -> 'b t

val incr : ?by:int -> int t -> Date.t -> unit

val invariant : 'a t -> unit

val is_empty : 'b t -> bool

val iter : 'b t -> f:(key:Date.t -> data:'b -> unit) -> unit

val iter_vals : 'b t -> f:('b -> unit) -> unit

val keys : 'b t -> Date.t list

val length : 'b t -> int

val map : 'b t -> f:('b -> 'c) -> 'c t

val mapi : 'b t -> f:(key:Date.t -> data:'b -> 'c) -> 'c t

val mem : 'b t -> Date.t -> bool

val merge :
  'b t
  -> 'c t
  -> f:(key:Date.t -> [ `Both of 'b * 'c | `Left of 'b | `Right of 'c ] -> 'd option)
  -> 'd t

val merge_into :
  f:(key:Date.t -> 'b -> 'b option -> 'b option) -> src:'b t -> dst:'b t -> unit

val of_alist_exn : ?growth_allowed:bool -> ?size:int -> (Date.t * 'a) list -> 'a t

val of_alist :
  ?growth_allowed:bool
  -> ?size:int
  -> (Date.t * 'a) list
  -> [`Duplicate_key of Date.t | `Ok of 'a t]

val of_alist_multi : ?growth_allowed:bool -> ?size:int -> (Date.t * 'a) list -> 'a list t

val of_alist_report_all_dups :
  ?growth_allowed:bool
  -> ?size:int
  -> (Date.t * 'a) list
  -> [ `Duplicate_keys of Date.t list | `Ok of 'a t ]

val partitioni_tf : 'b t -> f:(key:Date.t -> data:'b -> bool) -> 'b t * 'b t

val partition_map : 'b t -> f:('b -> [ `Fst of 'c | `Snd of 'd ]) -> 'c t * 'd t

val partition_mapi :
  'b t -> f:(key:Date.t -> data:'b -> [ `Fst of 'c | `Snd of 'd ]) -> 'c t * 'd t

val partition_tf : 'b t -> f:('b -> bool) -> 'b t * 'b t

val remove : 'b t -> Date.t -> unit

val remove_multi : 'b list t -> Date.t -> unit

val remove_one : 'b list t -> Date.t -> unit

val replace : 'b t -> key:Date.t -> data:'b -> unit

val set : 'b t -> key:Date.t -> data:'b -> unit

val sexp_of_key : 'a t -> Date.t -> Sexp.t

val to_alist : 'b t -> (Date.t * 'b) list

