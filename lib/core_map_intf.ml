(** This module defines interfaces used in [Core.Std.Map].  See the documentation in
    core_map.mli for a description of the approach.

    CRs and comments about [Map] functions do not belong in this file.  They belong next
    to the appropriate function in core_map.mli.

    This module defines module types
    [{Creators,Accessors}{1,2,3,_generic,_with_comparator}].  It uses check functors to
    ensure that each module types is an instance of the corresponding [_generic] one.

    We must treat [Creators] and [Accessors] separately, because we sometimes need to
    choose different instantiations of their [options].  In particular, [Map] itself
    matches [Creators3_with_comparator] but [Accessors3] (without comparator).
*)

open T

module Binable = Binable0
module List = Core_list
module type Key = Comparator.Pre
module type Key_binable = Comparator.Pre_binable

module Without_comparator = struct
  type ('key, 'cmp, 'z) t = 'z
end

module With_comparator = struct
  type ('key, 'cmp, 'z) t = comparator:('key, 'cmp) Comparator.t -> 'z
end

module type Accessors_generic = sig
  type ('a, 'b, 'cmp) t
  type ('a, 'b, 'cmp) tree
  type 'a key
  type ('a, 'cmp, 'z) options

  val invariants :
    ('k, 'cmp,
     ('k, 'v, 'cmp) t -> bool
    ) options

  val is_empty : (_, _, _) t -> bool

  val length : (_, _, _) t -> int

  val add :
    ('k, 'cmp,
     ('k, 'v, 'cmp) t -> key:'k key -> data:'v -> ('k, 'v, 'cmp) t
    ) options

  val add_multi
    : ('k, 'cmp,
       ('k, 'v list, 'cmp) t
       -> key:'k key
       -> data:'v
       -> ('k, 'v list, 'cmp) t
      ) options

  val change
    : ('k, 'cmp,
       ('k, 'v, 'cmp) t
       -> 'k key
       -> ('v option -> 'v option)
       -> ('k, 'v, 'cmp) t
      ) options

  val find     : ('k, 'cmp, ('k, 'v, 'cmp) t -> 'k key -> 'v option) options
  val find_exn : ('k, 'cmp, ('k, 'v, 'cmp) t -> 'k key -> 'v       ) options

  val remove :
    ('k, 'cmp, ('k, 'v, 'cmp) t -> 'k key -> ('k, 'v, 'cmp) t
    ) options

  val mem : ('k, 'cmp, ('k, _, 'cmp) t -> 'k key -> bool) options

  val iter : ('k, 'v, _) t -> f:(key:'k key -> data:'v -> unit) -> unit

  val iter2
    : ('k, 'cmp,
       ('k, 'v1, 'cmp) t
       -> ('k, 'v2, 'cmp) t
       -> f:(key:'k key
             -> data:[ `Left of 'v1 | `Right of 'v2 | `Both of 'v1 * 'v2 ]
             -> unit)
       -> unit
      ) options

  val map : ('k, 'v1, 'cmp) t -> f:('v1 -> 'v2) -> ('k, 'v2, 'cmp) t

  val mapi
    :  ('k, 'v1, 'cmp) t
    -> f:(key:'k key -> data:'v1 -> 'v2)
    -> ('k, 'v2, 'cmp) t

  val fold       : ('k, 'v, _) t -> init:'a -> f:(key:'k key -> data:'v -> 'a -> 'a) -> 'a
  val fold_right : ('k, 'v, _) t -> init:'a -> f:(key:'k key -> data:'v -> 'a -> 'a) -> 'a

  val filter
    : ('k, 'cmp,
       ('k, 'v, 'cmp) t
       -> f:(key:'k key -> data:'v -> bool)
       -> ('k, 'v, 'cmp) t
      ) options

  val filter_map
    : ('k, 'cmp,
       ('k, 'v1, 'cmp) t
       -> f:('v1 -> 'v2 option)
       -> ('k, 'v2, 'cmp) t
      ) options

  val filter_mapi
    : ('k, 'cmp,
       ('k, 'v1, 'cmp) t
       -> f:(key:'k key -> data:'v1 -> 'v2 option)
       -> ('k, 'v2, 'cmp) t
      ) options

  val compare_direct
    : ('k, 'cmp,
       ('v -> 'v -> int)
       -> ('k, 'v, 'cmp) t
       -> ('k, 'v, 'cmp) t
       -> int
      ) options

  val equal
    : ('k, 'cmp,
       ('v -> 'v -> bool)
       -> ('k, 'v, 'cmp) t
       -> ('k, 'v, 'cmp) t
       -> bool
      ) options

  val keys : ('k, _, _) t -> 'k key list

  val data : (_, 'v, _) t -> 'v list

  val to_alist : ('k, 'v, _) t -> ('k key * 'v) list

  val validate
    :  name:('k key -> string)
    -> 'v Validate.check
    -> ('k, 'v, _) t Validate.check

  val merge
    : ('k, 'cmp,
       ('k, 'v1, 'cmp) t
       -> ('k, 'v2, 'cmp) t
       -> f:(key:'k key
             -> [ `Left of 'v1 | `Right of 'v2 | `Both of 'v1 * 'v2 ]
             -> 'v3 option)
       -> ('k, 'v3, 'cmp) t
      ) options

  val symmetric_diff
    :  ('k, 'cmp,
        ('k, 'v, 'cmp) t
        -> ('k, 'v, 'cmp) t
        -> data_equal:('v -> 'v -> bool)
        -> ('k key * [ `Left of 'v | `Right of 'v |  `Unequal of 'v * 'v ]) list
       ) options

  val min_elt     : ('k, 'v, _) t -> ('k key * 'v) option
  val min_elt_exn : ('k, 'v, _) t ->  'k key * 'v

  val max_elt     : ('k, 'v, _) t -> ('k key * 'v) option
  val max_elt_exn : ('k, 'v, _) t ->  'k key * 'v

  val for_all : ('k, 'v, _) t -> f:('v -> bool) -> bool
  val exists  : ('k, 'v, _) t -> f:('v -> bool) -> bool

  val fold_range_inclusive
    : ('k, 'cmp,
       ('k, 'v, 'cmp) t
       -> min:'k key
       -> max:'k key
       -> init:'a
       -> f:(key:'k key -> data:'v -> 'a -> 'a)
       -> 'a
      ) options

  val range_to_alist
    : ('k, 'cmp,
       ('k, 'v, 'cmp) t -> min:'k key -> max:'k key -> ('k key * 'v) list
      ) options

  val prev_key
    : ('k, 'cmp,
       ('k, 'v, 'cmp) t -> 'k key -> ('k key * 'v) option
      ) options

  val next_key
    : ('k, 'cmp,
       ('k, 'v, 'cmp) t -> 'k key -> ('k key * 'v) option
      ) options

  val rank
    : ('k, 'cmp,
       ('k, _, 'cmp) t -> 'k key -> int option
      ) options

  val to_tree : ('k, 'v, 'cmp) t -> ('k key, 'v, 'cmp) tree
end

module type Accessors1 = sig
  type 'a t
  type 'a tree
  type key
  val invariants     : _ t -> bool
  val is_empty       : _ t -> bool
  val length         : _ t -> int
  val add            : 'a t -> key:key -> data:'a -> 'a t
  val add_multi      : 'a list t -> key:key -> data:'a -> 'a list t
  val change         : 'a t -> key -> ('a option -> 'a option) -> 'a t
  val find           : 'a t -> key -> 'a option
  val find_exn       : 'a t -> key -> 'a
  val remove         : 'a t -> key -> 'a t
  val mem            : _ t -> key -> bool
  val iter           : 'a t -> f:(key:key -> data:'a -> unit) -> unit
  val iter2
    : 'a t
    -> 'b t
    -> f:(key:key -> data:[ `Left of 'a | `Right of 'b | `Both of 'a * 'b ] -> unit)
    -> unit
  val map            : 'a t -> f:('a -> 'b) -> 'b t
  val mapi           : 'a t -> f:(key:key -> data:'a -> 'b) -> 'b t
  val fold           : 'a t -> init:'b -> f:(key:key -> data:'a -> 'b -> 'b) -> 'b
  val fold_right     : 'a t -> init:'b -> f:(key:key -> data:'a -> 'b -> 'b) -> 'b
  val filter         : 'a t -> f:(key:key -> data:'a -> bool) -> 'a t
  val filter_map     : 'a t -> f:('a -> 'b option) -> 'b t
  val filter_mapi    : 'a t -> f:(key:key -> data:'a -> 'b option) -> 'b t
  val compare_direct : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal          : ('a -> 'a -> bool)-> 'a t -> 'a t -> bool
  val keys           : _ t -> key list
  val data           : 'a t -> 'a list
  val to_alist       : 'a t -> (key * 'a) list
  val validate       : name:(key -> string) -> 'a Validate.check -> 'a t Validate.check
  val merge
    :  'a t
    -> 'b t
    -> f:(key:key -> [ `Left of 'a | `Right of 'b | `Both of 'a * 'b ] -> 'c option)
    -> 'c t
  val symmetric_diff
    :  'a t
    -> 'a t
    -> data_equal:('a -> 'a -> bool)
    -> (key * [ `Left of 'a | `Right of 'a |  `Unequal of 'a * 'a ]) list
  val min_elt        : 'a t -> (key * 'a) option
  val min_elt_exn    : 'a t -> key * 'a
  val max_elt        : 'a t -> (key * 'a) option
  val max_elt_exn    : 'a t -> key * 'a
  val for_all        : 'a t -> f:('a -> bool) -> bool
  val exists         : 'a t -> f:('a -> bool) -> bool
  val fold_range_inclusive
    :  'a t
    -> min:key
    -> max:key
    -> init:'b
    -> f:(key:key -> data:'a -> 'b -> 'b)
    -> 'b
  val range_to_alist : 'a t -> min:key -> max:key -> (key * 'a) list
  val prev_key       : 'a t -> key -> (key * 'a) option
  val next_key       : 'a t -> key -> (key * 'a) option
  val rank           : _  t -> key -> int option
  val to_tree        : 'a t -> 'a tree
end

module type Accessors2 = sig
  type ('a, 'b) t
  type ('a, 'b) tree
  val invariants     : (_, _) t -> bool
  val is_empty       : (_, _) t -> bool
  val length         : (_, _) t -> int
  val add            : ('a, 'b) t -> key:'a -> data:'b -> ('a, 'b) t
  val add_multi      : ('a, 'b list) t -> key:'a -> data:'b -> ('a, 'b list) t
  val change         : ('a, 'b) t -> 'a -> ('b option -> 'b option) -> ('a, 'b) t
  val find           : ('a, 'b) t -> 'a -> 'b option
  val find_exn       : ('a, 'b) t -> 'a -> 'b
  val remove         : ('a, 'b) t -> 'a -> ('a, 'b) t
  val mem            : ('a, 'b) t -> 'a -> bool
  val iter           : ('a, 'b) t -> f:(key:'a -> data:'b -> unit) -> unit
  val iter2
    :  ('a, 'b) t
    -> ('a, 'c) t
    -> f:(key:'a -> data:[ `Left of 'b | `Right of 'c | `Both of 'b * 'c ] -> unit)
    -> unit
  val map            : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t
  val mapi           : ('a, 'b) t -> f:(key:'a -> data:'b -> 'c) -> ('a, 'c) t
  val fold           : ('a, 'b) t -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c
  val fold_right     : ('a, 'b) t -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c
  val filter         : ('a, 'b) t -> f:(key:'a -> data:'b -> bool) -> ('a, 'b) t
  val filter_map     : ('a, 'b) t -> f:('b -> 'c option) -> ('a, 'c) t
  val filter_mapi    : ('a, 'b) t -> f:(key:'a -> data:'b -> 'c option) -> ('a, 'c) t
  val compare_direct : ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
  val equal          : ('b -> 'b -> bool)-> ('a, 'b) t -> ('a, 'b) t -> bool
  val keys           : ('a, _) t -> 'a list
  val data           : (_, 'b) t -> 'b list
  val to_alist       : ('a, 'b) t -> ('a * 'b) list
  val validate
    : name:('a -> string) -> 'b Validate.check -> ('a, 'b) t Validate.check
  val merge
    :  ('a, 'b) t
    -> ('a, 'c) t
    -> f:(key:'a -> [ `Left of 'b | `Right of 'c | `Both of 'b * 'c ] -> 'd option)
    -> ('a, 'd) t
  val symmetric_diff
    :  ('a, 'b) t
    -> ('a, 'b) t
    -> data_equal:('b -> 'b -> bool)
    -> ('a * [ `Left of 'b | `Right of 'b |  `Unequal of 'b * 'b ]) list
  val min_elt        : ('a, 'b) t -> ('a * 'b) option
  val min_elt_exn    : ('a, 'b) t -> 'a * 'b
  val max_elt        : ('a, 'b) t -> ('a * 'b) option
  val max_elt_exn    : ('a, 'b) t -> 'a * 'b
  val for_all        : (_,  'b) t -> f:('b -> bool) -> bool
  val exists         : (_,  'b) t -> f:('b -> bool) -> bool
  val fold_range_inclusive
    : ('a, 'b) t -> min:'a -> max:'a -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c
  val range_to_alist : ('a, 'b) t -> min:'a -> max:'a -> ('a * 'b) list
  val prev_key       : ('a, 'b) t -> 'a -> ('a * 'b) option
  val next_key       : ('a, 'b) t -> 'a -> ('a * 'b) option
  val rank           : ('a, _)  t -> 'a -> int option
  val to_tree        : ('a, 'b) t -> ('a, 'b) tree
end

module type Accessors3 = sig
  type ('a, 'b, 'cmp) t
  type ('a, 'b, 'cmp) tree
  val invariants     : (_, _, _) t -> bool
  val is_empty       : (_, _, _) t -> bool
  val length         : (_, _, _) t -> int
  val add            : ('a, 'b, 'cmp) t -> key:'a -> data:'b -> ('a, 'b, 'cmp) t
  val add_multi      : ('a, 'b list, 'cmp) t -> key:'a -> data:'b -> ('a, 'b list, 'cmp) t
  val change         : ('a, 'b, 'cmp) t -> 'a -> ('b option -> 'b option) -> ('a, 'b, 'cmp) t
  val find           : ('a, 'b, 'cmp) t -> 'a -> 'b option
  val find_exn       : ('a, 'b, 'cmp) t -> 'a -> 'b
  val remove         : ('a, 'b, 'cmp) t -> 'a -> ('a, 'b, 'cmp) t
  val mem            : ('a, 'b, 'cmp) t -> 'a -> bool
  val iter           : ('a, 'b, 'cmp) t -> f:(key:'a -> data:'b -> unit) -> unit
  val iter2
    :  ('a, 'b, 'cmp) t
    -> ('a, 'c, 'cmp) t
    -> f:(key:'a -> data:[ `Left of 'b | `Right of 'c | `Both of 'b * 'c ] -> unit)
    -> unit
  val map            : ('a, 'b, 'cmp) t -> f:('b -> 'c) -> ('a, 'c, 'cmp) t
  val mapi           : ('a, 'b, 'cmp) t -> f:(key:'a -> data:'b -> 'c) -> ('a, 'c, 'cmp) t
  val fold           : ('a, 'b, _) t -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c
  val fold_right     : ('a, 'b, _) t -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c
  val filter         : ('a, 'b, 'cmp) t -> f:(key:'a -> data:'b -> bool) -> ('a, 'b, 'cmp) t
  val filter_map     : ('a, 'b, 'cmp) t -> f:('b -> 'c option) -> ('a, 'c, 'cmp) t
  val filter_mapi
    : ('a, 'b, 'cmp) t -> f:(key:'a -> data:'b -> 'c option) -> ('a, 'c, 'cmp) t
  val compare_direct : ('b -> 'b -> int) -> ('a, 'b, 'cmp) t -> ('a, 'b, 'cmp) t -> int
  val equal          : ('b -> 'b -> bool)-> ('a, 'b, 'cmp) t -> ('a, 'b, 'cmp) t -> bool
  val keys           : ('a, _, _) t -> 'a list
  val data           : (_, 'b, _) t -> 'b list
  val to_alist       : ('a, 'b, _) t -> ('a * 'b) list
  val validate
    : name:('a -> string) -> 'b Validate.check -> ('a, 'b, _) t Validate.check
  val merge
    :  ('a, 'b, 'cmp) t -> ('a, 'c, 'cmp) t
    -> f:(key:'a -> [ `Left of 'b | `Right of 'c | `Both of 'b * 'c ] -> 'd option)
    -> ('a, 'd, 'cmp) t
  val symmetric_diff
    :  ('a, 'b, 'cmp) t
    -> ('a, 'b, 'cmp) t
    -> data_equal:('b -> 'b -> bool)
    -> ('a * [ `Left of 'b | `Right of 'b |  `Unequal of 'b * 'b ]) list
  val min_elt        : ('a, 'b, 'cmp) t -> ('a * 'b) option
  val min_elt_exn    : ('a, 'b, 'cmp) t -> 'a * 'b
  val max_elt        : ('a, 'b, 'cmp) t -> ('a * 'b) option
  val max_elt_exn    : ('a, 'b, 'cmp) t -> 'a * 'b
  val for_all        : (_,  'b, _)    t -> f:('b -> bool) -> bool
  val exists         : (_,  'b, _)    t -> f:('b -> bool) -> bool
  val fold_range_inclusive
    :  ('a, 'b, _) t
    -> min:'a
    -> max:'a
    -> init:'c
    -> f:(key:'a -> data:'b -> 'c -> 'c)
    -> 'c
  val range_to_alist : ('a, 'b, _)    t -> min:'a -> max:'a -> ('a * 'b) list
  val prev_key       : ('a, 'b, _)    t -> 'a -> ('a * 'b) option
  val next_key       : ('a, 'b, _)    t -> 'a -> ('a * 'b) option
  val rank           : ('a, _,  _)    t -> 'a -> int option
  val to_tree        : ('a, 'b, 'cmp) t -> ('a, 'b, 'cmp) tree
end

module type Accessors3_with_comparator = sig
  type ('a, 'b, 'cmp) t
  type ('a, 'b, 'cmp) tree
  val invariants     : comparator:('a, 'cmp) Comparator.t -> ('a, 'b, 'cmp) t -> bool
  val is_empty       : ('a, 'b, 'cmp) t -> bool
  val length         : ('a, 'b, 'cmp) t -> int
  val add
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t -> key:'a -> data:'b -> ('a, 'b, 'cmp) t
  val add_multi
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b list, 'cmp) t -> key:'a -> data:'b -> ('a, 'b list, 'cmp) t
  val change
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t -> 'a -> ('b option -> 'b option) -> ('a, 'b, 'cmp) t
  val find
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t -> 'a -> 'b option
  val find_exn
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t -> 'a -> 'b
  val remove
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t -> 'a -> ('a, 'b, 'cmp) t
  val mem
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t -> 'a -> bool
  val iter : ('a, 'b, 'cmp) t -> f:(key:'a -> data:'b -> unit) -> unit
  val iter2
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t -> ('a, 'c, 'cmp) t
    -> f:(key:'a -> data:[ `Left of 'b | `Right of 'c | `Both of 'b * 'c ]-> unit)
    -> unit
  val map            : ('a, 'b, 'cmp) t -> f:('b -> 'c) ->       ('a, 'c, 'cmp) t
  val mapi           : ('a, 'b, 'cmp) t -> f:(key:'a -> data:'b -> 'c) -> ('a, 'c, 'cmp) t
  val fold           : ('a, 'b, _) t -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c
  val fold_right     : ('a, 'b, _) t -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c
  val filter
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t -> f:(key:'a -> data:'b -> bool) -> ('a, 'b, 'cmp) t
  val filter_map
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t -> f:('b -> 'c option) -> ('a, 'c, 'cmp) t
  val filter_mapi
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t -> f:(key:'a -> data:'b -> 'c option) -> ('a, 'c, 'cmp) t
  val compare_direct
    :  comparator:('a, 'cmp) Comparator.t
    -> ('b -> 'b -> int)
    -> ('a, 'b, 'cmp) t
    -> ('a, 'b, 'cmp) t
    -> int
  val equal
    :  comparator:('a, 'cmp) Comparator.t
    -> ('b -> 'b -> bool) -> ('a, 'b, 'cmp) t -> ('a, 'b, 'cmp) t -> bool
  val keys           : ('a,  _, _) t -> 'a list
  val data           : (_ , 'b, _) t -> 'b list
  val to_alist       : ('a, 'b, _) t -> ('a * 'b) list
  val validate
    : name:('a -> string) -> 'b Validate.check -> ('a, 'b, _) t Validate.check
  val merge
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> ('a, 'c, 'cmp) t
    -> f:(key:'a -> [ `Left of 'b | `Right of 'c | `Both of 'b * 'c ] -> 'd option)
    -> ('a, 'd, 'cmp) t
  val symmetric_diff
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> ('a, 'b, 'cmp) t
    -> data_equal:('b -> 'b -> bool)
    -> ('a * [ `Left of 'b | `Right of 'b |  `Unequal of 'b * 'b ]) list
  val min_elt        : ('a, 'b, 'cmp) t -> ('a * 'b) option
  val min_elt_exn    : ('a, 'b, 'cmp) t -> 'a * 'b
  val max_elt        : ('a, 'b, 'cmp) t -> ('a * 'b) option
  val max_elt_exn    : ('a, 'b, 'cmp) t -> 'a * 'b
  val for_all        : ('a, 'b, 'cmp) t -> f:('b -> bool) -> bool
  val exists         : ('a, 'b, 'cmp) t -> f:('b -> bool) -> bool
  val fold_range_inclusive
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t
    -> min:'a -> max:'a -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c
  val range_to_alist
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t -> min:'a -> max:'a -> ('a * 'b) list
  val prev_key
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t -> 'a -> ('a * 'b) option
  val next_key
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t -> 'a -> ('a * 'b) option
  val rank
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) t -> 'a -> int option
  val to_tree : ('a, 'b, 'cmp) t -> ('a, 'b, 'cmp) tree
end

(* Consistency checks (same as in [Container]). *)
module Check_accessors (T : T3) (Tree : T3) (Key : T1) (Options : T3)
  (M : Accessors_generic
     with type ('a, 'b, 'c) options := ('a, 'b, 'c) Options.t
     with type ('a, 'b, 'c) t       := ('a, 'b, 'c) T.t
     with type ('a, 'b, 'c) tree    := ('a, 'b, 'c) Tree.t
     with type 'a key               := 'a Key.t)
  = struct end

module Check_accessors1 (M : Accessors1) =
  Check_accessors
    (struct type ('a, 'b, 'c) t = 'b M.t end)
    (struct type ('a, 'b, 'c) t = 'b M.tree end)
    (struct type 'a t           = M.key end)
    (Without_comparator)
    (M)

module Check_accessors2 (M : Accessors2) =
  Check_accessors
    (struct type ('a, 'b, 'c) t = ('a, 'b) M.t end)
    (struct type ('a, 'b, 'c) t = ('a, 'b) M.tree end)
    (struct type 'a t           = 'a end)
    (Without_comparator)
    (M)

module Check_accessors3 (M : Accessors3) =
  Check_accessors
    (struct type ('a, 'b, 'c) t = ('a, 'b, 'c) M.t end)
    (struct type ('a, 'b, 'c) t = ('a, 'b, 'c) M.tree end)
    (struct type 'a t           = 'a end)
    (Without_comparator)
    (M)

module Check_accessors3_with_comparator (M : Accessors3_with_comparator) =
  Check_accessors
    (struct type ('a, 'b, 'c) t = ('a, 'b, 'c) M.t end)
    (struct type ('a, 'b, 'c) t = ('a, 'b, 'c) M.tree end)
    (struct type 'a t           = 'a end)
    (With_comparator)
    (M)

module type Creators_generic = sig
  type ('k, 'v, 'cmp) t
  type ('k, 'v, 'cmp) tree
  type 'k key
  type ('a, 'cmp, 'z) options

  val empty : ('k, 'cmp, ('k, _, 'cmp) t) options

  val singleton : ('k, 'cmp, 'k key -> 'v -> ('k, 'v, 'cmp) t) options

  val of_sorted_array
    : ('k, 'cmp, ('k key * 'v) array -> ('k, 'v, 'cmp) t Or_error.t) options

  val of_sorted_array_unchecked
    : ('k, 'cmp, ('k key * 'v) array -> ('k, 'v, 'cmp) t) options

  val of_alist
    : ('k,
       'cmp,
       ('k key * 'v) list -> [ `Ok of ('k, 'v, 'cmp) t | `Duplicate_key of 'k key ]
      ) options

  val of_alist_exn : ('k, 'cmp, ('k key * 'v) list -> ('k, 'v, 'cmp) t) options

  val of_alist_multi : ('k, 'cmp, ('k key * 'v) list -> ('k, 'v list, 'cmp) t) options

  val of_alist_fold
    : ('k, 'cmp,
       ('k key * 'v1) list
       -> init:'v2
       -> f:('v2 -> 'v1 -> 'v2)
       -> ('k, 'v2, 'cmp) t
      ) options

  val of_tree
    : ('k, 'cmp,
       ('k key, 'v, 'cmp) tree -> ('k, 'v, 'cmp) t
      ) options
end

module type Creators1 = sig
  type 'a t
  type 'a tree
  type key
  val empty           : _ t
  val singleton       : key -> 'a -> 'a t
  val of_alist        : (key * 'a) list -> [ `Ok of 'a t | `Duplicate_key of key ]
  val of_alist_exn    : (key * 'a) list -> 'a t
  val of_alist_multi  : (key * 'a) list -> 'a list t
  val of_alist_fold   : (key * 'a) list -> init:'b -> f:('b -> 'a -> 'b) -> 'b t
  val of_sorted_array : (key * 'a) array -> 'a t Or_error.t
  val of_sorted_array_unchecked : (key * 'a) array -> 'a t
  val of_tree         : 'a tree -> 'a t
end

module type Creators2 = sig
  type ('a, 'b) t
  type ('a, 'b) tree
  val empty           : (_, _) t
  val singleton       : 'a -> 'b -> ('a, 'b) t
  val of_alist        : ('a * 'b) list -> [ `Ok of ('a, 'b) t | `Duplicate_key of 'a ]
  val of_alist_exn    : ('a * 'b) list -> ('a, 'b) t
  val of_alist_multi  : ('a * 'b) list -> ('a, 'b list) t
  val of_alist_fold   : ('a * 'b) list -> init:'c -> f:('c -> 'b -> 'c) -> ('a, 'c) t
  val of_sorted_array : ('a * 'b) array -> ('a, 'b) t Or_error.t
  val of_sorted_array_unchecked : ('a * 'b) array -> ('a, 'b) t
  val of_tree         : ('a, 'b) tree -> ('a, 'b) t
end

module type Creators3_with_comparator = sig
  type ('a, 'b, 'cmp) t
  type ('a, 'b, 'cmp) tree
  val empty     : comparator:('a, 'cmp) Comparator.t -> ('a, _, 'cmp) t
  val singleton : comparator:('a, 'cmp) Comparator.t -> 'a -> 'b -> ('a, 'b, 'cmp) t
  val of_alist
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) list -> [ `Ok of ('a, 'b, 'cmp) t | `Duplicate_key of 'a ]
  val of_alist_exn
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) list -> ('a, 'b, 'cmp) t
  val of_alist_multi
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) list -> ('a, 'b list, 'cmp) t
  val of_alist_fold
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) list -> init:'c -> f:('c -> 'b -> 'c) -> ('a, 'c, 'cmp) t
  val of_sorted_array
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) array -> ('a, 'b, 'cmp) t Or_error.t
  val of_sorted_array_unchecked
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a * 'b) array -> ('a, 'b, 'cmp) t
  val of_tree
    :  comparator:('a, 'cmp) Comparator.t
    -> ('a, 'b, 'cmp) tree -> ('a, 'b, 'cmp) t
end

module Check_creators (T : T3) (Tree : T3) (Key : T1) (Options : T3)
  (M : Creators_generic
     with type ('a, 'b, 'c) options := ('a, 'b, 'c) Options.t
     with type ('a, 'b, 'c) t       := ('a, 'b, 'c) T.t
     with type ('a, 'b, 'c) tree    := ('a, 'b, 'c) Tree.t
     with type 'a key               := 'a Key.t)
  = struct end

module Check_creators1 (M : Creators1) =
  Check_creators
    (struct type ('a, 'b, 'c) t = 'b M.t end)
    (struct type ('a, 'b, 'c) t = 'b M.tree end)
    (struct type 'a t           = M.key end)
    (Without_comparator)
    (M)

module Check_creators2 (M : Creators2) =
  Check_creators
    (struct type ('a, 'b, 'c) t = ('a, 'b) M.t end)
    (struct type ('a, 'b, 'c) t = ('a, 'b) M.tree end)
    (struct type 'a t           = 'a end)
    (Without_comparator)
    (M)

module Check_creators3_with_comparator (M : Creators3_with_comparator) =
  Check_creators
    (struct type ('a, 'b, 'c) t = ('a, 'b, 'c) M.t end)
    (struct type ('a, 'b, 'c) t = ('a, 'b, 'c) M.tree end)
    (struct type 'a t           = 'a end)
    (With_comparator)
    (M)

module type Creators_and_accessors_generic = sig
  include Creators_generic
  include Accessors_generic
    with type ('a, 'b, 'c) t       := ('a, 'b, 'c) t
    with type ('a, 'b, 'c) tree    := ('a, 'b, 'c) tree
    with type 'a key               := 'a key
    with type ('a, 'b, 'c) options := ('a, 'b, 'c) options
end

module type Creators_and_accessors1 = sig
  include Creators1
  include Accessors1
    with type 'a t    := 'a t
    with type 'a tree := 'a tree
    with type key     := key
end

module type Creators_and_accessors2 = sig
  include Creators2
  include Accessors2
    with type ('a, 'b) t    := ('a, 'b) t
    with type ('a, 'b) tree := ('a, 'b) tree
end

module type Creators_and_accessors3_with_comparator = sig
  include Creators3_with_comparator
  include Accessors3_with_comparator
    with type ('a, 'b, 'c) t    := ('a, 'b, 'c) t
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
end

module type S = sig
  type ('k, +'v, 'cmp) map
  type ('k, +'v, 'cmp) tree

  module Key : Comparator.S

  module Tree : sig
    type 'a t = (Key.t, 'a, Key.comparator) tree with sexp

    include Creators_and_accessors1
      with type 'a t    := 'a t
      with type 'a tree := 'a t
      with type key     := Key.t
  end

  type +'a t = (Key.t, 'a, Key.comparator) map with compare, sexp

  include Creators_and_accessors1
    with type 'a t    := 'a t
    with type 'a tree := 'a Tree.t
    with type key     := Key.t
end

module type S_binable = sig
  include S
  include Binable.S1 with type 'a t := 'a t
end
