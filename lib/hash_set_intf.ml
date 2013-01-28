module type Accessors = sig
  include Container.Generic

  val mem : 'a t -> 'a -> bool (* override [Container.Generic.mem] *)
  val copy : 'a t -> 'a t                 (* preserves the equality function *)
  val add               : 'a t -> 'a -> unit
  val strict_add        : 'a t -> 'a -> unit Or_error.t
  val strict_add_exn    : 'a t -> 'a -> unit
  val remove            : 'a t -> 'a -> unit
  val strict_remove     : 'a t -> 'a -> unit Or_error.t
  val strict_remove_exn : 'a t -> 'a -> unit
  val clear : 'a t -> unit
  val equal : 'a t -> 'a t -> bool
  val filter : 'a t -> f:('a -> bool) -> 'a t
  val diff : 'a t -> 'a t -> 'a t
  val of_hashtbl_keys : ('a, _) Core_hashtbl.t -> 'a t
  val filter_inplace : 'a t -> f:('a -> bool) -> unit
end

type ('key, 'z) create_options_without_hashable =
  ('key, 'z) Core_hashtbl_intf.create_options_without_hashable

type ('key, 'z) create_options_with_hashable_required =
  ('key, 'z) Core_hashtbl_intf.create_options_with_hashable

module type Creators = sig
  type 'a t
  type 'a elt
  type ('a, 'z) create_options

  val create  : ('a, unit        -> 'a t) create_options
  val of_list : ('a, 'a elt list -> 'a t) create_options
end

module type S = sig
  type elt
  type 'a hash_set
  type t = elt hash_set with sexp
  type 'a t_ = t
  type 'a elt_ = elt

  include Creators
    with type 'a t := 'a t_
    with type 'a elt := 'a elt_
    with type ('a, 'z) create_options := ('a, 'z) create_options_without_hashable
end

module type S_binable = sig
  include S
  include Binable.S with type t := t
end
