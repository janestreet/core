(** Imperative set-like data structure.

    There are a few differences from simple sets:

    - Duplicates are allowed.
    - It doesn't require anything (hashable, comparable) of elements in the bag.
    - Addition and removal are constant time operations.

    It is an error to modify a bag ([add], [remove], [remove_one], ...) during iteration
    ([fold], [iter], ...).  *)

open! Import

module type S = sig
  module Elt : sig
    type 'a t

    val equal : 'a t -> 'a t -> bool
    val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
    val value : 'a t -> 'a
  end

  type 'a t [@@deriving sexp]

  (** Much of a bag's interface comes from the generic {!Base.Container} module. *)
  include Container.S1 with type 'a t := 'a t

  include Invariant.S1 with type 'a t := 'a t

  (** [create ()] returns an empty bag. *)
  val create : unit -> 'a t

  (** [add t v] adds [v] to the bag [t], returning an element that can
      later be removed from the bag.  [add] runs in constant time. *)
  val add : 'a t -> 'a -> 'a Elt.t

  val add_unit : 'a t -> 'a -> unit

  (** [mem_elt t elt] returns whether or not [elt] is in [t].  It is like [mem] (included
      from [Container]), but it takes an ['a Elt.t] instead of an ['a] and runs in constant
      time instead of linear time. *)
  val mem_elt : 'a t -> 'a Elt.t -> bool

  (** [remove t elt] removes [elt] from the bag [t], raising an exception if [elt]
      is not in the bag.  [remove] runs in constant time. *)
  val remove : 'a t -> 'a Elt.t -> unit

  (** [choose t] returns some element in the bag. *)
  val choose : 'a t -> 'a Elt.t option

  (** [remove_one t] removes some element from the bag, and returns its value.
      [remove_one] runs in constant time. *)
  val remove_one : 'a t -> 'a option

  (** [clear t] removes all elements from the bag.  [clear] runs in constant time. *)
  val clear : 'a t -> unit

  (** [filter_inplace t ~f] removes all the elements from [t] that don't satisfy [f]. *)
  val filter_inplace : 'a t -> f:('a -> bool) -> unit

  (** [iter_elt t ~f] calls [f] on each element of the bag. *)
  val iter_elt : 'a t -> f:('a Elt.t -> unit) -> unit

  (** [find_elt t ~f] returns the first element in the bag satisfying [f], returning [None]
      if none is found. *)
  val find_elt : 'a t -> f:('a -> bool) -> 'a Elt.t option

  (** [until_empty t f] repeatedly removes values [v] from [t], running [f v] on each one,
      until [t] is empty.  Running [f] may add elements to [t] if it wants. *)
  val until_empty : 'a t -> ('a -> unit) -> unit

  (** [transfer ~src ~dst] moves all of the elements from [src] to [dst] in constant
      time. *)
  val transfer : src:'a t -> dst:'a t -> unit

  val of_list : 'a list -> 'a t
  val elts : 'a t -> 'a Elt.t list

  (** [unchecked_iter t ~f] behaves like [iter t ~f] except that [f] is allowed to modify
      [t]. Elements added by [f] may or may not be visited; elements removed by [f] that
      have not been visited will not be visited. It is an (undetected) error to delete the
      current element. *)
  val unchecked_iter : 'a t -> f:('a -> unit) -> unit
end

module type Bag = sig
  (** The module type of the Bag module.

      Example usage:
      {[
        module My_bag : Bag.S = Bag
      ]}

      Now [My_bag.Elt.t] can't be used with any other [Bag.t] type.
  *)
  module type S = S

  include S
end
