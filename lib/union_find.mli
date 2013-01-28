(** Disjoint-sets.

    Union find is used to implement an equivalence relation on objects, where
    the equivalence relation can dynamically be coarsened by "union"ing two
    equivalence classes together.

    All of the operations are effectively (amortized) constant time.

    @see <en.wikipedia.org/wiki/Disjoint-set_data_structure> wikipedia.

    This implementation is in no way thread-safe.
*)

(** [type 'a t] is the type of objects, where each object is part of an
    equivalence class that is associated with a single value of type ['a]. *)
type 'a t

(** [create v] returns a new object in its own equivalence class that has value [v]. *)
val create : 'a -> 'a t

(** [get t] returns the value of the class of [t]. *)
val get : 'a t -> 'a

(** [set t v] sets the value of the class of [t] to [v]. *)
val set : 'a t -> 'a -> unit

(** [same_class t1 t2] returns true iff [t1] and [t2] are in the same equivalence class.
*)
val same_class : 'a t -> 'a t -> bool

(** [union t1 t2] makes the class of [t1] and the class of [t2] be the same (if they are
    already equal, then nothing changes).  The value of the combined class is the value of
    [t1] or [t2]; it is unspecified which.  After [union t1 t2], it will always be the
    case that [equals t1 t2]. *)
val union: 'a t -> 'a t -> unit
