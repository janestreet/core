(** estokes:

    This module implements a very low level interface to a mutable AVL tree. It is not
    intended to be used directly by casual users. It is used for implementing other data
    structures. The interface is somewhat ugly, and it's that way for a reason. The goal
    of this module is minimum memory overhead, and maximum performance.

    ***************** Points of Ugliness *****************

    * compare is passed in to every function where it is used. If you pass a different
    compare to functions on the same tree, then all bets are off as far as what it does,
    and it's all your fault. Why? Because otherwise we'd need a top level record to store
    compare, and when building a hash table, or other structure, that little t is a block
    that increases memory overhead. However, if an empty tree is just a constructor
    'Empty', then it's just a number, and uses no extra memory beyond the array bucket
    that holds it. That's the first secret of how Core_hashtbl's memory overhead isn't
    higher than INRIA's, even though it uses a tree instead of a list for buckets.

    * But you said it's mutable, why do all the 'mutators' return t. Answer, it is
    mutable, but the root node might change due to balancing. Since we have no top level
    record to hold the current root node (see point 1), you have to do it. If you fail to
    do it, and use an old root node, you're responsible for the (sure to be nasty)
    consequences.

    * What on earth is up with the ~removed argument to some functions. See point 1, since
    there is no top level node, it isn't possible to keep track of how many nodes are in
    the tree unless each mutator tells you whether or not it added or removed a node, vs
    replacing an existing one. If you intend to keep a count (as you must in a hash
    table), then you will need to pay attention to this flag.

    After all this, you're probably asking yourself whether all these hacks are worth
    it. Yes! They are! With them, we built a hash table that is faster than INRIA's (no
    small feat actually), with the same memory overhead, with sane add semantics (the add
    semantics they used were a performance hack), and with worst case log(N) insertion,
    lookup, and removal. I'd say that's worth it. But for those of you who will feel
    morally compelled to put in a CR about this interface. I challenge you to write a
    better interface, implement a hash table with it, and show that your table has better
    performance than Core_hashtbl.
*)
(* estokes: We expose [t] to allow an optimization in Hashtbl that makes iter and fold
   more than twice as fast. *)
type ('k, 'v) t =
| Empty
| Node of ('k, 'v) t * 'k * 'v * int * ('k, 'v) t
| Leaf of 'k * 'v

val empty : ('k, 'v) t

(** check invariants, raise an exception if any invariants fail *)
val invariant : ('k, 'v) t -> compare:('k -> 'k -> int) -> unit

(** adds the specified key and data to the tree destructively (previous t's are no longer
    valid) using the specified comparison function. O(log(N)) time, O(1) space. The
    returned t is the new root node of the tree, and should be used on all further calls
    to any other function in this module. The bool ref, added, will be set to true if a
    new node is added to the tree, or false if an existing node is replaced (in the case
    that the key already exists). If [replace] (default true) is true then add will
    overwrite any existing mapping for [key]. If [replace] is false, and there is an
    existing mapping for key then add has no effect. *)
val add
  :  ?replace:bool
  -> ('k, 'v) t
  -> compare:('k -> 'k -> int)
  -> added:bool ref
  -> key:'k
  -> data:'v
  -> ('k, 'v) t

(** if the specified key exists in the tree, return the corresponding value.
    O(log(N)) time and O(1) space. *)
val find : ('k, 'v) t -> compare:('k -> 'k -> int) -> 'k -> 'v option

(** return true if key is present in the tree, otherwise return false. *)
val mem : ('k, 'v) t -> compare:('k -> 'k -> int) -> 'k -> bool

(** remove key destructively from the tree if it exists, return the new root node.
    Previous root nodes are not usable anymore, do so at your peril. the removed ref will
    be set to true if a node was actually removed, or false otherwise. *)
val remove
  :  ('k, 'v) t
  -> removed:bool ref
  -> compare:('k -> 'k -> int)
  -> 'k
  -> ('k, 'v) t

(** fold over the tree *)
val fold : ('k, 'v) t -> init:'a -> f:(key:'k -> data:'v -> 'a -> 'a) -> 'a

(** iterate over the tree *)
val iter : ('k, 'v) t -> f:(key:'k -> data:'v -> unit) -> unit
