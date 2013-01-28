(** Memoization code -- not re-entrant! *)

(** Returns memoized version of any function with a single argument. The
    default caching policy is to remember everything for the lifetime
    of the returned closure, but one may specify an upper bound on
    cache size.  Whenever a cache entry must be forgotten in order to
    obey this bound, we pick the least-recently-used one. *)
val general : ?cache_size_bound:int -> ('a -> 'b) -> ('a -> 'b)

(** efficient special case for argument type [unit] *)
val unit : (unit -> 'a) -> (unit -> 'a)
