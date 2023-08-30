open! Import

(** Memoization of OCaml functions of a single argument.

    The default caching policy is to remember everything for the lifetime of the returned
    closure, but [cache_size_bound] allows one to specify an upper bound on cache size.
    Whenever a cache entry must be forgotten in order to obey this bound, we pick the
    least-recently-used one. The functions raise exceptions if [cache_size_bound] is
    negative or zero.

    As you can tell from the type, the function that is memoized is the function of the
    first argument. To memoize a function with multiple arguments, pack them up in a
    tuple. See ../test/src/memo_argument.mlt for some examples.

    This module does not detect or prevent infinite loops (e.g., due to a recursive call
    that repeats an argument).

    The implementation is not thread-safe.
*)

(** A type definition to indicate that the expected use outputs a function *)
type ('a, 'b) fn = 'a -> 'b

(** Returns a memoized version of a function with a single argument.

    Of course, if the supplied function is recursive, only the outer calls are memoized;
    recursive calls are not. For that, see [recursive] below.
*)
val general
  :  ?hashable:'a Hashtbl.Hashable.t
  -> ?cache_size_bound:int
  -> ('a -> 'b)
  -> ('a, 'b) fn

(** [recursive] is like [general] but can be used to memoize recursive functions in such a
    way that the recursive calls are memoized as well.

    As a concrete example, consider the following definition of the Fibonacci function.

    {[ let rec fib x = if x < 2 then x else fib (x - 1) + fib (x - 2) ]}

    We can create a memoized version of this by first creating a non-recursive version of
    [fib] called [fib_nonrecursive], where the recursive knot has been untied.

    {[ let fib_nonrecursive fib x = if x < 2 then x else fib (x - 1) + fib (x - 2) ]}

    Here, rather than recursively calling itself, the function calls a function provided
    to it as an argument.

    We can now use [recursive] to retie the recursive knot, injecting memoization at that
    point.

    {[ let fib = Memo.recursive ~hashable:Int.hashable fib_nonrecursive ]}
    Note that calling [recursive ~hashable f_nonrecursive] does the partial application,
    [f_nonrecursive f], at the time it is called, so that any side-effects or expensive
    computations that happen at the partial application stage happen just once, not once
    per evaluation.

    [recursive] does not detect or prevent infinite loops, e.g., [Memo.recursive ~hashable
    (fun f x -> f x)] will just run until it overflows the stack.

    Finally, note that [recursive] keeps memory around between invocations of the produced
    function, which may not be what you want if you're only trying to optimize recursive
    calls.  You can achieve this with [recursive] by eta-expanding.

    {[ let fib x = Memo.recursive ~hashable:Int.hashable fib_nonrecursive x ]}

    Note that the above would be a mistake when using [general], since it would completely
    obviate the point of the call, but makes sense for [recursive], since it would still
    optimize recursive calls within one outer invocation.
*)
val recursive
  :  hashable:'a Hashtbl.Hashable.t
  -> ?cache_size_bound:int
  -> (('a -> 'b) -> 'a -> 'b)
  -> ('a, 'b) fn

(** efficient special case for argument type [unit] *)
val unit : (unit -> 'a) -> (unit, 'a) fn

(** Use a comparable instead of hashable type *)
val of_comparable
  :  (module Comparable.S_plain with type t = 'a)
  -> ('a -> 'b)
  -> ('a, 'b) fn
