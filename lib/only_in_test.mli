(** This module can be used to safely expose functions and values in signatures
    that can only be used in unit tests.

    Under the hood, ['a t = (unit -> 'a)] and the only thing that ever forces them
    is the [run_tests_and_exit] function below which actually runs the unit tests.

    For example, suppose in some module, [type t] is actually an [int].  You
    want to keep the type definition opaque, but use the underlying
    representation in unit tests.  You could write in the ml:

    {[let test_to_int t = Only_in_test.return t
    [let test_of_int n = Only_in_test.return n]}

    You would then expose in the mli:

    {[type t
    ... functions for use in regular programming...
    val test_to_int : t -> int Only_in_test.t
    val test_of_int : int -> t Only_in_test.t]}

    Finally, if you have specific values that you might want to use in unit
    tests, but that have top-level side-effects or take too long to compute, you
    can delay the side-effects or computation until the unit tests are run by
    writing, e.g.:

    [let (test_special_value : t Only_in_test.t) =
       Only_in_test.of_thunk (fun () ->  (factorial 100))]

    instead of

    [let (test_special_value : t Only_in_test.t) =
       Only_in_test.return (factorial 100)]
*)

type 'a t

include (Monad.S with type 'a t := 'a t)

val of_thunk : (unit -> 'a) -> 'a t

val force : 'a t -> 'a

