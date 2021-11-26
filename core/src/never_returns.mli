(** [never_returns] should be used as the return type of functions that don't return and
    might block forever, rather than ['a] or [_].  This forces callers of such functions
    to have a call to [never_returns] at the call site, which makes it clear to readers
    what's going on. We do not intend to use this type for functions such as [failwithf]
    that always raise an exception. *)

open! Import

type never_returns = Nothing.t [@@deriving sexp_of]

val never_returns : never_returns -> _
