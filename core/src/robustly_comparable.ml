(** This interface compares float-like objects with a small tolerance.

    For example [=.] returns true if the floats are almost but not quite equal, and [>.]
    returns false if the floats are almost equal. The tolerance is intended to be about
    right for human-entered values like prices and seconds. *)

module type S = sig
  type t

  val ( >=. ) : t @ local -> t @ local -> bool
  val ( <=. ) : t @ local -> t @ local -> bool
  val ( =. ) : t @ local -> t @ local -> bool
  val ( >. ) : t @ local -> t @ local -> bool
  val ( <. ) : t @ local -> t @ local -> bool
  val ( <>. ) : t @ local -> t @ local -> bool

  (** Warning: despite its name, this is not a comparator. It uses a fixed absolute
      tolerance, so the approximate equality it defines is non-transitive. Never pass it
      to sorting or deduplication functions, [Map] or [Set], or [Comparable.Make]. Use an
      exact comparator for ordering, and the robust operators above only for pairwise
      threshold checks. *)
  val robustly_compare : t @ local -> t @ local -> int
end
