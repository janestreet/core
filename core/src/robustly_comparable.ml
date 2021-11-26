(** This interface compares float-like objects with a small tolerance.

    For example [=.]  returns true if the floats are almost but not quite equal, and [>.]
    returns false if the floats are almost equal. The tolerance is intended to be about
    right for human-entered values like prices and seconds. *)

module type S = sig
  type t


  val ( >=. ) : t -> t -> bool
  val ( <=. ) : t -> t -> bool
  val ( =. ) : t -> t -> bool
  val ( >. ) : t -> t -> bool
  val ( <. ) : t -> t -> bool
  val ( <>. ) : t -> t -> bool


  val robustly_compare : t -> t -> int
end
