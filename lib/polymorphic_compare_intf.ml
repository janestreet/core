(** Interfaces used for hiding and replacing polymorphic compare.  Including a module with
    interface [S] should hide the majority of functions that use polymorphic compare.  *)

module type Infix = sig
  type t
  val ( >= ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( =  ) : t -> t -> bool
  val ( >  ) : t -> t -> bool
  val ( <  ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

module type S = sig
  include Infix

  val equal   : t -> t -> bool
  val compare : t -> t -> int
  val min     : t -> t -> t
  val max     : t -> t -> t
end
