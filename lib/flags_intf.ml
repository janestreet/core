(** [module Flags] implements Unix-style sets of flags that are represented as
    an [int] with various bits set, one bit for each flag.  E.g. [Linux_ext.Epoll.Flag].

    [Flags] defines a module type [Flags.S], the interface for a flags, and a functor
    [Flags.Make] for creating a flags implementation. *)

module Int63 = Core_int63

(** [module type S] is the interface for a set of flags.  Values of [type t] are set of
    flags, and the various functions operate on sets of flags.  There is a finite universe
    of flags (in particular 63 flags, one for each bit).

    [sexp_of_t] uses the flag names supplied to [Flags.Make] *)
module type S = sig
  type t with sexp_of

  val of_int : int -> t
  val to_int_exn : t -> int

  val equal : t -> t -> bool

  val empty : t

  val (+) : t -> t -> t       (* set union, bitwise or *)
  val (-) : t -> t -> t       (* set difference *)

  val intersect : t -> t -> t   (* bitwise and *)
  val complement : t -> t       (* bitwise not *)

  val do_intersect : t -> t -> bool
  val are_disjoint : t -> t -> bool
end

module type Make_arg = sig
  (** An entry [flag, name] in [known] means that the [flag] bit is called [name].
      [known] is only used to make [sexp_of_t]'s output human readable.

      The flags in the output of [sexp_of_t] will occur in the same order as they appear
      in [known].

      It is allowed to have a single flag with multiple bits set.

      It is an error if there are different flags with the same bit set. *)
  val known : (Int63.t * string) list
end

module type Flags = sig
  module type Make_arg = Make_arg
  module type S = S

  (** We expose [type t = int] in the result of [Flags.Make] so that one can easily use
      flag constants as values of the flag type without having to coerce them.  It is
      typical to hide the [t = int] in another signature [S]. *)
  module Make (M : Make_arg) : S with type t = Int63.t
end
