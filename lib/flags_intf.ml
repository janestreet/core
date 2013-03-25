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
  (** An entry [flag, name] in [known] means that the bit(s) in [flag] is (are) called
      [name]; i.e. if [bit_and flags flag = flag], then the bit(s) is (are) set and [name]
      will appear in [sexp_of_t flags].  [known] is only used to make [sexp_of_t]'s output
      human readable.

      The flags in the output of [sexp_of_t] will occur in the same order as they appear
      in [known].

      It is allowed to have a single flag with multiple bits set.

      It is an error if different flags intersect, and [allow_intersecting = false]. *)
  val known : (Int63.t * string) list

  (** [allow_intersecting] says whether to allow intersecting [known] flags.  It is
      common to do [allow_intersecting = false], however in some situations, e.g.
      Unix open flags, the flags intersect. *)
  val allow_intersecting : bool

  (** [should_print_error] says whether to print an error message if there is an error in
      the known flags.  It is typical to use [should_print_error = true] because
      [Flags.Make] is applied at the module level, where the exception raised isn't
      displayed nicely. *)
  val should_print_error : bool
end

module type Flags = sig
  module type Make_arg = Make_arg
  module type S = S

  (** [Flags.Make] builds a new flags module.  If there is an error in the [known] flags,
      it behaves as per [on_error].

      We expose [type t = int] in the result of [Flags.Make] so that one can easily use
      flag constants as values of the flag type without having to coerce them.  It is
      typical to hide the [t = int] in another signature [S]. *)
  module Make (M : Make_arg) : S with type t = Int63.t
end
