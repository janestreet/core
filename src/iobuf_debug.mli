(** {{!Core.Iobuf}[Iobuf]}s with extra controls for debugging. *)

open! Import

(** [Make] builds a module that is like [Iobuf], except that the module also has some
    controls for whether the various Iobuf functions do invariant checking and/or show
    debug messages.  Initially, the [bool ref]'s are [true].

    The performance of the functions in the module resulting from [Make] can be much worse
    than that of a plain [Iobuf], even with all the controls set to [false].
*)
module Make () : sig

  (** We use [module type of struct include Iobuf end] rather than [module type of Iobuf]
      so that the debugging functions work on normal Iobufs. *)
  include module type of struct include Iobuf end

  val check_invariant : bool ref
  val show_messages : bool ref

end
