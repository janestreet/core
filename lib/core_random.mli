(* This is a slightly modified version of the OCaml standard library's random.mli.  We
   want Core's [Random] module to be different from OCaml's standard one:

   - We expose [Random.State.default], so that user code can easily share the default
   random state if it wants.

   - We disallow [Random.get_state], because it misleadingly makes a copy of random state.
   And it is what people naturally, albeit incorrectly, grab for when they want to use
   shared random state.

   The fact that we construct our own default random state means that code using
   Core.Std.Random and code using OCaml's Random will not share the default state. *)

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: random.mli 10457 2010-05-21 18:30:12Z doligez $ *)

(** Pseudo-random number generators (PRNG). *)

(** {6 Basic functions} *)

(** Note that all of these "basic" functions mutate a global random state. *)

(** Initialize the generator, using the argument as a seed.  The same seed will always
    yield the same sequence of numbers. *)
val init : int -> unit

(** Same as {!Random.init} but takes more data as seed. *)
val full_init : int array -> unit

(** Initialize the generator with a more-or-less random seed chosen in a system-dependent
    way. *)
val self_init : unit -> unit

(** Return 30 random bits in a nonnegative integer.  @before 3.12.0 used a different
    algorithm (affects all the following functions)
*)
val bits : unit -> int

(** [Random.int bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive).  [bound] must be greater than 0 and less than 2{^30}. *)
val int : int -> int

(** [Random.int32 bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive).  [bound] must be greater than 0. *)
val int32 : Int32.t -> Int32.t;;

(** [Random.nativeint bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive).  [bound] must be greater than 0. *)
val nativeint : Nativeint.t -> Nativeint.t;;

(** [Random.int64 bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive).  [bound] must be greater than 0. *)
val int64 : Int64.t -> Int64.t;;

(** [Random.float bound] returns a random floating-point number between 0 (inclusive) and
    [bound] (exclusive).  If [bound] is negative, the result is negative or zero.  If
    [bound] is 0, the result is 0. *)
val float : float -> float

(** [Random.bool ()] returns [true] or [false] with probability 0.5 each. *)
val bool : unit -> bool

(** {6 Advanced functions} *)

(** The functions from module [State] manipulate the current state
    of the random generator explicitely.
    This allows using one or several deterministic PRNGs,
    even in a multi-threaded program, without interference from
    other parts of the program.
*)
module State : sig
  type t

  val default : t

  (** Create a new state and initialize it with the given seed. *)
  val make : int array -> t

  (** Create a new state and initialize it with a system-dependent low-entropy seed. *)
  val make_self_init : unit -> t

  val copy : t -> t

  (** These functions are the same as the basic functions, except that they use (and
      update) the given PRNG state instead of the default one.  *)
  val bits : t -> int
  val int : t -> int -> int
  val int32 : t -> Int32.t -> Int32.t
  val nativeint : t -> Nativeint.t -> Nativeint.t
  val int64 : t -> Int64.t -> Int64.t
  val float : t -> float -> float
  val bool : t -> bool
end;;

(** OCaml's [Random.get_state] makes a copy of the default state, which is almost
    certainly not what you want.  [State.default], which is the actual default state, is
    probably what you want. *)
val get_state : unit -> [ `Consider_using_Random_State_default ]

(** Set the state of the generator used by the basic functions. *)
val set_state : State.t -> unit
