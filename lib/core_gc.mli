(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: gc.mli,v 1.42 2005-10-25 18:34:07 doligez Exp $ *)

open Core_kernel.Std

include module type of Gc
   with module Stat = Gc.Stat
   with module Control = Gc.Control
   with type alarm = Gc.alarm

(** [add_finalizer b f] ensures that [f] runs after [b] becomes unreachable.  The
    OCaml runtime only supports finalizers on heap blocks, hence [add_finalizer] requires
    [b : _ Heap_block.t].  The runtime essentially maintains a set of finalizer pairs:

      'a Heap_block.t * ('a Heap_block.t -> unit)

    Each call to [add_finalizer] adds a new pair to the set.  It is allowed for many
    pairs to have the same heap block, the same function, or both.  Each pair is a
    distinct element of the set.

    After a garbage collection determines that a heap block [b] is unreachable, it removes
    from the set of finalizers all finalizer pairs [(b, f)] whose block is [b], and then
    and runs [f b] for all such pairs.  Thus, a finalizer registered with [add_finalizer]
    will run at most once.

    The GC will call the finalisation functions in the order of deallocation.  When
    several values become unreachable at the same time (i.e. during the same GC cycle),
    the finalisation functions will be called in the reverse order of the corresponding
    calls to [add_finalizer].  If [add_finalizer] is called in the same order as the
    values are allocated, that means each value is finalised before the values it depends
    upon.  Of course, this becomes false if additional dependencies are introduced by
    assignments.

    In a finalizer pair [(b, f)], it is a mistake for the closure of [f] to reference
    (directly or indirectly) [b] -- [f] should only access [b] via its argument.
    Referring to [b] in any other way will cause [b] to be kept alive forever, since [f]
    itself is a root of garbage collection, and can itself only be collected after the
    pair [(b, f)] is removed from the set of finalizers.

    The [f] function can use all features of O'Caml, including assignments that make the
    value reachable again.  It can also loop forever (in this case, the other finalisation
    functions will be called during the execution of f).  It can call [add_finalizer] on
    [v] or other values to register other functions or even itself.  It can raise an
    exception; in this case the exception will interrupt whatever the program was doing
    when the function was called.

    [add_finalizer_exn b f] is like [add_finalizer], but will raise if [b] is not a
    heap block. *)
val add_finalizer     : 'a Heap_block.t -> ('a Heap_block.t -> unit) -> unit
val add_finalizer_exn : 'a -> ('a -> unit) -> unit

val finalise_release : unit -> unit;;
(** A finalisation function may call [finalise_release] to tell the
    GC that it can launch the next finalisation function without waiting
    for the current one to return. *)
