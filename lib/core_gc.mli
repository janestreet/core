(** {!Core_kernel.Core_gc}, plus finalizers. *)

open Core_kernel.Std

include module type of Gc
   with module Stat = Gc.Stat
   with module Control = Gc.Control
   with type alarm = Gc.alarm

(** The [Expert] module contains functions that novice users should not use due to their
    complexity.

    In particular, finalizers are difficult to use correctly, because they can run at any
    time, even in the middle of other code, and because unhandled exceptions in a
    finalizer can be raised at any point in other code.  This is introduces all the
    semantic complexities of multithreading, which is usually a bad idea.  It is much
    easier to use async finalizers, see {!Async_core.Async_gc.add_finalizer}, which do
    not involve multithreading, and runs user code as ordinary async jobs.

    If you do use [Core] finalizers, you should strive to make the finalization function
    perform a simple idempotent action, like setting a ref.  The same rules as for
    signal handlers apply to finalizers.  *)
module Expert : sig

  (** [add_finalizer b f] ensures that [f] runs after [b] becomes unreachable.  The OCaml
      runtime only supports finalizers on heap blocks, hence [add_finalizer] requires [b :
      _ Heap_block.t].  The runtime essentially maintains a set of finalizer pairs:

      {v
        'a Heap_block.t * ('a Heap_block.t -> unit)
      v}

      Each call to [add_finalizer] adds a new pair to the set.  It is allowed for many
      pairs to have the same heap block, the same function, or both.  Each pair is a
      distinct element of the set.

      After a garbage collection determines that a heap block [b] is unreachable, it
      removes from the set of finalizers all finalizer pairs [(b, f)] whose block is [b],
      and then and runs [f b] for all such pairs.  Thus, a finalizer registered with
      [add_finalizer] will run at most once.

      The GC will call the finalisation functions in the order of deallocation.  When
      several values become unreachable at the same time (i.e. during the same GC cycle),
      the finalisation functions will be called in the reverse order of the corresponding
      calls to [add_finalizer].  If [add_finalizer] is called in the same order as the
      values are allocated, that means each value is finalised before the values it
      depends upon.  Of course, this becomes false if additional dependencies are
      introduced by assignments.

      In a finalizer pair [(b, f)], it is a mistake for the closure of [f] to reference
      (directly or indirectly) [b] -- [f] should only access [b] via its argument.
      Referring to [b] in any other way will cause [b] to be kept alive forever, since [f]
      itself is a root of garbage collection, and can itself only be collected after the
      pair [(b, f)] is removed from the set of finalizers.

      The [f] function can use all features of OCaml, including assignments that make the
      value reachable again.  It can also loop forever (in this case, the other
      finalisation functions will be called during the execution of f).  It can call
      [add_finalizer] on [v] or other values to register other functions or even itself.
      It can raise an exception; in this case the exception will interrupt whatever the
      program was doing when the function was called.  This is very hard to think about,
      so one should take care to make [f] not raise.

      [add_finalizer_exn b f] is like [add_finalizer], but will raise if [b] is not a heap
      block. *)
  val add_finalizer     : 'a Heap_block.t -> ('a Heap_block.t -> unit) -> unit
  val add_finalizer_exn : 'a -> ('a -> unit) -> unit

  (** The runtime essentially maintains a bool ref:

      {[
        val finalizer_is_running : bool ref
      ]}

      The runtime uses this bool ref to ensure that only one finalizer is running at a
      time, by setting it to [true] when a finalizer starts and setting it to [false] when
      a finalizer finishes.  The runtime will not start running a finalizer if
      [!finalizer_is_running = true].  Calling [finalize_release] essentially does
      [finalizer_is_running := false], which allows another finalizer to start whether
      or not the current finalizer finishes. *)
  val finalize_release : unit -> unit
end
