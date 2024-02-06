(** This is a wrapper around INRIA's standard [Gc] module. Provides memory
    management control and statistics, and finalized values. *)

open! Import

(*_
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
*)
module Stat : sig
  [%%if ocaml_version < (4, 12, 0)]

  type t =
    { minor_words : float
        (** Number of words allocated in the minor heap since
        the program was started.  This number is accurate in
        byte-code programs, but only an approximation in programs
        compiled to native code. *)
    ; promoted_words : float
        (** Number of words allocated in the minor heap that
        survived a minor collection and were moved to the major heap
        since the program was started. *)
    ; major_words : float
        (** Number of words allocated in the major heap, including
        the promoted words, since the program was started. *)
    ; minor_collections : int
        (** Number of minor collections since the program was started. *)
    ; major_collections : int
        (** Number of major collection cycles completed since the program
        was started. *)
    ; heap_words : int
        (** Total size of the major heap, in words.
        This metric is currently not available when using the OCaml 5 runtime: the field
        value is always [0]. *)
    ; heap_chunks : int
        (** Number of contiguous pieces of memory that make up the major heap.
        This metric is currently not available when using the OCaml 5 runtime: the field
        value is always [0]. *)
    ; live_words : int
        (** Number of words of live data in the major heap, including the header
        words. *)
    ; live_blocks : int (** Number of live blocks in the major heap. *)
    ; free_words : int (** Number of words in the free list. *)
    ; free_blocks : int
        (** Number of blocks in the free list.
        This metric is currently not available when using the OCaml 5 runtime: the field
        value is always [0]. *)
    ; largest_free : int
        (** Size (in words) of the largest block in the free list.
        This metric is currently not available when using the OCaml 5 runtime: the field
        value is always [0]. *)
    ; fragments : int
        (** Number of wasted words due to fragmentation.  These are
        1-words free blocks placed between two live blocks.  They
        are not available for allocation. *)
    ; compactions : int (** Number of heap compactions since the program was started. *)
    ; top_heap_words : int
        (** Maximum size reached by the major heap, in words.
        This metric is currently not available when using the OCaml 5 runtime: the field
        value is always [0]. *)
    ; stack_size : int
        (** Current size of the stack, in words.
        This metric is currently not available when using the OCaml 5 runtime: the field
        value is always [0]. *)
    }
  [@@deriving bin_io, sexp]

  [%%else]

  type t =
    { minor_words : float
        (** Number of words allocated in the minor heap since
        the program was started.  This number is accurate in
        byte-code programs, but only an approximation in programs
        compiled to native code. *)
    ; promoted_words : float
        (** Number of words allocated in the minor heap that
        survived a minor collection and were moved to the major heap
        since the program was started. *)
    ; major_words : float
        (** Number of words allocated in the major heap, including
        the promoted words, since the program was started. *)
    ; minor_collections : int
        (** Number of minor collections since the program was started. *)
    ; major_collections : int
        (** Number of major collection cycles completed since the program
        was started. *)
    ; heap_words : int (** Total size of the major heap, in words. *)
    ; heap_chunks : int
        (** Number of contiguous pieces of memory that make up the major heap. *)
    ; live_words : int
        (** Number of words of live data in the major heap, including the header
        words. *)
    ; live_blocks : int (** Number of live blocks in the major heap. *)
    ; free_words : int (** Number of words in the free list. *)
    ; free_blocks : int (** Number of blocks in the free list. *)
    ; largest_free : int (** Size (in words) of the largest block in the free list. *)
    ; fragments : int
        (** Number of wasted words due to fragmentation.  These are
        1-words free blocks placed between two live blocks.  They
        are not available for allocation. *)
    ; compactions : int (** Number of heap compactions since the program was started. *)
    ; top_heap_words : int (** Maximum size reached by the major heap, in words. *)
    ; stack_size : int (** Current size of the stack, in words. *)
    ; forced_major_collections : int
        (** Number of forced full major collection cycles completed since the program
        was started. *)
    }
  [@@deriving
    sexp_of
    , fields
        ~getters
        ~fields
        ~iterators:(create, fold, iter, map, to_list)
        ~direct_iterators:to_list]

  [%%endif]

  include Comparable.S_plain with type t := t

  (** [add first second] computes [first+second] pointwise across each field; this helps
      in aggregating statistics across processes. *)
  val add : t -> t -> t

  (** [diff after before] computes [after-before] pointwise across each field; this helps
      show the effect (on all stats) of some code block. *)
  val diff : t -> t -> t
end

type stat = Stat.t

(** The memory management counters are returned in a [stat] record.

    The total amount of memory allocated by the program since it was started
    is (in words) [minor_words + major_words - promoted_words].  Multiply by
    the word size (4 on a 32-bit machine, 8 on a 64-bit machine) to get
    the number of bytes.
*)

module Control : sig
  [%%if ocaml_version < (5, 0, 0)]

  type t =
    { mutable minor_heap_size : int
        (** The size (in words) of the minor heap.  Changing this parameter will
        trigger a minor collection.

        Default: 262144 words / 1MB (32bit) / 2MB (64bit).
    *)
    ; mutable major_heap_increment : int
        (** How much to add to the major heap when increasing it. If this
        number is less than or equal to 1000, it is a percentage of
        the current heap size (i.e. setting it to 100 will double the heap
        size at each increase). If it is more than 1000, it is a fixed
        number of words that will be added to the heap.

        Default: 15%.
    *)
    ; mutable space_overhead : int
        (** The major GC speed is computed from this parameter.
        This is the memory that will be "wasted" because the GC does not
        immediately collect unreachable blocks.  It is expressed as a
        percentage of the memory used for live data.
        The GC will work more (use more CPU time and collect
        blocks more eagerly) if [space_overhead] is smaller.

        Default: 80. *)
    ; mutable verbose : int
        (** This value controls the GC messages on standard error output.
        It is a sum of some of the following flags, to print messages
        on the corresponding events:
        - [0x001] Start of major GC cycle.
        - [0x002] Minor collection and major GC slice.
        - [0x004] Growing and shrinking of the heap.
        - [0x008] Resizing of stacks and memory manager tables.
        - [0x010] Heap compaction.
        - [0x020] Change of GC parameters.
        - [0x040] Computation of major GC slice size.
        - [0x080] Calling of finalisation functions.
        - [0x100] Bytecode executable search at start-up.
        - [0x200] Computation of compaction triggering condition.

        Default: 0. *)
    ; mutable max_overhead : int
        (** Heap compaction is triggered when the estimated amount
        of "wasted" memory is more than [max_overhead] percent of the
        amount of live data.  If [max_overhead] is set to 0, heap
        compaction is triggered at the end of each major GC cycle
        (this setting is intended for testing purposes only).
        If [max_overhead >= 1000000], compaction is never triggered.

        Default: 500. *)
    ; mutable stack_limit : int
        (** The maximum size of the stack (in words).  This is only
        relevant to the byte-code runtime, as the native code runtime
        uses the operating system's stack.

        Default: 1048576 words / 4MB (32bit) / 8MB (64bit). *)
    ; mutable allocation_policy : int
        (** The policy used for allocating in the heap.  Possible
        values are 0 and 1.  0 is the next-fit policy, which is
        quite fast but can result in fragmentation.  1 is the
        first-fit policy, which can be slower in some cases but
        can be better for programs with fragmentation problems.

        Default: 0. *)
    ; window_size : int
        (** The size of the window used by the major GC for smoothing
        out variations in its workload. This is an integer between
        1 and 50.

        Default: 1. @since 4.03.0 *)
    ; custom_major_ratio : int
        (** Target ratio of floating garbage to major heap size for
        out-of-heap memory held by custom values located in the major
        heap. The GC speed is adjusted to try to use this much memory
        for dead values that are not yet collected. Expressed as a
        percentage of major heap size. The default value keeps the
        out-of-heap floating garbage about the same size as the
        in-heap overhead.
        Note: this only applies to values allocated with
        [caml_alloc_custom_mem] (e.g. bigarrays).
        Default: 44.
        @since 4.08.0 *)
    ; custom_minor_ratio : int
        (** Bound on floating garbage for out-of-heap memory held by
        custom values in the minor heap. A minor GC is triggered when
        this much memory is held by custom values located in the minor
        heap. Expressed as a percentage of minor heap size.
        Note: this only applies to values allocated with
        [caml_alloc_custom_mem] (e.g. bigarrays).
        Default: 100.
        @since 4.08.0 *)
    ; custom_minor_max_size : int
        (** Maximum amount of out-of-heap memory for each custom value
        allocated in the minor heap. When a custom value is allocated
        on the minor heap and holds more than this many bytes, only
        this value is counted against [custom_minor_ratio] and the
        rest is directly counted against [custom_major_ratio].
        Note: this only applies to values allocated with
        [caml_alloc_custom_mem] (e.g. bigarrays).
        Default: 8192 bytes.
        @since 4.08.0 *)
    }
  [@@deriving sexp_of, fields ~iterators:to_list]

  [%%else]

  type t =
    { minor_heap_size : int
        (** The size (in words) of the minor heap.  Changing
        this parameter will trigger a minor collection.

        Under the OCaml 5 runtime: the total size of the minor heap used by the program
        can be obtained by summing the the heap sizes of the active domains.

        Default: 262144 words / 1MB (32bit) / 2MB (64bit).
    *)
    ; major_heap_increment : int
        (** How much to add to the major heap when increasing it. If this
        number is less than or equal to 1000, it is a percentage of
        the current heap size (i.e. setting it to 100 will double the heap
        size at each increase). If it is more than 1000, it is a fixed
        number of words that will be added to the heap.
        Default: 15%.
    *)
    ; space_overhead : int
        (** The major GC speed is computed from this parameter.
        This is the memory that will be "wasted" because the GC does not
        immediately collect unreachable blocks.  It is expressed as a
        percentage of the memory used for live data.
        The GC will work more (use more CPU time and collect
        blocks more eagerly) if [space_overhead] is smaller.
        Default: 80 for the OCaml 4 runtime, 120 for the OCaml 5 runtime
        (the latter subject to change). *)
    ; verbose : int
        (** This value controls the GC messages on standard error output.
        It is a sum of some of the following flags, to print messages
        on the corresponding events:
        - [0x001] Start of major GC cycle.
        - [0x002] Minor collection and major GC slice.
        - [0x004] Growing and shrinking of the heap.
        - [0x008] Resizing of stacks and memory manager tables.
        - [0x010] Heap compaction.
        - [0x020] Change of GC parameters.
        - [0x040] Computation of major GC slice size.
        - [0x080] Calling of finalisation functions.
        - [0x100] Bytecode executable search at start-up.
        - [0x200] Computation of compaction triggering condition.
        - [0x400] Output GC statistics at program exit (OCaml 5 runtime only).
          Default: 0. *)
    ; max_overhead : int
        (** Heap compaction is triggered when the estimated amount
        of "wasted" memory is more than [max_overhead] percent of the
        amount of live data.  If [max_overhead] is set to 0, heap
        compaction is triggered at the end of each major GC cycle
        (this setting is intended for testing purposes only).
        If [max_overhead >= 1000000], compaction is never triggered.
        Default: 500. *)
    ; stack_limit : int
        (** The maximum size of the stack (in words).  This is only
        relevant to the byte-code runtime, as the native code runtime
        uses the operating system's stack.
        Default: 1048576 words / 4MB (32bit) / 8MB (64bit). *)
    ; allocation_policy : int
        (** The policy used for allocating in the heap.  Possible
        values are 0 and 1.  0 is the next-fit policy, which is
        quite fast but can result in fragmentation.  1 is the
        first-fit policy, which can be slower in some cases but
        can be better for programs with fragmentation problems.
        Default: 0. *)
    ; window_size : int
        (** The size of the window used by the major GC for smoothing
        out variations in its workload. This is an integer between
        1 and 50.
        Default: 1. @since 4.03.0 *)
    ; custom_major_ratio : int
        (** Target ratio of floating garbage to major heap size for
        out-of-heap memory held by custom values located in the major
        heap. The GC speed is adjusted to try to use this much memory
        for dead values that are not yet collected. Expressed as a
        percentage of major heap size. The default value keeps the
        out-of-heap floating garbage about the same size as the
        in-heap overhead.
        Note: this only applies to values allocated with
        [caml_alloc_custom_mem] (e.g. bigarrays).
        Default: 44.
        @since 4.08.0 *)
    ; custom_minor_ratio : int
        (** Bound on floating garbage for out-of-heap memory held by
        custom values in the minor heap. A minor GC is triggered when
        this much memory is held by custom values located in the minor
        heap. Expressed as a percentage of minor heap size.
        Note: this only applies to values allocated with
        [caml_alloc_custom_mem] (e.g. bigarrays).
        Default: 100.
        @since 4.08.0 *)
    ; custom_minor_max_size : int
        (** Maximum amount of out-of-heap memory for each custom value
        allocated in the minor heap. When a custom value is allocated
        on the minor heap and holds more than this many bytes, only
        this value is counted against [custom_minor_ratio] and the
        rest is directly counted against [custom_major_ratio].
        Note: this only applies to values allocated with
        [caml_alloc_custom_mem] (e.g. bigarrays).
        Default: 8192 bytes.
        @since 4.08.0 *)
    }
  [@@deriving sexp_of, fields ~iterators:to_list]

  [%%endif]

  include Comparable.S_plain with type t := t
end

type control = Control.t

(** The GC parameters are given as a [control] record.
    Note that these parameters can also be initialised
    by setting the OCAMLRUNPARAM environment variable.
    See the documentation of ocamlrun. *)

(** Return the current values of the memory management counters in a
    [stat] record that represent the program's total memory stats.

    OCaml 4 runtime: This function examines every heap block to get the
    statistics.

    OCaml 5 runtime: This function causes a full major collection. *)
external stat : unit -> stat = "caml_gc_stat"

(** Creating a [Stat.t] can allocate on the minor heap. Return the expected number of
    words allocated. **)
val stat_size : unit -> int

(** Same as [stat] except that [live_words], [live_blocks], [free_words],
    [free_blocks], [largest_free], and [fragments] are set to 0.

    OCaml 4 runtime: This function is much faster than [stat] because it does not need to
    go through the heap.

    OCaml 5 runtime: Due to per-domain buffers it may only represent the state of the
    program's total memory usage since the last minor collection. As a result returned
    values are representing an approximation of the gc statistics.

    This function is much
    faster than [stat] because it does not need to trigger a full major collection. *)
external quick_stat : unit -> stat = "caml_gc_quick_stat"

(** Return [(minor_words, promoted_words, major_words)] (on the OCaml 5 runtime, for the
    current domain or potentially previous domains).  This function is as fast as
    [quick_stat].

    Note: on the OCaml 5 runtime, [quick_stat] and [counters] might not return the same
    value, even if there's only a single domain.
    If there's only one domain running then [counters] returns the exact number of minor
    words, promoted words and major words allocated so far.
*)
external counters : unit -> float * float * float = "caml_gc_counters"

(** On 32-bit machines the [int]s returned by the following functions may overflow. *)

(** Number of words allocated in the minor heap by this domain (or, on the OCaml
    5 runtime, potentially previous domains). This number is accurate in byte-code
    programs, but only an approximation in programs compiled to native code.

    Note that [minor_words] does not allocate, but we do not annotate it as [noalloc]
    because we want the compiler to save the value of the allocation pointer register
    (%r15 on x86-64) to the global variable [caml_young_ptr] before the C stub tries to
    read its value. *)
external minor_words : unit -> int = "core_gc_minor_words"

external major_words : unit -> int = "core_gc_major_words" [@@noalloc]
external promoted_words : unit -> int = "core_gc_promoted_words" [@@noalloc]
external minor_collections : unit -> int = "core_gc_minor_collections" [@@noalloc]
external major_collections : unit -> int = "core_gc_major_collections" [@@noalloc]
val compactions : unit -> int
val heap_words : unit -> int
val heap_chunks : unit -> int
val top_heap_words : unit -> int

(** This function returns [major_words () + minor_words ()].  It exists purely for speed
    (one call into C rather than two).  Like [major_words] and [minor_words],
    [major_plus_minor_words] avoids allocating a [stat] record or a float, and may
    overflow on 32-bit machines.

    This function is not marked [[@@noalloc]] to ensure that the allocation pointer is
    up-to-date when the minor-heap measurement is made.
*)
external major_plus_minor_words : unit -> int = "core_gc_major_plus_minor_words"

(** This function returns [major_words () - promoted_words () + minor_words ()], as fast
    as possible. As [major_plus_minor_words], we avoid allocating but cannot be marked
    [@@noalloc] yet. It may overflow in 32-bit mode.
*)
external allocated_words : unit -> int = "core_gc_allocated_words"

(** Return the current values of the GC parameters in a [control] record. *)
external get : unit -> control = "caml_gc_get"

(** [set r] changes the GC parameters according to the [control] record [r].
    The normal usage is:
    [Gc.set { (Gc.get()) with Gc.Control.verbose = 0x00d }] *)
external set : control -> unit = "caml_gc_set"

(** Trigger a minor collection. *)
external minor : unit -> unit = "caml_gc_minor"

(** Do a minor collection and a slice of major collection.  The argument
    is the size of the slice, 0 to use the automatically-computed
    slice size.  In all cases, the result is the computed slice size. *)
external major_slice : int -> int = "caml_gc_major_slice"

(** Do a minor collection and finish the current major collection cycle. *)
external major : unit -> unit = "caml_gc_major"

(** Do a minor collection, finish the current major collection cycle,
    and perform a complete new cycle.  This will collect all currently
    unreachable blocks. *)
external full_major : unit -> unit = "caml_gc_full_major"

(** Perform a full major collection and compact the heap.  Note that heap
    compaction is a lengthy operation. *)
external compact : unit -> unit = "caml_gc_compaction"

(** Print the current values of the memory management counters (in
    human-readable form) into the channel argument. *)
val print_stat : out_channel -> unit

(** Return the total number of bytes allocated since the program was
    started.  It is returned as a [float] to avoid overflow problems
    with [int] on 32-bit machines. *)
val allocated_bytes : unit -> float

(** [keep_alive a] ensures that [a] is live at the point where [keep_alive a] is called.
    It is like [ignore a], except that the compiler won't be able to simplify it and
    potentially collect [a] too soon. *)
val keep_alive : _ -> unit

(** The policy used for allocating in the heap.

    The Next_fit policy is quite fast but can result in fragmentation.

    The First_fit policy can be slower in some cases but can be better for programs with
    fragmentation problems.

    The Best_fit policy is as fast as Next_fit and has less fragmentation than First_fit.

    The default is Best_fit.
*)
module Allocation_policy : sig
  type t =
    | Next_fit
    | First_fit
    | Best_fit
  [@@deriving compare, equal, hash, sexp_of]
end

(** Adjust the specified GC parameters. *)
val tune
  :  ?logger:(string -> unit)
  -> ?minor_heap_size:int
  -> ?major_heap_increment:int
  -> ?space_overhead:int
  -> ?verbose:int
  -> ?max_overhead:int
  -> ?stack_limit:int
  -> ?allocation_policy:Allocation_policy.t
  -> ?window_size:int
  -> ?custom_major_ratio:int
  -> ?custom_minor_ratio:int
  -> ?custom_minor_max_size:int
  -> unit
  -> unit

val disable_compaction
  :  ?logger:(string -> unit)
  -> allocation_policy:[ `Don't_change | `Set_to of Allocation_policy.t ]
  -> unit
  -> unit

module For_testing : sig
  module Allocation_report : sig
    type t =
      { major_words_allocated : int
      ; minor_words_allocated : int
      }
    [@@deriving sexp_of]
  end

  (** [measure_allocation f] measures the words allocated by running [f ()] *)
  val measure_allocation : (unit -> 'a) -> 'a * Allocation_report.t

  (** Same as [measure_allocation], but for functions that return a local value. *)
  val measure_allocation_local : (unit -> 'a) -> 'a * Allocation_report.t

  module Allocation_log : sig
    type t =
      { size_in_words : int
      ; is_major : bool
      ; backtrace : string
      }
    [@@deriving sexp_of, globalize]
  end

  (** [measure_and_log_allocation f] logs each allocation that [f ()] performs, as well as
      reporting the total. (This can be slow if [f] allocates heavily).

      This function is only supported since OCaml 4.11. On prior versions, the function
      always returns an empty log. *)
  val measure_and_log_allocation
    :  (unit -> 'a)
    -> 'a * Allocation_report.t * Allocation_log.t list

  (** Same as [measure_and_log_allocation], but for functions that return a local
      value. *)
  val measure_and_log_allocation_local
    :  (unit -> 'a)
    -> 'a * Allocation_report.t * Allocation_log.t list

  (** [is_zero_alloc f] runs [f ()] and returns [true] if it does not allocate, or [false]
      otherwise. [is_zero_alloc] does not allocate. *)
  val is_zero_alloc : (unit -> _) -> bool

  (** Same as [is_zero_alloc], but for functions that return a local value. *)
  val is_zero_alloc_local : (unit -> _) -> bool

  (** [assert_no_allocation [%here] f] raises if [f] allocates. *)
  val assert_no_allocation : Source_code_position.t -> (unit -> 'a) -> 'a

  (** Same as [assert_no_allocation], but for functions that return a local value. *)
  val assert_no_allocation_local : Source_code_position.t -> (unit -> 'a) -> 'a
end

(** The [Expert] module contains functions that novice users should not use, due to their
    complexity.

    In particular, finalizers are difficult to use correctly, because they can run at any
    time, even in the middle of other code, and because unhandled exceptions in a
    finalizer can be raised at any point in other code.  This introduces all the semantic
    complexities of multithreading, which is usually a bad idea.  It is much easier to use
    async finalizers, see {!Async_kernel.Async_gc.add_finalizer}, which do not involve
    multithreading, and runs user code as ordinary async jobs.

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

      All finalizers are called with [Exn.handle_uncaught_and_exit], to prevent the
      finalizer from raising, because raising from a finalizer could raise to any
      allocation or GC point in any thread, which would be impossible to reason about.

      [add_finalizer_exn b f] is like [add_finalizer], but will raise if [b] is not a heap
      block.
  *)
  val add_finalizer : 'a Heap_block.t -> ('a Heap_block.t -> unit) -> unit

  val add_finalizer_exn : 'a -> ('a -> unit) -> unit

  (** Same as {!add_finalizer} except that the function is not called until the value has
      become unreachable for the last time.  This means that the finalization function
      does not receive the value as an argument.  Every weak pointer and ephemeron that
      contained this value as key or data is unset before running the finalization
      function. *)
  val add_finalizer_last : 'a Heap_block.t -> (unit -> unit) -> unit

  val add_finalizer_last_exn : 'a -> (unit -> unit) -> unit

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

  (** A GC alarm calls a user function at the end of each major GC cycle. *)
  module Alarm : sig
    type t [@@deriving sexp_of]

    (** [create f] arranges for [f] to be called at the end of each major GC cycle,
        starting with the current cycle or the next one.  [f] can be called in any thread,
        and so introduces all the complexity of threading.  [f] is called with
        [Exn.handle_uncaught_and_exit], to prevent it from raising, because raising could
        raise to any allocation or GC point in any thread, which would be impossible to
        reason about. *)
    val create : (unit -> unit) -> t

    (** [delete t] will stop the calls to the function associated to [t].  Calling [delete
        t] again has no effect. *)
    val delete : t -> unit
  end
end

module Stable : sig
  module Stat : sig
    [%%if ocaml_version < (4, 12, 0)]

    module V1 : sig
      type nonrec t = Stat.t
      [@@deriving bin_io, compare, equal, hash, sexp, stable_witness]
    end

    module V2 : sig
      type nonrec t [@@deriving bin_io, compare, equal, hash, sexp, stable_witness]
    end

    [%%else]

    module V1 : sig
      type nonrec t [@@deriving bin_io, compare, equal, hash, sexp, stable_witness]
    end

    module V2 : sig
      type nonrec t = Stat.t
      [@@deriving bin_io, compare, equal, hash, sexp, stable_witness]
    end

    [%%endif]
  end

  module Allocation_policy : sig
    module V1 : sig
      type nonrec t = Allocation_policy.t
      [@@deriving bin_io, compare, equal, hash, sexp, stable_witness]
    end
  end

  module Control : sig
    module V1 : sig
      type nonrec t = Control.t [@@deriving bin_io, compare, equal, sexp, stable_witness]
    end
  end
end
