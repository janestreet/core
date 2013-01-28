open Sexplib.Std
open Bin_prot.Std
include Caml.Gc

module Int = Core_int
module Sexp = Sexplib.Sexp
let sprintf = Printf.sprintf

let read_finaliser_queue, write_finaliser_queue =
  Thread_safe_queue.create' ()
;;

let maybe_start_finaliser_thread =
  let mutex = Core_mutex.create () in
  let started = ref false in
  let start_finaliser_thread () =
    ignore (Thread.create (fun () -> Fn.forever (fun () ->
      match read_finaliser_queue () with
      | None -> Thread.delay 1.0
      | Some f -> Exn.handle_uncaught ~exit:false f)) ())
  in
  (fun () ->
    if not !started then (* performance hack! *)
      Core_mutex.critical_section mutex ~f:(fun () ->
        if not !started then
          (started := true; start_finaliser_thread ())))
;;

(* Ocaml permits finalisers to be run in any thread and at any time after the object
 * becomes unreachable -- they are essentially concurrent.  This changes forces all
 * finaliser code to run sequentially and in a fixed thread. *)
let finalise f x =
  maybe_start_finaliser_thread ();
  let finaliser v = write_finaliser_queue (fun () -> f v) in
  Caml.Gc.finalise finaliser x
;;

module Stat = struct
  type pretty_float = float with bin_io, sexp
  let sexp_of_pretty_float f = Sexp.Atom (sprintf "%.2e" f)

  type t = Caml.Gc.stat = {
    minor_words : pretty_float;
    promoted_words : pretty_float;
    major_words : pretty_float;
    minor_collections : int;
    major_collections : int;
    heap_words : int;
    heap_chunks : int;
    live_words : int;
    live_blocks : int;
    free_words : int;
    free_blocks : int;
    largest_free : int;
    fragments : int;
    compactions : int;
    top_heap_words : int;
    stack_size : int
  } with bin_io, sexp
end

module Control = struct
  (* The GC parameters are given as a control record.
     Note that these parameters can also be initialised
     by setting the OCAMLRUNPARAM environment variable.
     See the documentation of ocamlrun. *)
  type t = Caml.Gc.control = {
    mutable minor_heap_size : int; (* The size (in words) of the minor heap. Changing this parameter will trigger a minor collection. Default: 32k. *)
    mutable major_heap_increment : int; (* The minimum number of words to add to the major heap when increasing it. Default: 62k. *)
    mutable space_overhead : int; (* The major GC speed is computed from this parameter. This is the memory that will be "wasted" because the GC does not immediatly collect unreachable blocks. It is expressed as a percentage of the memory used for live data. The GC will work more (use more CPU time and collect blocks more eagerly) if space_overhead is smaller. Default: 80. *)
    mutable verbose : int; (* This value controls the GC messages on standard error output. It is a sum of some of the following flags, to print messages on the corresponding events:
    * 0x001 Start of major GC cycle.
    * 0x002 Minor collection and major GC slice.
    * 0x004 Growing and shrinking of the heap.
    * 0x008 Resizing of stacks and memory manager tables.
    * 0x010 Heap compaction.
    * 0x020 Change of GC parameters.
    * 0x040 Computation of major GC slice size.
    * 0x080 Calling of finalisation functions.
    * 0x100 Bytecode executable search at start-up.
    * 0x200 Computation of compaction triggering condition. Default: 0. *)
    mutable max_overhead : int; (* Heap compaction is triggered when the estimated amount of "wasted" memory is more than max_overhead percent of the amount of live data. If max_overhead is set to 0, heap compaction is triggered at the end of each major GC cycle (this setting is intended for testing purposes only). If max_overhead >= 1000000, compaction is never triggered. Default: 500. *)
    mutable stack_limit : int; (* The maximum size of the stack (in words). This is only relevant to the byte-code runtime, as the native code runtime uses the operating system's stack. Default: 256k. *)
    mutable allocation_policy : int; (** The policy used for allocating in the heap.  Possible values are 0 and 1.  0 is the next-fit policy, which is quite fast but can result in fragmentation.  1 is the first-fit policy, which can be slower in some cases but can be better for programs with fragmentation problems.  Default: 0. *)
  } with bin_io, sexp
end

let tune__field logger ?(fmt = ("%d" : (_, _, _) format)) name arg current =
  match arg with
  | None -> current
  | Some v ->
      Option.iter logger
        ~f:(fun f -> Printf.ksprintf f "Gc.Control.%s: %(%d%) -> %(%d%)"
              name fmt current fmt v);
      v
;;

(*
  *\(.*\) -> \1 = f "\1" \1 c.\1;
*)
let tune ?logger ?minor_heap_size ?major_heap_increment ?space_overhead
    ?verbose ?max_overhead ?stack_limit ?allocation_policy () =
  let c = get () in
  let f = tune__field logger in
  set {
    minor_heap_size = f "minor_heap_size" minor_heap_size c.minor_heap_size;
    major_heap_increment = f "major_heap_increment" major_heap_increment
      c.major_heap_increment;
    space_overhead = f "space_overhead" space_overhead c.space_overhead;
    verbose = f "verbose" ~fmt:"0x%x" verbose c.verbose;
    max_overhead = f "max_overhead" max_overhead c.max_overhead;
    stack_limit = f "stack_limit" stack_limit c.stack_limit;
    allocation_policy = f "allocation_policy" allocation_policy
      c.allocation_policy
  }
;;

(* Reasonable defaults for the YEAR 2000! *)
let () =
  tune
    ~minor_heap_size:1_000_000 (* 32K words -> 1M words *)
    ~major_heap_increment:1_000_000 (* 32K words -> 1M words *)
    ~space_overhead:100 (* 80 -> 100 (because we have sooo much memory) *)
    ()
;;
