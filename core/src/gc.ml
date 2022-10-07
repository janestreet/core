open! Import

module Stable = struct
  module Allocation_policy = struct
    module V1 = struct
      type t =
        | Next_fit
        | First_fit
        | Best_fit
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  module Stat = struct
    [%%if ocaml_version < (4, 12, 0)]

    module V1 = struct
      type t = Caml.Gc.stat =
        { minor_words : float
        ; promoted_words : float
        ; major_words : float
        ; minor_collections : int
        ; major_collections : int
        ; heap_words : int
        ; heap_chunks : int
        ; live_words : int
        ; live_blocks : int
        ; free_words : int
        ; free_blocks : int
        ; largest_free : int
        ; fragments : int
        ; compactions : int
        ; top_heap_words : int
        ; stack_size : int
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end

    module V2 = struct
      type t =
        { minor_words : float
        ; promoted_words : float
        ; major_words : float
        ; minor_collections : int
        ; major_collections : int
        ; heap_words : int
        ; heap_chunks : int
        ; live_words : int
        ; live_blocks : int
        ; free_words : int
        ; free_blocks : int
        ; largest_free : int
        ; fragments : int
        ; compactions : int
        ; top_heap_words : int
        ; stack_size : int
        ; forced_major_collections : int
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end

    [%%else]

    module V1 = struct
      type t =
        { minor_words : float
        ; promoted_words : float
        ; major_words : float
        ; minor_collections : int
        ; major_collections : int
        ; heap_words : int
        ; heap_chunks : int
        ; live_words : int
        ; live_blocks : int
        ; free_words : int
        ; free_blocks : int
        ; largest_free : int
        ; fragments : int
        ; compactions : int
        ; top_heap_words : int
        ; stack_size : int
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end

    module V2 = struct
      type t = Caml.Gc.stat =
        { minor_words : float
        ; promoted_words : float
        ; major_words : float
        ; minor_collections : int
        ; major_collections : int
        ; heap_words : int
        ; heap_chunks : int
        ; live_words : int
        ; live_blocks : int
        ; free_words : int
        ; free_blocks : int
        ; largest_free : int
        ; fragments : int
        ; compactions : int
        ; top_heap_words : int
        ; stack_size : int
        ; forced_major_collections : int
        }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end

    [%%endif]
  end

  module Control = struct
    [%%if ocaml_version < (5, 0, 0)]

    module V1 = struct
      [@@@ocaml.warning "-3"]

      type t = Caml.Gc.control =
        { mutable minor_heap_size : int
        ; mutable major_heap_increment : int
        ; mutable space_overhead : int
        ; mutable verbose : int
        ; mutable max_overhead : int
        ; mutable stack_limit : int
        ; mutable allocation_policy : int
        ; window_size : int
        ; custom_major_ratio : int
        ; custom_minor_ratio : int
        ; custom_minor_max_size : int
        }
      [@@deriving bin_io, compare, equal, sexp]
    end

    [%% else]

    module V1 = struct
      [@@@ocaml.warning "-3"]

      type t = Caml.Gc.control =
        { minor_heap_size : int
        ; major_heap_increment : int
        ; space_overhead : int
        ; verbose : int
        ; max_overhead : int
        ; stack_limit : int
        ; allocation_policy : int
        ; window_size : int
        ; custom_major_ratio : int
        ; custom_minor_ratio : int
        ; custom_minor_max_size : int
        }
      [@@deriving bin_io, compare, equal, sexp]
    end

    [%%endif]
  end
end

include Caml.Gc

module Stat = struct
  module T = struct
    [%%if ocaml_version < (4, 12, 0)]

    type t = Caml.Gc.stat =
      { minor_words : float
      ; promoted_words : float
      ; major_words : float
      ; minor_collections : int
      ; major_collections : int
      ; heap_words : int
      ; heap_chunks : int
      ; live_words : int
      ; live_blocks : int
      ; free_words : int
      ; free_blocks : int
      ; largest_free : int
      ; fragments : int
      ; compactions : int
      ; top_heap_words : int
      ; stack_size : int
      }
    [@@deriving compare, hash, bin_io, sexp, fields]

    [%%else]

    type t = Caml.Gc.stat =
      { minor_words : float
      ; promoted_words : float
      ; major_words : float
      ; minor_collections : int
      ; major_collections : int
      ; heap_words : int
      ; heap_chunks : int
      ; live_words : int
      ; live_blocks : int
      ; free_words : int
      ; free_blocks : int
      ; largest_free : int
      ; fragments : int
      ; compactions : int
      ; top_heap_words : int
      ; stack_size : int
      ; forced_major_collections : int
      }
    [@@deriving compare, hash, sexp_of, fields]

    [%%endif]
  end

  include T
  include Comparable.Make_plain (T)

  [%%if ocaml_version < (4, 12, 0)]

  let diff after before =
    { minor_words = after.minor_words -. before.minor_words
    ; promoted_words = after.promoted_words -. before.promoted_words
    ; major_words = after.major_words -. before.major_words
    ; minor_collections = after.minor_collections - before.minor_collections
    ; major_collections = after.major_collections - before.major_collections
    ; heap_words = after.heap_words - before.heap_words
    ; heap_chunks = after.heap_chunks - before.heap_chunks
    ; live_words = after.live_words - before.live_words
    ; live_blocks = after.live_blocks - before.live_blocks
    ; free_words = after.free_words - before.free_words
    ; free_blocks = after.free_blocks - before.free_blocks
    ; largest_free = after.largest_free - before.largest_free
    ; fragments = after.fragments - before.fragments
    ; compactions = after.compactions - before.compactions
    ; top_heap_words = after.top_heap_words - before.top_heap_words
    ; stack_size = after.stack_size - before.stack_size
    }
  ;;

  [%%else]

  let diff after before =
    { minor_words = after.minor_words -. before.minor_words
    ; promoted_words = after.promoted_words -. before.promoted_words
    ; major_words = after.major_words -. before.major_words
    ; minor_collections = after.minor_collections - before.minor_collections
    ; major_collections = after.major_collections - before.major_collections
    ; heap_words = after.heap_words - before.heap_words
    ; heap_chunks = after.heap_chunks - before.heap_chunks
    ; live_words = after.live_words - before.live_words
    ; live_blocks = after.live_blocks - before.live_blocks
    ; free_words = after.free_words - before.free_words
    ; free_blocks = after.free_blocks - before.free_blocks
    ; largest_free = after.largest_free - before.largest_free
    ; fragments = after.fragments - before.fragments
    ; compactions = after.compactions - before.compactions
    ; top_heap_words = after.top_heap_words - before.top_heap_words
    ; stack_size = after.stack_size - before.stack_size
    ; forced_major_collections =
        after.forced_major_collections - before.forced_major_collections
    }
  ;;

  [%%endif]
end

module Control = struct
  [%%if ocaml_version < (5, 0, 0)]

  module T = struct
    [@@@ocaml.warning "-3"]

    type t = Caml.Gc.control =
      { mutable minor_heap_size : int
      ; mutable major_heap_increment : int
      ; mutable space_overhead : int
      ; mutable verbose : int
      ; mutable max_overhead : int
      ; mutable stack_limit : int
      ; mutable allocation_policy : int
      ; window_size : int
      ; custom_major_ratio : int
      ; custom_minor_ratio : int
      ; custom_minor_max_size : int
      }
    [@@deriving compare, sexp_of, fields]
  end

  [%% else]

  module T = struct
    [@@@ocaml.warning "-3"]

    type t = Caml.Gc.control =
      { minor_heap_size : int
      ; major_heap_increment : int
      ; space_overhead : int
      ; verbose : int
      ; max_overhead : int
      ; stack_limit : int
      ; allocation_policy : int
      ; window_size : int
      ; custom_major_ratio : int
      ; custom_minor_ratio : int
      ; custom_minor_max_size : int
      }
    [@@deriving compare, sexp_of, fields]
  end

  [%% endif]

  include T
  include Comparable.Make_plain (T)
end

module Allocation_policy = struct
  type t = Stable.Allocation_policy.V1.t =
    | Next_fit
    | First_fit
    | Best_fit
  [@@deriving compare, equal, hash, sexp_of]

  let to_int = function
    | Next_fit -> 0
    | First_fit -> 1
    | Best_fit -> 2
  ;;
end

let tune
      ?logger
      ?minor_heap_size
      ?major_heap_increment
      ?space_overhead
      ?verbose
      ?max_overhead
      ?stack_limit
      ?allocation_policy
      ?window_size
      ?custom_major_ratio
      ?custom_minor_ratio
      ?custom_minor_max_size
      ()
  =
  let old_control_params = get () in
  let f opt to_string field =
    let old_value = Field.get field old_control_params in
    match opt with
    | None -> old_value
    | Some new_value ->
      Option.iter logger ~f:(fun f ->
        Printf.ksprintf
          f
          "Gc.Control.%s: %s -> %s"
          (Field.name field)
          (to_string old_value)
          (to_string new_value));
      new_value
  in
  let allocation_policy = Option.map allocation_policy ~f:Allocation_policy.to_int in
  let new_control_params =
    Control.Fields.map
      ~minor_heap_size:(f minor_heap_size string_of_int)
      ~major_heap_increment:(f major_heap_increment string_of_int)
      ~space_overhead:(f space_overhead string_of_int)
      ~verbose:(f verbose string_of_int)
      ~max_overhead:(f max_overhead string_of_int)
      ~stack_limit:(f stack_limit string_of_int)
      ~allocation_policy:(f allocation_policy string_of_int)
      ~window_size:(f window_size string_of_int)
      ~custom_major_ratio:(f custom_major_ratio string_of_int)
      ~custom_minor_ratio:(f custom_minor_ratio string_of_int)
      ~custom_minor_max_size:(f custom_minor_max_size string_of_int)
  in
  set new_control_params
;;

let disable_compaction ?logger ~allocation_policy () =
  let allocation_policy =
    match allocation_policy with
    | `Don't_change -> None
    | `Set_to policy -> Some policy
  in
  (* The value 1_000_000, according to
     http://caml.inria.fr/pub/docs/manual-ocaml-4.02/libref/Gc.html
     will disable compactions.
  *)
  tune ?logger ?allocation_policy ~max_overhead:1_000_000 ()
;;

external minor_words : unit -> int = "core_gc_minor_words"
external major_words : unit -> int = "core_gc_major_words" [@@noalloc]
external promoted_words : unit -> int = "core_gc_promoted_words" [@@noalloc]
external minor_collections : unit -> int = "core_gc_minor_collections" [@@noalloc]
external major_collections : unit -> int = "core_gc_major_collections" [@@noalloc]
external compactions : unit -> int = "core_gc_compactions" [@@noalloc]
external major_plus_minor_words : unit -> int = "core_gc_major_plus_minor_words"
external allocated_words : unit -> int = "core_gc_allocated_words"

let zero = Sys.opaque_identity (int_of_string "0")

(* The compiler won't optimize int_of_string away so it won't
   perform constant folding below. *)
let rec keep_alive o = if zero <> 0 then keep_alive (Sys.opaque_identity o)

module For_testing = struct
  let prepare_heap_to_count_minor_allocation () =
    (* We call [minor] to empty the minor heap, so that our allocation is unlikely to
       trigger a minor gc. *)
    minor ();
    (* We allocate two words in case the [Gc.minor] finishes a major gc cycle, in which
       case it requests a minor gc to occur at the next minor allocation.  We don't want
       the subsequent minor allocation to trigger a minor GC, because there is a bug
       (https://github.com/ocaml/ocaml/issues/7798) in the OCaml runtime that double
       counts [Gc.minor_words] in that case. *)
    ignore (Sys.opaque_identity (ref (Sys.opaque_identity 1)) : int ref)
  ;;

  (* We disable inlining for this function so the GC stats and the call to [f] are never
     rearranged. *)
  let[@cold] measure_internal ~on_result f =
    let minor_words_before = minor_words () in
    let major_words_before = major_words () in
    (* We wrap [f ()] with [Sys.opaque_identity] to prevent the return value from being
       optimized away. *)
    let x = Sys.opaque_identity (f ()) in
    let minor_words_after = minor_words () in
    let major_words_after = major_words () in
    let major_words_allocated = major_words_after - major_words_before in
    let minor_words_allocated = minor_words_after - minor_words_before in
    on_result ~major_words_allocated ~minor_words_allocated x
  ;;

  let is_zero_alloc (type a) (f : unit -> a) =
    (* Instead of using [Allocation_report.measure], and matching on the result, we use
       this construction, in order to have [is_zero_alloc] not allocate itself. This
       enables [is_zero_alloc] to be used in a nested way.

       This also means we cannot call [prepare_heap_to_count_minor_allocation]. This is
       okay, since we do not need a precise count, we only need to check if the count is
       zero or not. *)
    measure_internal
      f
      ~on_result:(fun ~major_words_allocated ~minor_words_allocated value ->
        ignore (Sys.opaque_identity value : a);
        major_words_allocated == 0 && minor_words_allocated == 0)
  ;;

  module Allocation_report = struct
    type t =
      { major_words_allocated : int
      ; minor_words_allocated : int
      }
    [@@deriving sexp_of]

    let create ~major_words_allocated ~minor_words_allocated =
      { major_words_allocated; minor_words_allocated }
    ;;
  end

  let measure_allocation f =
    prepare_heap_to_count_minor_allocation ();
    measure_internal f ~on_result:(fun ~major_words_allocated ~minor_words_allocated x ->
      x, Allocation_report.create ~major_words_allocated ~minor_words_allocated)
  ;;

  module Allocation_log = struct
    type t =
      { size_in_words : int
      ; is_major : bool
      ; backtrace : string
      }
    [@@deriving sexp_of]
  end

  [%%if ocaml_version >= (4, 11, 0)]

  let measure_and_log_allocation f =
    let log : Allocation_log.t list ref = ref []
    and major_allocs = ref 0
    and minor_allocs = ref 0 in
    let on_alloc ~is_major (info : Caml.Gc.Memprof.allocation) =
      if is_major
      then major_allocs := !major_allocs + info.n_samples
      else minor_allocs := !minor_allocs + info.n_samples;
      let backtrace = Caml.Printexc.raw_backtrace_to_string info.callstack in
      (* Make backtraces easier to read by deleting everything below this function *)
      let backtrace =
        match String.substr_index backtrace ~pattern:"measure_and_log_allocation" with
        | None ->
          (* This case is possible: we may have logged allocations in another thread *)
          backtrace
        | Some p ->
          String.sub ~pos:0 ~len:p backtrace
          |> String.rstrip ~drop:(function
            | '\n' -> false
            | _ -> true)
      in
      let info : Allocation_log.t =
        { size_in_words = info.n_samples; is_major; backtrace }
      in
      log := info :: !log;
      None
    in
    let tracker =
      { Caml.Gc.Memprof.null_tracker with
        alloc_minor = on_alloc ~is_major:false
      ; alloc_major = on_alloc ~is_major:true
      }
    in
    Caml.Gc.Memprof.start ~sampling_rate:1.0 tracker;
    (* Exn.protect, manually inlined to guarantee no allocations *)
    let result =
      match f () with
      | x ->
        (* Memprof.stop does not guarantee that all memprof callbacks are run (some may be
           delayed if they happened during C code and there has been no allocation since),
           so we explictly flush them *)
        Caml.Gc.Memprof.stop ();
        x
      | exception e ->
        Caml.Gc.Memprof.stop ();
        raise e
    in
    ( result
    , Allocation_report.create
        ~major_words_allocated:!major_allocs
        ~minor_words_allocated:!minor_allocs
    , List.rev !log )
  ;;

  [%%else]

  let measure_and_log_allocation f =
    let x, report = measure_allocation f in
    x, report, []
  ;;

  [%%endif]
end

module Expert = struct
  let add_finalizer x f =
    try Caml.Gc.finalise (fun x -> Exn.handle_uncaught_and_exit (fun () -> f x)) x with
    | Invalid_argument _ ->
      (* The type of add_finalizer ensures that the only possible failure
         is due to [x] being static data. In this case, we simply drop the
         finalizer since static data would never have been collected by the
         GC anyway. *)
      ()
  ;;

  (* [add_finalizer_exn] is the same as [add_finalizer].  However, their types in
     core_gc.mli are different, and the type of [add_finalizer] guarantees that it always
     receives a heap block, which ensures that it will not raise, while
     [add_finalizer_exn] accepts any type, and so may raise. *)
  let add_finalizer_exn x f =
    try Caml.Gc.finalise (fun x -> Exn.handle_uncaught_and_exit (fun () -> f x)) x with
    | Invalid_argument _ ->
      ignore (Heap_block.create x : _ Heap_block.t option);
      (* If [Heap_block.create] succeeds then [x] is static data and so
         we can simply drop the finaliser. *)
      ()
  ;;

  let add_finalizer_last x f =
    try Caml.Gc.finalise_last (fun () -> Exn.handle_uncaught_and_exit f) x with
    | Invalid_argument _ ->
      (* The type of add_finalizer_last ensures that the only possible failure
         is due to [x] being static data. In this case, we simply drop the
         finalizer since static data would never have been collected by the
         GC anyway. *)
      ()
  ;;

  let add_finalizer_last_exn x f =
    try Caml.Gc.finalise_last (fun () -> Exn.handle_uncaught_and_exit f) x with
    | Invalid_argument _ ->
      ignore (Heap_block.create x : _ Heap_block.t option);
      (* If [Heap_block.create] succeeds then [x] is static data and so
         we can simply drop the finaliser. *)
      ()
  ;;

  let finalize_release = Caml.Gc.finalise_release

  module Alarm = struct
    type t = alarm

    let sexp_of_t _ = "<gc alarm>" |> [%sexp_of: string]
    let create f = create_alarm (fun () -> Exn.handle_uncaught_and_exit f)
    let delete = delete_alarm
  end
end
