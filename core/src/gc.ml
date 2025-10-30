open! Import
module String = Base.String

module Stable = struct
  module Allocation_policy = struct
    module V1 = struct
      type t =
        | Next_fit
        | First_fit
        | Best_fit
      [@@deriving
        bin_io
        , compare ~localize
        , equal ~localize
        , hash
        , sexp
        , sexp_grammar
        , stable_witness]
    end
  end

  module Stat = struct
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
      [@@deriving
        bin_io
        , compare ~localize
        , equal ~localize
        , hash
        , sexp
        , sexp_grammar
        , stable_witness]
    end

    module V2 = struct
      type t = Stdlib.Gc.stat =
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
      [@@deriving
        bin_io
        , compare ~localize
        , equal ~localize
        , hash
        , sexp
        , sexp_grammar
        , stable_witness]
    end
  end

  module Control = struct
    module V1 = struct
      [@@@ocaml.warning "-3"]

      type t = Stdlib.Gc.control =
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
      [@@deriving
        bin_io, compare ~localize, equal ~localize, sexp, sexp_grammar, stable_witness]
    end
  end
end

include Stdlib.Gc

module Stat = struct
  module T = struct
    type t = Stdlib.Gc.stat =
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
    [@@deriving
      compare ~localize
      , hash
      , sexp_of
      , fields
          ~getters
          ~setters
          ~fields
          ~iterators:(create, fold, iter, map, to_list)
          ~direct_iterators:to_list]
  end

  include T

  include%template Comparable.Make_plain [@mode portable] (T)

  let combine first second ~float_f ~int_f =
    { minor_words = float_f first.minor_words second.minor_words
    ; promoted_words = float_f first.promoted_words second.promoted_words
    ; major_words = float_f first.major_words second.major_words
    ; minor_collections = int_f first.minor_collections second.minor_collections
    ; major_collections = int_f first.major_collections second.major_collections
    ; heap_words = int_f first.heap_words second.heap_words
    ; heap_chunks = int_f first.heap_chunks second.heap_chunks
    ; live_words = int_f first.live_words second.live_words
    ; live_blocks = int_f first.live_blocks second.live_blocks
    ; free_words = int_f first.free_words second.free_words
    ; free_blocks = int_f first.free_blocks second.free_blocks
    ; largest_free = int_f first.largest_free second.largest_free
    ; fragments = int_f first.fragments second.fragments
    ; compactions = int_f first.compactions second.compactions
    ; top_heap_words = int_f first.top_heap_words second.top_heap_words
    ; stack_size = int_f first.stack_size second.stack_size
    ; forced_major_collections =
        int_f first.forced_major_collections second.forced_major_collections
    }
  ;;

  let add = combine ~float_f:Float.( + ) ~int_f:Int.( + )
  let diff = combine ~float_f:Float.( - ) ~int_f:Int.( - )
end

module Control = struct
  module T = struct
    [@@@ocaml.warning "-3"]

    type t = Stdlib.Gc.control =
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
    [@@deriving
      compare ~localize
      , sexp_of
      , fields ~getters ~setters ~fields ~iterators:(map, to_list)]
  end

  include T

  include%template Comparable.Make_plain [@mode portable] (T)
end

module Allocation_policy = struct
  type t = Stable.Allocation_policy.V1.t =
    | Next_fit
    | First_fit
    | Best_fit
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of]

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
external major_plus_minor_words : unit -> int = "core_gc_major_plus_minor_words"
external allocated_words : unit -> int = "core_gc_allocated_words"
external run_memprof_callbacks : unit -> unit = "core_gc_run_memprof_callbacks"
external compactions : unit -> int = "core_gc_compactions" [@@noalloc]
external heap_words : unit -> int = "core_gc_heap_words" [@@noalloc]
external heap_chunks : unit -> int = "core_gc_heap_chunks" [@@noalloc]
external top_heap_words : unit -> int = "core_gc_top_heap_words" [@@noalloc]

let stat_size_lazy =
  Portable_lazy.from_fun (fun () ->
    Obj.reachable_words (Obj.repr (Stdlib.Gc.quick_stat () : Stat.t)))
;;

let stat_size () = Portable_lazy.force stat_size_lazy
let zero = Sys.opaque_identity (int_of_string "0")
let compact_if_not_running_test () = if not am_running_test then compact ()

(* The compiler won't optimize int_of_string away so it won't
   perform constant folding below. *)
let rec keep_alive o = if zero <> 0 then keep_alive (Sys.opaque_identity o)

module For_testing = struct
  module Allocation_report = struct
    type t =
      { major_words_allocated : int [@globalized]
      ; minor_words_allocated : int [@globalized]
      }
    [@@deriving sexp_of, globalize]

    let create ~major_words_allocated ~minor_words_allocated =
      { major_words_allocated; minor_words_allocated }
    ;;
  end

  module Allocation_log = struct
    type t =
      { size_in_words : int [@globalized]
      ; is_major : bool [@globalized]
      ; backtrace : string
      }
    [@@deriving sexp_of, globalize]
  end

  type memprof = Stdlib.Gc.Memprof.t

  [%%template
  [@@@kind.default k = base]

  type 'a globl = { g : 'a [@globalized] } [@@unboxed]

  (* We disable inlining for this function so the GC stats and the call to [f] are never
     rearranged. *)
  let[@cold] measure_internal ~on_result (f : unit -> 'a) =
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
  [@@kind k = (k, k & value)]
  ;;

  let is_zero_alloc_local (type a) (f : unit -> a) =
    (* Instead of using [Allocation_report.measure], and matching on the result, we use
       this construction, in order to have [is_zero_alloc] not allocate itself. This
       enables [is_zero_alloc] to be used in a nested way. *)
    (measure_internal [@kind k])
      f
      ~on_result:(fun ~major_words_allocated ~minor_words_allocated value ->
        ignore (Sys.opaque_identity value : a);
        major_words_allocated == 0 && minor_words_allocated == 0)
    [@nontail]
  ;;

  let is_zero_alloc f =
    (is_zero_alloc_local [@kind k]) (fun () -> { g = f () }) [@nontail]
  ;;

  let measure_allocation_local f =
    (measure_internal [@kind k & value])
      f
      ~on_result:(fun ~major_words_allocated ~minor_words_allocated x ->
        x, Allocation_report.create ~major_words_allocated ~minor_words_allocated)
  ;;

  let measure_allocation_for_runtime5_local f =
    let minor_words_before = minor_words () in
    let promoted_words_before = promoted_words () in
    let major_words_before = major_words () in
    let x = Sys.opaque_identity (f ()) in
    let minor_words_after = minor_words () in
    let promoted_words_after = promoted_words () in
    let major_words_after = major_words () in
    let major_words_allocated =
      major_words_after
      - promoted_words_after
      - (major_words_before - promoted_words_before)
    in
    let minor_words_allocated = minor_words_after - minor_words_before in
    x, Allocation_report.create ~major_words_allocated ~minor_words_allocated
  ;;

  let measure_allocation f =
    let { g }, allocation_report =
      (measure_allocation_local [@kind k]) (fun () -> { g = f () })
    in
    g, [%globalize: Allocation_report.t] allocation_report
  ;;

  let measure_and_log_allocation_local (f : unit -> 'a) =
    let log : Allocation_log.t list ref = ref []
    and major_allocs = ref 0
    and minor_allocs = ref 0 in
    let on_alloc ~is_major (info : Stdlib.Gc.Memprof.allocation) =
      if is_major
      then major_allocs := !major_allocs + info.n_samples
      else minor_allocs := !minor_allocs + info.n_samples;
      let backtrace = Backtrace.to_string info.callstack in
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
      { Stdlib.Gc.Memprof.null_tracker with
        alloc_minor = on_alloc ~is_major:false
      ; alloc_major = on_alloc ~is_major:true
      }
    in
    match Memprof.start ~sampling_rate:1.0 tracker with
    | (_ : memprof) ->
      (* Exn.protect, manually inlined to guarantee no allocations *)
      let result =
        match f () with
        | x ->
          (* Memprof.stop does not guarantee that all memprof callbacks are run (some may be
             delayed if they happened during C code and there has been no allocation since),
             so we explictly flush them *)
          run_memprof_callbacks ();
          Memprof.stop ();
          x
        | exception e ->
          run_memprof_callbacks ();
          Memprof.stop ();
          raise e |> (Never_returns.never_returns [@kind k])
      in
      ( result
      , Allocation_report.create
          ~major_words_allocated:!major_allocs
          ~minor_words_allocated:!minor_allocs
      , List.rev !log )
    | exception Failure msg ->
      if String.equal msg "Gc.memprof.start: not implemented in multicore"
      then (
        let a, b = (measure_allocation_for_runtime5_local [@kind k]) f in
        a, b, [])
      else (
        match failwith msg with
        | (_ : Nothing.t) -> .)
  ;;

  let measure_and_log_allocation f =
    let { g }, allocation_report, log =
      (measure_and_log_allocation_local [@kind k]) (fun () -> { g = f () })
    in
    ( g
    , [%globalize: Allocation_report.t] allocation_report
    , [%globalize: Allocation_log.t list] log )
  ;;

  let[@cold] require_no_allocation_local_failed here allocation_report allocation_log =
    let allocation_report = [%globalize: Allocation_report.t] allocation_report in
    let allocation_log = [%globalize: Allocation_log.t list] allocation_log in
    let here = if Source_code_position.is_dummy here then None else Some here in
    raise_s
      [%message
        "allocation detected"
          (here : (Source_code_position.t option[@sexp.option]))
          (allocation_report : Allocation_report.t)
          (allocation_log : Allocation_log.t list)]
  ;;

  let assert_no_allocation_local ?(here = Stdlib.Lexing.dummy_pos) f =
    let result, allocation_report, allocation_log =
      (measure_and_log_allocation_local [@kind k]) f
    in
    if allocation_report.major_words_allocated > 0
       || allocation_report.minor_words_allocated > 0
    then
      (require_no_allocation_local_failed [@kind k]) here allocation_report allocation_log;
    result
  ;;

  let assert_no_allocation ?(here = Stdlib.Lexing.dummy_pos) f =
    let { g } = (assert_no_allocation_local [@kind k]) ~here (fun () -> { g = f () }) in
    g
  ;;]
end

module Expert = struct
  let add_finalizer x f =
    try Stdlib.Gc.finalise (fun x -> Exn.handle_uncaught_and_exit (fun () -> f x)) x with
    | Invalid_argument _ ->
      (* The type of add_finalizer ensures that the only possible failure
         is due to [x] being static data. In this case, we simply drop the
         finalizer since static data would never have been collected by the
         GC anyway. *)
      ()
  ;;

  (* [add_finalizer_ignore] is the same as [add_finalizer].  However, their types in
     core_gc.mli are different, and the type of [add_finalizer] guarantees that it always
     receives a heap block, which ensures that it will not raise, while
     [add_finalizer_exn] accepts any type, and so may raise. *)
  let add_finalizer_ignore x f =
    try Stdlib.Gc.finalise (fun x -> Exn.handle_uncaught_and_exit (fun () -> f x)) x with
    | Invalid_argument _ -> ()
  ;;

  let add_finalizer_exn x f =
    try Stdlib.Gc.finalise (fun x -> Exn.handle_uncaught_and_exit (fun () -> f x)) x with
    | Invalid_argument _ ->
      ignore (Heap_block.create_exn x : _ Heap_block.t);
      (* If [Heap_block.create_exn] succeeds then [x] is static data and so
         we can simply drop the finaliser. *)
      ()
  ;;

  let add_finalizer_last x f =
    try Stdlib.Gc.finalise_last (fun () -> Exn.handle_uncaught_and_exit f) x with
    | Invalid_argument _ ->
      (* The type of add_finalizer_last ensures that the only possible failure
         is due to [x] being static data. In this case, we simply drop the
         finalizer since static data would never have been collected by the
         GC anyway. *)
      ()
  ;;

  let add_finalizer_last_ignore x f =
    try Stdlib.Gc.finalise_last (fun () -> Exn.handle_uncaught_and_exit f) x with
    | Invalid_argument _ -> ()
  ;;

  let add_finalizer_last_exn x f =
    try Stdlib.Gc.finalise_last (fun () -> Exn.handle_uncaught_and_exit f) x with
    | Invalid_argument _ ->
      ignore (Heap_block.create_exn x : _ Heap_block.t);
      (* If [Heap_block.create_exn] succeeds then [x] is static data and so
         we can simply drop the finaliser. *)
      ()
  ;;

  module With_leak_protection = struct
    let protect_finalizer x finalizer =
      let ephemeron = Ephemeron.K1.make x finalizer in
      fun x ->
        match Ephemeron.K1.query ephemeron x with
        | None -> assert false
        | Some finalizer -> finalizer x
    ;;

    let add_finalizer x f =
      let f = protect_finalizer x f in
      add_finalizer x f
    ;;

    let add_finalizer_exn x f =
      let f = protect_finalizer x f in
      add_finalizer_exn x f
    ;;

    let add_finalizer_ignore x f =
      let f = protect_finalizer x f in
      add_finalizer_ignore x f
    ;;
  end

  let finalize_release = Stdlib.Gc.finalise_release

  (* Sys.opaque_identity means accesses to the ref cannot be optimized out *)
  let leaked_values : Obj.t list ref = Sys.opaque_identity (ref [])
  let leak a = leaked_values := Obj.repr a :: !leaked_values

  module Alarm = struct
    type t = alarm

    let sexp_of_t _ = "<gc alarm>" |> [%sexp_of: string]
    let create f = create_alarm (fun () -> Exn.handle_uncaught_and_exit f)
    let delete = delete_alarm
  end
end
