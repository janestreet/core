open! Import
include Date_cache_intf.Definitions

module Make (Time : Time) () = struct
  open struct
    module Date_and_ofday = Time.Date_and_ofday
    module Ofday = Time.Ofday
    module Span = Time.Span
    module Zone = Time.Zone
  end

  module Changed_or_unchanged = struct
    type t =
      | Changed
      | Unchanged
    [@@deriving compare ~localize, sexp_of]

    (* Optimized construction. *)
    external of_is_unchanged : is_unchanged:bool -> t = "%identity"

    let%test_unit _ =
      [%test_result: t] (of_is_unchanged ~is_unchanged:true) ~expect:Unchanged;
      [%test_result: t] (of_is_unchanged ~is_unchanged:false) ~expect:Changed
    ;;
  end

  module Locked_or_unlocked = struct
    type _ t =
      | Unlocked : [ `unlocked ] t
      | Locked : [ `locked ] t
    [@@deriving sexp_of]

    type packed = Packed : _ t -> packed [@@unboxed] [@@deriving sexp_of]

    let compare_packed = (Poly.compare : packed -> packed -> int)

    (* Optimized construction. *)
    external of_is_locked : is_locked:bool -> packed = "%identity"

    let%test_unit _ =
      [%test_result: packed] (of_is_locked ~is_locked:false) ~expect:(Packed Unlocked);
      [%test_result: packed] (of_is_locked ~is_locked:true) ~expect:(Packed Locked)
    ;;

    (* Unsafe unpacking, avoiding an existential type. *)
    external unsafe_unpack : packed -> _ t = "%identity"

    let%test_unit _ =
      [%test_result: packed]
        (Packed (unsafe_unpack (Packed Unlocked)))
        ~expect:(Packed Unlocked);
      [%test_result: packed]
        (Packed (unsafe_unpack (Packed Locked)))
        ~expect:(Packed Locked)
    ;;
  end

  (* Lock state mediates both readers and writers.

     The type encodes both a locked vs unlocked flag and a timestamp used to detect
     changes. Constructors [initial], [lock], and [unlock] create and advance the state.
     Accessor [changed_or_unchanged] compares two timestamps, and [locked_or_unlocked]
     checks if the state is locked or not.

     Writers must:
     1. wait for an unlocked state,
     2. atomically advance that unlocked state to locked,
     3. perform their (non-atomic) write,
     4. then advance the state to unlocked.

     Readers must:
     1. wait for an unlocked state,
     2. save the initial unlocked timestamp,
     3. perform their (non-atomic) read,
     4. then check the timestamp again,
     5. and discard the result if the timestamp changed. *)
  module Lock_state : sig
    type _ t [@@immediate]

    type packed = Packed : _ t -> packed
    [@@unboxed] [@@deriving compare ~localize, sexp_of]

    val initial : [ `unlocked ] t
    val lock : [ `unlocked ] t -> [ `locked ] t
    val unlock : [ `locked ] t -> [ `unlocked ] t
    val changed_or_unchanged : _ t -> since:_ t -> Changed_or_unchanged.t
    val locked_or_unlocked : 'a t -> 'a Locked_or_unlocked.t
  end = struct
    type _ t = int
    type packed = Packed : _ t -> packed [@@unboxed]

    let%template compare_packed (Packed a) (Packed b) = (Int.compare [@mode m]) a b
    [@@mode m = (local, global)]
    ;;

    let sexp_of_packed (Packed t) = Int.sexp_of_t t

    (* The representation is an integer timestamp representing the number of lock/unlock
       operations. Even timestamps are unlocked; odd timestamps are locked. *)

    let initial = 0
    let lock = Int.succ
    let unlock = Int.succ

    external unsafe_bool_of_int : int -> bool = "%identity"

    let%test_unit _ =
      let test i b =
        [%test_result: int] (Bool.to_int b) ~expect:i;
        [%test_result: bool] (unsafe_bool_of_int i) ~expect:b
      in
      test 0 false;
      test 1 true
    ;;

    let is_odd t = unsafe_bool_of_int (t land 1)
    let is_locked = is_odd

    let changed_or_unchanged t ~since =
      Changed_or_unchanged.of_is_unchanged ~is_unchanged:(t = since)
    ;;

    let[@inline] locked_or_unlocked (type a) (t : a t) =
      let packed_state = Locked_or_unlocked.of_is_locked ~is_locked:(is_locked t) in
      (Locked_or_unlocked.unsafe_unpack packed_state : a Locked_or_unlocked.t)
    ;;
  end

  module Date_cache = struct
    include struct
      type t =
        { mutable lock_state : Lock_state.packed Atomic.t
        ; mutable zone : Zone.t
        ; mutable cache_start_incl : Time.t
        ; mutable cache_until_excl : Time.t
        ; mutable effective_day_start : Time.t
        ; mutable date : Date0.t
        }
      [@@deriving fields ~getters]

      let create () =
        { lock_state = Atomic.make (Lock_state.Packed Lock_state.initial)
        ; zone = Zone.utc
        ; cache_start_incl = Time.epoch
        ; cache_until_excl = Time.epoch
        ; effective_day_start = Time.epoch
        ; date = Date0.unix_epoch
        }
      ;;

      let get_lock_state { lock_state; _ } = Atomic.get lock_state [@@inline]
      let set_lock_state { lock_state; _ } v = Atomic.set lock_state v [@@inline]

      let compare_and_set_lock_state t ~if_phys_equal_to ~replace_with =
        Atomic.compare_and_set t.lock_state ~if_phys_equal_to ~replace_with
      [@@inline]
      ;;

      let reset (t : t) =
        t.zone <- Zone.utc;
        t.cache_start_incl <- Time.epoch;
        t.cache_until_excl <- Time.epoch;
        t.effective_day_start <- Time.epoch;
        t.date <- Date0.unix_epoch
      ;;

      let is_in_cache t time ~zone =
        phys_equal zone t.zone
        && Time.( >= ) time t.cache_start_incl
        && Time.( < ) time t.cache_until_excl
      ;;

      let update t time ~zone =
        let index = Zone.index zone time in
        (* no exn because [Zone.index] always returns a valid index *)
        let offset_from_utc = Zone.index_offset_from_utc_exn zone index in
        let rel = Date_and_ofday.of_absolute time ~offset_from_utc in
        let date = Date_and_ofday.to_date rel in
        let span = Date_and_ofday.to_ofday rel |> Ofday.to_span_since_start_of_day in
        let effective_day_start =
          Time.sub (Date_and_ofday.to_absolute rel ~offset_from_utc) span
        in
        let effective_day_until = Time.add effective_day_start Span.day in
        let cache_start_incl =
          match Zone.index_has_prev_clock_shift zone index with
          | false -> effective_day_start
          | true ->
            effective_day_start
            |> Time.max (Zone.index_prev_clock_shift_time_exn zone index)
        in
        let cache_until_excl =
          match Zone.index_has_next_clock_shift zone index with
          | false -> effective_day_until
          | true ->
            effective_day_until
            |> Time.min (Zone.index_next_clock_shift_time_exn zone index)
        in
        t.zone <- zone;
        t.cache_start_incl <- cache_start_incl;
        t.cache_until_excl <- cache_until_excl;
        t.effective_day_start <- effective_day_start;
        t.date <- date
      ;;
    end
  end

  (* NOTE: The following uses an unsafe implementation but is exposed with a safe API. The
     implementation is pretty carefully optimized to perform well in the cache-hit case,
     as this is expected to be hit many times in hot-loops with log statements; safety
     could be much more easily guaranteed if we used a mutex or a single immutable record
     in an atomic, but the implementation below avoids locks and allocation in the
     cache-hit case. Our safety guarantees mostly come from [Lock_state] and its intended
     read/write protocol.

     The safety of this operation requires the specific guarantees of the OCaml memory
     model - between the two writes of [lock_state], readers and writers do actually
     race - but the race is bounded in space to only occur on the nonatomic locations
     constituting the date cache, and in time to only occur between those two writes to
     [lock_state]. *)

  (* The shared cache over which we are synchronizing. *)
  let date_cache = Date_cache.create ()

  (* Call [write date_cache]; if the call raises, we have to assume the [date_cache] is in
     an arbitrary state, so we reset it to the initial value (along with [atomic_counter])
     before re-raising the exception.

     [protect] is pulled out of [with_cache] below for performance reasons: its codegen
     is large, but it is in the cold path of the function. *)
  let[@cold] protect ~write ~date_cache ~time ~zone =
    try write ~date_cache ~time ~zone with
    | exn ->
      Date_cache.reset date_cache;
      (* Be sure to set [lock_state] back to [init] *after* resetting the
         [date_cache] so that no one tries to read the invalid [date_cache]. *)
      Date_cache.set_lock_state date_cache Lock_state.(Packed initial);
      raise exn
  ;;

  let[@inline] with_cache
    :  may_read_without_writing:(Date_cache.t -> bool) -> read:(Date_cache.t -> 'result)
    -> write:(date_cache:Date_cache.t -> time:'time -> zone:'zone -> unit)
    -> date_cache:Date_cache.t -> time:'time -> zone:'zone -> 'result
    =
    fun ~may_read_without_writing ~read ~write ~date_cache ~time ~zone ->
    let[@inline] rec loop backoff =
      let loop () = loop (Basement.Stdlib_shim.Backoff.once backoff) in
      (* Atomically read the counter to see if there are currently any writers. *)
      let (Packed original_lock_state) = Date_cache.get_lock_state date_cache in
      match Lock_state.locked_or_unlocked original_lock_state with
      | Unlocked ->
        (* While someone else may claim write access to the shared cache, we assume for
           now that no one does, and verify later that no one did so before returning the
           result. The following definition uses magic to make the compiler believe the
           above conclusion. *)
        let date_cache = Basement.Stdlib_shim.Obj.magic_uncontended date_cache in
        (match may_read_without_writing date_cache with
         | true ->
           let result = read date_cache in
           let (Packed post_read_lock_state) = Date_cache.get_lock_state date_cache in
           (* Verify that no one has claimed write access to the shared cache since we
              started reading. *)
           (match
              Lock_state.changed_or_unchanged
                post_read_lock_state
                ~since:original_lock_state
            with
            | Unchanged -> result
            | Changed -> loop ())
         | false ->
           (* Atomically increment the counter to claim write access to the shared cache. *)
           let locked_lock_state = Lock_state.lock original_lock_state in
           (match
              Date_cache.compare_and_set_lock_state
                date_cache
                ~if_phys_equal_to:(Packed original_lock_state)
                ~replace_with:(Packed locked_lock_state)
            with
            | Compare_failed ->
              (* Someone else has claimed the lock; retry. *)
              loop ()
            | Set_here ->
              (* No one else is accessing (and trusting) the shared cache, so we can have
                 uncontended access to it. *)
              let date_cache = Basement.Stdlib_shim.Obj.magic_uncontended date_cache in
              protect ~date_cache ~time ~zone ~write;
              let result = read date_cache in
              (* Increment the counter to signal that we are done writing. *)
              Date_cache.set_lock_state
                date_cache
                (Packed (Lock_state.unlock locked_lock_state));
              result))
      | Locked -> loop ()
    in
    loop Basement.Stdlib_shim.Backoff.default
  ;;

  let update_cache ~date_cache ~time ~zone = Date_cache.update date_cache time ~zone

  let get time ~zone ~f =
    with_cache
      ~may_read_without_writing:(fun [@inline] date_cache ->
        Date_cache.is_in_cache date_cache time ~zone)
      ~read:f
      ~write:update_cache
        (* Explicitly pass the following as arguments rather than allocating a closure for
         [write]. *)
      ~date_cache
      ~time
      ~zone
  ;;

  let get_date time ~zone = get time ~zone ~f:Date_cache.date
  let get_day_start time ~zone = get time ~zone ~f:Date_cache.effective_day_start

  (* Add dummy arguments to [Date_cache.reset] to make it align with [Date_cache.update].
     This enables us to avoid allocating a closure in the cases above. *)
  let reset_cache ~date_cache ~time:() ~zone:() = Date_cache.reset date_cache

  let reset () =
    with_cache
      ~may_read_without_writing:(fun [@inline] _ -> false)
      ~read:(fun [@inline] _ -> ())
      ~write:reset_cache
      ~date_cache
      ~time:()
      ~zone:()
  ;;
end
