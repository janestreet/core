(* Be sure and first read the implementation overview in timing_wheel_intf.ml.

   A timing wheel is represented as an array of "levels", where each level is an array of
   "slots".  Each slot represents a range of keys, and holds elements associated with
   those keys.  Each level is determined by two parameters: [bits], the number of key bits
   that that level is responsible for distinguishing, and [bits_per_slot], the size of the
   range of keys that correspond to a single slot in the array.  Conceptually, each level
   breaks up all possible keys into ranges of size [2^bits_per_slot].  The length of a
   level array is [2^bits], and the array is used like a circular buffer to traverse the
   ranges as the timing wheel's [min_allowed_key] increases.  A key [k], if stored in the
   level, is stored at index [(k / 2^bits_per_slot) mod 2^bits].

   The settings of the [bits] values are configurable by user code using [Level_bits],
   although there is a reasonable default setting.  Given the [bits] values, the
   [bits_per_slot] are chosen so that [bits_per_slot] at level [i] is the sum of the
   [bits] at all lower levels.  Thus, a slot's range at level [i] is as large as the
   entire range of the array at level [i - 1].

   Each level has a [min_allowed_key] and a [max_allowed_key] that determine the range of
   keys that it currently represents.  The crucial invariant of the timing wheel data
   structure is that the [min_allowed_key] at level [i] is no more than the
   [max_allowed_key + 1] of level [i - 1].  This ensures that the levels can represent all
   keys from the [min_allowed_key] of the lowest level to the [max_allowed_key] of the
   highest level.  The [increase_min_allowed_key] function is responsible for restoring
   this invariant.

   At level 0, [bits_per_slot = 0], and so the size of each slot is [1].  That is, level 0
   precisely distinguishes all the keys between its [min_allowed_key] (which is the same
   as the [min_allowed_key] of the entire timing wheel) and [max_allowed_key].  As the
   levels increase, the [min_allowed_key] increases, the [bits_per_slot] increases, and
   the range of keys stored in the level increases (dramatically).

   The idea of the implementation is similar to the hierarchical approach described in:

   {v
     Hashed and Hierarchical Timing Wheels:
     Efficient Data Structures for Implementing a Timer Facility

     Varghese & Lauck, 1996
   v}

   However, the code is completely new.
*)

open Core_kernel.Std
include Timing_wheel_intf

let sexp_of_t_style : [ `Pretty | `Internal ] ref = ref `Pretty

let sec = Time.Span.of_sec

module Level_bits = struct
  type raw = int list with sexp
  type t = raw with sexp_of

  let max_num_bits = Word_size.(num_bits word_size) - 3

  let invariant t =
    assert (not (List.is_empty t));
    assert (List.for_all t ~f:(fun i -> i > 0));
    assert (List.fold t ~init:0 ~f:(+) <= max_num_bits);
  ;;

  let create_exn ints =
    if List.is_empty ints then
      failwith "Level_bits.create_exn requires a nonempty list";
    if List.exists ints ~f:(fun bits -> bits <= 0) then
      failwiths "Level_bits.create_exn got nonpositive num bits" ints
        <:sexp_of< int list >>;
    let num_bits = List.fold ints ~init:0 ~f:(+) in
    if num_bits > max_num_bits then
      failwiths "Level_bits.create_exn got too many bits"
        (ints, `got num_bits, `max max_num_bits)
        <:sexp_of< int list * [ `got of int ] * [ `max of int ] >>;
    ints
  ;;

  let t_of_sexp sexp = create_exn (<:of_sexp< raw >> sexp)

  let num_bits t = List.fold t ~init:0 ~f:(+)

  let default word_size =
    match word_size with
    | Word_size.W64 -> [ 11; 10; 10; 10; 10; 10 ]
    | Word_size.W32 -> [ 10; 10; 9 ]
  ;;
end

module Config = struct

  module Alarm_precision =
    Validated.Make (struct
      include Time.Span
      let here = _here_
      let validate = Time.Span.validate_positive
    end)

  let alarm_precision_default = Alarm_precision.create_exn (sec 1.)
  let level_bits_default = Level_bits.default Word_size.word_size

  type t =
    { alarm_precision : Alarm_precision.t with default ( alarm_precision_default )
    ; level_bits      : Level_bits.t      with default ( level_bits_default )
    }
  with fields, sexp

  let alarm_precision t = Alarm_precision.raw t.alarm_precision

  let invariant t =
    Invariant.invariant _here_ t <:sexp_of< t >> (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~alarm_precision:ignore
        ~level_bits:(check Level_bits.invariant))
  ;;

  let create
        ?alarm_precision
        ?(level_bits = level_bits_default)
        ()
    =
    let alarm_precision =
      Option.value_map alarm_precision ~f:Alarm_precision.create_exn
        ~default:alarm_precision_default
    in
    { alarm_precision; level_bits }
  ;;

  let default = create ()

  let durations t =
    let _, durations =
      List.fold t.level_bits ~init:(alarm_precision t, [])
        ~f:(fun (interval_duration, durations) num_bits ->
          let duration =
            Time.Span.scale interval_duration (2. ** Float.of_int num_bits)
          in
          duration, duration :: durations)
    in
    List.rev durations
  ;;
end

module Priority_queue = struct

  (* Each slot in a level is a (possibly null) pointer to a circular doubly-linked list of
     elements.  We pool the elements so that we can reuse them after they are removed from
     the timing wheel (either via [remove] or [increase_min_allowed_key]).  In addition to
     storing the [key], [at], and [value] in the element, we store the [level_index] so
     that we can quickly get to the level holding an element when we [remove] it.

     We distinguish between [External_elt] and [Internal_elt], which are the same
     underneath.  We maintain the invariant that an [Internal_elt] is either [null] or a
     valid pointer.  On the other hand, [External_elt]s are returned to user code, so
     there is no guarantee of validity -- we always validate an [External_elt] before
     doing anything with it.

     It is therefore OK to use [Pool.Unsafe], because we will never attempt to access a
     slot of an invalid pointer.
  *)
  module Pool    = Pool.Unsafe
  module Pointer = Pool.Pointer

  let max_representable_key = 1 lsl Level_bits.max_num_bits - 1

  module External_elt = struct

    (* The [pool_slots] here has nothing to do with the slots in a level array.  This is
       for the slots in the pool tuple representing a level element. *)
    type 'a pool_slots =
      (int, Time.t, 'a, int,
       'a pool_slots Pointer.t,
       'a pool_slots Pointer.t
      ) Pool.Slots.t6
    with sexp_of

    type 'a t = 'a pool_slots Pointer.t with sexp_of

    let null = Pointer.null
  end

  module Internal_elt : sig
    module Pool : sig
      type 'a t with sexp_of

      include Invariant.S1 with type 'a t := 'a t

      val create : unit -> _ t
      val is_full : _ t -> bool
      val grow : ?capacity:int -> 'a t -> 'a t
    end

    type 'a t = private 'a External_elt.t with sexp_of

    val null : unit -> _ t
    val is_null : _ t -> bool
    val is_valid : 'a Pool.t -> 'a t -> bool

    (* Dealing with [External_elt]s. *)
    val external_is_valid : 'a Pool.t -> 'a External_elt.t -> bool
    val to_external : 'a t -> 'a External_elt.t
    val of_external_exn : 'a Pool.t -> 'a External_elt.t -> 'a t

    val equal : 'a t -> 'a t -> bool

    val invariant : 'a Pool.t -> ('a -> unit) -> 'a t -> unit

    (* [create] returns an element whose [next] and [prev] are [null]. *)
    val create
        :  'a Pool.t
        -> key:int
        (* [at] is used when the priority queue is used to implement a timing wheel.  If
           unused, it will be [Time.epoch]. *)
        -> at:Time.t
        -> value:'a
        -> level_index:int
        -> 'a t

    val free : 'a Pool.t -> 'a t -> unit

    (* accessors *)
    val key         : 'a Pool.t -> 'a t -> int
    val at          : 'a Pool.t -> 'a t -> Time.t
    val level_index : 'a Pool.t -> 'a t -> int
    val next        : 'a Pool.t -> 'a t -> 'a t
    val value       : 'a Pool.t -> 'a t -> 'a

    (* mutators *)
    val set_level_index : 'a Pool.t -> 'a t -> int -> unit

    (* [link pool t ~after] links [t] into the circular doubly-linked list containing
       [after], placing [t] after [after]. *)
    val link : 'a Pool.t -> 'a t -> after:'a t -> unit

    (* [link_to_self pool t] makes [t] be a singleton circular doubly-linked list. *)
    val link_to_self : 'a Pool.t -> 'a t -> unit

    (* [unlink p t] unlinks [t] from the circularly doubly-linked list that it is in.  It
       changes the pointers of [t]'s [prev] and [next] elts, but not [t]'s [prev] and
       [next] pointers.  [unlink] is meaningless if [t] is a singleton. *)
    val unlink : 'a Pool.t -> 'a t -> unit

    (* Iterators.  [iter p t ~init ~f] visits each element in the doubly-linked list
       containing [t], starting at [t], and following [next] pointers.  [length] counts by
       visiting each element in the list. *)
    val iter   : 'a Pool.t -> 'a t -> f:('a t -> unit) -> unit
    val length : 'a Pool.t -> 'a t -> int

  end = struct

    type 'a pool_slots = 'a External_elt.pool_slots with sexp_of

    type 'a t = 'a External_elt.t with sexp_of

    let null    = Pointer.null
    let is_null = Pointer.is_null

    let equal t1 t2 = Pointer.phys_equal t1 t2

    let create pool ~key ~at ~value ~level_index =
      Pool.new6 pool key at value level_index (null ()) (null ())
    ;;

    let free = Pool.free

    let key p t               = Pool.get p t Pool.Slot.t0
    let at p t                = Pool.get p t Pool.Slot.t1
    let value p t             = Pool.get p t Pool.Slot.t2
    let level_index p t       = Pool.get p t Pool.Slot.t3
    let set_level_index p t i = Pool.set p t Pool.Slot.t3 i
    let prev p t              = Pool.get p t Pool.Slot.t4
    let set_prev p t x        = Pool.set p t Pool.Slot.t4 x
    let next p t              = Pool.get p t Pool.Slot.t5
    let set_next p t x        = Pool.set p t Pool.Slot.t5 x

    let is_valid p t = Pool.pointer_is_valid p t
    let external_is_valid = is_valid

    let invariant pool invariant_a t =
      Invariant.invariant _here_ t <:sexp_of< _ t >> (fun () ->
        assert (is_valid pool t);
        invariant_a (value pool t);
        let n = next pool t in
        assert (is_null n || Pointer.phys_equal t (prev pool n));
        let p = prev pool t in
        assert (is_null p || Pointer.phys_equal t (next pool p)));
    ;;

    module Pool = struct

      type 'a t = 'a pool_slots Pool.t with sexp_of

      let invariant _invariant_a t = Pool.invariant ignore t

      let create () = Pool.create Pool.Slots.t6 ~capacity:1

      let grow = Pool.grow
      let is_full = Pool.is_full
    end

    let to_external t = t

    let of_external_exn pool t =
      if is_valid pool t
      then t
      else failwiths "Timing_wheel.Priority_queue got invalid elt" t <:sexp_of< _ t >>
    ;;

    let unlink pool t =
      set_next pool (prev pool t) (next pool t);
      set_prev pool (next pool t) (prev pool t);
    ;;

    let link_to_self pool t =
      set_next pool t t;
      set_prev pool t t;
    ;;

    let link pool t ~after =
      let n = next pool after in
      set_prev pool t after;
      set_next pool t n;
      set_next pool after t;
      set_prev pool n t;
    ;;

    let iter pool first ~f =
      let current = ref first in
      let continue = ref true in
      while !continue do
        (* We get [next] before calling [f] so that [f] can modify or [free] [!current]. *)
        let next = next pool !current in
        f !current;
        if phys_equal next first then continue := false else current := next;
      done;
    ;;

    let length pool first =
      let r = ref 0 in
      let current = ref first in
      let continue = ref true in
      while !continue do
        incr r;
        let next = next pool !current in
        if phys_equal next first then continue := false else current := next;
      done;
      !r
    ;;
  end

  module Level = struct
    (* For given level, one can break the bits into a key into three regions:

       {v
         | higher levels | this level | lower levels |
       v}

       "Lower levels" is [bits_per_slot] bits wide.  "This level" is [bits] wide. *)
    type 'a t =
      { (* The [index] in the timing wheel's array of levels where this level is. *)
        index : int;
        (* How many [bits] this level is responsible for. *)
        bits : int;
        (* [slots_mask = Array.length slots - 1].  This used to quickly mod a key by
           [Array.length slots] to determine what slot it goes in. *)
        slots_mask : int;
        (* [bits_per_slot] is how many bits each slot distinguishes, and is the sum of
           of the [bits] of all the lower levels. *)
        bits_per_slot : int;
        (* [keys_per_slot = 1 lsl bits_per_slot] *)
        keys_per_slot : int;
        (* [min_key_in_same_slot_mask = lnot (keys_per_slot - 1)]. *)
        min_key_in_same_slot_mask : int;
        (* [num_allowed_keys = keys_per_slot * 1 lsl bits] *)
        num_allowed_keys : int;
        (* [length] is the number of elts currently in this level. *)
        mutable length : int;
        (* All elements at this level have their [key] satisfy [min_allowed_key <= key <=
           max_allowed_key].  Also, [min_allowed_key] is a multiple of [keys_per_slot]. *)
        mutable min_allowed_key : int;
        mutable max_allowed_key : int;
        (* [slots] holds the (possibly null) pointers to the circular doubly-linked lists
           of elts.  [Array.length slots = 1 lsl bits]. *)
        slots : 'a Internal_elt.t array sexp_opaque;
      }
    with fields, sexp_of

    let num_slots t = Array.length t.slots

    let slot t ~key = (key lsr t.bits_per_slot) land t.slots_mask

    let next_slot t slot = (slot + 1) land t.slots_mask

    let min_key_in_same_slot t ~key = key land t.min_key_in_same_slot_mask
  end

  type 'a t =
    { mutable length : int;
      mutable pool : 'a Internal_elt.Pool.t;
      mutable min_elt : 'a Internal_elt.t;
      (* All elements in the priority queue have their key [>= elt_key_lower_bound]. *)
      mutable elt_key_lower_bound : int;
      levels : 'a Level.t array;
    }
  with fields, sexp_of

  type 'a priority_queue = 'a t

  module Elt = struct
    type 'a t = 'a External_elt.t with sexp_of

    let invariant p invariant_a t =
      Internal_elt.invariant p.pool invariant_a (Internal_elt.of_external_exn p.pool t)
    ;;

    let null = External_elt.null

    let at    p t = Internal_elt.at    p.pool (Internal_elt.of_external_exn p.pool t)
    let key   p t = Internal_elt.key   p.pool (Internal_elt.of_external_exn p.pool t)
    let value p t = Internal_elt.value p.pool (Internal_elt.of_external_exn p.pool t)
  end

  let sexp_of_t_internal = sexp_of_t

  let is_empty t = length t = 0

  let num_levels t = Array.length t.levels

  let min_allowed_key t = Level.min_allowed_key t.levels.(0)

  let max_allowed_key t =
    (* We do [min max_representable_key] because a level's [max_allowed_key] can be [>
       max_representable_key].  E.g. consider a timing wheel after one does
       [increase_min_allowed_key t ~key:max_representable_key].  Then level 0 will have
       [min_allowed_key = max_representable_key] and [max_allowed_key =
       max_representable_key + num_allowed_keys - 1].  And by the inter-level invariant,
       the higher levels will even have their [min_allowed_key] and [max_allowed_key]
       greater than [max_representable_key]. *)
    min max_representable_key (Level.max_allowed_key t.levels.(num_levels t - 1))
  ;;

  let internal_iter t ~f =
    if t.length > 0 then begin
      let pool = t.pool in
      let levels = t.levels in
      for level_index = 0 to Array.length levels - 1 do
        let level = levels.(level_index) in
        if level.Level.length > 0 then begin
          let slots = level.Level.slots in
          for slot_index = 0 to Array.length slots - 1 do
            let elt = slots.(slot_index) in
            if not (Internal_elt.is_null elt) then Internal_elt.iter pool elt ~f;
          done;
        end;
      done;
    end;
  ;;

  let iter t ~f =
    internal_iter t ~f:(f : _ Elt.t -> unit :> _ Internal_elt.t -> unit)
  ;;

  module Pretty = struct
    module Elt = struct
      type 'a t =
        { key : int;
          value : 'a;
        }
      with sexp_of
    end

    type 'a t =
      { min_allowed_key : int;
        max_allowed_key : int;
        elts : 'a Elt.t list;
      }
    with sexp_of
  end

  let pretty t =
    let pool = t.pool in
    { Pretty.
      min_allowed_key = min_allowed_key t;
      max_allowed_key = max_allowed_key t;
      elts =
        let r = ref [] in
        internal_iter t ~f:(fun elt ->
          r := { Pretty.Elt.
                 key   = Internal_elt.key   pool elt;
                 value = Internal_elt.value pool elt;
               } :: !r);
        List.rev !r;
    }
  ;;

  let sexp_of_t sexp_of_a t =
    match !sexp_of_t_style with
    | `Internal -> <:sexp_of< a t_internal >> t
    | `Pretty -> <:sexp_of< a Pretty.t >> (pretty t)
  ;;

  let invariant invariant_a t : unit =
    let module L = Level in
    let pool = t.pool in
    let level_invariant level =
      Invariant.invariant _here_ level <:sexp_of< _ Level.t >> (fun () ->
        let check f = Invariant.check_field level f in
        Level.Fields.iter
          ~index:(check (fun index -> assert (index >= 0)))
          ~bits:(check (fun bits -> assert (bits > 0)))
          ~slots_mask:(check (fun mask -> assert (mask = Level.num_slots level - 1)))
          ~bits_per_slot:(check (fun bits_per_slot -> assert (bits_per_slot >= 0)))
          ~keys_per_slot:(check (fun keys_per_slot ->
            assert (keys_per_slot = 1 lsl level.L.bits_per_slot)))
          ~min_key_in_same_slot_mask:(check (fun min_key_in_same_slot_mask ->
            assert (min_key_in_same_slot_mask = lnot (level.L.keys_per_slot - 1))))
          ~num_allowed_keys:(check (fun num_allowed_keys ->
            assert (num_allowed_keys
                    = level.L.keys_per_slot * Level.num_slots level)))
          ~length:(check (fun length ->
            assert (length
                    = Array.fold level.L.slots ~init:0 ~f:(fun n elt ->
                      if Internal_elt.is_null elt then
                        n
                      else
                        n + Internal_elt.length pool elt))))
          ~min_allowed_key:(check (fun min_allowed_key ->
            assert (min_allowed_key >= 0);
            assert (Int.rem min_allowed_key level.L.keys_per_slot = 0)))
          ~max_allowed_key:
          (check (fun max_allowed_key ->
            assert (max_allowed_key
                    = level.L.min_allowed_key + level.L.num_allowed_keys - 1)))
          ~slots:(check (fun slots ->
            Array.iter slots ~f:(fun elt ->
              if not (Internal_elt.is_null elt) then begin
                Internal_elt.invariant pool invariant_a elt;
                Internal_elt.iter pool elt ~f:(fun elt ->
                  assert (Internal_elt.key pool elt >= level.L.min_allowed_key);
                  assert (Internal_elt.key pool elt <= level.L.max_allowed_key);
                  assert (Internal_elt.key pool elt >= t.elt_key_lower_bound);
                  assert (Internal_elt.level_index pool elt = level.L.index);
                  invariant_a (Internal_elt.value pool elt));
              end))))
    in
    Invariant.invariant _here_ t <:sexp_of< _ t_internal >> (fun () ->
      let check f = Invariant.check_field t f in
      assert (min_allowed_key t >= 0);
      assert (min_allowed_key t <= max_representable_key);
      assert (max_allowed_key t >= min_allowed_key t);
      assert (max_allowed_key t <= max_representable_key);
      Fields.iter
        ~length:(check (fun length -> assert (length >= 0)))
        ~pool:(check (Internal_elt.Pool.invariant ignore))
        ~min_elt:(check (fun elt_ ->
          if not (Internal_elt.is_null elt_) then begin
            assert (Internal_elt.is_valid t.pool elt_);
            assert (t.elt_key_lower_bound = Internal_elt.key t.pool elt_);
          end))
        ~elt_key_lower_bound:(check (fun elt_key_lower_bound ->
          assert (elt_key_lower_bound >= min_allowed_key t);
          assert (elt_key_lower_bound <= max_allowed_key t);
          if not (Internal_elt.is_null t.min_elt) then
            assert (elt_key_lower_bound = Internal_elt.key t.pool t.min_elt);
        ))
        ~levels:(check (fun levels ->
          assert (num_levels t > 0);
          Array.iteri levels ~f:(fun level_index level ->
            assert (level_index = Level.index level);
            level_invariant level;
            if level_index > 0 then begin
              let prev_level = levels.(level_index - 1) in
              let module L = Level in
              assert (L.keys_per_slot level = L.num_allowed_keys prev_level);
              let bound = L.max_allowed_key prev_level + 1 in
              assert (L.min_allowed_key level <= bound);
              assert (L.min_allowed_key level + L.keys_per_slot level > bound);
            end))))
  ;;

  (* [min_elt_] returns [null] if it can't find the desired element.  We wrap it up
     afterwards to return an [option]. *)
  let min_elt_ t =
    if is_empty t
    then Internal_elt.null ()
    else if not (Internal_elt.is_null t.min_elt)
    then t.min_elt
    else begin
      let module L = Level in
      let pool = t.pool in
      let min_elt_already_found = ref (Internal_elt.null ()) in
      let min_key_already_found = ref (max_representable_key + 1) in
      let level_index = ref 0 in
      let num_levels = num_levels t in
      while !level_index < num_levels do
        let level = t.levels.(!level_index) in
        if Level.min_allowed_key level >= !min_key_already_found
        then
          (* We don't need to consider any more levels.  Quit the loop. *)
          level_index := num_levels
        else if level.L.length = 0
        then incr level_index
        else begin
          (* Look in [level]. *)
          let slots = level.L.slots in
          let slot_min_key =
            ref (Level.min_key_in_same_slot level
                   ~key:(max level.L.min_allowed_key t.elt_key_lower_bound))
          in
          let slot = ref (Level.slot level ~key:!slot_min_key) in
          (* Find the first nonempty slot with a small enough [slot_min_key]. *)
          while
            Internal_elt.is_null slots.(!slot)
            && !slot_min_key < !min_key_already_found
          do
            slot := L.next_slot level !slot;
            slot_min_key := !slot_min_key + level.L.keys_per_slot;
          done;
          let first = slots.(!slot) in
          if not (Internal_elt.is_null first) then begin
            (* Visit all of the elts in this slot and find one with minimum key. *)
            let continue = ref true in
            let current = ref first in
            while !continue do
              let current_key = Internal_elt.key pool !current in
              if current_key < !min_key_already_found then begin
                min_elt_already_found := !current;
                min_key_already_found := current_key;
              end;
              let next = Internal_elt.next pool !current in
              (* If [!level_index = 0] then all elts in this slot have the same [key],
                 i.e. [!slot_min_key].  So, we don't have to check any elements after
                 [first].  This is a useful short cut in the common case that there are
                 multiple elements in the same min slot in level 0. *)
              if phys_equal next first || !level_index = 0
              then continue := false
              else current := next;
            done;
          end;
          (* Finished looking in [level].  Move up to the next level. *)
          incr level_index;
        end;
      done;
      t.min_elt <- !min_elt_already_found;
      t.elt_key_lower_bound <- !min_key_already_found;
      t.min_elt;
    end;
  ;;

  let min_elt t =
    let elt = min_elt_ t in
    if Internal_elt.is_null elt then
      None
    else
      Some (Internal_elt.to_external elt)
  ;;

  let min_key t =
    let elt = min_elt_ t in
    if Internal_elt.is_null elt then
      None
    else
      Some (Internal_elt.key t.pool elt)
  ;;

  let add_elt t elt =
    let pool = t.pool in
    let module L = Level in
    let key = Internal_elt.key pool elt in
    assert (key >= min_allowed_key t && key <= max_allowed_key t);
    (* Find the lowest level that will hold [elt]. *)
    let level_index =
      let level_index = ref 0 in
      while key > Level.max_allowed_key t.levels.(!level_index) do
        incr level_index;
      done;
      !level_index
    in
    let level = t.levels.(level_index) in
    assert (key >= level.L.min_allowed_key && key <= level.L.max_allowed_key);
    level.L.length <- level.L.length + 1;
    Internal_elt.set_level_index pool elt level_index;
    let slot = Level.slot level ~key in
    let slots = level.L.slots in
    let first = slots.(slot) in
    if not (Internal_elt.is_null first) then
      Internal_elt.link pool elt ~after:first
    else begin
      slots.(slot) <- elt;
      Internal_elt.link_to_self pool elt;
    end
  ;;

  let internal_add t ~key ~at value =
    if key < min_allowed_key t || key > max_allowed_key t then
      failwiths "Timing_wheel.add got invalid key" (key, t) <:sexp_of< int * _ t >>;
    if Internal_elt.Pool.is_full t.pool then t.pool <- Internal_elt.Pool.grow t.pool;
    let elt = Internal_elt.create t.pool ~key ~at ~value ~level_index:(-1) in
    if key < t.elt_key_lower_bound then begin
      t.min_elt <- elt;
      t.elt_key_lower_bound <- key;
    end;
    add_elt t elt;
    t.length <- t.length + 1;
    elt
  ;;

  let add t ~key value =
    Internal_elt.to_external (internal_add t ~key ~at:Time.epoch value)
  ;;

  (* [remove_or_re_add_elts] visits each element in the circular doubly-linked list
     [first].  If the element's key is [>= t_min_allowed_key], then it adds the element
     back at a lower level.  If not, then it calls [handle_removed] and [free]s the
     element. *)
  let remove_or_re_add_elts t level first ~t_min_allowed_key ~handle_removed =
    let pool = t.pool in
    let current = ref first in
    let continue = ref true in
    while !continue do
      (* We extract [next] from [current] first, because we will modify or [free]
         [current] before continuing the loop. *)
      let next = Internal_elt.next pool !current in
      level.Level.length <- level.Level.length - 1;
      if Internal_elt.key pool !current >= t_min_allowed_key then
        add_elt t !current
      else begin
        t.length <- t.length - 1;
        handle_removed (Internal_elt.to_external !current);
        Internal_elt.free pool !current;
      end;
      if phys_equal next first then continue := false else current := next;
    done;
  ;;

  (* [increase_level_min_allowed_key] increases the [min_allowed_key] of [level] to as
     large a value as possible, but no more than [max_level_min_allowed_key].
     [t_min_allowed_key] is the minimum allowed key for the entire timing wheel.  As
     elements are encountered, they are removed from the timing wheel if their key is
     smaller than [t_min_allowed_key], or added at a lower level if not. *)
  let increase_level_min_allowed_key
        t level ~max_level_min_allowed_key ~t_min_allowed_key ~handle_removed =
    let module L = Level in
    (* We require that [mod level.L.min_allowed_key level.L.keys_per_slot = 0].  So,
       we start [level_min_allowed_key] where that is true, and then increase it by
       [keys_per_slot] each iteration of the loop. *)
    let level_min_allowed_key =
      Level.min_key_in_same_slot level
        ~key:(min max_level_min_allowed_key
                (max level.L.min_allowed_key t.elt_key_lower_bound))
    in
    let level_min_allowed_key = ref level_min_allowed_key in
    assert (!level_min_allowed_key <= max_level_min_allowed_key);
    let slot = ref (Level.slot level ~key:!level_min_allowed_key) in
    let keys_per_slot = level.L.keys_per_slot in
    let slots = level.L.slots in
    while !level_min_allowed_key + keys_per_slot <= max_level_min_allowed_key do
      if level.L.length = 0 then
        (* If no elements remain at this level, we can just set [min_allowed_key] to the
           desired value. *)
        level_min_allowed_key :=
          keys_per_slot * (max_level_min_allowed_key / keys_per_slot)
      else begin
        let first = slots.(!slot) in
        if not (Internal_elt.is_null first) then begin
          slots.(!slot) <- Internal_elt.null ();
          remove_or_re_add_elts t level first ~t_min_allowed_key ~handle_removed;
        end;
        slot := Level.next_slot level !slot;
        level_min_allowed_key := !level_min_allowed_key + keys_per_slot;
      end
    done;
    assert (!level_min_allowed_key <= max_level_min_allowed_key);
    assert (!level_min_allowed_key + keys_per_slot > max_level_min_allowed_key);
    level.L.min_allowed_key <- !level_min_allowed_key;
    level.L.max_allowed_key <- !level_min_allowed_key + level.L.num_allowed_keys - 1;
  ;;

  let increase_min_allowed_key t ~key ~handle_removed =
    if key > max_representable_key then
      failwiths "Timing_wheel.increase_min_allowed_key got invalid key" (key, t)
        <:sexp_of< int * _ t >>;
    if key > min_allowed_key t then begin
      (* We increase the [min_allowed_key] of levels in order to restore the invariant
         that they have as large as possible a [min_allowed_key], while leaving no gaps in
         keys. *)
      let level_index = ref 0 in
      let max_level_min_allowed_key = ref key in
      let levels = t.levels in
      let num_levels = num_levels t in
      while !level_index < num_levels do
        let level = levels.(!level_index) in
        let min_allowed_key_before = level.Level.min_allowed_key in
        increase_level_min_allowed_key t level
          ~max_level_min_allowed_key:!max_level_min_allowed_key
          ~t_min_allowed_key:key ~handle_removed;
        if Level.min_allowed_key level = min_allowed_key_before then
          (* This level did not shift.  Don't shift any higher levels. *)
          level_index := num_levels
        else begin
          (* Level [level_index] shifted.  Consider shifting higher levels. *)
          level_index := !level_index + 1;
          max_level_min_allowed_key := 1 + Level.max_allowed_key level;
        end;
      done;
      if key > t.elt_key_lower_bound then begin
        (* We have removed [t.min_elt] or it was already null, so just set it to null. *)
        t.min_elt <- Internal_elt.null ();
        t.elt_key_lower_bound <- min_allowed_key t;
      end;
    end;
  ;;

  let create ?level_bits () =
    let level_bits =
      match level_bits with
      | Some l -> l
      | None -> Level_bits.default Word_size.word_size
    in
    let _, _, levels =
      List.foldi level_bits ~init:(0, 0, [])
        ~f:(fun index (bits_per_slot, max_level_min_allowed_key, levels) bits ->
          let num_slots = 1 lsl bits in
          let keys_per_slot = 1 lsl bits_per_slot in
          let num_allowed_keys = 1 lsl (bits + bits_per_slot) in
          let min_allowed_key =
            keys_per_slot * (max_level_min_allowed_key / keys_per_slot)
          in
          let max_allowed_key = min_allowed_key + num_allowed_keys - 1 in
          let level =
            { Level.
              index;
              bits;
              slots_mask = num_slots - 1;
              bits_per_slot;
              keys_per_slot;
              min_key_in_same_slot_mask = lnot (keys_per_slot - 1);
              num_allowed_keys;
              length = 0;
              min_allowed_key;
              max_allowed_key;
              slots = Array.create ~len:num_slots (Internal_elt.null ());
            }
          in
          (bits + bits_per_slot,
           max_allowed_key + 1,
           level :: levels))
    in
    { length = 0;
      pool = Internal_elt.Pool.create ();
      min_elt = Internal_elt.null ();
      elt_key_lower_bound = 0;
      levels = Array.of_list_rev levels;
    }
  ;;

  let mem t elt = Internal_elt.external_is_valid t.pool elt

  let remove t elt =
    let pool = t.pool in
    let elt = Internal_elt.of_external_exn pool elt in
    if Internal_elt.equal elt t.min_elt then begin
      t.min_elt <- Internal_elt.null ();
      (* We keep [t.elt_lower_bound] since it is valid even though [t.min_elt] is being
         removed. *)
    end;
    let module L = Level in
    t.length <- t.length - 1;
    let level = t.levels.(Internal_elt.level_index pool elt) in
    level.L.length <- level.L.length - 1;
    let slots = level.L.slots in
    let slot = Level.slot level ~key:(Internal_elt.key pool elt) in
    let first = slots.(slot) in
    if phys_equal elt (Internal_elt.next pool elt) then
      (* [elt] is the only element in the slot *)
      slots.(slot) <- Internal_elt.null ()
    else begin
      if phys_equal elt first then slots.(slot) <- Internal_elt.next pool elt;
      Internal_elt.unlink pool elt;
    end;
    Internal_elt.free pool elt;
  ;;

  let clear t =
    if not (is_empty t) then begin
      t.length <- 0;
      let pool = t.pool in
      let free_elt elt = Internal_elt.free pool elt in
      let levels = t.levels in
      for level_index = 0 to Array.length levels - 1 do
        let level = levels.(level_index) in
        let module L = Level in
        if level.L.length > 0 then begin
          level.L.length <- 0;
          let slots = level.L.slots in
          for slot_index = 0 to Array.length slots - 1 do
            let elt = slots.(slot_index) in
            if not (Internal_elt.is_null elt) then begin
              Internal_elt.iter pool elt ~f:free_elt;
              slots.(slot_index) <- Internal_elt.null ();
            end;
          done;
        end;
      done;
    end;
  ;;
end

module Internal_elt = Priority_queue.Internal_elt

(* All time from [start] onwards is broken into half-open intervals of size
   [Config.alarm_precision config].  The intervals are numbered starting at zero, and a
   time's interval number serves as its key in [priority_queue]. *)
type 'a t =
  { config : Config.t;
    start : Time.t;
    mutable now : Time.t;
    priority_queue : 'a Priority_queue.t;
  }
with fields, sexp_of

type 'a timing_wheel = 'a t

let alarm_precision t = Config.alarm_precision t.config

module Alarm = struct
  type 'a t = 'a Priority_queue.Elt.t with sexp_of

  let null = Priority_queue.Elt.null

  let at    tw t = Priority_queue.Elt.at    tw.priority_queue t
  let value tw t = Priority_queue.Elt.value tw.priority_queue t
  let key   tw t = Priority_queue.Elt.key   tw.priority_queue t
end

let sexp_of_t_internal = sexp_of_t

let iter t ~f = Priority_queue.iter t.priority_queue ~f

module Pretty = struct
  module Alarm = struct
    type 'a t =
      { at : Time.t;
        value : 'a;
      }
    with fields, sexp_of

    let create t alarm = { at = Alarm.at t alarm; value = Alarm.value t alarm }

    let compare t1 t2 = Time.compare (at t1) (at t2)
  end

  type 'a t =
    { config : Config.t;
      start : Time.t;
      now : Time.t;
      alarms : 'a Alarm.t list;
    }
  with sexp_of
end

let pretty ({ config; start; now; priority_queue = _ } as t) =
  let r = ref [] in
  iter t ~f:(fun a -> r := Pretty.Alarm.create t a :: !r);
  let alarms = List.sort !r ~cmp:Pretty.Alarm.compare in
  { Pretty. config; start; now; alarms }
;;

let sexp_of_t sexp_of_a t =
  match !sexp_of_t_style with
  | `Internal -> sexp_of_t_internal sexp_of_a t
  | `Pretty -> <:sexp_of< a Pretty.t >> (pretty t)
;;

let length t = Priority_queue.length t.priority_queue

let is_empty t = length t = 0

let interval_num_start t interval_num =
  Time.add t.start (Time.Span.scale (alarm_precision t) (Float.of_int interval_num))
;;

let alarm_upper_bound t =
  interval_num_start t (Priority_queue.max_allowed_key t.priority_queue + 1)
;;

let interval_num t time =
  (* We would like to do something like:

     {[
       try
         Float.iround_down_exn (Time.Span.(//) (Time.diff time t.start) t.alarm_precision)
       with exn ->
         failwiths "Timing_wheel got time too far in the future to represent" (time, t, exn)
           <:sexp_of< Time.t * _ t * exn >>
     ]}

     But that is too slow due to compiler issues.  So we have the code below.
  *)
  let float =
    Caml.Pervasives.(/.)
      (Caml.Pervasives.(-.) (time : Time.t :> float) (t.start : Time.t :> float))
      (alarm_precision t : Time.Span.t :> float)
  in
  if float <= Float.iround_lbound || float >= Float.iround_ubound then
    failwiths "Timing_wheel got time too far in the future to represent"
      (time, t) <:sexp_of< Time.t * _ t >>;
  Caml.Pervasives.int_of_float float
;;

let now_interval_num t = Priority_queue.min_allowed_key t.priority_queue

let interval_start t time = interval_num_start t (interval_num t time)

let invariant invariant_a t =
  Invariant.invariant _here_ t <:sexp_of< _ t >> (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~config:(check Config.invariant)
      ~start:ignore
      ~now:(check (fun now ->
        assert (Time.(>=) now t.start);
        assert (interval_num t t.now = Priority_queue.min_allowed_key t.priority_queue)))
      ~priority_queue:(check (Priority_queue.invariant invariant_a))
    ;
    iter t ~f:(fun alarm ->
      assert (Alarm.key t alarm = interval_num t (Alarm.at t alarm));
      assert (Time.(>=) (interval_start t (Alarm.at t alarm)) (interval_start t (now t)));
      assert (Time.(>=) (Alarm.at t alarm) (Time.sub (now t) (alarm_precision t)));
    ))
;;

let create ~config ~start =
  { config;
    start;
    now = start;
    priority_queue = Priority_queue.create ~level_bits:config.Config.level_bits ();
  }
;;

let advance_clock t ~to_ ~handle_fired =
  if Time.(>) to_ (now t) then begin
    t.now <- to_;
    Priority_queue.increase_min_allowed_key t.priority_queue ~key:(interval_num t to_)
      ~handle_removed:handle_fired
  end;
;;

let add_at_interval_num t ~at value =
  Internal_elt.to_external
    (Priority_queue.internal_add t.priority_queue
       ~key:at ~at:(interval_num_start t at) value);
;;

let add t ~at value =
  if Caml.Pervasives.(<) at t.start then
    failwiths "[Timing_wheel.add] got [~at] before clock start" (at, t.start)
      <:sexp_of< Time.t * Time.t >>;
  Internal_elt.to_external
    (Priority_queue.internal_add t.priority_queue ~key:(interval_num t at) ~at value);
;;

let remove t alarm = Priority_queue.remove t.priority_queue alarm

let clear t = Priority_queue.clear t.priority_queue

let mem t alarm = Priority_queue.mem t.priority_queue alarm

let next_alarm_fires_at t =
  let elt = Priority_queue.min_elt_ t.priority_queue in
  if Internal_elt.is_null elt then
    None
  else
    let key = Internal_elt.key t.priority_queue.Priority_queue.pool elt in
    (* [interval_num_start t key] is the key corresponding to the start of the time
       interval holding the first alarm in [t].  Advancing to that would not be enough,
       since the alarms in that interval don't fire until the clock is advanced to the
       start of the next interval.  So, we use [key + 1] to advance to the start of the
       next interval. *)
    Some (interval_num_start t (key + 1))
;;

module Debug (M : S) = struct

  module Debug = Debug.Make (struct end)

  include Debug

  open M

  type nonrec 'a t = 'a t with sexp_of

  type 'a timing_wheel = 'a t

  module Level_bits = struct
    open Level_bits

    type nonrec t = t with sexp

    let invariant = invariant

    let debug x = Debug.debug invariant ~module_name:"Timing_wheel.Level_bits" x

    let max_num_bits = max_num_bits

    let create_exn ints =
      debug "create_exn" [] ints <:sexp_of< int list >> <:sexp_of< t >>
        (fun () -> create_exn ints)
    ;;

    let default = default

    let num_bits = num_bits
  end

  module Config = struct
    open Config

    type nonrec t = t with sexp

    let alarm_precision = alarm_precision
    let create          = create
    let default         = default
    let durations       = durations
    let invariant       = invariant
    let level_bits      = level_bits
  end

  module Alarm = struct
    open Alarm

    type nonrec 'a t = 'a t with sexp_of

    let null = null

    let at = at
    let key = key
    let value = value
  end

  let invariant = invariant

  let debug x = Debug.debug (invariant ignore) ~module_name:"Timing_wheel" x

  let create ~config ~start =
    debug "create" [] (config, start) <:sexp_of< Config.t * Time.t >>
      <:sexp_of< _ t >>
      (fun () -> create ~config ~start)
  ;;

  let alarm_precision = alarm_precision
  let now = now
  let start = start

  let is_empty = is_empty
  let length = length

  let iter t ~f =
    debug "iter" [t] () <:sexp_of< unit >> <:sexp_of< unit >>
      (fun () -> iter t ~f)
  ;;

  let interval_num t time =
    debug "interval_num" [t] time <:sexp_of< Time.t >> <:sexp_of< int >>
      (fun () -> interval_num t time)
  ;;

  let now_interval_num t =
    debug "now_interval_num" [t] () <:sexp_of< unit >> <:sexp_of< int >>
      (fun () -> now_interval_num t)
  ;;

  let interval_start t time =
    debug "interval_start" [t] time <:sexp_of< Time.t >> <:sexp_of< Time.t >>
      (fun () -> interval_start t time)
  ;;

  let interval_num_start t interval_num =
    debug "interval_num_start" [t] interval_num <:sexp_of< int >> <:sexp_of< Time.t >>
      (fun () -> interval_num_start t interval_num)
  ;;

  let advance_clock t ~to_ ~handle_fired =
    debug "advance_clock" [t] to_ <:sexp_of< Time.t >> <:sexp_of< unit >>
      (fun () -> advance_clock t ~to_ ~handle_fired)
  ;;

  let alarm_upper_bound = alarm_upper_bound

  let add t ~at a =
    debug "add" [t] at <:sexp_of< Time.t >> <:sexp_of< _ Alarm.t >>
      (fun () -> add t ~at a)
  ;;

  let add_at_interval_num t ~at a =
    debug "add_at_interval_num" [t] at <:sexp_of< int >> <:sexp_of< _ Alarm.t >>
      (fun () -> add_at_interval_num t ~at a)
  ;;

  let remove t alarm =
    debug "remove" [t] alarm <:sexp_of< _ Alarm.t >> <:sexp_of< unit >>
      (fun () -> remove t alarm)
  ;;

  let clear t =
    debug "clear" [t] () <:sexp_of< unit >> <:sexp_of< unit >>
      (fun () -> clear t)
  ;;

  let mem t alarm =
    debug "mem" [t] alarm <:sexp_of< _ Alarm.t >> <:sexp_of< bool >>
      (fun () -> mem t alarm)
  ;;

  let next_alarm_fires_at = next_alarm_fires_at

  module Priority_queue = struct
    open Priority_queue

    type nonrec 'a t = 'a t with sexp_of

    type 'a priority_queue = 'a t

    module Elt = struct
      open Elt

      type nonrec 'a t = 'a t with sexp_of

      let invariant = invariant
      let key = key
      let value = value
    end

    let invariant = invariant

    let debug x = Debug.debug (invariant ignore) ~module_name:"Priority_queue" x

    let create ?level_bits () =
      debug "create" [] level_bits <:sexp_of< Level_bits.t option >> <:sexp_of< _ t >>
        (fun () -> create ?level_bits ())
    ;;

    let length = length

    let is_empty = is_empty

    let max_representable_key = max_representable_key

    let min_allowed_key = min_allowed_key

    let max_allowed_key = max_allowed_key

    let min_elt t =
      debug "min_elt" [t] () <:sexp_of< unit >> <:sexp_of< _ Elt.t option >>
        (fun () -> min_elt t)
    ;;

    let min_key t =
      debug "min_key" [t] () <:sexp_of< unit >> <:sexp_of< int option >>
        (fun () -> min_key t)
    ;;

    let add t ~key a =
      debug "add" [t] key <:sexp_of< int >> <:sexp_of< _ Elt.t >>
        (fun () -> add t ~key a)
    ;;

    let remove t elt =
      debug "remove" [t] elt <:sexp_of< _ Elt.t >> <:sexp_of< unit >>
        (fun () -> remove t elt)
    ;;

    let clear t =
      debug "clear" [t] () <:sexp_of< unit >> <:sexp_of< unit >>
        (fun () -> clear t)
    ;;

    let mem t elt =
      debug "mem" [t] elt <:sexp_of< _ Elt.t >> <:sexp_of< bool >>
        (fun () -> mem t elt)
    ;;

    let increase_min_allowed_key t ~key ~handle_removed =
      debug "increase_min_allowed_key" [t] key <:sexp_of< int >> <:sexp_of< unit >>
        (fun () -> increase_min_allowed_key t ~key ~handle_removed)
    ;;

    let iter = iter
  end

end
