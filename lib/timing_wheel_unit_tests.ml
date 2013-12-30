open Core_kernel.Std
open Int.Replace_polymorphic_compare  let () = _squelch_unused_module_warning_

(* module Timing_wheel = Timing_wheel.Debug (Timing_wheel)
 * let () = Timing_wheel.show_messages := false *)

module Debug_in_this_dir = Debug
open Timing_wheel
module Debug = Debug_in_this_dir

let sec = Time.Span.of_sec

module Level_bits = struct

  open Level_bits

  type nonrec t = t

  let invariant = invariant

  let max_num_bits = max_num_bits

  TEST = max_num_bits = Word_size.(num_bits word_size) - 3

  let create_exn = create_exn
  let t_of_sexp = t_of_sexp

  TEST_UNIT =
    List.iter
      [ [];
        [ 0 ];
        [ -1 ];
        [ 2; 0; 1 ];
        [ max_num_bits + 1 ];
        List.init (max_num_bits + 1) ~f:Fn.id;
      ]
      ~f:(fun level_bits ->
        assert (does_raise (fun () -> Level_bits.create_exn level_bits));
        assert (does_raise (fun () -> t_of_sexp (<:sexp_of< int list >> level_bits))))
  ;;

  let default = default

  TEST_UNIT = invariant (default Word_size.W32)
  TEST_UNIT = invariant (default Word_size.W64)

  (* Check that default [level_bits] gives desired range of times. *)
  TEST_UNIT =
    let start =
      Time.of_date_ofday
        (Time.Zone.find_office `nyc)
        (Date.create_exn ~y:2000 ~m:Month.Jan ~d:1)
        Time.Ofday.start_of_day
    in
    List.iter
      [ Word_size.W32, Time.Span.millisecond, Date.create_exn ~y:2000 ~m:Month.Jan ~d:7;
        Word_size.W64, Time.Span.nanosecond , Date.create_exn ~y:2073 ~m:Month.Jan ~d:1;
      ]
      ~f:(fun (word_size, alarm_precision, max_alarm_lower_bound) ->
        let level_bits = default word_size in
        let t =
          create ~config:(Config.create ~level_bits ~alarm_precision ()) ~start
        in
        assert (Date.(>=)
                  (Time.to_local_date (alarm_upper_bound t))
                  max_alarm_lower_bound))
  ;;

  let num_bits = num_bits
  let sexp_of_t = sexp_of_t

  TEST_UNIT =
    List.iter
      [ [ 1 ]      , 1;
        [ 1; 1 ]   , 2;
        [ 1; 2; 3 ], 6;
      ]
      ~f:(fun (bits, expect) ->
        let t = create_exn bits in
        assert (num_bits t = expect);
        let sexp = sexp_of_t t in
        assert (Sexp.equal sexp (sexp_of_t (t_of_sexp sexp))))
  ;;

end

module Config = struct

  open Config

  type nonrec t = t with sexp

  let invariant = invariant

  let create = create
  let alarm_precision = alarm_precision

  TEST = does_raise (fun () -> create ~alarm_precision:(sec (-1.)) ())
  TEST = does_raise (fun () -> create ~alarm_precision:(sec 0.) ())
  TEST = Time.Span.equal (sec 1.) (alarm_precision (create ~alarm_precision:(sec 1.) ()))

  let level_bits = level_bits

  let default = default

  TEST_UNIT = invariant default
  TEST = Poly.equal default (create ())

  let durations = durations

  TEST_UNIT =
    List.iter
      [ [ 1 ], [ sec 2. ];
        [ 2; 1 ], [ sec 4.; sec 8. ];
      ]
      ~f:(fun (level_bits, expect) ->
        assert (Poly.equal expect
                  (durations (create
                                ~alarm_precision:(sec 1.)
                                ~level_bits:(Level_bits.create_exn level_bits)
                                ()))))
  ;;

end

module Priority_queue = struct

  open Priority_queue

  type nonrec 'a t = 'a t with sexp_of

  type 'a priority_queue = 'a t

  let invariant = invariant

  module Elt = struct
    open Elt

    type nonrec 'a t = 'a t with sexp_of

    let invariant = invariant
    let key = key
    let value = value
  end

  let create = create

  let create_unit ~level_bits =
    create ~level_bits:(Level_bits.create_exn level_bits) ()
  ;;

  let min_allowed_key = min_allowed_key
  let max_allowed_key = max_allowed_key

  TEST_UNIT =
    List.iter
      [ [ 1 ], 1;
        [ 1; 1 ], 5;
        [ 1; 1; 1 ], 11;
        [ 2 ], 3;
        [ 3 ], 7;
        [ 3; 1 ], 23;
      ]
      ~f:(fun (level_bits, expected_max_allowed_key) ->
        let t = create_unit ~level_bits in
        assert (min_allowed_key t = 0);
        assert (max_allowed_key t = expected_max_allowed_key))
  ;;

  let add      = add
  let remove   = remove
  let mem      = mem
  let is_empty = is_empty
  let length   = length

  TEST_UNIT =
    let t = create_unit ~level_bits:[1] in
    assert (is_empty t);
    assert (length t = 0);
    let e1 = add t ~key:0 () in
    let e2 = add t ~key:0 () in
    assert (mem t e1);
    assert (mem t e2);
    assert (not (is_empty t));
    assert (length t = 2);
    assert (not (is_empty t));
    remove t e1;
    assert (not (mem t e1));
    assert (mem t e2);
    assert (length t = 1);
    assert (not (is_empty t));
    remove t e2;
    assert (not (mem t e1));
    assert (not (mem t e2));
    assert (length t = 0);
    assert (is_empty t);
  ;;

  TEST_UNIT =
    let t = create_unit ~level_bits:[1] in
    let add ~key = ignore (add t ~key () : _ Elt.t) in
    for key = min_allowed_key t to max_allowed_key t do
      add ~key;
    done;
    let check_adds_fail () =
      List.iter
        [ Int.min_value;
          min_allowed_key t - 1;
          max_allowed_key t + 1;
          max_representable_key + 1;
          Int.max_value;
        ]
        ~f:(fun key -> assert (does_raise (fun () -> add ~key)))
    in
    check_adds_fail ();
    increase_min_allowed_key t ~key:1 ~handle_removed:ignore;
    check_adds_fail ();
    increase_min_allowed_key t ~key:(max_allowed_key t) ~handle_removed:ignore;
    check_adds_fail ();
    increase_min_allowed_key t ~key:max_representable_key ~handle_removed:ignore;
    check_adds_fail ();
  ;;

  let clear = clear
  TEST_UNIT =
    let t = create_unit ~level_bits:[ 1; 1 ] in
    clear t;
    let e1 = add t ~key:0 () in
    let e2 = add t ~key:2 () in
    clear t;
    assert (is_empty t);
    assert (not (mem t e1));
    assert (not (mem t e2));
  ;;

  let max_representable_key = max_representable_key

  let increase_min_allowed_key_return_removed_keys t ~key =
    let r = ref [] in
    let handle_removed elt = r := Elt.key t elt :: !r in
    increase_min_allowed_key t ~key ~handle_removed;
    !r
  ;;

  TEST_UNIT =
    let t = create_unit ~level_bits:[1] in
    let add ~key = ignore (add t ~key () : _ Elt.t) in
    add ~key:0;
    add ~key:1;
    assert (does_raise (fun () ->
      increase_min_allowed_key t ~key:(max_representable_key + 1)
        ~handle_removed:ignore));
    increase_min_allowed_key t ~key:max_representable_key ~handle_removed:ignore;
    assert (is_empty t);
    assert (min_allowed_key t = max_representable_key);
    assert (max_allowed_key t = max_representable_key);
    add ~key:max_representable_key;
    assert (length t = 1);
  ;;

  let increase_min_allowed_key = increase_min_allowed_key

  TEST_UNIT =
    (* [all_sums n] returns all combinations of nonnegative ints that sum to [n]. *)
    let all_sums n =
      let results = Array.create ~len:(n + 1) [] in
      results.(0) <- [[]];
      for i = 1 to n do
        results.(i) <-
          List.concat
            (List.init i ~f:(fun j ->
               let first = j + 1 in
               List.map results.(i - first) ~f:(fun rest -> first :: rest)));
      done;
      results.(n)
    in
    let test ~num_bits ~level_bits ~initial_min_allowed_key ~step =
      if false then
        Debug.eprints "test" (`num_bits num_bits, `level_bits level_bits,
                          `initial_min_allowed_key initial_min_allowed_key,
                          `step step)
          (<:sexp_of< ([`num_bits of int] * [`level_bits of int list]
                       * [`initial_min_allowed_key of int]
                       * [`step of int]) >>);
      let t = create_unit ~level_bits in
      try
        increase_min_allowed_key t ~key:initial_min_allowed_key ~handle_removed:ignore;
        assert (min_allowed_key t = initial_min_allowed_key);
        assert (max_allowed_key t >= min_allowed_key t + 1 lsl num_bits - 1);
        let keys =
          List.range
            ~start:`inclusive (min_allowed_key t)
            ~stop: `inclusive (max_allowed_key t - 1)
        in
        let n = ref 0 in
        List.iter keys ~f:(fun key ->
          ignore (add t ~key () : _ Elt.t);
          incr n;
          assert (length t = !n));
        let removed = ref [] in
        while length t > 0 do
          let keys_removed =
            increase_min_allowed_key_return_removed_keys t
              ~key:(min max_representable_key (min_allowed_key t + step))
          in
          removed := keys_removed @ !removed;
          List.iter keys_removed ~f:(fun key -> assert (key < min_allowed_key t))
        done;
        let keys_removed = List.sort !removed ~cmp:Int.compare in
        assert (Poly.equal keys_removed keys);
      with exn ->
        failwiths "failure" (exn, t) <:sexp_of< exn * _ t >>
    in
    let num_bits = 6 in
    let all_sums = all_sums num_bits in
    List.iter
      [ 0;
        max_representable_key - (1 lsl num_bits);
      ]
      ~f:(fun initial_min_allowed_key ->
        for step = 1 to 1 lsl num_bits do
          List.iter all_sums ~f:(fun level_bits ->
            test ~num_bits ~level_bits ~initial_min_allowed_key ~step)
        done);
  ;;

  let min_elt = min_elt
  let min_key = min_key

  TEST_UNIT =
    let t = create_unit ~level_bits:[ 1; 1; 1; 1 ] in
    assert (is_none (min_key t));
    let _elt = add t ~key:0 () in
    assert (Poly.equal (min_key t) (Some 0));
    let max_key = 10 in
    for key = 1 to max_key; do
      assert (is_ok (Result.try_with (fun () -> add t ~key ())));
      assert (Poly.equal (min_key t) (Some 0));
    done;
    for key = 1 to max_key + 1; do
      begin match increase_min_allowed_key_return_removed_keys t ~key with
      | [key'] -> assert (key' = key - 1);
      | _ -> assert false
      end;
      assert (Poly.equal (min_key t) (if key <= max_key then Some key else None));
    done;
  ;;

  TEST_UNIT =
    let t = create_unit ~level_bits:[1; 1; 1; 1] in
    let max_key = 10 in
    let elts = List.init (max_key + 1) ~f:(fun key -> add t ~key ()) in
    List.iter elts ~f:(fun elt ->
      let key = Elt.key t elt in
      remove t elt;
      assert (Poly.equal (min_key t) (if key < max_key then Some (key + 1) else None)));
  ;;

  let iter = iter

  TEST_UNIT =
    let t = create_unit ~level_bits:[1; 1; 1; 1] in
    let count () =
      let r = ref 0 in
      iter t ~f:(fun _ -> incr r);
      !r
    in
    assert (count () = 0);
    let num_elts = 10 in
    for key = 0 to num_elts - 1; do
      ignore (add t ~key () : _ Elt.t);
    done;
    assert (count () = num_elts);
    increase_min_allowed_key t ~key:1 ~handle_removed:ignore;
    assert (count () = num_elts - 1);
    increase_min_allowed_key t ~key:num_elts ~handle_removed:ignore;
    assert (count () = 0);
  ;;

  TEST_UNIT =
    let t = create_unit ~level_bits:[ 1; 1; 1; 1 ] in
    let elts = ref [] in
    for key = 0 to max_allowed_key t do
      elts := add t ~key () :: !elts
    done;
    let elts' = ref [] in
    iter t ~f:(fun elt -> elts' := elt :: !elts');
    let sort elts =
      List.sort elts ~cmp:(fun elt1 elt2 -> Int.compare (Elt.key t elt1) (Elt.key t elt2))
    in
    assert (List.equal ~equal:phys_equal (sort !elts) (sort !elts'));
  ;;

end

type nonrec 'a t = 'a t with sexp_of

type 'a timing_wheel = 'a t

module Alarm = struct
  open Alarm

  type nonrec 'a t = 'a t with sexp_of

  let null = null

  let at = at
  let key = key
  let value = value
end

let invariant = invariant

let create_unit ?level_bits ?(start = Time.epoch) ?(alarm_precision = sec 1.) () =
  create ~config:(Config.create ?level_bits ~alarm_precision ()) ~start
;;

let create = create

TEST_UNIT =
  List.iter
    [ sec (-1.);
      sec 0.;
    ]
    ~f:(fun alarm_precision ->
      assert (does_raise (fun () -> create_unit ~alarm_precision ())))
;;

let alarm_upper_bound = alarm_upper_bound

let interval_num       = interval_num
let interval_num_start = interval_num_start
let interval_start     = interval_start

TEST_UNIT =
  let t = create_unit () in
  assert (not (mem t (Alarm.null ())));
  let start = start t in
  List.iter
    [ Time.sub start (sec (2. *. Float.of_int Int.max_value));
      Time.add start (sec (2. *. Float.of_int Int.max_value));
      Time.of_float Float.max_value;
    ]
    ~f:(fun time ->
      assert (does_raise (fun () -> interval_num t time));
      assert (does_raise (fun () -> interval_start t time)));
  assert (Time.(<) (interval_num_start t (-1)) start);
  List.iter
    [ 0.,   0;
      0.1,  0;
      0.99, 0;
      1.,   1;
      1.5,  1;
      1.99, 1;
      2.,   2;
    ]
    ~f:(fun (after, expected_interval) ->
      let time = Time.add start (sec after) in
      assert (interval_num t time = expected_interval);
      assert (Time.equal
                (interval_num_start t (interval_num t time))
                (interval_start t time));
      assert (Time.equal
                (interval_start t time)
                (Time.add start (sec (Float.of_int expected_interval)))));
;;

let now = now
let start = start
let alarm_precision = alarm_precision

TEST_UNIT =
  let t = create_unit () in
  assert (Time.Span.equal (alarm_precision t) (sec 1.));
  assert (Time.equal (now t) Time.epoch);
  assert (Time.equal (start t) Time.epoch);
  let to_ = Time.add (now t) (sec 1.) in
  advance_clock t ~to_ ~handle_fired:ignore;
  assert (Time.equal (now t) to_);
  assert (Time.equal (start t) Time.epoch);
;;

let is_empty = is_empty
let length = length

TEST_UNIT =
  let t = create_unit () in
  assert (is_empty t);
  assert (length t = 0);
  let alarm = add t ~at:(now t) () in
  assert (not (is_empty t));
  assert (length t = 1);
  remove t alarm;
  assert (is_empty t);
  assert (length t = 0);
;;

let iter = iter

TEST_UNIT =
  let t = create_unit () in
  iter t ~f:(fun _ -> assert false);
  let alarm1 = add t ~at:(now t) () in
  iter t ~f:(fun alarm -> assert (phys_equal alarm alarm1));
  let alarm2 = add t ~at:(now t) () in
  let r = ref 0 in
  iter t ~f:(fun alarm ->
    assert (phys_equal alarm alarm1 || phys_equal alarm alarm2);
    incr r);
  assert (!r = 2);
  remove t alarm1;
  remove t alarm2;
  iter t ~f:(fun _ -> assert false);
;;

let clear = clear
(* Already tested above for [Priority_queue]. *)

let mem = mem
let remove = remove

(* Check that access to a removed alarm doesn't segfault. *)
TEST_UNIT =
  let t =
    create
      ~config:(Config.create ~alarm_precision:(sec 1.) ())
      ~start:Time.epoch
  in
  let alarm = add t ~at:(Time.add (now t) (sec 5.)) (ref 1) in
  assert (mem t alarm);
  remove t alarm;
  assert (not (mem t alarm));
  assert (does_raise (fun _ -> Alarm.key t alarm));
  assert (does_raise (fun _ -> Alarm.at t alarm));
  assert (does_raise (fun _ -> Alarm.value t alarm));
;;

let add                 = add
let add_at_interval_num = add_at_interval_num
let advance_clock       = advance_clock
let now_interval_num    = now_interval_num

(* No early alarms *)
TEST_UNIT =
  let test ~add ~num_alarms ~alarm_precision ~alarm_separation ~advance_by =
    if false then
      Debug.eprints "test" (num_alarms, alarm_precision, alarm_separation, advance_by)
        (<:sexp_of< int * Time.Span.t * Time.Span.t * Time.Span.t >>);
    let t =
      create ~config:(Config.create ~alarm_precision ()) ~start:Time.epoch
    in
    for i = 1 to num_alarms do
      let at = Time.add (now t) (Time.Span.scale alarm_separation (Float.of_int i)) in
      ignore (add t ~at (fun () -> assert (Time.(<=) at (now t))) : _ Alarm.t);
    done;
    while not (is_empty t) do
      let to_ = Time.add (now t) advance_by in
      advance_clock t ~to_ ~handle_fired:(fun alarm -> Alarm.value t alarm ());
      assert (now_interval_num t = interval_num t to_);
    done;
  in
  List.iter
    [ add
    ; (fun t ~at a -> add_at_interval_num t ~at:(interval_num t at) a)
    ]
    ~f:(fun add ->
      List.iter [ 100 ] ~f:(fun num_alarms ->
        List.iter [ 1.; 0.5; 0.1 ] ~f:(fun s ->
          let alarm_precision = sec s in
          List.iter [ 0.01; 0.1; 0.5; 1.; 2.; 10. ] ~f:(fun s ->
            let alarm_separation = sec s in
            List.iter [ 0.1; 0.5; 1.; 2.; 10. ] ~f:(fun s ->
              let advance_by = sec s in
              test ~add ~num_alarms ~alarm_precision ~alarm_separation ~advance_by)))));
;;

TEST_UNIT =
  let t =
    create
      ~config:(Config.create
                 ~alarm_precision:(sec 1.)
                 ~level_bits:(Level_bits.create_exn [10])
                 ())
      ~start:Time.epoch
  in
  let num_alarms () =
    let r = ref 0 in
    iter t ~f:(fun _ -> incr r);
    !r
  in
  let add ~after f = ignore (add t ~at:(Time.add (now t) after) f : _ Alarm.t) in
  let advance_clock by =
    advance_clock t ~to_:(Time.add (now t) by)
      ~handle_fired:(fun alarm -> Alarm.value t alarm ())
  in
  assert (does_raise (fun () -> add ~after:(sec (-1.)) ignore));
  assert (Time.equal (alarm_upper_bound t) (Time.add (now t) (sec 1024.)));
  assert (num_alarms () = 0);
  assert (is_none (next_alarm_fires_at t));
  let fired = ref false in
  add ~after:(sec 30.) (fun () -> fired := true);
  assert (Poly.equal (next_alarm_fires_at t) (Some (Time.add (now t) (sec 31.))));
  assert (num_alarms () = 1);
  advance_clock (sec 30.);
  assert (not !fired);
  advance_clock (sec 1.);
  assert !fired;
;;

let next_alarm_fires_at = next_alarm_fires_at

TEST_UNIT =
  let t = create_unit ~level_bits:(Level_bits.create_exn [10]) () in
  let add_at at = ignore (add t ~at:(Time.add Time.epoch at) () : _ Alarm.t) in
  let no_next_alarm () = is_none (next_alarm_fires_at t) in
  let next_alarm_fires_at span =
    match next_alarm_fires_at t with
    | None -> false
    | Some at -> Time.(=) at (Time.add Time.epoch span)
  in
  let advance_clock span =
    advance_clock t ~to_:(Time.add Time.epoch span) ~handle_fired:ignore
  in
  assert (no_next_alarm ());
  add_at (sec 2.);
  assert (next_alarm_fires_at (sec 3.));
  add_at (sec 1.5);
  assert (next_alarm_fires_at (sec 2.));
  add_at (sec 1.0);
  assert (next_alarm_fires_at (sec 2.));
  add_at (sec 0.5);
  assert (next_alarm_fires_at (sec 1.));
  add_at (sec 0.1);
  assert (next_alarm_fires_at (sec 1.));
  advance_clock (sec 0.5);
  assert (next_alarm_fires_at (sec 1.));
  advance_clock (sec 1.);
  assert (next_alarm_fires_at (sec 2.));
  advance_clock (sec 1.5);
  assert (next_alarm_fires_at (sec 2.));
  advance_clock (sec 2.);
  assert (next_alarm_fires_at (sec 3.));
  advance_clock (sec 3.);
  assert (no_next_alarm ());
;;

