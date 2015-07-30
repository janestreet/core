open Core_kernel.Std
open Int.Replace_polymorphic_compare

module Infinite_or_finite = struct
  module T = struct
    type 'a t =
      | Infinite
      | Finite of 'a
    with sexp, bin_io
  end
  include T

  let compare compare t1 t2 =
    match t1, t2 with
    | Infinite, Infinite -> 0
    | Infinite, Finite _ -> 1
    | Finite _, Infinite -> -1
    | Finite a, Finite b -> compare a b
  ;;

  let map t ~f =
    match t with
    | Infinite -> Infinite
    | Finite v -> Finite (f v)
  ;;
end
open Infinite_or_finite.T

module type Arith = sig
  type t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t

  val ( < )  : t -> t -> bool
  val ( > )  : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val min : t -> t -> t
  val max : t -> t -> t

  val zero : t
end

(* We implement token math using fixed point math so that moving tokens through

   in_hopper -> in_bucket -> in_flight

   never results in dropped tokens or rounding errors due to floating point math. *)
module Tokens : sig
  type t with sexp, bin_io, compare

  module Arith : Arith with type t := t
  include Arith with type t := t

  (* the underlying scaling factor used for conversion from fixed <-> float space *)
  val scaling_factor : Int63.t

  val of_float_round_up_exn : float -> t
  val of_float_round_down_exn : float -> t
  val to_float : t -> float

  val to_string : t -> string
end = struct
  type t = Int63.t with sexp, bin_io, compare

  (* The scaling factor effectively sets how small of a sub-job one can ask for. Choosing
     10m lets us specify quite high rates as fractional jobs, while leaving us with the
     ability of having buckets containing about 10 billion jobs.  This gives us plenty of
     space on both sides. *)
  let scaling_factor = Int63.of_int 10_000_000
  let float_scaling_factor = Float.of_int (Int63.to_int_exn scaling_factor)

  let zero = Int63.of_int 0
  let to_string t = Int63.to_string t

  let of_float_gen round x =
    match Float.classify x with
    | Infinite | Nan -> failwithf "cannot convert (%g) into Tokens.t" x ()
    | Normal | Subnormal | Zero ->
      Int63.of_int (round (x *. float_scaling_factor))
  ;;

  let of_float_round_up_exn      = of_float_gen Float.iround_up_exn
  let of_float_round_down_exn    = of_float_gen Float.iround_down_exn

  let to_float t = Int63.to_float t /. float_scaling_factor

  module Arith = struct
    let ( + ) t1 t2 = Int63.(t1 + t2)
    let ( - ) t1 t2 = Int63.(t1 - t2)
    let zero = zero
    include Comparable.Make_binable(Int63)
    let min = Int63.min
    let max = Int63.max
  end
  include Arith

end

let gen_fixed_checks fixed_op float_op =
  let checks =
    [ (10., 1.)
    ; (0., 1.)
    ; (0., 0.4)
    ; (-1., 1.3)
    ; (-1., -0.3)
    ; (-1., 0.3) ]
  in
  List.iter checks ~f:(fun (a, b) ->
    let fixed_a = Tokens.of_float_round_up_exn a in
    let fixed_b = Tokens.of_float_round_up_exn b in
    let fixed_result = fixed_op fixed_a fixed_b |> Tokens.to_float in
    let float_result = float_op a b in
    let max_diff = 1. /. Int63.to_float Tokens.scaling_factor in
    let actual_diff = Float.abs (fixed_result -. float_result) in
    if Float.(>) actual_diff max_diff
    then failwithf !"Tokens math failure.  (a = %g) (b = %g) (fixed_a = %{Tokens}) \
                    (fixed_b = %{Tokens}) (a op b) = (%g) (a fixed_op b) = (%g)"
           a b fixed_a fixed_b float_result fixed_result ())
;;

TEST_UNIT "add" = gen_fixed_checks Tokens.( + ) Float.( + )
TEST_UNIT "sub" = gen_fixed_checks Tokens.( - ) Float.( - )

type t =
  { start_time                            : Time.t
  (** The current time of the rate limiter.  Note that when this is moved forward,
      [in_hopper] must be updated accordingly. *)
  ; mutable time                          : Time.t
  (** the amount of time that has passed expressed in token terms, since start_time. *)
  ; mutable time_in_token_space           : Tokens.t Infinite_or_finite.t
  (** number of tokens in the bucket *)
  ; mutable in_bucket                     : Tokens.t
  (** number of tokens in the hopper.  May be [inf] *)
  ; mutable in_hopper                     : Tokens.t Infinite_or_finite.t
  (** Everything that has been taken from bucket but not returned to hopper *)
  ; mutable in_flight                     : Tokens.t
  (** maximum size allowable in the hopper *)
  ; mutable bucket_size                   : Tokens.t
  ; mutable hopper_to_bucket_rate_per_sec : float Infinite_or_finite.t
  (** see .mli documentation for [set_token_target_level] *)
  ; mutable token_target_level             : Tokens.t Infinite_or_finite.t
  }
with sexp_of, fields, compare

let fill_rate_must_be_positive (fill_rate : float Infinite_or_finite.t) =
  match fill_rate with
  | Infinite    -> ()
  | Finite rate ->
    if Float.(rate < 0.)
    then failwithf !"hopper_to_bucket_rate_per_sec (%G) must be >= 0" rate ()
;;

let in_system t : Tokens.t Infinite_or_finite.t =
  match t.in_hopper with
  | Infinite         -> Infinite
  | Finite in_hopper -> Finite Tokens.(t.in_flight + in_hopper + t.in_bucket)
;;

let invariant t =
  let open Tokens.Arith in
  fill_rate_must_be_positive t.hopper_to_bucket_rate_per_sec;

  (* bucket is limited to size *)
  if t.in_bucket > t.bucket_size
  then failwithf !"amount in_bucket (%{Tokens}) cannot be greater than bucket_size \
                   (%{Tokens})"
          t.in_bucket t.bucket_size ();

  (* sizes must be positive *)
  if t.bucket_size <= zero
  then failwithf !"bucket_size (burst_size) (%{Tokens}) must be > 0" t.bucket_size ();

  if t.in_bucket < zero
  then failwithf !"in_bucket (%{Tokens}) must be >= 0." t.in_bucket ();

  begin match t.in_hopper with
  | Infinite         -> ()
  | Finite in_hopper ->
    if in_hopper < zero
    then failwithf !"in_hopper (%{Tokens}) must be >= 0." in_hopper ();
  end;

  if t.in_flight < zero
  then failwithf !"in_flight (%{Tokens}) must be >= 0." t.in_flight ();

  begin match t.hopper_to_bucket_rate_per_sec, t.time_in_token_space with
  | Infinite, Finite _
  | Finite _, Infinite ->
    failwith
      "hopper_to_bucket_rate_per_sec can only be infinite if time_in_token_space is \
       infinite";
  | Infinite, Infinite
  | Finite _, Finite _ -> ()
  end;

  begin match t.token_target_level, in_system t with
  | Infinite, Infinite         -> ()
  | Infinite, Finite in_system ->
    failwithf
      !"token_target_level is infinite while tokens in system (%{Tokens}) is finite"
      in_system ()
  | Finite token_target_level, Infinite ->
    failwithf !"token_target_level (%{Tokens}) is finite and tokens in system is infinite"
      token_target_level ()
  | Finite token_target_level, Finite in_system ->
    if token_target_level < zero
    then failwithf !"token_target_level (%{Tokens}) must be >= 0." token_target_level ();

    (* max tokens can only exceed the total in the system when we are waiting
       for excess in_flight tokens to return. *)

    if in_system > token_target_level
    then begin
      match t.in_hopper with
      | Infinite         ->
        failwithf !"total tokens in_system (Infinite) > token_target_level (%{Tokens})"
          token_target_level ()
      | Finite in_hopper ->
        if    in_hopper   <> zero
           || t.in_bucket <> zero
        then
          failwithf !"total tokens in_system (%{Tokens}) > token_target_level (%{Tokens})"
            in_system token_target_level ()
    end
  end;
;;

(* this module provides machinery to save and restore the state of a t.  It is used below
   in operations that may leave [t] in an inconsistent state so that invariants are always
   maintained for t's accessable outside of this module.  It's a bit heavy for what it
   does, but is also robust. *)
module Update : sig
  val protect : t -> (unit -> unit) -> unit
end = struct
  let empty () =
    { start_time                    = Time.epoch
    ; time                          = Time.epoch
    ; time_in_token_space           = Infinite
    ; in_bucket                     = Tokens.zero
    ; in_hopper                     = Finite Tokens.zero
    ; in_flight                     = Tokens.zero
    ; bucket_size                   = Tokens.zero
    ; hopper_to_bucket_rate_per_sec = Infinite
    ; token_target_level            = Finite Tokens.zero }
  ;;

  module T : sig
    type state

    val save : t -> state
    val restore : state -> t -> unit
  end = struct
    type state = t

    let copy t1 t2 =
      let copy_to_t2 field =
        match Field.setter field with
        | None ->
          failwith "bug in Core.Limiter.  Unexpected immutable field"
        | Some set -> set t2 (Field.get field t1)
      in
      Fields.iter
        (* start time is immutable, and therefore doesn't need to be saved or restored *)
        ~start_time:(fun _ -> ())
        ~time:copy_to_t2
        ~time_in_token_space:copy_to_t2
        ~in_bucket:copy_to_t2
        ~in_hopper:copy_to_t2
        ~in_flight:copy_to_t2
        ~bucket_size:copy_to_t2
        ~hopper_to_bucket_rate_per_sec:copy_to_t2
        ~token_target_level:copy_to_t2;
    ;;

    let save t =
      let state = empty () in
      copy t state;
      state
    ;;

    let restore state t = copy state t
  end

  let protect t f =
    let state = T.save t in
    try f (); invariant t with
    | e ->
      T.restore state t;
      raise e
  ;;

  (* this test ensures that save and restore work as intended and don't throw because we
     accidentaly added a non-mutable field *)
  TEST_UNIT =
    let t = empty () in
    begin try
      protect t (fun () ->
        (* set all the fields to something to check that they all get reset when we hit an
           exception *)
        t.time                          <- Time.now ();
        t.in_bucket                     <- Tokens.of_float_round_up_exn 7.;
        t.in_hopper                     <- Finite (Tokens.of_float_round_up_exn 7.);
        t.in_flight                     <- Tokens.of_float_round_up_exn 7.;
        t.bucket_size                   <- Tokens.of_float_round_up_exn 15.;
        t.hopper_to_bucket_rate_per_sec <- Finite 4.;
        t.token_target_level            <- Finite (Tokens.of_float_round_up_exn 7.);
        assert false);
    with
    | _ -> ()
    end;
    assert (compare t (empty ()) = 0)
  ;;
end

type limiter = t with sexp_of

let create_exn
    ~now
    ~hopper_to_bucket_rate_per_sec
    ~bucket_size
    ~initial_bucket_level
    ~initial_hopper_level
  =
  let in_hopper =
    Infinite_or_finite.map initial_hopper_level ~f:Tokens.of_float_round_up_exn
  in
  let initial_bucket_level = Tokens.of_float_round_up_exn initial_bucket_level in
  let token_target_level =
    Infinite_or_finite.map in_hopper ~f:(fun in_hopper ->
      Tokens.(in_hopper + initial_bucket_level))
  in
  let time_in_token_space =
    Infinite_or_finite.map hopper_to_bucket_rate_per_sec ~f:(fun _ -> Tokens.zero)
  in
  let t =
    { start_time = now
    ; time = now
    ; time_in_token_space
    ; in_bucket = initial_bucket_level
    ; in_hopper
    ; in_flight = Tokens.zero
    ; bucket_size = Tokens.of_float_round_up_exn bucket_size
    ; hopper_to_bucket_rate_per_sec
    ; token_target_level
    }
  in
  invariant t;
  t
;;

(** used for moving tokens from hopper to bucket in response to changes in time.  Note
    that this approximately preserves the sum of in_bucket and in_hopper.  *)
let move_at_most_from_hopper_to_bucket t max_move =
  let open Tokens.Arith in
  let space_in_bucket = max zero (t.bucket_size - t.in_bucket) in
  let actual_move =
    match max_move with
    | Infinite        -> space_in_bucket
    | Finite max_move -> min max_move space_in_bucket
  in
  t.in_bucket <- t.in_bucket + actual_move;
  t.in_hopper <- Infinite_or_finite.map t.in_hopper
                   ~f:(fun in_hopper -> in_hopper - actual_move)
;;

(** Computes the number of tokens that would have dropped since start_time given the
    current rate *)
let calculate_time_in_token_space t =
  Infinite_or_finite.map t.hopper_to_bucket_rate_per_sec ~f:(fun tokens_per_sec ->
    let time_elapsed_since_start = Time.Span.to_sec (Time.diff t.time t.start_time) in
    Tokens.of_float_round_down_exn (time_elapsed_since_start *. tokens_per_sec))
;;

(* advances [t]s notion of time, moving tokens from the hopper down into the bucket as
   dictated by the passage of time and the hopper_to_bucket_rate_per_sec.  This function
   is careful to only move time forward in increments that drop whole numbers of
   micro-tokens (see Token), otherwise it might be possible for an (admitedly degenerate)
   program to call advance_time in a tight loop without dropping any tokens at all. *)
let advance_time t ~now =
  let time_before_call = t.time in
  t.time <- Time.max time_before_call now;
  match t.time_in_token_space, calculate_time_in_token_space t with
  | Infinite, Finite _
  | Finite _, Infinite -> invariant t; assert false
  | Infinite, Infinite  ->
    move_at_most_from_hopper_to_bucket t t.in_hopper
  | Finite previous_time_in_token_space, Finite new_time_in_token_space ->
    t.time_in_token_space <- Finite new_time_in_token_space;
    let amount_that_could_fall =
      (* this will always be >= 0 because time always moves forward *)
      Tokens.(-) new_time_in_token_space previous_time_in_token_space
    in
    let max_move =
      match t.in_hopper with
      | Infinite         -> amount_that_could_fall
      | Finite in_hopper -> Tokens.min in_hopper amount_that_could_fall
    in
    move_at_most_from_hopper_to_bucket t (Finite max_move);
;;

TEST_UNIT "time can only move forward" =
  let t =
    create_exn
      ~now:Time.epoch
      ~hopper_to_bucket_rate_per_sec:Infinite
      ~bucket_size:1.
      ~initial_bucket_level:0.
      ~initial_hopper_level:Infinite
  in
  advance_time t ~now:(Time.add Time.epoch (Time.Span.of_sec 1.));
  let expected_time = t.time in
  advance_time t ~now:Time.epoch;
  assert (Time.(=) t.time expected_time)
;;

let set_hopper_to_bucket_rate_per_sec_exn t ~now rate =
  Update.protect t (fun () ->
    advance_time t ~now;
    t.hopper_to_bucket_rate_per_sec <- rate;
    t.time_in_token_space <- calculate_time_in_token_space t;
    (* The only role of this is to fill the bucket in the case that the hopper rate has
       been set to infinite.  Otherwise, this call to [advance_time] has no effect. *)
    advance_time t ~now)
;;

let set_bucket_size_exn t ~now bucket_size =
  Update.protect t (fun () ->
    advance_time t ~now;
    t.bucket_size <- Tokens.of_float_round_up_exn bucket_size)
;;

let try_take t ~now amount =
  let open Tokens.Arith in
  advance_time t ~now;
  let amount = Tokens.of_float_round_up_exn amount in
  if amount > t.bucket_size
  then `Asked_for_more_than_bucket_size
  else if amount > t.in_bucket
  then `Unable
  else begin
    t.in_bucket <- t.in_bucket - amount;
    t.in_flight <- t.in_flight + amount;
    `Taken
  end
;;

(* when returning tokens to the hopper we diminish in_flight by the amount returned, and
   then only return tokens to the hopper until: in_system t = token_target_level *)
let return_to_hopper t ~now amount =
  let open Tokens.Arith in
  let amount = Tokens.of_float_round_up_exn amount in
  if amount < zero
  then failwithf !"return_to_hopper passed a negative amount (%{Tokens})" amount ();

  if amount > t.in_flight
  then failwithf !"return_to_hopper passed an amount (%{Tokens}) > in_flight (%{Tokens})"
         amount t.in_flight ();

  advance_time t ~now;
  let currently_in_system = in_system t in
  let amount_to_add =
    match t.token_target_level, currently_in_system with
    | Infinite, Finite _
    | Finite _, Infinite        -> invariant t; assert false
    | Infinite, Infinite        -> amount
    | Finite token_target_level, Finite currently_in_system ->
      if currently_in_system <= token_target_level
      then amount
      else begin
        let excess_tokens  = currently_in_system - token_target_level in
        let tokens_to_drop = min amount excess_tokens in
        amount - tokens_to_drop
      end
  in
  t.in_flight <- t.in_flight - amount;
  t.in_hopper <- Infinite_or_finite.map t.in_hopper
                   ~f:(fun in_hopper -> in_hopper + amount_to_add)
;;

let tokens_may_be_available_when t ~now amount =
  let open Tokens.Arith in
  (* Note that because we're rounding up, we may slightly overestimate the required
     waiting time *)
  let amount = Tokens.of_float_round_up_exn amount in
  if amount > t.bucket_size then
    `Never_because_greater_than_bucket_size
  else begin
    advance_time t ~now;
    let amount_missing = amount - t.in_bucket in
    if amount_missing <= zero
    then `At t.time
    else begin
      match t.hopper_to_bucket_rate_per_sec with
      | Infinite -> `When_return_to_hopper_is_called
      | Finite tokens_per_sec ->
        let min_seconds_left =
          Tokens.to_float amount_missing /. tokens_per_sec
        in
        let min_time = `At (Time.add t.time (Time.Span.of_sec min_seconds_left)) in
        match t.in_hopper with
        | Finite in_hopper ->
          if amount_missing > in_hopper
          then `When_return_to_hopper_is_called
          else min_time
        | Infinite -> min_time
    end
  end
;;

let in_bucket t ~now =
  advance_time t ~now;
  Tokens.to_float t.in_bucket
;;

let in_hopper t ~now =
  advance_time t ~now;
  Infinite_or_finite.map t.in_hopper ~f:Tokens.to_float
;;

let in_flight t ~now =
  advance_time t ~now;
  Tokens.to_float t.in_flight
;;

let in_limiter t ~now =
  Infinite_or_finite.map (in_hopper t ~now) ~f:(fun in_hopper ->
    in_bucket t ~now +. in_hopper)
;;

let in_system t ~now =
  advance_time t ~now;
  Infinite_or_finite.map (in_system t) ~f:Tokens.to_float
;;

let set_token_target_level_exn t ~now token_target_level =
  let open Tokens.Arith in
  Update.protect t (fun () ->
    advance_time t ~now;
    begin match token_target_level with
    | Infinite ->
      t.token_target_level <- Infinite;
      t.in_hopper <- Infinite;
    | Finite token_target_level ->
      let token_target_level = Tokens.of_float_round_up_exn token_target_level in
      let target_for_bucket_plus_hopper = max zero (token_target_level - t.in_flight) in
      t.in_bucket <- min target_for_bucket_plus_hopper t.in_bucket;
      t.in_hopper <- Finite (target_for_bucket_plus_hopper - t.in_bucket);
      t.token_target_level <- Finite token_target_level
    end)
;;

let bucket_size t = Tokens.to_float t.bucket_size

let hopper_to_bucket_rate_per_sec t = t.hopper_to_bucket_rate_per_sec

module Token_bucket = struct
  type t = limiter

  let create_exn
        ~now
        ~burst_size:bucket_size
        ~sustained_rate_per_sec:fill_rate
        ?(initial_bucket_level = 0.)
        ()
    =
    create_exn
      ~now
      ~bucket_size
      ~hopper_to_bucket_rate_per_sec:(Finite fill_rate)
      ~initial_bucket_level
      ~initial_hopper_level:Infinite
  ;;

  let try_take t ~now = try_take t ~now
end

module Throttled_rate_limiter = struct
  type t = limiter

  let create_exn
        ~now
        ~burst_size
        ~sustained_rate_per_sec:fill_rate
        ~max_concurrent_jobs
    =
    let bucket_size          = Float.of_int burst_size in
    let max_concurrent_jobs  = Float.of_int max_concurrent_jobs in
    let initial_bucket_level = Float.min bucket_size max_concurrent_jobs in
    let initial_hopper_level =
      Finite (Float.max 0. (max_concurrent_jobs -. initial_bucket_level))
    in
    create_exn
      ~now
      ~bucket_size
      ~hopper_to_bucket_rate_per_sec:(Finite fill_rate)
      ~initial_bucket_level
      ~initial_hopper_level
  ;;

  let try_start_job t ~now =
    match try_take t ~now 1. with
    | `Asked_for_more_than_bucket_size -> assert false (* see create *)
    | `Taken -> `Start
    | `Unable ->
      begin match tokens_may_be_available_when t ~now 1. with
      | `Never_because_greater_than_bucket_size -> assert false (* see create *)
      | `When_return_to_hopper_is_called   -> `Max_concurrent_jobs_running
      | `At time                      -> `Unable_until_at_least time
      end
  ;;

  let finish_job t ~now = return_to_hopper t ~now 1.
end

module Throttle = struct
  include Throttled_rate_limiter

  let create_exn ~now ~max_concurrent_jobs =
    (* the sustained rate is immediately overridden with
       set_hopper_to_bucket_rate_per_sec *)
    let sustained_rate_unused = 1. in
    let t =
      create_exn
        ~now
        ~burst_size:max_concurrent_jobs
        ~sustained_rate_per_sec:sustained_rate_unused
        ~max_concurrent_jobs
    in
    set_hopper_to_bucket_rate_per_sec_exn t ~now Infinite;
    t
  ;;

  let try_start_job t ~now =
    match try_start_job t ~now with
    | `Start                       -> `Start
    | `Max_concurrent_jobs_running -> `Max_concurrent_jobs_running
    | `Unable_until_at_least _     -> assert false
  ;;
end


module Expert = struct

  let create_exn                            = create_exn
  let try_take                              = try_take
  let return_to_hopper                      = return_to_hopper
  let set_hopper_to_bucket_rate_per_sec_exn = set_hopper_to_bucket_rate_per_sec_exn
  let tokens_may_be_available_when          = tokens_may_be_available_when
  let set_token_target_level_exn            = set_token_target_level_exn
  let set_bucket_size_exn                   = set_bucket_size_exn

end
