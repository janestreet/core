open Core_kernel.Std
open Int.Replace_polymorphic_compare

module Infinite_or_finite = struct
  module T = struct
    type 'a t =
      | Infinite
      | Finite of 'a
      [@@deriving sexp, bin_io]
  end
  include T

  let compare compare t1 t2 =
    match t1, t2 with
    | Infinite, Infinite -> 0
    | Infinite, Finite _ -> 1
    | Finite _, Infinite -> -1
    | Finite a, Finite b -> compare a b
  ;;
end

(** Mutable version of Infinite_or_finite, for internal use, to avoid allocation *)
module Iofm : sig
  type 'a t [@@deriving sexp_of]

  val infinite : unit -> 'a t
  val finite   : 'a   -> 'a t

  val is_infinite : 'a t -> bool
  val is_finite   : 'a t -> bool

  val set_infinite   : 'a t -> unit
  val set_finite     : 'a t -> 'a -> unit
  val get_finite_exn : 'a t -> 'a

  val to_ordinary : 'a t -> 'a Infinite_or_finite.t
  val of_ordinary : 'a Infinite_or_finite.t -> 'a t
end = struct
  type 'a t = 'a Moption.t [@@deriving sexp_of]

  let infinite () = Moption.create ()

  let finite v    =
    let t = Moption.create () in
    Moption.set_some t v;
    t
  ;;

  let is_infinite    = Moption.is_none
  let is_finite      = Moption.is_some
  let set_infinite   = Moption.set_none
  let set_finite     = Moption.set_some
  let get_finite_exn = Moption.get_some_exn

  let [@inline always] to_ordinary t : _ Infinite_or_finite.t =
    if Moption.is_none t
    then Infinite
    else Finite (Moption.get_some_exn t)
  ;;

  let [@inline always] of_ordinary (ext : _ Infinite_or_finite.t) =
    match ext with
    | Infinite -> infinite ()
    | Finite v -> finite v
  ;;
end
open Infinite_or_finite.T

module Try_take_result = struct
  type t =
    | Taken
    | Unable
    | Asked_for_more_than_bucket_limit
end

module Try_return_to_bucket_result = struct
  type t =
    | Returned_to_bucket
    | Unable
end

module Tokens_may_be_available_result = struct
  type t =
    | At of Time_ns.t
    | Never_because_greater_than_bucket_limit
    | When_return_to_hopper_is_called
end

module Time_ns = struct
  include Time_ns

  let sexp_of_t = Time_ns.Alternate_sexp.sexp_of_t
end

type t =
  { start_time                    : Time_ns.t
  (** The current time of the rate limiter.  Note that when this is moved forward,
      [in_hopper] must be updated accordingly. *)
  ; mutable time                  : Time_ns.t
  (** the amount of time that has passed expressed in token terms, since start_time. *)
  ; time_in_token_space           : int Iofm.t
  (** number of tokens in the bucket *)
  ; mutable in_bucket             : int
  (** number of tokens in the hopper.  May be [inf] *)
  ; in_hopper                     : int Iofm.t
  (** Everything that has been taken from bucket but not returned to hopper *)
  ; mutable in_flight             : int
  (** maximum size allowable in the bucket *)
  ; mutable bucket_limit          : int
  (** maximum size allowable in flight *)
  ; in_flight_limit               : int Iofm.t
  (** rate at which tokens "fall" from the hopper into the bucket *)
  ; hopper_to_bucket_rate_per_ns  : Float.t Iofm.t
  }
  [@@deriving sexp_of, fields]

let fill_rate_must_be_positive fill_rate =
  if Iofm.is_finite fill_rate
  then begin
    let rate = Iofm.get_finite_exn fill_rate in
    if Float.(<) rate Float.zero
    then raise_s [%message
           "hopper_to_bucket_rate_per_ns must be >= 0"
             (rate : Float.t) ];
  end
;;

let in_system t =
  if Iofm.is_infinite t.in_hopper
  then Infinite
  else Finite (t.in_flight + Iofm.get_finite_exn t.in_hopper + t.in_bucket)
;;

let invariant t =
  fill_rate_must_be_positive t.hopper_to_bucket_rate_per_ns;

  (* bucket is limited to size *)
  if t.in_bucket > t.bucket_limit
  then failwithf !"amount in_bucket (%{Int}) cannot be greater than bucket_limit \
                   (%{Int})"
         t.in_bucket t.bucket_limit ();

  (* sizes must be positive *)
  if t.bucket_limit <= 0
  then failwithf !"bucket_limit (burst_size) (%{Int}) must be > 0" t.bucket_limit ();

  if t.in_bucket < 0
  then failwithf !"in_bucket (%{Int}) must be >= 0." t.in_bucket ();

  begin match Iofm.to_ordinary t.in_hopper with
  | Infinite         -> ()
  | Finite in_hopper ->
    if in_hopper < 0
    then failwithf !"in_hopper (%{Int}) must be >= 0." in_hopper ();
  end;

  if t.in_flight < 0
  then failwithf !"in_flight (%{Int}) must be >= 0." t.in_flight ();

  begin match
    Iofm.to_ordinary t.hopper_to_bucket_rate_per_ns
  , Iofm.to_ordinary t.time_in_token_space
  with
  | Infinite, Finite _
  | Finite _, Infinite ->
    failwith
      "hopper_to_bucket_rate_per_sec can only be infinite if time_in_token_space is \
       infinite";
  | Infinite, Infinite
  | Finite _, Finite _ -> ()
  end;
;;

type limiter = t [@@deriving sexp_of]

let create_exn
      ~now
      ~hopper_to_bucket_rate_per_sec
      ~bucket_limit
      ~in_flight_limit
      ~initial_bucket_level
      ~initial_hopper_level
  =
  let in_hopper = Iofm.of_ordinary initial_hopper_level in
  let time_in_token_space =
    match hopper_to_bucket_rate_per_sec with
    | Infinite -> Iofm.infinite ()
    | Finite _ -> Iofm.finite 0
  in
  let hopper_to_bucket_rate_per_ns =
    match hopper_to_bucket_rate_per_sec with
    | Infinite -> Iofm.infinite ()
    | Finite rate_per_sec -> Iofm.finite (rate_per_sec /. 1E9)
  in
  let t =
    { start_time          = now
    ; time                = now
    ; time_in_token_space
    ; in_bucket           = initial_bucket_level
    ; in_hopper
    ; in_flight           = 0
    ; bucket_limit        = bucket_limit
    ; in_flight_limit     = Iofm.of_ordinary in_flight_limit
    ; hopper_to_bucket_rate_per_ns
    }
  in
  invariant t;
  t
;;

let move_from_hopper_to_bucket t max_move =
  let space_in_bucket = t.bucket_limit - t.in_bucket in
  let actual_move     = Int.min max_move space_in_bucket in
  if actual_move > 0 then begin
    t.in_bucket <- t.in_bucket + actual_move;
    if Iofm.is_finite t.in_hopper
    then Iofm.set_finite t.in_hopper (Iofm.get_finite_exn t.in_hopper - actual_move)
  end
;;

(* Computes the number of tokens that would have dropped since start_time given the
   current rate *)
let update_time_in_token_space (t : t) =
  (* if it's infinite then time_in_token_space was set to infinite in [create_exn] *)
  if Iofm.is_finite t.hopper_to_bucket_rate_per_ns
  then begin
    let tokens_per_ns = Iofm.get_finite_exn t.hopper_to_bucket_rate_per_ns in
    let time_elapsed_since_start_in_ns =
      Time_ns.Span.to_ns (Time_ns.diff t.time t.start_time)
    in
    (* this will raise when there is an int overflow, but in a way that will be annoying
       to understand/track down if it fails.  This comment is here to help while keeping
       the common case fast. *)
    let time_in_token_space =
      Float.iround_down_exn (time_elapsed_since_start_in_ns *. tokens_per_ns)
    in
    Iofm.set_finite t.time_in_token_space time_in_token_space
  end
;;

(* advances [t]s notion of time, moving tokens from the hopper down into the bucket as
   dictated by the passage of time and the hopper_to_bucket_rate_per_ns. *)
let advance_time =
  (* Just updates [t] to match the current value of [t.time]. We write it this way to make
     it clear that now is not directly used in update_tokens. *)
  let update_tokens t =
    if Iofm.is_infinite t.time_in_token_space
    then begin
      let max_move =
        if Iofm.is_infinite t.in_hopper
        then t.bucket_limit
        else Iofm.get_finite_exn t.in_hopper
      in
      move_from_hopper_to_bucket t max_move
    end else begin
      let previous_time_in_token_space = Iofm.get_finite_exn t.time_in_token_space in
      update_time_in_token_space t;
      let new_time_in_token_space = Iofm.get_finite_exn t.time_in_token_space in
      let amount_that_could_fall =
        (* this will always be >= 0 because time always moves forward *)
        new_time_in_token_space - previous_time_in_token_space
      in
      let max_move =
        if Iofm.is_infinite t.in_hopper
        then amount_that_could_fall
        else Int.min (Iofm.get_finite_exn t.in_hopper) amount_that_could_fall
      in
      move_from_hopper_to_bucket t max_move
    end
  in
  fun t ~now ->
    if Time_ns.(>) now t.time then t.time <- now;
    (* this has to be run even if time doesn't move foward to handle the case of an
       Infinite hopper to bucket drop rate.  In that case tokens in the hopper may
       instantaneously move into the bucket. *)
    update_tokens t
;;

let can_put_n_tokens_in_flight t ~n =
  if Iofm.is_infinite t.in_flight_limit
  then true
  else t.in_flight + n <= Iofm.get_finite_exn t.in_flight_limit
;;

let try_take t ~now amount : Try_take_result.t =
  advance_time t ~now;
  if not (can_put_n_tokens_in_flight t ~n:amount)
  then Unable
  else if amount > t.bucket_limit
  then Asked_for_more_than_bucket_limit
  else if amount > t.in_bucket
  then Unable
  else begin
    t.in_bucket <- t.in_bucket - amount;
    t.in_flight <- t.in_flight + amount;
    Taken
  end
;;

let return_to_hopper t ~now amount =
  if amount < 0
  then failwithf !"return_to_hopper passed a negative amount (%{Int})" amount ();
  if amount > t.in_flight
  then failwithf !"return_to_hopper passed an amount (%{Int}) > in_flight (%{Int})"
         amount t.in_flight ();
  advance_time t ~now;
  t.in_flight <- t.in_flight - amount;
  if Iofm.is_finite t.in_hopper
  then Iofm.set_finite t.in_hopper (Iofm.get_finite_exn t.in_hopper + amount)
;;

let try_return_to_bucket t ~now amount : Try_return_to_bucket_result.t =
  advance_time t ~now;
  let space_in_bucket = t.bucket_limit - t.in_bucket in
  if amount < 0
  || amount > t.in_flight
  || amount > space_in_bucket
  then Unable
  else begin
    t.in_flight <- t.in_flight - amount;
    t.in_bucket <- t.in_bucket + amount;
    Returned_to_bucket
  end
;;


let tokens_may_be_available_when t ~now amount : Tokens_may_be_available_result.t =
  if not (can_put_n_tokens_in_flight t ~n:amount) then
    When_return_to_hopper_is_called
  else if amount > t.bucket_limit then
    Never_because_greater_than_bucket_limit
  else begin
    advance_time t ~now;
    let amount_missing = amount - t.in_bucket in
    if amount_missing <= 0
    then At t.time
    else begin
      if Iofm.is_infinite t.hopper_to_bucket_rate_per_ns
      then When_return_to_hopper_is_called
      else begin
        let tokens_per_ns = Iofm.get_finite_exn t.hopper_to_bucket_rate_per_ns in
        let min_seconds_left =
          Float.of_int amount_missing /. (tokens_per_ns *. 1E9)
        in
        let (min_time : Tokens_may_be_available_result.t) =
          At (Time_ns.add t.time (Time_ns.Span.of_sec min_seconds_left))
        in
        if Iofm.is_infinite t.in_hopper
        then min_time
        else begin
          if amount_missing > Iofm.get_finite_exn t.in_hopper
          then When_return_to_hopper_is_called
          else min_time
        end
      end
    end
  end
;;

let in_bucket t ~now =
  advance_time t ~now;
  t.in_bucket
;;

let in_hopper t ~now =
  advance_time t ~now;
  Iofm.to_ordinary t.in_hopper
;;

let in_flight t ~now =
  advance_time t ~now;
  t.in_flight
;;

let in_limiter t ~now =
  match in_hopper t ~now with
  | Infinite         -> Infinite
  | Finite in_hopper -> Finite ((in_bucket t ~now) + in_hopper)
;;

let in_system t ~now =
  advance_time t ~now;
  in_system t
;;

let bucket_limit t = t.bucket_limit

let hopper_to_bucket_rate_per_sec t =
  if Iofm.is_infinite t.hopper_to_bucket_rate_per_ns
  then Infinite
  else Finite (Iofm.get_finite_exn t.hopper_to_bucket_rate_per_ns *. 1E9)
;;

module Token_bucket = struct
  type t = limiter

  let create_exn
        ~now
        ~burst_size:bucket_limit
        ~sustained_rate_per_sec:fill_rate
        ?(initial_bucket_level = 0)
        ()
    =
    create_exn
      ~now
      ~bucket_limit
      ~in_flight_limit:Infinite
      ~hopper_to_bucket_rate_per_sec:(Finite fill_rate)
      ~initial_bucket_level
      ~initial_hopper_level:Infinite
  ;;

  let try_take = try_take
end

module Throttled_rate_limiter = struct
  type t = limiter

  let create_exn
        ~now
        ~burst_size
        ~sustained_rate_per_sec:fill_rate
        ~max_concurrent_jobs
    =
    let bucket_limit         = burst_size in
    let max_concurrent_jobs  = max_concurrent_jobs in
    let initial_bucket_level = Int.min bucket_limit max_concurrent_jobs in
    let initial_hopper_level =
      Finite (Int.max
                0
                (max_concurrent_jobs - initial_bucket_level))
    in
    create_exn
      ~now
      ~bucket_limit
      ~in_flight_limit:Infinite
      ~hopper_to_bucket_rate_per_sec:(Finite fill_rate)
      ~initial_bucket_level
      ~initial_hopper_level
  ;;

  let try_start_job t ~now =
    match try_take t ~now 1 with
    | Asked_for_more_than_bucket_limit -> assert false (* see create *)
    | Taken                            -> `Start
    | Unable                           ->
      begin match tokens_may_be_available_when t ~now 1 with
      | Never_because_greater_than_bucket_limit -> assert false (* see create *)
      | When_return_to_hopper_is_called         -> `Max_concurrent_jobs_running
      | At time                                 -> `Unable_until_at_least time
      end
  ;;

  let finish_job t ~now = return_to_hopper t ~now 1
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
    Iofm.set_infinite t.hopper_to_bucket_rate_per_ns;
    Iofm.set_infinite t.time_in_token_space;
    (* Since we set the hopper rate to infinite then the bucket can immediately be
       filled. *)
    t.in_bucket <- t.bucket_limit;
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
  let try_return_to_bucket                  = try_return_to_bucket
  let tokens_may_be_available_when          = tokens_may_be_available_when

end
