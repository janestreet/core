(** Implements a token bucket based throttling rate limiter.  This module is useful for
    limiting network clients to a sensible query rate, or in any case where you have jobs
    that consume a scarce, but replenishable resource.

    In a standard token bucket there is an infinite incoming supply of tokens that fill a
    single bucket.

    This version implements a closed system where tokens move through three possible
    states:

      - in hopper
      - in bucket
      - in flight

    tokens "drop" from the hopper into the bucket at a set rate, and can be taken from
    the bucket by clients and put into flight.  Once the client is finished with whatever
    task required tokens they are responsible for moving them from "in flight" back into
    the hopper.

    Most use cases are covered by the [Token_bucket], [Throttle], and
    [Throttled_rate_limiter] modules, but the [Expert] module provides full access
    to the module internals.

    This interface is the simple, non-concurrent interface, and requires machinery on top
    to implement a specific strategy.  See Async_extra for an async-friendly implementation
    on top of this module.

    Most functions in this interface take an explicit time as an argument.  [now] is
    expected to be monotonically increasing.  [now]'s that are set in the past are
    effectively moved up to the current time of the bucket.

    This API allows one to deal with fractional tokens.  Note that tokens are divisible
    into 10k pieces, which limits the effective granularity to which your job request will
    be rounded.
*)
open! Core_kernel.Std

type t [@@deriving sexp_of]
type limiter = t [@@deriving sexp_of]

module Infinite_or_finite : sig
  type 'a t =
    | Infinite
    | Finite of 'a
  [@@deriving sexp, bin_io]
end

module Try_take_result : sig
  type t =
    | Taken
    | Unable
    | Asked_for_more_than_bucket_limit
end

module Tokens_may_be_available_result : sig
  type t =
    | At of Time.t
    | Never_because_greater_than_bucket_limit
    | When_return_to_hopper_is_called
end

(** Implements a basic token bucket based rate limiter.  Users of the throttle
    must successfully call [try_take] before doing work. *)
module Token_bucket : sig
  type t = private limiter

  val create_exn
    :  now:Time.t
    -> burst_size:float
    -> sustained_rate_per_sec:float
    -> ?initial_bucket_level:float      (** Defaults to zero *)
    -> unit
    -> t

  val try_take
    :  t
    -> now:Time.t
    -> float
    -> Try_take_result.t
end

(** Implements a basic throttle.  Users of the throttle must successfully call [start_job]
    before beginning work and must call finish_job once, and only once, when a job is
    completed. *)
module Throttle : sig
  type t = private limiter

  val create_exn
    :  now:Time.t
    -> max_concurrent_jobs:int
    -> t

  val try_start_job
    :  t
    -> now:Time.t
    -> [ `Start | `Max_concurrent_jobs_running ]

  val finish_job
    :  t
    -> now:Time.t
    -> unit
end

(** A [Throttled_rate_limiter] combines a [Token_bucket] and a [Throttle].  Unlike a
    [Token_bucket] jobs cannot consume variable numbers of tokens, but the number
    of outstanding jobs is also limited to [max_concurrent_jobs].  Like a [Throttle]
    [finish_job] must be called once, and only once when a job is completed. *)
module Throttled_rate_limiter : sig
  type t = private limiter

  val create_exn
    :  now:Time.t
    -> burst_size:int
    -> sustained_rate_per_sec:float
    -> max_concurrent_jobs:int
    -> t

  val try_start_job
    :  t
    -> now:Time.t
    -> [ `Start | `Max_concurrent_jobs_running | `Unable_until_at_least of Time.t ]

  val finish_job
    :  t
    -> now:Time.t
    -> unit
end


(** {5 common read-only operations} *)

val bucket_limit : t -> float

(** tokens available to immediately take *)
val in_bucket   : t -> now:Time.t -> float

(** tokens waiting to drop at the [hopper_to_bucket_rate_per_sec] *)
val in_hopper   : t -> now:Time.t -> float Infinite_or_finite.t

(** tokens that have been taken, but not yet returned *)
val in_flight   : t -> now:Time.t -> float

(** total number of tokens in the limiter [in_hopper + in_bucket] *)
val in_limiter  : t -> now:Time.t -> float Infinite_or_finite.t

(** total number of tokens in the entire system [in_hopper + in_bucket + in_flight] *)
val in_system   : t -> now:Time.t -> float Infinite_or_finite.t

val hopper_to_bucket_rate_per_sec : t -> float Infinite_or_finite.t


(** {5 expert operations} *)
module Expert : sig

  (**
     - [time] is the reference time that other time accepting functions will use when
     they adjust [now].  It is almost always correct to set this to Time.now.

     - [hopper_to_bucket_rate_per_sec] bounds the maximum rate at which tokens fall from
     the hopper into the bucket where they can be taken.

     - [bucket_limit] bounds the number of tokens that the lower bucket can hold.  This
     corresponds to the maximum burst in a standard token bucket setup.

     - [in_flight_limit] bounds the number of tokens that can be in flight.  This
     corresponds to a running job limit/throttle.

     - [initial_hopper_level] sets the number of tokens placed into the hopper when the
     [Limiter] is created.

     - [initial_bucket_level] sets the number of tokens placed into the bucket when the
     [Limiter] is created.  If this amount exceeds the bucket size it will be silently
     limited to [bucket_limit].

     These tunables can be combined in several ways:

    - to produce a simple rate limiter, where the hopper is given an infinite number of
     tokens and clients simply take tokens as they are delivered to the bucket.

    - to produce a rate_limiter that respects jobs that are more than instantaneous.  In
     this case [initial_hopper_level + initial_bucket_level] should be bounded and clients
     hold tokens for the duration of their work.

    - to produce a throttle that doesn't limit the rate of jobs at all, but always keeps a
     max of n jobs running.  In this case [hopper_to_bucket_rate_per_sec] should be
     infinite but [in_flight_limit] should be bounded to the upper job rate.

    In all cases above throttling and rate limiting combine nicely when the unit of work
    for both is the same (e.g. one token per message).  If the unit of work is different
    (e.g. rate limit base on a number of tokens equal to message size, but throttle base
    on simple message count) then a single [t] probably cannot be used to get the correct
    behavior, and two instances should be used with tokens taken from both. *)
  val create_exn
    :  now:Time.t
    -> hopper_to_bucket_rate_per_sec:float Infinite_or_finite.t
    -> bucket_limit:float
    -> in_flight_limit:float Infinite_or_finite.t
    -> initial_bucket_level:float
    -> initial_hopper_level:float Infinite_or_finite.t
    -> t

  (** returns the earliest time when the requested number of tokens could possibly be
      delivered.  There is no guarantee that the requested number of tokens will actually
      be available at this time.  You must call [try_take] to actually attempt to take the
      tokens. *)
  val tokens_may_be_available_when
    :  t
    -> now:Time.t
    -> float
    -> Tokens_may_be_available_result.t

  (** attempts to take the given number of tokens from the bucket. [try_take t ~now n]
      succeeds iff [in_bucket t ~now >= n]. *)
  val try_take
    :  t
    -> now:Time.t
    -> float
    -> Try_take_result.t

  (** return the given number of tokens to the hopper.  These tokens will fill the tokens
      available to [try_take] at the [fill_rate].  Note that if [return] is called on more
      tokens then have actually been removed, this can cause the number of concurrent jobs
      to exceed [max_concurrent_jobs].

      Note that, due to rounding issues, one should only return precisely the number of
      tokens that were previously taken.  Returning a sum of different values taken
      previously may or may not work precisely, depending on the specific floating point
      numbers used.  *)
  val return_to_hopper : t -> now:Time.t -> float -> unit

  val set_hopper_to_bucket_rate_per_sec_exn
    :  t
    -> now:Time.t
    -> float Infinite_or_finite.t
    -> unit
end

include Invariant.S with type t := t
