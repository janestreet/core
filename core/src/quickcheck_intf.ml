(** Quickcheck is a library that uses predicate-based tests and pseudo-random inputs to
    automate testing.

    For examples see {e lib/base_quickcheck/examples}.
*)

open! Import
open Base_quickcheck

module type Generator = sig
  (** An ['a t] a generates values of type ['a] with a specific probability distribution.

      Generators are constructed as functions that produce a value from a splittable
      pseudorandom number generator (see [Splittable_random]), with a [~size] argument
      threaded through to bound the size of the result value and the depth of recursion.

      There is no prescribed semantics for [size] other than that it must be non-negative.
      Non-recursive generators are free to ignore it, and recursive generators need only
      make sure it decreases in recursive calls and that recursion bottoms out at 0. *)

  type +'a t = 'a Generator.t

  val create : (size:int -> random:Splittable_random.t -> 'a) -> 'a t
  val generate : 'a t -> size:int -> random:Splittable_random.t -> 'a

  (** Generators form a monad.  [t1 >>= fun x -> t2] replaces each value [x] in [t1] with
      the values in [t2]; each value's probability is the product of its probability in
      [t1] and [t2].

      This can be used to form distributions of related values.  For instance, the
      following expression creates a distribution of pairs [x,y] where [x <= y]:

      {[
        Int.gen
        >>= fun x ->
        Int.gen_incl x Int.max_value
        >>| fun y ->
        x, y
      ]}
  *)
  include Monad.S with type 'a t := 'a t

  include Applicative.S with type 'a t := 'a t

  (** [size = create (fun ~size _ -> size)] *)
  val size : int t

  (** [with_size t ~size = create (fun ~size:_ random -> generate t ~size random)] *)
  val with_size : 'a t -> size:int -> 'a t

  val bool : bool t
  val char : char t
  val char_digit : char t
  val char_lowercase : char t
  val char_uppercase : char t
  val char_alpha : char t
  val char_alphanum : char t
  val char_print : char t
  val char_whitespace : char t
  val singleton : 'a -> 'a t
  val doubleton : 'a -> 'a -> 'a t

  (** Produce any of the given values, weighted equally.

      [of_list [ v1 ; ... ; vN ] = union [ singleton v1 ; ... ; singleton vN ]] *)
  val of_list : 'a list -> 'a t

  (** Combine arbitrary generators, weighted equally.

      [ union [ g1 ; ... ; gN ] = weighted_union [ (1.0, g1) ; ... ; (1.0, gN) ] ] *)
  val union : 'a t list -> 'a t

  (** Generator for the values from a potentially infinite sequence.  Chooses each value
      with probability [p], or continues with probability [1-p].  Must satisfy [0. < p &&
      p <= 1.]. *)
  val of_sequence : p:float -> 'a Sequence.t -> 'a t

  val tuple2 : 'a t -> 'b t -> ('a * 'b) t
  val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  val tuple5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t

  val tuple6
    :  'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> 'e t
    -> 'f t
    -> ('a * 'b * 'c * 'd * 'e * 'f) t

  val variant2 : 'a t -> 'b t -> [ `A of 'a | `B of 'b ] t
  val variant3 : 'a t -> 'b t -> 'c t -> [ `A of 'a | `B of 'b | `C of 'c ] t

  val variant4
    :  'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd ] t

  val variant5
    :  'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> 'e t
    -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd | `E of 'e ] t

  val variant6
    :  'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> 'e t
    -> 'f t
    -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd | `E of 'e | `F of 'f ] t

  (** [geometric init ~p] produces a geometric distribution (think "radioactive decay")
      that produces [init] with probability [p], and otherwise effectively recursively
      chooses from [geometric (init+1) ~p]. The implementation can be more efficient than
      actual recursion. Must satisfy [0. <= p && p <= 1.]. *)
  val geometric : int -> p:float -> int t

  (** [small_non_negative_int] produces a non-negative int of a tractable size, e.g.
      allocating a value of this size should not run out of memory. *)
  val small_non_negative_int : int t

  (** [small_positive_int] produces a positive int of a tractable size, e.g. allocating a
      value of this size should not run out of memory. *)
  val small_positive_int : int t

  (** Generators for functions; take observers for inputs and a generator for outputs. *)
  val fn : 'a Observer.t -> 'b t -> ('a -> 'b) t

  val fn2 : 'a Observer.t -> 'b Observer.t -> 'c t -> ('a -> 'b -> 'c) t

  val fn3
    :  'a Observer.t
    -> 'b Observer.t
    -> 'c Observer.t
    -> 'd t
    -> ('a -> 'b -> 'c -> 'd) t

  val fn4
    :  'a Observer.t
    -> 'b Observer.t
    -> 'c Observer.t
    -> 'd Observer.t
    -> 'e t
    -> ('a -> 'b -> 'c -> 'd -> 'e) t

  val fn5
    :  'a Observer.t
    -> 'b Observer.t
    -> 'c Observer.t
    -> 'd Observer.t
    -> 'e Observer.t
    -> 'f t
    -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f) t

  val fn6
    :  'a Observer.t
    -> 'b Observer.t
    -> 'c Observer.t
    -> 'd Observer.t
    -> 'e Observer.t
    -> 'f Observer.t
    -> 'g t
    -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) t

  (** Generator for comparison functions; result is guaranteed to be a partial order. *)
  val compare_fn : 'a Observer.t -> ('a -> 'a -> int) t

  (** Generator for equality functions; result is guaranteed to be an equivalence
      relation. *)
  val equal_fn : 'a Observer.t -> ('a -> 'a -> bool) t

  (** [filter_map t ~f] produces [y] for every [x] in [t] such that [f x = Some y]. *)
  val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

  (** [filter t ~f] produces every [x] in [t] such that [f x = true]. *)
  val filter : 'a t -> f:('a -> bool) -> 'a t

  (** Generator for recursive data type with multiple clauses. At size 0, chooses only
      among the non-recursive cases; at sizes greater than 0, chooses among non-recursive
      and recursive cases, calling the recursive cases with decremented size.

      {[
        type tree = Leaf | Node of tree * int * tree;;
        recursive_union [return Leaf] ~f:(fun self ->
          [let%map left = self
           and int = Int.gen
           and right = self
           in Node (left, int, right)])
      ]} *)
  val recursive_union : 'a t list -> f:('a t -> 'a t list) -> 'a t

  (** Like [recursive_union], with the addition of non-uniform weights for each clause. *)
  val weighted_recursive_union
    :  (float * 'a t) list
    -> f:('a t -> (float * 'a t) list)
    -> 'a t

  (** Fixed-point generator. Use [size] to bound the size of the value and the depth of
      the recursion. There is no prescribed semantics for [size] except that it must be
      non-negative. For example, the following produces a naive generator for natural
      numbers:

      {[
        fixed_point (fun self ->
          match%bind size with
          | 0 -> singleton 0
          | n -> with_size self ~size:(n-1) >>| Int.succ)
      ]}
  *)
  val fixed_point : ('a t -> 'a t) -> 'a t

  (** [weighted_union alist] produces a generator that combines the distributions of each
      [t] in [alist] with the associated weights, which must be finite positive floating
      point values. *)
  val weighted_union : (float * 'a t) list -> 'a t

  (** [of_fun f] produces a generator that lazily applies [f].

      It is recommended that [f] not be memoized.  Instead, spread out the work of
      generating a whole distribution over many [of_fun] calls combined with
      [weighted_union].  This allows lazily generated generators to be garbage collected
      after each test and the relevant portions cheaply recomputed in subsequent tests,
      rather than accumulating without bound over time. *)
  val of_fun : (unit -> 'a t) -> 'a t

  (** Generators for lists, choosing each element independently from the given element
      generator. [list] and [list_non_empty] distribute [size] among the list length and
      the sizes of each element. [list_non_empty] never generates the empty list.
      [list_with_length] generates lists of the given length, and distributes [size] among
      the sizes of the elements. *)
  val list : 'a t -> 'a list t

  val list_non_empty : 'a t -> 'a list t
  val list_with_length : int -> 'a t -> 'a list t
end

module type Deriving_hash = sig
  type t [@@deriving hash]
end

module type Observer = sig
  (** An ['a Quickcheck.Observer.t] represents a hash function on ['a].  Observers are
      used to construct distributions of random functions; see [Quickcheck.Generator.fn].

      Like generators, observers have a [~size] argument that is threaded through to bound
      the depth of recursion in potentially infinite cases.  For finite values, [size] can
      be ignored.

      For hashable types, one can construct an observer using [of_hash].  For other types,
      use the built-in observers and observer combinators below, or use [create] directly.
  *)

  type -'a t = 'a Observer.t

  val create : ('a -> size:int -> hash:Hash.state -> Hash.state) -> 'a t
  val observe : 'a t -> 'a -> size:int -> hash:Hash.state -> Hash.state

  (** [of_hash] creates an observer for any hashable type. *)
  val of_hash : (module Deriving_hash with type t = 'a) -> 'a t

  val bool : bool t
  val char : char t

  (** [doubleton f] maps values to two "buckets" (as described in [t] above),
      depending on whether they satisfy [f]. *)
  val doubleton : ('a -> bool) -> 'a t

  (** [enum n ~f] maps values to [n] buckets, where [f] produces the index for a bucket
      from [0] to [n-1] for each value. *)
  val enum : int -> f:('a -> int) -> 'a t

  (** [of_list list ~equal] maps values in [list] to separate buckets, and compares
      observed values to the elements of [list] using [equal]. *)
  val of_list : 'a list -> equal:('a -> 'a -> bool) -> 'a t

  (** Fixed point observer for recursive types. For example:

      {[
        let sexp_obs =
          fixed_point (fun sexp_t ->
            unmap (variant2 string (list sexp_t))
              ~f:(function
                | Sexp.Atom atom -> `A atom
                | Sexp.List list -> `B list))
      ]}
  *)
  val fixed_point : ('a t -> 'a t) -> 'a t

  val variant2 : 'a t -> 'b t -> [ `A of 'a | `B of 'b ] t
  val variant3 : 'a t -> 'b t -> 'c t -> [ `A of 'a | `B of 'b | `C of 'c ] t

  val variant4
    :  'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd ] t

  val variant5
    :  'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> 'e t
    -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd | `E of 'e ] t

  val variant6
    :  'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> 'e t
    -> 'f t
    -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd | `E of 'e | `F of 'f ] t

  (** [of_predicate t1 t2 ~f] combines [t1] and [t2], where [t1] observes values that
      satisfy [f] and [t2] observes values that do not satisfy [f]. *)
  val of_predicate : 'a t -> 'a t -> f:('a -> bool) -> 'a t

  (** [comparison ~compare ~eq ~lt ~gt] combines observers [lt] and [gt], where [lt]
      observes values less than [eq] according to [compare], and [gt] observes values
      greater than [eq] according to [compare]. *)
  val comparison : compare:('a -> 'a -> int) -> eq:'a -> lt:'a t -> gt:'a t -> 'a t

  (** maps all values to a single bucket. *)
  val singleton : unit -> _ t

  (** [unmap t ~f] applies [f] to values before observing them using [t]. *)
  val unmap : 'a t -> f:('b -> 'a) -> 'b t

  val tuple2 : 'a t -> 'b t -> ('a * 'b) t
  val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  val tuple5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t

  val tuple6
    :  'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> 'e t
    -> 'f t
    -> ('a * 'b * 'c * 'd * 'e * 'f) t

  (** Observer for function type.  [fn gen t] observes a function by generating random
      inputs from [gen], applying the function, and observing the output using [t]. *)
  val fn : 'a Generator.t -> 'b t -> ('a -> 'b) t

  (** [of_fun f] produces an observer that lazily applies [f].

      It is recommended that [f] should not do a lot of expensive work and should not be
      memoized.  Instead, spread out the work of generating an observer over many [of_fun]
      calls combined with, e.g., [variant] or [tuple].  This allows lazily generated
      observers to be garbage collected after each test and the relevant portions cheaply
      recomputed in subsequent tests, rather than accumulating without bound over time. *)
  val of_fun : (unit -> 'a t) -> 'a t
end

module type Shrinker = sig
  (** A ['a Quickcheck.Shrinker.t] takes a value of type ['a] and produces similar values
      that are smaller by some metric.

      The defined shrinkers generally try to make a single change for each value based on
      the assumption that the first resulting value that preserves the desired property
      will be used to create another sequence of shrunk values.

      Within [Quickcheck.test] the shrinker is used as described above.

      Shrinkers aim to aid understanding of what's causing an error by reducing the input
      down to just the elements making it fail.  The default shrinkers remove elements of
      compound structures, but leave atomic values alone.  For example, the default list
      shrinker tries removing elements from the list, but the default int shrinker does
      nothing.  This default strikes a balance between performance and precision.
      Individual tests can use different shrinking behavior as necessary.

      See lib/base_quickcheck/examples/shrinker_example.ml for some example shrinkers.
  *)

  type 'a t = 'a Shrinker.t

  val shrink : 'a t -> 'a -> 'a Sequence.t
  val create : ('a -> 'a Sequence.t) -> 'a t
  val empty : unit -> 'a t
  val bool : bool t
  val char : char t
  val map : 'a t -> f:('a -> 'b) -> f_inverse:('b -> 'a) -> 'b t
  val filter : 'a t -> f:('a -> bool) -> 'a t

  (** Filters and maps according to [f], and provides input to [t] via [f_inverse]. Only
      the [f] direction produces options, intentionally. *)
  val filter_map : 'a t -> f:('a -> 'b option) -> f_inverse:('b -> 'a) -> 'b t

  val tuple2 : 'a t -> 'b t -> ('a * 'b) t
  val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  val tuple5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t

  val tuple6
    :  'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> 'e t
    -> 'f t
    -> ('a * 'b * 'c * 'd * 'e * 'f) t

  val variant2 : 'a t -> 'b t -> [ `A of 'a | `B of 'b ] t
  val variant3 : 'a t -> 'b t -> 'c t -> [ `A of 'a | `B of 'b | `C of 'c ] t

  val variant4
    :  'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd ] t

  val variant5
    :  'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> 'e t
    -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd | `E of 'e ] t

  val variant6
    :  'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> 'e t
    -> 'f t
    -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd | `E of 'e | `F of 'f ] t

  (** [fixed_point] assists with shrinking structures recursively. Its advantage over
      directly using [rec] in the definition of the shrinker is that it causes lazy
      evaluation where possible. *)
  val fixed_point : ('a t -> 'a t) -> 'a t
end

module type S = sig
  type t

  val quickcheck_generator : t Generator.t
  val quickcheck_observer : t Observer.t
  val quickcheck_shrinker : t Shrinker.t
end

module type S1 = sig
  type 'a t

  val quickcheck_generator : 'a Generator.t -> 'a t Generator.t
  val quickcheck_observer : 'a Observer.t -> 'a t Observer.t
  val quickcheck_shrinker : 'a Shrinker.t -> 'a t Shrinker.t
end

module type S2 = sig
  type ('a, 'b) t

  val quickcheck_generator : 'a Generator.t -> 'b Generator.t -> ('a, 'b) t Generator.t
  val quickcheck_observer : 'a Observer.t -> 'b Observer.t -> ('a, 'b) t Observer.t
  val quickcheck_shrinker : 'a Shrinker.t -> 'b Shrinker.t -> ('a, 'b) t Shrinker.t
end

module type S_range = sig
  include S

  (** [gen_incl lower_bound upper_bound] produces values between [lower_bound] and
      [upper_bound], inclusive.  It uses an ad hoc distribution that stresses boundary
      conditions more often than a uniform distribution, while still able to produce any
      value in the range.  Raises if [lower_bound > upper_bound]. *)
  val gen_incl : t -> t -> t Generator.t

  (** [gen_uniform_incl lower_bound upper_bound] produces a generator for values uniformly
      distributed between [lower_bound] and [upper_bound], inclusive.  Raises if
      [lower_bound > upper_bound]. *)
  val gen_uniform_incl : t -> t -> t Generator.t
end

module type S_int = sig
  include S_range

  (** [gen_log_uniform_incl lower_bound upper_bound] produces a generator for values
      between [lower_bound] and [upper_bound], inclusive, where the number of bits used to
      represent the value is uniformly distributed.  Raises if [(lower_bound < 0) ||
      (lower_bound > upper_bound)]. *)
  val gen_log_uniform_incl : t -> t -> t Generator.t

  (** [gen_log_incl lower_bound upper_bound] is like [gen_log_uniform_incl], but weighted
      slightly more in favor of generating [lower_bound] and [upper_bound]
      specifically. *)
  val gen_log_incl : t -> t -> t Generator.t
end

(** [seed] specifies how to initialize a pseudo-random number generator.  When multiple
    tests share a deterministic seed, they each get a separate copy of the random
    generator's state; random choices in one test do not affect those in another.  The
    nondeterministic seed causes a fresh random state to be generated nondeterministically
    for each test. *)
type seed =
  [ `Deterministic of string
  | `Nondeterministic
  ]

type shrink_attempts =
  [ `Exhaustive
  | `Limit of int
  ]

module type Quickcheck_config = sig
  (** [default_seed] is used initialize the pseudo-random generator that chooses random
      values from generators, in each test that is not provided its own seed. *)
  val default_seed : seed

  (** [default_sizes] determines the default sequence of sizes used in generating
      values. *)
  val default_sizes : int Sequence.t

  (** [default_trial_count] determines the number of trials per test, except in tests
      that explicitly override it. *)
  val default_trial_count : int

  (** [default_can_generate_trial_count] determines the number of trials used in attempts
      to generate satisfying values, except in tests that explicitly override it. *)
  val default_can_generate_trial_count : int

  (** [default_shrink_attempts] determines the number of attempts at shrinking
      when running [test] or [iter] with [~shrinker] and without
      [~shrink_attempts] *)
  val default_shrink_attempts : shrink_attempts
end

module type Quickcheck_configured = sig
  include Quickcheck_config

  (** [random_value gen] produces a single value chosen from [gen] using [seed]. *)
  val random_value : ?seed:seed -> ?size:int -> 'a Generator.t -> 'a

  (** [iter gen ~f] runs [f] on up to [trials] different values generated by [gen]. It
      stops successfully after [trials] successful trials or if [gen] runs out of values.
      It raises an exception if [f] raises an exception. *)
  val iter
    :  ?seed:seed
    -> ?sizes:int Sequence.t
    -> ?trials:int
    -> 'a Generator.t
    -> f:('a -> unit)
    -> unit

  (** [test gen ~f] is like [iter], with optional concrete [examples] that are tested
      before values from [gen], and additional information provided on failure. If [f]
      raises an exception and [sexp_of] is provided, the exception is re-raised with a
      description of the random input that triggered the failure. If [f] raises an
      exception and [shrinker] is provided, it will be used to attempt to shrink the value
      that caused the exception with re-raising behaving the same as for unshrunk inputs.
  *)
  val test
    :  ?seed:seed
    -> ?sizes:int Sequence.t
    -> ?trials:int
    -> ?shrinker:'a Shrinker.t
    -> ?shrink_attempts:shrink_attempts
    -> ?sexp_of:('a -> Base.Sexp.t)
    -> ?examples:'a list
    -> 'a Generator.t
    -> f:('a -> unit)
    -> unit

  (** [test_or_error] is like [test], except failure is determined using [Or_error.t]. Any
      exceptions raised by [f] are also treated as failures. *)
  val test_or_error
    :  ?seed:seed
    -> ?sizes:int Sequence.t
    -> ?trials:int
    -> ?shrinker:'a Shrinker.t
    -> ?shrink_attempts:shrink_attempts
    -> ?sexp_of:('a -> Base.Sexp.t)
    -> ?examples:'a list
    -> 'a Generator.t
    -> f:('a -> unit Or_error.t)
    -> unit Or_error.t

  (** [test_can_generate gen ~f] is useful for testing [gen] values, to make sure they can
      generate useful examples. It tests [gen] by generating up to [trials] values and
      passing them to [f]. Once a value satisfies [f], the iteration stops. If no values
      satisfy [f], [test_can_generate] raises an exception. If [sexp_of] is provided, the
      exception includes all of the generated values. *)
  val test_can_generate
    :  ?seed:seed
    -> ?sizes:int Sequence.t
    -> ?trials:int
    -> ?sexp_of:('a -> Base.Sexp.t)
    -> 'a Generator.t
    -> f:('a -> bool)
    -> unit

  (** [test_distinct_values gen] is useful for testing [gen] values, to make sure they
      create sufficient distinct values. It tests [gen] by generating up to [trials]
      values and making sure at least [distinct_values] of the resulting values are unique
      with respect to [compare]. If too few distinct values are generated,
      [test_distinct_values] raises an exception. If [sexp_of] is provided, the exception
      includes the values generated. *)
  val test_distinct_values
    :  ?seed:seed
    -> ?sizes:int Sequence.t
    -> ?sexp_of:('a -> Base.Sexp.t)
    -> 'a Generator.t
    -> trials:int
    -> distinct_values:int
    -> compare:('a -> 'a -> int)
    -> unit

  (** [random_sequence ~seed gen] produces a sequence of values chosen from [gen]. *)
  val random_sequence
    :  ?seed:seed
    -> ?sizes:int Sequence.t
    -> 'a Generator.t
    -> 'a Sequence.t
end

(** Includes [Let_syntax] from [Monad.Syntax]. Sets [Open_on_rhs] to be all of
    [Generator], except that it does not shadow [Let_syntax] itself. Both [Generator] and
    [Open_on_rhs] are meant to be destructively assigned. *)
module type Syntax = sig
  module Generator : Generator

  module Open_on_rhs :
    Generator
      with type 'a t := 'a Generator.t
       and module Let_syntax := Generator.Let_syntax

  include
    Monad.Syntax
      with type 'a t := 'a Generator.t
       and module Let_syntax.Let_syntax.Open_on_rhs = Open_on_rhs
end

module type Quickcheck = sig
  type nonrec seed = seed
  type nonrec shrink_attempts = shrink_attempts

  module Generator : Generator
  module Observer : Observer
  module Shrinker : Shrinker

  module type S = S
  module type S1 = S1
  module type S2 = S2
  module type S_int = S_int
  module type S_range = S_range

  include Syntax with module Generator := Generator and module Open_on_rhs := Generator

  module type Quickcheck_config = Quickcheck_config
  module type Quickcheck_configured = Quickcheck_configured

  (** with a default config *)
  include Quickcheck_configured

  module Configure (Config : Quickcheck_config) : Quickcheck_configured
end
