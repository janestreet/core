(** Witnesses that express whether a type's values are always, sometimes, or never
    immediate.

    A value is immediate when it is internally represented unboxed, using one word of
    memory rather than a pointer to a heap-allocated block.

    Some examples:

    - All [int] values are by definition immediate, i.e., unboxed, and so [int]
      is always immediate.

    - A ['a list] is either [[]], which is internally represented as 0 (immediate), or a
      non-empty list, which is represented as a pointer to a heap block (boxed), which
      contains the first element and the pointer to the rest of the list.  Therefore ['a
      list] is sometimes immediate.

    - All values of type ['a ref] are represented as a pointer to a heap block containing
      the actual values ['a].  Therefore ['a ref] is never immediate.

    The witness values can be used to perform safe optimizations such as allowing a more
    efficient ['a array] blit operations if ['a] is always immediate.  These witnesses can
    also be used to perform safe conversions between immediate values of type ['a] and
    [int] instead of using [Obj.magic].

    {2 Converting between values and ints}

    Consider an arbitrary type ['a] for which you have built a type-immediacy witness
    using this interface. Let's call it [w : 'a t].

    You can use the two following functions and [w] to cast back and forth values from
    the type ['a] to the type [int]:

    {[
      val int_as_value : 'a t -> int -> 'a option
      val value_as_int : 'a t -> 'a  -> int option
    ]}

    For the rest of this section, we will assume [int_as_value] and [value_as_int]
    partially applied to [w].

    Consider the following cases:

    {ul {- let [v] be an immediate value.

    Let [i] be the [int] that internally represents [v].  Then, [value_as_int v] returns
    [Some i].

    We can also recover [v] by using the conversions that go the other way.  In
    particular, [int_as_value i] returns [Some v].}}

    {ul {- let [v] be a boxed value that cannot be converted to an [int].

    [value_as_int v] returns [None] because there does not exist an int s.t. [int_as_value
    i] evaluates to [Some v].}}

    {ul {- let [i] be an int that does not represent any value of type ['a]

    [int_as_value i] returns [None].}}

    {2 Faster *_exn functions and functions with boolean results}

    [value_is_int v]     is a faster equivalent to [Option.is_some   (value_as_int v)].
    [value_as_int_exn v] is a faster equivalent to [Option.value_exn (value_as_int v)].

    [int_is_value i]     is a faster equivalent to [Option.is_some   (int_as_value i)].
    [int_as_value_exn i] is a faster equivalent to [Option.value_exn (int_as_value i)].

    These are lightweight functions that avoid allocating the option.  [value_is_int]
    (resp [int_is_value]) can be used with [value_as_int_exn] (resp [int_as_value_exn]) to
    avoid both allocation or using a [try with] statement, paying only some small amount
    of CPU time for calling [value_is_int] (resp [int_is_value]):

    {[
      match value_as_int v with
      | Some v -> some v
      | None -> none
    ]}

    VS

    {[
      if value_is_int v
      then some (value_as_int_exn v)
      else none
    ]}

    {2 Example}

    Consider the following type:

    {[
      type test =
        | A
        | B
        | C of int
      with typerep
    ]}

    Type [test] is sometimes immediate, as [A] is represented as [0], [B] as [1], and [C]
    is a boxed value.  We can construct a witness of type [test Sometimes.t] by using
    [Sometimes.of_typerep] or [of_typerep] and extracting the witness.  Let's call the
    witness [w] here.  We can now use it to safely convert between values of [test] and
    [int]:

    [Sometimes.value_as_int w A]      evaluates to [Some 0]
    [Sometimes.value_as_int w B]      evaluates to [Some 1]
    [Sometimes.value_as_int w (C 1)]  evaluates to [None]

    [Sometimes.int_as_value w 0]      evaluates to [Some A]
    [Sometimes.int_as_value w 1]      evaluates to [Some B]
    [Sometimes.int_as_value w n]      evaluates to [None] for all other values n

    Consider this other example:

    {[
      type test = bool with typerep
    ]}

    Type [test] is always immediate, since [true] is represented as [1] and [false] as
    [0].  We can construct a witness of type [test Always.t] by using [Always.of_typerep]
    or [of_typerep] and extracting the witness.  Let's call the witness [w]:

    [Always.value_as_int w false]      evaluates to [Some 0]
    [Always.value_as_int w true]       evaluates to [Some 1]

    [Always.value_as_int_exn w false]  evaluates to [0]
    [Always.value_as_int_exn w true]   evaluates to [1]

    [Always.int_as_value w 0]          evaluates to [Some false]
    [Always.int_as_value w 1]          evaluates to [Some true]
    [Always.int_as_value w (-1)]       evaluates to [None]

    [Always.int_as_value_exn w 0]      evaluates to [false]
    [Always.int_as_value_exn w 1]      evaluates to [true]
    [Always.int_as_value_exn w (-1)]   raises

    {2 N-ary types that are immediate independently of their type arguments}

    We also provide [For_all_parameters_S*] functors.  Those are useful when one has a
    type with type parameters, but knows that values of that type will always be immediate
    (for example) no matter what the actual parameter is.  They can use
    [Always.For_all_parameters_S*] to obtain access to a polymorphic witness.

    An exception is raised on functor application if such witness cannot be obtained.
    That happens either because the witness depends on the actual type parameter, or
    because the type has a different witness (e.g. [Sometimes] instead of [Always]).
*)

open! Import

type 'a t

module Always : sig
  type 'a t

  val of_typerep : 'a Typerep.t -> 'a t option
  val of_typerep_exn : Source_code_position.t -> 'a Typerep.t -> 'a t
  val int_as_value : 'a t -> int -> 'a option
  val int_as_value_exn : 'a t -> int -> 'a
  val int_is_value : 'a t -> int -> bool
  val value_as_int : 'a t -> 'a -> int

  module For_all_parameters_S1 (X : Typerepable.S1) : sig
    val witness : unit -> _ X.t t
  end

  module For_all_parameters_S2 (X : Typerepable.S2) : sig
    val witness : unit -> (_, _) X.t t
  end

  module For_all_parameters_S3 (X : Typerepable.S3) : sig
    val witness : unit -> (_, _, _) X.t t
  end

  module For_all_parameters_S4 (X : Typerepable.S4) : sig
    val witness : unit -> (_, _, _, _) X.t t
  end

  module For_all_parameters_S5 (X : Typerepable.S5) : sig
    val witness : unit -> (_, _, _, _, _) X.t t
  end

  val int : int t
  val char : char t
  val bool : bool t
  val unit : unit t
end

module Sometimes : sig
  type 'a t

  val of_typerep : 'a Typerep.t -> 'a t option
  val of_typerep_exn : Source_code_position.t -> 'a Typerep.t -> 'a t
  val int_as_value : 'a t -> int -> 'a option
  val int_as_value_exn : 'a t -> int -> 'a
  val int_is_value : 'a t -> int -> bool
  val value_as_int : 'a t -> 'a -> int option
  val value_as_int_exn : 'a t -> 'a -> int
  val value_is_int : 'a t -> 'a -> bool

  module For_all_parameters_S1 (X : Typerepable.S1) : sig
    val witness : unit -> _ X.t t
  end

  module For_all_parameters_S2 (X : Typerepable.S2) : sig
    val witness : unit -> (_, _) X.t t
  end

  module For_all_parameters_S3 (X : Typerepable.S3) : sig
    val witness : unit -> (_, _, _) X.t t
  end

  module For_all_parameters_S4 (X : Typerepable.S4) : sig
    val witness : unit -> (_, _, _, _) X.t t
  end

  module For_all_parameters_S5 (X : Typerepable.S5) : sig
    val witness : unit -> (_, _, _, _, _) X.t t
  end

  val option : _ option t
  val list : _ list t
end

module Never : sig
  type 'a t

  val of_typerep : 'a Typerep.t -> 'a t option
  val of_typerep_exn : Source_code_position.t -> 'a Typerep.t -> 'a t

  module For_all_parameters_S1 (X : Typerepable.S1) : sig
    val witness : unit -> _ X.t t
  end

  module For_all_parameters_S2 (X : Typerepable.S2) : sig
    val witness : unit -> (_, _) X.t t
  end

  module For_all_parameters_S3 (X : Typerepable.S3) : sig
    val witness : unit -> (_, _, _) X.t t
  end

  module For_all_parameters_S4 (X : Typerepable.S4) : sig
    val witness : unit -> (_, _, _, _) X.t t
  end

  module For_all_parameters_S5 (X : Typerepable.S5) : sig
    val witness : unit -> (_, _, _, _, _) X.t t
  end

  val int32 : int32 t
  val int64 : int64 t
  val nativeint : nativeint t
  val float : float t
  val string : string t
  val bytes : bytes t
  val array : _ array t
  val ref_ : _ ref t
  val tuple2 : (_ * _) t
  val tuple3 : (_ * _ * _) t
  val tuple4 : (_ * _ * _ * _) t
  val tuple5 : (_ * _ * _ * _ * _) t
end

val of_typerep : 'a Typerep.t -> 'a t

type 'a dest =
  | Always of 'a Always.t
  | Sometimes of 'a Sometimes.t
  | Never of 'a Never.t
  | Unknown

val dest : 'a t -> 'a dest
val int_as_value : 'a t -> int -> 'a option
val int_as_value_exn : 'a t -> int -> 'a
val int_is_value : 'a t -> int -> bool
val value_as_int : 'a t -> 'a -> int option
val value_as_int_exn : 'a t -> 'a -> int
val value_is_int : 'a t -> 'a -> bool
