(** Boolean expressions. *)

open! Import

(** A blang is a boolean expression built up by applying the usual boolean operations to
    properties that evaluate to true or false in some context.

    {2 Usage}

    For example, imagine writing a config file for an application that filters a stream of
    integers. Your goal is to keep only those integers that are multiples of either -3 or
    5. Using [Blang] for this task, the code might look like:

    {[
      module Property = struct
        type t =
          | Multiple_of of int
          | Positive
          | Negative
        [@@deriving sexp]

        let eval t num =
          match t with
          | Multiple_of n -> num % n = 0
          | Positive      -> num > 0
          | Negative      -> num < 0
      end

      type config = {
        keep : Property.t Blang.t;
      } [@@deriving sexp]

      let config = {
        keep =
          Blang.t_of_sexp
            Property.t_of_sexp
            (Sexp.of_string
               "(or (and negative (multiple_of 3)) (and positive (multiple_of 5)))";
      }

      let keep config num : bool =
        Blang.eval config.keep (fun p -> Property.eval p num)
    ]}

    Note how [positive] and [negative] and [multiple_of] become operators in a small,
    newly-defined boolean expression language that allows you to write statements like
    [(and negative (multiple_of 3))].

    {2 Blang sexp syntax}

    The blang sexp syntax is almost exactly the derived one, except that:

    1. Base properties are not marked explicitly.  Thus, if your base
    property type has elements FOO, BAR, etc., then you could write
    the following Blang s-expressions:

    {v
        FOO
        (and FOO BAR)
        (if FOO BAR BAZ)
    v}

    and so on.  Note that this gets in the way of using the blang
    "keywords" in your value language.

    2. [And] and [Or] take a variable number of arguments, so that one can
    (and probably should) write

    {v (and FOO BAR BAZ QUX) v}

    instead of

    {v (and FOO (and BAR (and BAZ QUX))) v}

    If you want to see the derived sexp, use [Raw.sexp_of_t].
*)

open Std_internal

(** Note that the sexps are not directly inferred from the type below -- there are lots of
    fancy shortcuts.  Also, the sexps for ['a] must not look anything like blang sexps.
    Otherwise [t_of_sexp] will fail.  The directly inferred sexps are available via
    [Raw.sexp_of_t]. *)
type +'a t = private
  | True
  | False
  | And of 'a t * 'a t
  | Or of 'a t * 'a t
  | Not of 'a t
  | If of 'a t * 'a t * 'a t
  | Base of 'a
[@@deriving bin_io ~localize, compare, equal, hash, sexp, sexp_grammar, typerep]

(** [Raw] provides the automatically derived [sexp_of_t], useful in debugging the actual
    structure of the blang. *)
module Raw : sig
  type nonrec 'a t = 'a t [@@deriving sexp_of]
end

(** {2 Smart constructors that simplify away constants whenever possible} *)

module type Constructors = sig
  val base : 'a -> 'a t
  val true_ : _ t
  val false_ : _ t

  (** [function true -> true_ | false -> false_] *)
  val constant : bool -> _ t

  val not_ : 'a t -> 'a t

  (** n-ary [And] *)
  val and_ : 'a t list -> 'a t

  (** n-ary [Or] *)
  val or_ : 'a t list -> 'a t

  (** [if_ if then else] *)
  val if_ : 'a t -> 'a t -> 'a t -> 'a t
end

include Constructors

module O : sig
  include Constructors

  val ( && ) : 'a t -> 'a t -> 'a t
  val ( || ) : 'a t -> 'a t -> 'a t

  (** [a ==> b] is "a implies b".  This is not [=>] to avoid making it look like a
      comparison operator. *)
  val ( ==> ) : 'a t -> 'a t -> 'a t

  val not : 'a t -> 'a t
end

(** [constant_value t = Some b] iff [t = constant b] *)
val constant_value : 'a t -> bool option

(** The following two functions are useful when one wants to pretend
    that ['a t] has constructors [And] and [Or] of type ['a t list -> 'a t].
    The pattern of use is

    {[
      match t with
      | And (_, _) as t -> let ts = gather_conjuncts t in ...
      | Or (_, _) as t -> let ts = gather_disjuncts t in ...
      | ...
    ]}

    or, in case you also want to handle [True] (resp. [False]) as a special
    case of conjunction (disjunction)

    {[
      match t with
      | True | And (_, _) as t -> let ts = gather_conjuncts t in ...
      | False | Or (_, _) as t -> let ts = gather_disjuncts t in ...
      | ...
    ]}
*)

(** [gather_conjuncts t] gathers up all toplevel conjuncts in [t].  For example,
    {ul {- [gather_conjuncts (and_ ts) = ts] }
    {- [gather_conjuncts (And (t1, t2)) = gather_conjuncts t1 @ gather_conjuncts t2] }
    {- [gather_conjuncts True = [] ] }
    {- [gather_conjuncts t = [t]] when [t] matches neither [And (_, _)] nor [True] } }
*)
val gather_conjuncts : 'a t -> 'a t list

(** [gather_disjuncts t] gathers up all toplevel disjuncts in [t].  For example,
    {ul {- [gather_disjuncts (or_ ts) = ts] }
    {- [gather_disjuncts (Or (t1, t2)) = gather_disjuncts t1 @ gather_disjuncts t2] }
    {- [gather_disjuncts False = [] ] }
    {- [gather_disjuncts t = [t]] when [t] matches neither [Or (_, _)] nor [False] } }
*)
val gather_disjuncts : 'a t -> 'a t list

include Container.S1 with type 'a t := 'a t
include Quickcheckable.S1 with type 'a t := 'a t

(** [Blang.t] sports a substitution monad:
    {ul {- [return v] is [Base v] (think of [v] as a variable) }
    {- [bind t f] replaces every [Base v] in [t] with [f v]
    (think of [v] as a variable and [f] as specifying the term to
    substitute for each variable) } }

    Note: [bind t f] does short-circuiting, so [f] may not be called on every variable in
    [t]. *)
include Monad with type 'a t := 'a t

(** [values t] forms the list containing every [v]
    for which [Base v] is a subexpression of [t] *)
val values : 'a t -> 'a list

(** [eval t f] evaluates the proposition [t] relative to an environment
    [f] that assigns truth values to base propositions. *)
val eval : 'a t -> ('a -> bool) -> bool

(** [eval_set ~universe set_of_base expression] returns the subset of elements [e] in
    [universe] that satisfy [eval expression (fun base -> Set.mem (set_of_base base) e)].

    [eval_set] assumes, but does not verify, that [set_of_base] always returns a subset of
    [universe]. If this doesn't hold, then [eval_set]'s result may contain elements not
    in [universe].

    [And set1 set2] represents the elements that are both in [set1] and [set2], thus in
    the intersection of the two sets. Symmetrically, [Or set1 set2] represents the union
    of [set1] and [set2]. *)
val eval_set
  :  universe:('elt, 'comparator) Set.t Lazy.t
  -> ('a -> ('elt, 'comparator) Set.t)
  -> 'a t
  -> ('elt, 'comparator) Set.t

(** [specialize t f] partially evaluates [t] according to a
    perhaps-incomplete assignment [f] of the values of base propositions.
    The following laws (at least partially) characterize its behavior.

    - [specialize t (fun _ -> `Unknown) = t]

    - [specialize t (fun x -> `Known (f x)) = constant (eval t f)]

    - [List.for_all (values (specialize t g)) ~f:(fun x -> g x = `Unknown)]

    - {[
      if
        List.for_all (values t) ~f:(fun x ->
          match g x with
          | `Known b -> b = f x
          | `Unknown -> true)
      then
        eval t f = eval (specialize t g) f
    ]}
*)
val specialize : 'a t -> ('a -> [ `Known of bool | `Unknown ]) -> 'a t

module type Monadic = sig
  module M : Monad.S

  val map : 'a t -> f:('a -> 'b M.t) -> 'b t M.t
  val bind : 'a t -> f:('a -> 'b t M.t) -> 'b t M.t
  val eval : 'a t -> f:('a -> bool M.t) -> bool M.t
end

(** Generalizes some of the blang operations above to work in a monad. *)
module For_monad (M : Monad.S) : Monadic with module M := M

val invariant : 'a t -> unit

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t = private
      | True
      | False
      | And of 'a t * 'a t
      | Or of 'a t * 'a t
      | Not of 'a t
      | If of 'a t * 'a t * 'a t
      | Base of 'a
    [@@deriving
      sexp, sexp_grammar, bin_io ~localize, stable_witness, compare, equal, hash]
  end
end
