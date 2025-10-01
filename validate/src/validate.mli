@@ portable

[@@@warning "-incompatible-with-upstream"]

(** A module for organizing validations of data structures.

    Allows standardized ways of checking for conditions, and keeps track of the location
    of errors by keeping a path to each error found. Thus, if you were validating the
    following datastructure:

    {[
      { foo = 3; bar = { snoo = 34.5; blue = Snoot - 6 } }
    ]}

    One might end up with an error with the error path:

    {v bar.blue.Snoot : value -6 <= bound 0 v}

    By convention, the validations for a type defined in module [M] appear in module [M],
    and have their name prefixed by [validate_]. E.g., [Int.validate_positive].

    Here's an example of how you would use [validate] with a record:

    {[
      type t =
        { foo : int
        ; bar : float
        }
      [@@deriving fields ~iterators:to_list]

      let validate t =
        let module V = Validate in
        let w check = V.field check t in
        Fields.to_list ~foo:(w Int.validate_positive) ~bar:(w Float.validate_non_negative)
        |> V.of_list
      ;;
    ]}

    And here's an example of how you would use it with a variant type:

    {[
      type t =
        | Foo of int
        | Bar of (float * int)
        | Snoo of Floogle.t

      let validate = function
        | Foo i -> V.name "Foo" (Int.validate_positive i)
        | Bar p ->
          V.name
            "Bar"
            (V.pair ~fst:Float.validate_positive ~snd:Int.validate_non_negative p)
        | Snoo floogle -> V.name "Snoo" (Floogle.validate floogle)
      ;;
    ]} *)

open Base

(** The result of a validation. This effectively contains the list of errors, qualified by
    their location path *)
type t : value mod contended

(** To make function signatures easier to read. *)
type%template ('a : any) check = 'a -> t @ p [@@mode p = (portable, nonportable)]

(** [(check[@mode portable])] can be read as "a check that returns a [Validate.t] at mode
    portable".

    If you have [f] of type [(check[@mode portable])], sometimes the type checker takes
    some convincing to treat it as a [check], even though a returning-at-portable function
    is morally a subtype of a returning-at-nonportable function. Writing [(fun x -> f x)]
    should be enough to convert the [(check[@mode portable])] into a [check]. *)

(** A result containing no errors. *)
val pass : t

val get_pass : unit -> t @ portable

(** A result containing a single error. *)
val fail : string -> t @ portable

val fails : string -> 'a -> ('a -> Sexp.t) -> t

(** This can be used with the [%sexp] extension. *)
val fail_s : Sexp.t -> t @ portable

(** Like [sprintf] or [failwithf] but produces a [t] instead of a string or exception. *)
val failf : ('a, unit, string, t) format4 -> 'a

val%template combine : t @ p -> t @ p -> t @ p [@@mode p = (portable, nonportable)]

(** Combines multiple results, merging errors. *)
val%template of_list : t list @ p -> t @ p
[@@mode p = (portable, nonportable)]

(** Extends location path by one name. *)
val%template name : string -> t @ p -> t @ p
[@@mode p = (portable, nonportable)]

(** Extends location path by one lazy name, which will be forced only in the case of
    failure. *)
val%template lazy_name : string Lazy.t -> t @ p -> t @ p
[@@mode p = (portable, nonportable)]

val%template name_list : string -> t list @ p -> t @ p
[@@mode p = (portable, nonportable)]

val%template lazy_name_list : string Lazy.t -> t list @ p -> t @ p
[@@mode p = (portable, nonportable)]

(** [fail_fn err] returns a function that always returns fail, with [err] as the error
    message. (Note that there is no [pass_fn] so as to discourage people from ignoring the
    type of the value being passed unconditionally irrespective of type.) *)
val%template fail_fn : string -> (_ check[@mode p]) @ portable
[@@mode p = (portable, nonportable)]

(** Checks for unconditionally passing a bool. *)
val%template pass_bool : (bool check[@mode p])
[@@mode p = (portable, nonportable)]

(** Checks for unconditionally passing a unit. *)
val%template pass_unit : (unit check[@mode p])
[@@mode p = (portable, nonportable)]

(** [protect f x] applies the validation [f] to [x], catching any exceptions and returning
    them as errors. *)
val%template protect : ('a : k). 'a check -> 'a check
[@@kind k = (value_or_null, float64, bits32, bits64, word, immediate, immediate64)]

(** [try_with f] runs [f] catching any exceptions and returning them as errors. *)
val%template try_with : (unit -> unit) @ p -> t @ p
[@@mode p = (portable, nonportable)]

val result : t -> unit Or_error.t

(** Returns a list of formatted error strings, which include both the error message and
    the path to the error. *)
val errors : t -> string list

(** If the result contains any errors, then raises an exception with a formatted error
    message containing a message for every error. *)
val maybe_raise : t -> unit
[@@zero_alloc]

(** Returns an error if validation fails. *)
val valid_or_error : 'a check -> 'a -> 'a Or_error.t

(** Used for validating an individual field. Should be used with [Fields.to_list]. *)
val%template field
  : ('a : k) 'record.
  'a check -> 'record -> ([> `Read ], 'record, 'a) Field.t_with_perm -> t
[@@kind k = (value_or_null, float64, bits32, bits64, word, immediate, immediate64)]

(** Used for validating an individual field. Should be used with [Fields.Direct.to_list]. *)
val%template field_direct
  : ('a : k) 'record.
  'a check -> ([> `Read ], 'record, 'a) Field.t_with_perm -> 'record -> 'a -> t
[@@kind k = (value_or_null, float64, bits32, bits64, word, immediate, immediate64)]

(** Creates a function for use in a [Fields.fold]. *)
val%template field_folder
  :  ('a check[@mode p])
  -> 'record
  -> t list @ p
  -> ([> `Read ], 'record, 'a) Field.t_with_perm
  -> t list @ p
[@@mode p = (portable, nonportable)]

(** Like [field_folder], but for producing a list of portable ts. It's marginally easier
    to use directly when folding over fields to produce a portable result. *)
val%template portable_field_folder
  :  ('a check[@mode portable])
  -> 'record
  -> t portable list
  -> ([> `Read ], 'record, 'a) Field.t_with_perm
  -> t portable list

(** Creates a function for use in a [Fields.Direct.fold]. *)
val field_direct_folder
  :  'a check
  -> (t list -> ([> `Read ], 'record, 'a) Field.t_with_perm -> 'record -> 'a -> t list)
       Staged.t

(** Combines a list of validation functions into one that does all validations. *)
val all : 'a check list -> 'a check

(** Creates a validation function from a function that produces a [Result.t]. *)
val%template of_result
  :  ('a -> (unit, string) Result.t) @ p
  -> ('a check[@mode portable]) @ p
[@@mode p = (portable, nonportable)]

val%template of_error : ('a -> unit Or_error.t) @ p -> 'a check @ p
[@@mode p = (portable, nonportable)]

(** Creates a validation function from a function that produces a bool. *)
val booltest : ('a -> bool) -> if_false:string -> 'a check

(** Creates a validation function from a function that produces a bool. Forces the name
    only when validation fails. *)
val lazy_booltest : ('a -> bool) -> if_false:string Lazy.t -> 'a check

(** Validation functions for particular data types. *)
val%template pair
  :  fst:('a check[@mode p1]) @ p2
  -> snd:('b check[@mode p1]) @ p2
  -> (('a * 'b) check[@mode p1]) @ p2
[@@mode p1 = (portable, nonportable), p2 = (portable, nonportable)]

(** Validates a list, naming each element by its position in the list (where the first
    position is 1, not 0). *)
val list_indexed : 'a check -> 'a list check

(** Validates a list, naming each element using a user-defined function for computing the
    name. *)
val list : name:('a -> string) -> 'a check -> 'a list check

val%template first_failure : t @ p -> t @ p -> t @ p [@@mode p = (portable, nonportable)]

val of_error_opt : string option -> t @ portable

(** Validates an association list, naming each element using a user-defined function for
    computing the name. *)
val alist : name:('a -> string) -> 'b check -> ('a * 'b) list check

val%template bounded
  :  name:('a -> string)
  -> lower:'a Maybe_bound.t
  -> upper:'a Maybe_bound.t
  -> compare:('a -> 'a -> int)
  -> ('a check[@mode portable])

module Infix : sig
  (** Infix operator for [combine] above. *)
  val ( ++ ) : t -> t -> t
end
