(** A module for organizing validations of data structures.

    Allows standardized ways of checking for conditions, and keeps track of the location
    of errors by keeping a path to each error found. Thus, if you were validating the
    following datastructure:

    {[
      { foo = 3;
        bar = { snoo = 34.5;
                blue = Snoot -6; }
      }
    ]}

    One might end up with an error with the error path:

    {v bar.blue.Snoot : value -6 <= bound 0 v}

    By convention, the validations for a type defined in module [M] appear in module [M],
    and have their name prefixed by [validate_]. E.g., [Int.validate_positive].

    Here's an example of how you would use [validate] with a record:

    {[
      type t =
        { foo: int;
          bar: float;
        }
      [@@deriving fields ~iterators:to_list]

      let validate t =
        let module V = Validate in
        let w check = V.field check t in
        Fields.to_list
          ~foo:(w Int.validate_positive)
          ~bar:(w Float.validate_non_negative)
        |> V.of_list
    ]}


    And here's an example of how you would use it with a variant type:

    {[
      type t =
        | Foo of int
        | Bar of (float * int)
        | Snoo of Floogle.t

      let validate = function
        | Foo i -> V.name "Foo" (Int.validate_positive i)
        | Bar p -> V.name "Bar" (V.pair
                                   ~fst:Float.validate_positive
                                   ~snd:Int.validate_non_negative p)
        | Snoo floogle -> V.name "Snoo" (Floogle.validate floogle)
    ]} *)

open Base

(** The result of a validation.  This effectively contains the list of errors, qualified
    by their location path *)
type t

(** To make function signatures easier to read. *)
type 'a check = 'a -> t

(** A result containing no errors. *)
val pass : t

(** A result containing a single error. *)
val fail : string -> t

val fails : string -> 'a -> ('a -> Sexp.t) -> t

(** This can be used with the [%sexp] extension. *)
val fail_s : Sexp.t -> t

(** Like [sprintf] or [failwithf] but produces a [t] instead of a string or exception. *)
val failf : ('a, unit, string, t) format4 -> 'a

val combine : t -> t -> t

(** Combines multiple results, merging errors. *)
val of_list : t list -> t

(** Extends location path by one name. *)
val name : string -> t -> t

val name_list : string -> t list -> t

(** [fail_fn err] returns a function that always returns fail, with [err] as the error
    message.  (Note that there is no [pass_fn] so as to discourage people from ignoring
    the type of the value being passed unconditionally irrespective of type.) *)
val fail_fn : string -> _ check

(** Checks for unconditionally passing a bool. *)
val pass_bool : bool check

(** Checks for unconditionally passing a unit. *)
val pass_unit : unit check

(** [protect f x] applies the validation [f] to [x], catching any exceptions and returning
    them as errors. *)
val protect : 'a check -> 'a check

(** [try_with f] runs [f] catching any exceptions and returning them as errors. *)
val try_with : (unit -> unit) -> t

val result : t -> unit Or_error.t

(** Returns a list of formatted error strings, which include both the error message and
    the path to the error. *)
val errors : t -> string list

(** If the result contains any errors, then raises an exception with a formatted error
    message containing a message for every error. *)
val maybe_raise : t -> unit

(** Returns an error if validation fails. *)
val valid_or_error : 'a check -> 'a -> 'a Or_error.t

(** Used for validating an individual field. Should be used with [Fields.to_list]. *)
val field : 'a check -> 'record -> ([> `Read ], 'record, 'a) Field.t_with_perm -> t

(** Used for validating an individual field. Should be used with [Fields.Direct.to_list]. *)
val field_direct
  :  'a check
  -> ([> `Read ], 'record, 'a) Field.t_with_perm
  -> 'record
  -> 'a
  -> t

(** Creates a function for use in a [Fields.fold]. *)
val field_folder
  :  'a check
  -> 'record
  -> t list
  -> ([> `Read ], 'record, 'a) Field.t_with_perm
  -> t list

(** Creates a function for use in a [Fields.Direct.fold]. *)
val field_direct_folder
  :  'a check
  -> (t list -> ([> `Read ], 'record, 'a) Field.t_with_perm -> 'record -> 'a -> t list)
     Staged.t

(** Combines a list of validation functions into one that does all validations. *)
val all : 'a check list -> 'a check

(** Creates a validation function from a function that produces a [Result.t]. *)
val of_result : ('a -> (unit, string) Result.t) -> 'a check

val of_error : ('a -> unit Or_error.t) -> 'a check

(** Creates a validation function from a function that produces a bool. *)
val booltest : ('a -> bool) -> if_false:string -> 'a check

(** Validation functions for particular data types. *)
val pair : fst:'a check -> snd:'b check -> ('a * 'b) check

(** Validates a list, naming each element by its position in the list (where the first
    position is 1, not 0). *)
val list_indexed : 'a check -> 'a list check

(** Validates a list, naming each element using a user-defined function for computing the
    name. *)
val list : name:('a -> string) -> 'a check -> 'a list check

val first_failure : t -> t -> t
val of_error_opt : string option -> t

(** Validates an association list, naming each element using a user-defined function for
    computing the name. *)
val alist : name:('a -> string) -> 'b check -> ('a * 'b) list check

val bounded
  :  name:('a -> string)
  -> lower:'a Maybe_bound.t
  -> upper:'a Maybe_bound.t
  -> compare:('a -> 'a -> int)
  -> 'a check

module Infix : sig
  (** Infix operator for [combine] above. *)
  val ( ++ ) : t -> t -> t
end
