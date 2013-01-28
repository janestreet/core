(** [Option] wraps the output [x] of successful functions in [Some x].  Failed
    functions return [None]. *)

(** Options are preferred over exceptions.  For example, use
    {[
    let data = [(2, "two"); (5, "five"); (8, "eight")];;
    let f x = match List.Assoc.find_opt x data with
    | Some y -> y
    | None -> "zero" (* where "zero" is some default value *);; ]}
    rather than
    {[
    let f x = try List.Assoc.find x data with Not_found -> "zero";; ]}
    In this case using an exception is shorter, but in nontrivial code options
    are easier to understand. *)
type 'a t = 'a option with sexp

include Container.S1 with type 'a t := 'a t

(** Options form a monad, where [return x =  Some x],
    [(None >>= f) = None], and [(Some x >>= f) = f x]. *)
include Monad.S with type 'a t := 'a t

(** [is_none t] returns true iff t = None. *)
val is_none : 'a t -> bool

(** [is_some t] returns true iff t = Some x. *)
val is_some : 'a t -> bool

(** [value_map t ~f ~default] is equivalent to [value (map t ~f) ~default], except that
    it is slightly faster since it avoids creating the intermediate option.  I.e.

      [value_map None     ~default ~f] = [default]
      [value_map (Some x) ~default ~f] = [f x] *)
val value_map : 'a t -> default:'b -> f:('a -> 'b) -> 'b

(** [map2 o f] map 'a option and 'b option to a 'c option using ~f *)
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

(** [call x f] run optional function on argument *)
val call : 'a -> f:('a -> unit) t -> unit

(** [apply x f] run optional function on argument and return an option *)
val apply : 'a -> f:('a -> 'b) t -> 'b t

(** [value None ~default] = [default]
    [value (Some x) ~default] = [x]
*)
val value : 'a t -> default:'a -> 'a

(** [value_exn (Some x)] = [x].
    [value_exn None] raises an exception. *)
val value_exn : 'a t -> 'a

(** [value_exn_message message (Some x)] = [x].
    [value_exn_message message None] raises exception Failure with
    string [message]. *)
val value_exn_message : string -> 'a t -> 'a

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val some : 'a -> 'a t

val both : 'a t -> 'b t -> ('a * 'b) t

val first_some : 'a t -> 'a t -> 'a t

val some_if : bool -> 'a -> 'a t

val filter : f:('a -> bool) -> 'a t -> 'a t

(** [try_with f] returns [Some x] if [f] returns [x] and [None] if [f] raises an
    exception.  See [Result.try_with] if you'd like to know which exception. *)
val try_with : (unit -> 'a) -> 'a t

val compare : cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
