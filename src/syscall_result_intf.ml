open! Import
module type S = sig
  type ok_value
  type 'a syscall_result

  type t = ok_value syscall_result [@@deriving compare, sexp_of]

  include Equal.S with type t := t

  val create_ok    : ok_value     -> t
  val create_error : Unix_error.t -> t

  val is_ok    : t -> bool
  val is_error : t -> bool

  (** This returns a preallocated object for all errors and at least a few [ok_value]s, so
      can be used in many contexts where avoiding allocation is important. *)
  val to_result : t -> (ok_value, Unix_error.t) Result.t

  val ok_exn    : t -> ok_value
  val error_exn : t -> Unix_error.t

  (** This is more efficient than calling [error_exn] and then the [create_error] of the
      destination type. *)
  val reinterpret_error_exn : t -> _ syscall_result

  val ok_or_unix_error_exn
    : t -> syscall_name:string ->                         ok_value
  val ok_or_unix_error_with_args_exn
    : t -> syscall_name:string -> 'a -> ('a -> Sexp.t) -> ok_value

  module Optional_syntax : Optional_syntax.S 
    with type t := t
     and type value := ok_value
end

module type Arg = sig
  type t [@@deriving sexp_of, compare]

  (** [to_int t] must be >= 0, otherwise [create_ok] will raise. *)
  val to_int : t -> int
  val of_int_exn : int -> t
end
