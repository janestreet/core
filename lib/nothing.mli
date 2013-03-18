(** An uninhabited type.

    This is useful when interfaces require that a type be specified, but the implementer
    knows this type will not be used in their implementation of the interface.

    For instance, [Async.Std.Rpc.Pipe_rpc.t] is parameterized by an error type, but a user
    may want to define a Pipe RPC that can't fail. *)

type t = Nothing0.t

(** Because there are no values of type [Nothing.t], a piece of code that has a value of
    type [Nothing.t] must be unreachable.  In such an unreachable piece of code, one can
    use [unreachable_code] to give the code whatever type one needs.  For example:

    {[
      let f (r : (int, Nothing.t) Result.t) : int =
        match r with
        | Ok i -> i
        | Error n -> Nothing.unreachable_code n
      ;;
    ]} *)
val unreachable_code : t -> _

(** It may seem weird that this is identifiable, but we're just trying to anticipate all
    the contexts in which people may need this. It would be a crying shame if you had some
    variant type involving [Nothing.t] that you wished to make identifiable, but were
    prevented for lack of [Identifiable.S] here.

    Obviously, [of_string] and [t_of_sexp] will raise an exception. *)
include Identifiable.S with type t := t

module Stable : sig
  module V1 : sig
    type t with sexp, bin_io, compare
  end with type t = t
end
