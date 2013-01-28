module Sexp = Sexplib.Sexp
open Sexplib.Std
open Bin_prot.Std

include Sexp

exception Of_sexp_error = Sexplib.Conv.Of_sexp_error

module O = struct
  type sexp = Sexp.t = Atom of string | List of t list
end

module T : sig
  include Interfaces.Sexpable with type t := Sexp.t
  include Interfaces.Binable  with type t := Sexp.t
  val compare : t -> t -> int
end = struct
  type t = Sexp.t = Atom of string | List of t list with bin_io, compare

  let sexp_of_t t = t
  let t_of_sexp t = t
end

include T

module Sexp_option = struct
  type 'a t = 'a option with bin_io, compare
end

module Sexp_list = struct
  type 'a t = 'a list with bin_io, compare
end

module Sexp_array = struct
  type 'a t = 'a array with bin_io, compare
end

module Sexp_opaque = struct
  type 'a t = 'a with bin_io, compare
end

module Sexp_maybe = struct

  type sexp = t with bin_io, compare             (* avoid recursive type *)

  (* to satisfy pa_compare *)
  module Error = struct
    include Error
    include Comparable.Poly (Error)
  end

  type 'a t = ('a, sexp * Error.t) Result.t with bin_io, compare

  let sexp_of_t sexp_of_a t =
    match t with
    | Result.Ok a -> sexp_of_a a
    | Result.Error (sexp, err) ->
      Sexp.List [
        Sexp.Atom "sexp_parse_error";
        sexp;
        Error.sexp_of_t err;
      ]

  let t_of_sexp a_of_sexp sexp =
    match sexp with
    | Sexp.List [ Sexp.Atom "sexp_parse_error"; sexp; _ ]
    | sexp ->
      try Result.Ok (a_of_sexp sexp)
      with exn -> Result.Error (sexp, Error.of_exn exn)

end

let of_int_style = Int_conversions.sexp_of_int_style

type 'a no_raise = 'a with bin_io, sexp

let sexp_of_no_raise sexp_of_a a =
  try sexp_of_a a
  with exn ->
    try List [ Atom "failure building sexp"; sexp_of_exn exn ]
    with _ -> Atom "could not build sexp for exn raised when building sexp for value"
;;

include Comparable.Make (struct
  type t = Sexp.t
  include T
end)
