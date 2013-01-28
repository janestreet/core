module Sexp = Sexplib.Sexp
open Sexplib.Std
open Bin_prot.Std

include Sexp

include (struct
  type t = Sexp.t = Atom of string | List of t list with bin_io
end : Interfaces.Binable with type t := t)

module Sexp_option = struct
  type 'a sexp_option = 'a option with bin_io
end

module Sexp_list = struct
  type 'a sexp_list = 'a list with bin_io
end

module Sexp_array = struct
  type 'a sexp_array = 'a array with bin_io
end

module Sexp_opaque = struct
  type 'a sexp_opaque = 'a with bin_io
end

module Sexp_maybe = struct

  type sexp = t with bin_io             (* avoid recursive type *)
  type 'a t = ('a, sexp) Result.t with bin_io

  let sexp_of_t sexp_of_a t =
    match t with
    | Result.Ok a -> sexp_of_a a
    | Result.Error sexp -> sexp

  let t_of_sexp a_of_sexp sexp =
    try Result.Ok (a_of_sexp sexp)
    with exn -> Result.Error (Exn.sexp_of_t exn)

end

let of_int_style = Int_conversions.sexp_of_int_style

type 'a no_raise = 'a with bin_io, sexp

let sexp_of_no_raise sexp_of_a a =
  try sexp_of_a a
  with exn ->
    try List [ Atom "failure building sexp"; sexp_of_exn exn ]
    with _ -> Atom "could not build sexp for exn raised when building sexp for value"
;;
