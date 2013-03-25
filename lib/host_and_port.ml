open Std_internal

module Stable = struct
  module V1 = struct
    type t = string * int with sexp, bin_io, compare
  end
end

module T = struct
  include Stable.V1

  let hash = Hashtbl.hash
end
include T

let create ~host ~port = (host, port)

let host = fst
let port = snd
let tuple t = t

let to_string (host, port) = sprintf "%s:%d" host port
let of_string s =
  match String.split s ~on:':' with
  | [host; port] ->
    let port =
      try Int.of_string port
      with _exn -> failwithf "Host_and_port.of_string: bad port: %s" s ()
    in
    host, port
  | _ -> failwithf "Host_and_port.of_string: %s" s ()

include Pretty_printer.Register (struct
  type nonrec t = t
  let to_string = to_string
  let module_name = "Core.Std.Host_and_port"
end)

include (Hashable.Make_binable (T) : Hashable.S_binable with type t := t)

include Comparable.Make_binable (T)

let t_of_sexp = function
  | Sexp.Atom s as sexp ->
    (try of_string s with Failure err -> of_sexp_error err sexp)
  | sexp -> t_of_sexp sexp

