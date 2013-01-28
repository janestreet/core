open Std_internal

module T = struct
  type t = string * int with sexp, bin_io
  let compare = Pervasives.compare
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

let t_of_sexp = function
  | Sexp.Atom s as sexp ->
    (try of_string s with Failure err -> of_sexp_error err sexp)
  | sexp -> t_of_sexp sexp

let pp ppf t = Format.fprintf ppf "%s" (to_string t)
let () = Pretty_printer.register "Core.Host_and_port.pp"

include (Hashable.Make_binable (T) : Hashable.S_binable with type t := t)

include Comparable.Make_binable (T)
