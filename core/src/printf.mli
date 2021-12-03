open! Import

(** @inline *)
include module type of struct
  include Base.Printf
end

val eprintf : ('a, out_channel, unit) format -> 'a
val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a

val kfprintf
  :  (out_channel -> 'a)
  -> out_channel
  -> ('b, out_channel, unit, 'a) format4
  -> 'b

val printf : ('a, out_channel, unit) format -> 'a

(** print to stderr; exit 1 *)
val exitf : ('a, unit, string, unit -> _) format4 -> 'a

type printf = { printf : 'a. ('a, Buffer.t, unit) format -> 'a }

(** [collect_to_string (fun { printf } -> ...)] lets you easily convert code that was
    printing to stdout into code that produces a string.

    For example, this original code...
    {[
      printf "hello ";
      (* long computation *)
      printf "%s%c" "world" '!'
    ]}

    ... can be wrapped like so.
    {[
      Printf.collect_to_string (fun { printf } ->
        printf "hello ";
        (* long computation *)
        printf "%s%c" "world" '!')
    ]}

    The above is easier than manually editing many lines of the original:
    {[
      let hello = sprintf "hello " in
      (* long computation *)
      let world = sprintf "%s%c" "world" '!' in
      hello ^ world
    ]}
*)
val collect_to_string : (printf -> unit) -> string
