type spec = Caml.Arg.spec =
| Unit of (unit -> unit)      (* Call the function with unit argument *)
| Bool of (bool -> unit)      (* Call the function with a bool argument *)
| Set of bool ref             (* Set the reference to true *)
| Clear of bool ref           (* Set the reference to false *)
| String of (string -> unit)  (* Call the function with a string argument *)
| Set_string of string ref    (* Set the reference to the string argument *)
| Int of (int -> unit)        (* Call the function with an int argument *)
| Set_int of int ref          (* Set the reference to the int argument *)
| Float of (float -> unit)    (* Call the function with a float argument *)
| Set_float of float ref      (* Set the reference to the float argument *)
| Tuple of spec list          (* Take several arguments according to the spec list *)
| Symbol of string list * (string -> unit) (* Take one of the symbols as argument and call the function with the symbol *)
| Rest of (string -> unit) (* Stop interpreting keywords and call the function with each remaining argument *)

type key = string 

type doc = string

type t = key * spec * doc

type usage_msg = string 

type anon_fun = string -> unit 

val parse : t list -> anon_fun -> usage_msg -> unit

val parse_argv :
  ?current:int ref
  -> string array
  -> t list -> anon_fun -> usage_msg -> unit

exception Help of string

exception Bad of string

val usage : t list -> usage_msg -> unit

val align : t list -> t list

val sort_and_align : t list -> t list

val current : int ref
