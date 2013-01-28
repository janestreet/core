(** In_channel collects all of the pervasive functions that work on in_channels.
    * It adds some new functions (like [input_all] and [input_lines]).
    * It names things using the fact that there is no worry about toplevel name
      conflicts (since we are in a module).
    * It uses labelled arguments.
    * It returns an option rather than raising End_of_file.
*)

type t = in_channel

val stdin : t

(** Channels are opened in binary mode iff [binary] is true.  This only has an effect on
    Windows.
*)

val create : ?binary:bool -> string -> t

(** [with_file ~f fname] executes [~f] on the open channel from
    [fname], and closes it afterwards. *)
val with_file : ?binary:bool -> string -> f:(t -> 'a) -> 'a

(* [close t] closes t, and may raise an exception. *)
val close : t -> unit
(* [close_noerr] closes t and never raises an exception. *)
val close_noerr : t -> unit

val input : t -> buf:string -> pos:int -> len:int -> int
val really_input : t -> buf:string -> pos:int -> len:int -> unit option
val input_byte : t -> int option
val input_char : t -> char option

val input_binary_int : t -> int option
val unsafe_input_value : t -> _ option  (* Ocaml's built-in marshal format *)
val input_all : t -> string

(** [input_line ?fix_win_eol t] reads a line from [t] and returns it, without
    the newline ("\n") character at the end, and, if [fix_win_eol] the trailing
    "\r\n" is dropped.
*)
val input_line : ?fix_win_eol:bool -> t -> string option

(** [fold_lines ?fix_win_eol t ~init ~f] folds over the lines read from [t]
    using [input_line].  Lines are provided to [f] in the order they are
    found in the file. *)
val fold_lines :
  ?fix_win_eol:bool -> t -> init:'a -> f:('a -> string -> 'a) -> 'a

(** [input_lines ?fix_win_eol t] returns the list of lines read from [t] using
    [input_line].
*)
val input_lines : ?fix_win_eol:bool -> t -> string list

(** Completely reads an input channel and returns the results as a list of
    strings. Each line in one string. *)
val input_lines : ?fix_win_eol:bool -> t -> string list

(** [iter_lines ?fix_win_eol t ~f] applies [f] to each line read from [t] using
    [input_line]. *)
val iter_lines : ?fix_win_eol:bool -> t -> f:(string -> unit) -> unit

val seek : t -> int64 -> unit
val pos : t -> int64
val length : t -> int64

val set_binary_mode : t -> bool -> unit

(** [read_lines filename] Opens filename, reads all lines, and closes the file. *)
val read_lines : string -> string list

(** [read_all filename] Opens filename, reads all input, and closes the file. *)
val read_all : string -> string
