type t = out_channel

val stdout : t
val stderr : t

type 'a with_create_args = ?binary:bool -> ?append:bool -> ?perm:int -> 'a

val create : (string -> t) with_create_args
val with_file : (string -> f:(t -> 'a) -> 'a) with_create_args


(* [close t] flushes and closes [t], and may raise an exception.  [close] returns () and
   does not raise if [t] is already closed.  [close] raises an exception if the close()
   system call on the underlying file descriptor fails (i.e. returns -1), which would
   happen if the underlying file descriptor was closed. *)
val close : t -> unit

(* [close_noerr] flushes and closes [t] and never raises an exception. *)
val close_noerr : t -> unit

val set_binary_mode : t -> bool -> unit

val flush : t -> unit

val output : t -> buf:string -> pos:int -> len:int -> unit
val output_string : t -> string -> unit
val output_char : t -> char -> unit
val output_byte : t -> int -> unit
val output_binary_int : t -> int -> unit
val output_value : t -> _ -> unit

val newline : t -> unit

(** Outputs a list of lines, each terminated by a newline character *)
val output_lines : t -> string list -> unit

val seek : t -> int64 -> unit
val pos : t -> int64
val length : t -> int64

(** [write_lines file lines] works like output_lines, except that the output is sent to
    the named file. *)
val write_lines : string -> string list -> unit

(** [write_all file data] writes all data to the named file. *)
val write_all : string -> data:string -> unit


