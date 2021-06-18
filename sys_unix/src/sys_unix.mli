(** System interface. *)

open! Core
open! Import

(** The name of the file containing the executable currently running. *)
val executable_name : string


(** For all of the following functions, [?follow_symlinks] defaults to [true]. *)

(** [file_exists ~follow_symlinks path]

    Test whether the file in [path] exists on the file system.
    If [follow_symlinks] is [true] and [path] is a symlink the result concerns
    the target of the symlink.

    [`Unknown] is returned for files for which we cannot successfully determine
    whether they are on the system or not (e.g. files in directories to which we
    do not have read permission). *)
val file_exists
  :  ?follow_symlinks:bool (** defaults to true *)
  -> string
  -> [ `Yes | `No | `Unknown ]

(** Same as [file_exists] but blows up on [`Unknown] *)
val file_exists_exn : ?follow_symlinks:bool (** defaults to true *) -> string -> bool

(** Returns [`Yes] if the file exists and is a directory*)
val is_directory
  :  ?follow_symlinks:bool (** defaults to true *)
  -> string
  -> [ `Yes | `No | `Unknown ]

(** Returns [`Yes] if the file exists and is a regular file *)
val is_file
  :  ?follow_symlinks:bool (** defaults to true *)
  -> string
  -> [ `Yes | `No | `Unknown ]

val is_directory_exn : ?follow_symlinks:bool (** defaults to true *) -> string -> bool
val is_file_exn : ?follow_symlinks:bool (** defaults to true *) -> string -> bool

(** Remove the given file name from the file system. *)
val remove : string -> unit


(** Rename a file. The first argument is the old name and the second is the new
    name. If there is already another file under the new name, [rename] may
    replace it, or raise an exception, depending on your operating system. *)
val rename : string -> string -> unit

(** Return the value associated to a variable in the process environment.

    Unlike {!getenv}, this function returns the value even if the
    process has special privileges. It is considered unsafe because the
    programmer of a setuid or setgid program must be careful to avoid
    using maliciously crafted environment variables in the search path
    for executables, the locations for temporary files or logs, and the
    like. *)
val unsafe_getenv : string -> string option

val unsafe_getenv_exn : string -> string

(** Execute the given shell command and return its exit code. *)
val command : string -> int

(** [command_exn command] runs [command] and then raises an exception if it
    returns with nonzero exit status. *)
val command_exn : string -> unit

(** Change the current working directory of the process. *)
val chdir : string -> unit

(** Return the current working directory of the process. *)
val getcwd : unit -> string

(** Return the names of all files present in the given directory.  Names
    denoting the current directory and the parent directory (["."] and [".."] in
    Unix) are not returned.  Each string in the result is a file name rather
    than a complete path.  There is no guarantee that the name strings in the
    resulting array will appear in any specific order; they are not, in
    particular, guaranteed to appear in alphabetical order. *)
val readdir : string -> string array


(**
   Call [readdir], and fold over the elements of the array.
   @raise Sys_error _ if readdir fails.
   As with [readdir], ["."] and [".."] are not returned
   raises the same exception than opendir and closedir.
*)
val fold_dir : init:'acc -> f:('acc -> string -> 'acc) -> string -> 'acc


(**
   Same as [readdir], but return a list rather than an array.
*)
val ls_dir : string -> string list

(** Exception raised on interactive interrupt if {!Sys.catch_break} is on. *)
exception Break

(** Warning: this function clobbers the Signal.int (SIGINT) handler.  SIGINT is the
    signal that's sent to your program when you hit CTRL-C.

    Warning: catch_break uses deep ocaml runtime magic to raise Sys.Break inside of the
    main execution context.  Consider explicitly handling Signal.int instead.  If
    all you want to do is terminate on CTRL-C you don't have to do any special setup,
    that's the default behavior.

    [catch_break] governs whether interactive interrupt (ctrl-C) terminates the
    program or raises the [Break] exception.  Call [catch_break true] to enable
    raising [Break], and [catch_break false] to let the system terminate the
    program on user interrupt.
*)
val catch_break : bool -> unit


(** [execution_mode] tests whether the code being executed was compiled natively
    or to bytecode. *)
val execution_mode : unit -> [ `Bytecode | `Native ]

(** [c_int_size] returns the number of bits in a C [int], as specified in header
    files. Note that this can be different from [word_size] and [Nativeint.num_bits]. For
    example, Linux x86-64 should have [word_size = 64], but [c_int_size () = 32]. *)
external c_int_size : unit -> int = "c_int_size"
[@@noalloc]


(** Return the home directory, using the [HOME] environment variable if that is defined,
    and if not, using the effective user's information in the Unix password database. *)
val home_directory : unit -> string

(** {6 Optimization} *)

(** [override_argv new_argv] makes subsequent calls to {!get_argv} return [new_argv].

    Prior to OCaml version 4.09, this function has two noteworthy behaviors:

    - it may raise if the length of [new_argv] is greater than the length of [argv] before
      the call;
    - it re-uses and mutates the previous [argv] value instead of using the new one; and
    - it even mutates its length, which can be observed by inspecting the array returned
      by an earlier call to {!get_argv}.
*)
val override_argv : string array -> unit

