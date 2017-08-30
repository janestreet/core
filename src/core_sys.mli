(** System interface. *)

open! Import

(** The command line arguments given to the process.  The first element is the
    command name used to invoke the program.  The following elements are the
    command-line arguments given to the program. *)
val argv : string array

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
  : ?follow_symlinks:bool (** defaults to true *) -> string ->  [ `Yes | `No | `Unknown ]

(** Same as [file_exists] but blows up on [`Unknown] *)
val file_exists_exn
  : ?follow_symlinks:bool (** defaults to true *) -> string -> bool

(** Returns [`Yes] if the file exists and is a directory*)
val is_directory
  : ?follow_symlinks:bool (** defaults to true *) -> string -> [ `Yes | `No | `Unknown ]

(** Returns [`Yes] if the file exists and is a regular file *)
val is_file
  : ?follow_symlinks:bool (** defaults to true *) -> string -> [ `Yes | `No | `Unknown ]
val is_directory_exn : ?follow_symlinks:bool (** defaults to true *) -> string -> bool
val is_file_exn      : ?follow_symlinks:bool (** defaults to true *) -> string -> bool

(** Remove the given file name from the file system. *)
val remove : string -> unit

(** Rename a file. The first argument is the old name and the second is the new
    name. If there is already another file under the new name, [rename] may
    replace it, or raise an exception, depending on your operating system. *)
val rename : string -> string -> unit

(** Return the value associated to a variable in the process environment.  Return [None]
    if the variable is unbound.  *)
val getenv : string -> string option
val getenv_exn : string -> string

(** Execute the given shell command and return its exit code. *)
val command : string -> int

(** [command_exn command] runs [command] and then raises an exception if it
    returns with nonzero exit status. *)
val command_exn : string -> unit

(** [quote s] quotes the string in a format suitable for the shell of the current system
    (e.g. suitable for [command]).  On Unix, this function only quotes as necessary, which
    makes its output more legible than [Filename.quote].

    WARNING: This may not work with some shells, but should work with sh, bash, and zsh.
*)
val quote : string -> string

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

(** This reference is initially set to [false] in standalone programs and to
    [true] if the code is being executed under the interactive toplevel system
    [ocaml]. *)
val interactive : bool ref

(** Operating system currently executing the Caml program. One of
    -  ["Unix"] (for all Unix versions, including Linux and Mac OS X),
    -  ["Win32"] (for MS-Windows, OCaml compiled with MSVC++ or Mingw),
    -  ["Cygwin"] (for MS-Windows, OCaml compiled with Cygwin). *)
val os_type : string

(** Size of one word on the machine currently executing the Caml program, in
    bits: 32 or 64. *)
val word_size : int

(** Size of an int.  It is 31 bits (resp. 63 bits) when using the OCaml compiler on a 32
    bits (resp. 64 bits) platform.  It may differ for other compilers, e.g. it is 32 bits
    when compiling to JavaScript.  @since 4.03.0 *)
val int_size : int

(** Whether the machine currently executing the Caml program is big-endian. *)
val big_endian : bool

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

(** [ocaml_version] is the version of Objective Caml.  It is a string of the form
    ["major.minor[.patchlevel][+additional-info]"], where [major], [minor], and
    [patchlevel] are integers, and [additional-info] is an arbitrary string. The
    [[.patchlevel]] and [[+additional-info]] parts may be absent. *)
val ocaml_version : string

(** [execution_mode] tests whether the code being executed was compiled natively
    or to bytecode. *)
val execution_mode : unit -> [ `Bytecode | `Native ]

(** [c_int_size] returns the number of bits in a C [int]. Note that this can be
    different from [word_size]. For example, Linux x86-64 should have
    [word_size = 64], but [c_int_size () = 32] *)
external c_int_size : unit -> int = "c_int_size" [@@noalloc]

(** Return the home directory, using the [HOME] environment variable if that is defined,
    and if not, using the effective user's information in the Unix password database. *)
val home_directory : unit -> string

(** {6 Optimization} *)

(** For the purposes of optimization, [opaque_identity] behaves like an unknown (and thus
    possibly side-effecting) function.  At runtime, [opaque_identity] disappears
    altogether.  A typical use of this function is to prevent pure computations from being
    optimized away in benchmarking loops.  For example:

    {[
      for _round = 1 to 100_000 do
        ignore (Sys.opaque_identity (my_pure_computation ()))
      done
    ]}
*)
external opaque_identity : 'a -> 'a = "%opaque"

