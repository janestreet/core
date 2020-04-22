(** Warning! this library assumes we are in a POSIX compliant OS. *)

open! Import

include module type of Core_kernel.Filename

(** [realpath path] @return the canonicalized absolute pathname of [path].
    @raise Unix_error on errors. *)
val realpath : string -> string

(** Same as {!Core_filename.temp_file}, but returns both the name of a fresh
    temporary file, and an output channel opened (atomically) on
    this file.  This function is more secure than [temp_file]: there
    is no risk that the temporary file will be modified (e.g. replaced
    by a symbolic link) before the program opens it. *)
val open_temp_file
  : ?perm: int -> ?in_dir: string -> string -> string -> string * Out_channel.t

(** Similar to {!Core_filename.open_temp_file}, but returns a Unix file descriptor
    open in read&write mode instead of an [Out_channel.t]. *)
val open_temp_file_fd
  : ?perm: int -> ?in_dir: string -> string -> string -> string * Unix.file_descr

(** [temp_file ?perm ?in_dir_name prefix suffix]

    Returns the name of a fresh temporary file in the temporary directory. The base name
    of the temporary file is formed by concatenating prefix, then [.tmp.], then a 6-digit
    hex number, then suffix. The temporary file is created empty. The file is guaranteed
    to be fresh, i.e. not already existing in the directory.

    @param in_dir the directory in which to create the temporary file.  The default is
    [temp_dir_name]

    @param perm the permission of the temporary file. The default value is [0o600]
    (readable and writable only by the file owner)

    Note that prefix and suffix will be changed when necessary to make the final filename
    valid POSIX.

    [temp_dir] is the same as [temp_file] but creates a temporary directory. *)

val temp_file: ?perm:int -> ?in_dir: string -> string -> string -> string
val temp_dir : ?perm:int -> ?in_dir: string -> string -> string -> string

(** [create_arg_type]'s resulting [Arg_type.t] does bash autocompletion, via [compgen]. *)
val create_arg_type
  :  ?key:'a Univ_map.Multi.Key.t
  -> (string -> 'a)
  -> 'a Core_kernel.Command.Arg_type.t

(** [arg_type] is [create_arg_type Fn.id] *)
val arg_type : string Core_kernel.Command.Arg_type.t
