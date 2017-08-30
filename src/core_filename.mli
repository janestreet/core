(** Warning! this library assumes we are in a POSIX compliant OS. *)

open! Import

(**  The path of the root.*)
val root : string

(** {2 Pathname resolution} *)

(** [realpath path] @return the canonicalized absolute pathname of [path].
    @raise Unix_error on errors. *)
val realpath : string -> string

(** [is_posix_pathname_component f]
    @return true if [f] is a valid path component on a POSIX compliant OS

    Note that this checks a path component, and not a full path.

    http://www.opengroup.org/onlinepubs/000095399/basedefs/xbd_chap03.html#tag_03_169
*)
val is_posix_pathname_component : string -> bool

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

(** The name of the temporary directory:

    Under Unix, the value of the [TMPDIR] environment variable, or "/tmp" if the variable
    is not set.

    Under Windows, the value of the [TEMP] environment variable, or "."  if the variable
    is not set. *)
val temp_dir_name : string

(** Same as {!Core_filename.temp_file}, but returns both the name of a fresh
    temporary file, and an output channel opened (atomically) on
    this file.  This function is more secure than [temp_file]: there
    is no risk that the temporary file will be modified (e.g. replaced
    by a symbolic link) before the program opens it. *)
val open_temp_file
  : ?perm: int -> ?in_dir: string -> string -> string -> string * Out_channel.t

(** The conventional name for the current directory (e.g. [.] in Unix). *)
val current_dir_name : string

(** The conventional name for the parent of the current directory
    (e.g. [..] in Unix). *)
val parent_dir_name : string

(** The directory separator (e.g. [/] in Unix). *)
val dir_sep : string

(** [concat p1 p2] returns a path equivalent to [p1 ^ "/" ^ p2].
    In the resulting path p1 (resp. p2) has all its trailing (resp. leading)
    "." and "/" removed. eg:
    concat "a/." ".//b" => "a/b"
    concat "." "b" => "./b"
    concat "a" "." => "a/."
    concat "a" "/b" => "a/b"

    @throws Failure if [p1] is empty.
*)
val concat : string -> string -> string

(** Return [true] if the file name is relative to the current
    directory, [false] if it is absolute (i.e. in Unix, starts
    with [/]). *)
val is_relative : string -> bool

val is_absolute : string -> bool

(** Return [true] if the file name is relative and does not start
    with an explicit reference to the current directory ([./] or
    [../] in Unix), [false] if it starts with an explicit reference
    to the root directory or the current directory. *)
val is_implicit : string -> bool

(** [check_suffix name suff] returns [true] if the filename [name]
    ends with the suffix [suff]. *)
val check_suffix : string -> string -> bool

(** [chop_suffix name suff] removes the suffix [suff] from
    the filename [name]. The behavior is undefined if [name] does not
    end with the suffix [suff]. *)
val chop_suffix : string -> string -> string

(** Return the given file name without its extension. The extension
    is the shortest suffix starting with a period and not including
    a directory separator, [.xyz] for instance.

    Raise [Invalid_argument] if the given name does not contain
    an extension. *)
val chop_extension : string -> string

(** [split_extension fn] return the portion of the filename before the
    extension and the (optional) extension.
    Example:
    split_extension "/foo/my_file" = ("/foo/my_file", None)
    split_extension "/foo/my_file.txt" = ("/foo/my_file", Some "txt")
    split_extension "/home/c.falls/my_file" = ("/home/c.falls/my_file", None)
*)
val split_extension : string -> (string * string option)

(** Respects the posix semantic.

    Split a file name into directory name / base file name.
    [concat (dirname name) (basename name)] returns a file name
    which is equivalent to [name]. Moreover, after setting the
    current directory to [dirname name] (with {!Sys.chdir}),
    references to [basename name] (which is a relative file name)
    designate the same file as [name] before the call to {!Sys.chdir}.

    The result is not specified if the argument is not a valid file name
    (for example, under Unix if there is a NUL character in the string). *)
val basename : string -> string

(** See {!Filename.basename}. *)
val dirname : string -> string

(** [split filename] returns (dirname filename, basename filename) *)
val split : string -> string * string

(** [parts filename] returns a list of path components in order.  For instance:
    /tmp/foo/bar/baz -> ["/"; "tmp"; "foo"; "bar"; "baz"]. The first component is always
    either "." for relative paths or "/" for absolute ones. *)
val parts : string -> string list

(** [of_parts parts] joins a list of path components into a path. It does roughly the
    opposite of [parts], but they fail to be precisely mutually inverse because of
    ambiguities like multiple consecutive slashes and . components.

    Raises an error if given an empty list. *)
val of_parts : string list -> string

(** Return a quoted version of a file name, suitable for use as one argument in a command
    line, escaping all meta-characters.
    Warning: under Windows, the output is only suitable for use with programs that follow
    the standard Windows quoting conventions.

    See [Sys.quote] for an alternative implementation that is more human readable but less
    portable.
*)
val quote : string -> string

