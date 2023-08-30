open! Base

type t = string [@@deriving compare, hash, sexp, sexp_grammar]

include
  Comparable.S with type t := t with type comparator_witness = String.comparator_witness

(**  The path of the root.*)
val root : string

(** {2 Pathname resolution} *)

(** [is_posix_pathname_component f]
    @return true if [f] is a valid path component on a POSIX compliant OS

    Note that this checks a path component, and not a full path.

    http://www.opengroup.org/onlinepubs/000095399/basedefs/xbd_chap03.html#tag_03_169
*)
val is_posix_pathname_component : string -> bool

(** The name of the temporary directory:

    Under Unix, the value of the [TMPDIR] environment variable, or "/tmp" if the variable
    is not set.

    Under Windows, the value of the [TEMP] environment variable, or "."  if the variable
    is not set. *)
val temp_dir_name : string

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
    end with the suffix [suff]. [chop_suffix_opt] is thus recommended
    instead.*)
val chop_suffix : string -> string -> string

(** [chop_suffix_opt ~suffix filename] removes the suffix from
    the [filename] if possible, or returns [None] if the
    filename does not end with the suffix.
*)
val chop_suffix_opt : suffix:string -> string -> string option

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
val split_extension : string -> string * string option

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

(** Returns the absolute path by prepending [relative_to] if the path is not already
    absolute.

    Using the result of [Core_unix.getcwd] as [relative_to] is often a reasonable choice.

    Note that [to_absolute_exn] may return a non-canonical path (e.g. /foo/bar/../baz).

    Raises if [relative_to] is a relative path.
*)
val to_absolute_exn : string -> relative_to:string -> string

(** Converts an absolute path to a relative one.

    Raises if either argument is a relative path.
*)
val of_absolute_exn : string -> relative_to:string -> string

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

val arg_type : [ `Use_Filename_unix ] [@@deprecated "[since 2021-04] Use [Filename_unix]"]

val create_arg_type : [ `Use_Filename_unix ]
  [@@deprecated "[since 2021-04] Use [Filename_unix]"]

val open_temp_file : [ `Use_Filename_unix ]
  [@@deprecated "[since 2021-04] Use [Filename_unix]"]

val open_temp_file_fd : [ `Use_Filename_unix ]
  [@@deprecated "[since 2021-04] Use [Filename_unix]"]

val realpath : [ `Use_Filename_unix ] [@@deprecated "[since 2021-04] Use [Filename_unix]"]
val temp_dir : [ `Use_Filename_unix ] [@@deprecated "[since 2021-04] Use [Filename_unix]"]

val temp_file : [ `Use_Filename_unix ]
  [@@deprecated "[since 2021-04] Use [Filename_unix]"]
