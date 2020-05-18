open! Import

include (module type of Core_kernel.Command
          with module Shape := Command.Shape
          with module Deprecated := Command.Deprecated)

(** Runs a command against [Sys.argv], or [argv] if it is specified.

    [extend] can be used to add extra command line arguments to basic subcommands of the
    command.  [extend] will be passed the (fully expanded) path to a command, and its
    output will be appended to the list of arguments being processed.  For example,
    suppose a program like this is compiled into [exe]:

    {[
      let bar = Command.basic ___
      let foo = Command.group ~summary:___ ["bar", bar]
      let main = Command.group ~summary:___ ["foo", foo]
      let () = Command.run ~extend:(fun _ -> ["-baz"]) main
    ]}

    Then if a user ran [exe f b], [extend] would be passed [["foo"; "bar"]] and ["-baz"]
    would be appended to the command line for processing by [bar].  This can be used to
    add a default flags section to a user config file.

    [verbose_on_parse_error] controls whether to print a line suggesting the user try the
    "-help" flag when an exception is raised while parsing the arguments.  By default it
    is true.

    [when_parsing_succeeds] is invoked after argument parsing has completed successfully,
    but before the main function of the associated command has run. One use-case is for
    performing logging when a command is being invoked, where there's no reason to log
    incorrect invocations or -help calls.
*)
val run
  :  ?verbose_on_parse_error : bool
  -> ?version    : string
  -> ?build_info : string
  -> ?argv       : string list
  -> ?extend     : (string list -> string list)
  -> ?when_parsing_succeeds:(unit -> unit)
  -> t
  -> unit

module Path : sig
  (** [Path.t] is a top-level executable name and sequence of subcommand names that can be
      used to identify a command. *)
  type t

  (** [create] creates a path from a toplevel executable given by [path_to_exe]. *)
  val create : path_to_exe:string -> t

  (** [append] appends a subcommand to [t]. *)
  val append : t -> subcommand:string -> t

  (** [parts] returns a list containing the path's executable name followed by its
      subcommands. *)
  val parts : t -> string list
end

module Shape : sig
  include module type of struct include Core_kernel.Command.Shape end

  (** Get the help text for a command shape.

      The [Path.t] argument should be the path that identifies the shape argument.

      [expand_dots]: expand subcommands in recursive help. (default: false)
      This is the same as the [help] subcommand's ["-expand-dots"] flag.

      [flags]: show flags in recursive help. (default: false)
      This is the same as the [help] subcommand's ["-flags"] flag.

      [recursive]: show subcommands of subcommands. (default: false)
      This is the same as the [help] subcommand's ["-recursive"] flag. *)
  val help_text
    :  t
    -> Path.t
    -> expand_dots:bool
    -> flags:bool
    -> recursive:bool
    -> string
end

(** Exposes the shape of a command. *)
val shape : t -> Shape.t

(** [Deprecated] should be used only by [Deprecated_command].  At some point
    it will go away. *)
module Deprecated : sig
  include module type of struct include Core_kernel.Command.Deprecated end

  val run
    :  t
    -> cmd               : string
    -> args              : string list
    -> is_help           : bool
    -> is_help_rec       : bool
    -> is_help_rec_flags : bool
    -> is_expand_dots    : bool
    -> unit
end
