open! Import

include (module type of Core_kernel.Command
          with module Deprecated := Command.Deprecated)

(** Exposes the shape of a command. *)
val shape : t -> Shape.t

(** Runs a command against [Sys.argv], or [argv] if it is specified.

    [extend] can be used to add extra command line arguments to basic subcommands of the
    command.  [extend] will be passed the (fully expanded) path to a command, and its
    output will be appended to the list of arguments being processed.  For example,
    suppose a program like this is compiled into [exe]:

    {[
      let bar = Command.basic ...
                  let foo = Command.group ~summary:... ["bar", bar]
      let main = Command.group ~summary:... ["foo", foo]
                   Command.run ~extend:(fun _ -> ["-baz"]) main
    ]}

    Then if a user ran [exe f b], [extend] would be passed [["foo"; "bar"]] and ["-baz"]
    would be appended to the command line for processing by [bar].  This can be used to
    add a default flags section to a user config file.

    [verbose_on_parse_error] controls whether to print a line suggesting the user try the
    "-help" flag when an exception is raised while parsing the arguments.  By default it
    is true.
*)
val run
  :  ?verbose_on_parse_error : bool
  -> ?version    : string
  -> ?build_info : string
  -> ?argv       : string list
  -> ?extend     : (string list -> string list)
  -> t
  -> unit

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
