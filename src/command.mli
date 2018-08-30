open! Import
open Import_time

include (module type of Core_kernel.Command
          with module Arg_type := Command.Arg_type
          with module Deprecated := Command.Deprecated
          with module Param := Command.Param
          with module Let_syntax := Command.Let_syntax
          with module Spec := Command.Spec)

(** Argument types. *)
module Arg_type : sig
  include (module type of struct include Core_kernel.Command.Arg_type end
            with module Export := Core_kernel.Command.Arg_type.Export)

  (** [file] defines an [Arg_type.t] that completes in the same way as
      [Command.Spec.file], but perhaps with a different type than [string] or with an
      autocompletion key. *)
  val file
    :  ?key:'a Univ_map.Multi.Key.t
    -> (string -> 'a)
    -> 'a t

  module Export : sig
    include module type of struct include Core_kernel.Command.Arg_type.Export end

    val time               : Time.t             t

    (** Requires a time zone. *)
    val time_ofday         : Time.Ofday.Zoned.t t

    (** For when the time zone is implied. *)
    val time_ofday_unzoned : Time.Ofday.t       t

    val time_zone          : Time.Zone.t        t
    val time_span          : Time.Span.t        t

    (** Uses bash autocompletion. *)
    val file               : string             t

    val ip_address         : Unix.inet_addr     t
  end
end

module Param : sig
  include module type of Arg_type.Export
  module Arg_type = Arg_type
  include (module type of struct include Core_kernel.Command.Param end
            with module Arg_type := Core_kernel.Command.Param.Arg_type)
end

module Let_syntax : sig
  type 'a t (** Substituted below. *)

  val return : 'a -> 'a t
  include Applicative.Applicative_infix with type 'a t := 'a t

  module Let_syntax : sig
    type 'a t (** Substituted below. *)

    val return : 'a -> 'a t
    val map    : 'a t -> f:('a -> 'b) -> 'b t
    val both   : 'a t -> 'b t -> ('a * 'b) t
    module Open_on_rhs = Param
  end with type 'a t := 'a Param.t
end with type 'a t := 'a Param.t

module Spec : sig
  include module type of Arg_type.Export
  module Arg_type = Arg_type
  include (module type of struct include Core_kernel.Command.Spec end
            with module Arg_type := Core_kernel.Command.Spec.Arg_type)
end

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
*)
val run
  :  ?version    : string
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
