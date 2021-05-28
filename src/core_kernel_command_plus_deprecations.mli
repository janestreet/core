include module type of struct
  include Core_kernel.Command
end

val run : [ `Use_Command_unix ] [@@deprecated "[since 2021-03] Use [Command_unix]"]

module Path : sig end [@@deprecated "[since 2021-03] Use [Command_unix]"]

module Shape : sig
  include module type of struct
    include Shape
  end

  val help_text : [ `Use_Command_unix ]
  [@@deprecated "[since 2021-03] Use [Command_unix]"]
end

val shape : [ `Use_Command_unix ] [@@deprecated "[since 2021-03] Use [Command_unix]"]
