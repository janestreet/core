open! Core
open! Import

module Simple_group : sig
  (** A simple example for completion at various depths in the command tree *)

  val basic : Command.t
  val group : Command.t
  val command : Command.t
end
