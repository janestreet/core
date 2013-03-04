(** A list of pretty printers for various types, for use in toplevels.

    [Pretty_printer] has a [string list ref] with the names of [pp] functions matching the
    interface:

    {[
      val pp : Format.formatter -> t -> unit
    ]}

    The names are actually OCaml identifier names, e.g. "Core.Date.pp".  Code for building
    toplevels (this code is not in Core) evaluates the strings to yield the pretty
    printers and register them with OCaml runtime.
*)

(** [all ()] returns all pretty printers that have been [register]ed. *)
val all : unit -> string list

(** [register name] adds [name] to the list of pretty printers. *)
val register : string -> unit

(** Modules that provide a pretty printer will match [S]. *)
module type S = sig
  type t
  val pp : Format.formatter -> t -> unit
end

(** [Register] builds a [pp] function from a [to_string] function, and adds the
    [module_name ^ ".pp"] to the list of pretty printers.  The idea is to statically
    guarantee that one has the desired [pp] function at the same point where the [name] is
    added. *)
module Register (M : sig
  type t
  val module_name : string
  val to_string : t -> string
end) : S with type t := M.t

(** [Register_pp] is like [Register], but allows a custom [pp] function rather than using
    [to_string]. *)
module Register_pp (M : sig
  include S
  val module_name : string
end) : S with type t := M.t
