(** A time of day along with a time zone.

    Expresses things like "Seinfeld moved to 6:30pm EST.", while a plain [Ofday]
    expresses something more like "I eat lunch at noon (in whichever timezone I'm in at
    the time).". *)

open! Import

module Date = Core_kernel.Std.Date

module Time = struct
  include Time_internal.T
  module Ofday = Core_kernel.Ofday
end

module type Ofday_zoned = sig
  (** Sexps look like "(12:01 nyc)"

      Two [t]'s may or may not correspond to the same times depending on which date
      they're evaluated. *)
  type t [@@deriving bin_io, sexp]

  include Comparable_binable  with type t := t
  include Hashable_binable    with type t := t
  include Pretty_printer.S    with type t := t
  include Stringable          with type t := t (** Strings look like "12:01 nyc" *)


  val create       : Time.Ofday.t -> Zone.t -> t
  val create_local : Time.Ofday.t ->           t

  val ofday : t -> Time.Ofday.t
  val zone  : t -> Zone.t

  val to_time : t -> Date.t -> Time.t

  module Stable : sig
    module V1 : Stable with type t = t
  end
end
