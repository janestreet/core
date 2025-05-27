module type Date = sig @@ portable
  type t = Date0.t

  include module type of Date0 with type t := t (** @inline *)

  val of_time : Time_float.t -> zone:Zone.t -> t
  val today : zone:Zone.t -> t

  (** Deprecations *)

  val format : [ `Use_Date_unix ] [@@deprecated "[since 2021-03] Use [Date_unix]"]
  val of_tm : [ `Use_Date_unix ] [@@deprecated "[since 2021-03] Use [Date_unix]"]
  val parse : [ `Use_Date_unix ] [@@deprecated "[since 2021-03] Use [Date_unix]"]
end
