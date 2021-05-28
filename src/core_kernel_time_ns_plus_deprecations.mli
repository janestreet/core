(** This module extends [Core_kernel.Time_ns] with deprecations for all of the
    functionality that used to be in [Core.Time_ns], but has been moved to
    [Time_ns_unix]. *)

include module type of struct
  include Core_kernel.Time_ns
end

val arg_type : [ `Use_Time_ns_unix ] [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
val comparator : [ `Use_Time_ns_unix ] [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val get_sexp_zone : [ `Use_Time_ns_unix ]
[@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val interruptible_pause : [ `Use_Time_ns_unix ]
[@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val of_date_ofday_zoned : [ `Use_Time_ns_unix ]
[@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val of_string : [ `Use_Time_ns_unix ] [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val of_string_abs : [ `Use_Time_ns_unix ]
[@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val of_string_fix_proto : [ `Use_Time_ns_unix ]
[@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val of_string_gen : [ `Use_Time_ns_unix ]
[@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val pause : [ `Use_Time_ns_unix ] [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val pause_forever : [ `Use_Time_ns_unix ]
[@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val pp : [ `Use_Time_ns_unix ] [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val set_sexp_zone : [ `Use_Time_ns_unix ]
[@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val sexp_of_t : [ `Use_Time_ns_unix ] [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val sexp_of_t_abs : [ `Use_Time_ns_unix ]
[@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val t_of_sexp : [ `Use_Time_ns_unix ] [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val t_of_sexp_abs : [ `Use_Time_ns_unix ]
[@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val to_date_ofday_zoned : [ `Use_Time_ns_unix ]
[@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val to_ofday_zoned : [ `Use_Time_ns_unix ]
[@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val to_string : [ `Use_Time_ns_unix ] [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val to_string_fix_proto : [ `Use_Time_ns_unix ]
[@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val validate_bound : [ `Use_Time_ns_unix ]
[@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val validate_lbound : [ `Use_Time_ns_unix ]
[@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

val validate_ubound : [ `Use_Time_ns_unix ]
[@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

module Hash_queue : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
module Hash_set : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
module Map : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

module Ofday : sig
  include module type of struct
    include Ofday
  end

  val arg_type : [ `Use_Time_ns_unix ] [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
  val now : [ `Use_Time_ns_unix ] [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val of_ofday_float_round_nearest : [ `Use_Time_ns_unix ]
  [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val of_ofday_float_round_nearest_microsecond : [ `Use_Time_ns_unix ]
  [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val to_ofday_float_round_nearest : [ `Use_Time_ns_unix ]
  [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val to_ofday_float_round_nearest_microsecond : [ `Use_Time_ns_unix ]
  [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  module Option : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
  module Zoned : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
end

module Option : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

module Replace_polymorphic_compare : sig end
[@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

module Set : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

module Span : sig
  include module type of struct
    include Span
  end

  val arg_type : [ `Use_Time_ns_unix ] [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  module Option : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
  module Stable : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
end

module Stable : sig
  include module type of struct
    include Stable
  end

  module Ofday : sig
    include module type of struct
      include Ofday
    end

    module Option : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
  end

  module Option : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
  module V1 : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
end

module Table : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
module Zone : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
