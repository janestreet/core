open! Import
open! Std_internal

module type S_kernel_without_zone = Time0_intf.S
module type S_kernel = Time_intf.S

include Time_intf.S with module Time := Time_float0

module Ofday : sig
  include module type of struct
    include Ofday
  end

  val arg_type : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
  val now : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  module Zoned : sig end [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
end

module Span : sig
  include module type of struct
    include Span
  end

  val arg_type : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
end

module Zone : sig
  include module type of struct
    include Zone
  end

  module Hash_queue : sig end [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
  module Hash_set : sig end [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
  module Map : sig end [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  module Replace_polymorphic_compare : sig end
  [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  module Set : sig end [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
  module Table : sig end [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val ( < ) : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
  val ( <= ) : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
  val ( <> ) : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
  val ( = ) : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
  val ( > ) : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
  val ( >= ) : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val __bin_read_t__ : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val arg_type : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val ascending : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val between : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val bin_read_t : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val bin_reader_t : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val bin_shape_t : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val bin_size_t : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val bin_t : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val bin_write_t : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val bin_writer_t : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val clamp : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val clamp_exn : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val comparator : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val descending : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val equal : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
  val find : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
  val find_exn : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
  val hash : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val hash_fold_t : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val hashable : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
  val init : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val initialized_zones : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val local : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
  val max : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
  val min : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val of_string : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val pp : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val t_of_sexp : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val to_string : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val validate_bound : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val validate_lbound : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  val validate_ubound : [ `Use_Time_unix ]
    [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
end

module Stable : sig
  include module type of struct
    include Time_float0.Stable
  end

  module Ofday : sig
    include module type of struct
      include Ofday
    end

    module Zoned : sig end [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
  end

  module V1 : sig end [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  module With_t_of_sexp_abs : sig end
  [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

  module With_utc_sexp : sig
    module V1 : sig end [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

    (** [Time_float_unix.Stable.V1.t_of_sexp] has parsing that understands non-UTC time
        zones. *)
    module V2 : sig
      type t = Time_float0.t [@@deriving hash, sexp_grammar]

      include Stable_without_comparator_with_witness with type t := t

      val comparator : [ `Use_Time_unix ]
        [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

      module Map : sig end [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
      module Set : sig end [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
    end
  end

  module Zone : sig
    module V1 : sig end [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
    include Zone_intf.S_stable with type t := Zone.t
  end
end

module Exposed_for_tests : sig end [@@deprecated "[since 2021-03] use [Time_float_unix]"]
module Hash_queue : sig end [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
module Hash_set : sig end [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
module Map : sig end [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
module Set : sig end [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
module Table : sig end [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

val arg_type : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
val format : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

val get_sexp_zone : [ `Use_Time_unix ]
  [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

val hashable : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

val interruptible_pause : [ `Use_Time_unix ]
  [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

val of_date_ofday_zoned : [ `Use_Time_unix ]
  [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

val of_string : string -> t
  [@@deprecated
    "[since 2021-03] Use [of_string_with_utc_offset] or [Time_float_unix.of_string]"]

val of_string_abs : [ `Use_Time_unix ]
  [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

val of_string_fix_proto : [ `Use_Time_unix ]
  [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

val of_tm : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
val parse : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
val pause : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

val pause_forever : [ `Use_Time_unix ]
  [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

val pp : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

val set_sexp_zone : [ `Use_Time_unix ]
  [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

val sexp_of_t : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

val sexp_of_t_abs : [ `Use_Time_unix ]
  [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

val t_of_sexp : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

val t_of_sexp_abs : [ `Use_Time_unix ]
  [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

val to_date_ofday_zoned : [ `Use_Time_unix ]
  [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

val to_ofday_zoned : [ `Use_Time_unix ]
  [@@deprecated "[since 2021-03] Use [Time_float_unix]"]

val to_string : t -> string
  [@@deprecated "[since 2021-03] Use [to_string_utc] or [Time_float_unix.to_string]"]

val to_string_fix_proto : [ `Use_Time_unix ]
  [@@deprecated "[since 2021-03] Use [Time_float_unix]"]
