include module type of struct
  include Core_kernel.Time
end

module Exposed_for_tests : sig end [@@deprecated "[since 2021-03] use [Time_unix]"]
module Hash_queue : sig end [@@deprecated "[since 2021-03] Use [Time_unix]"]
module Hash_set : sig end [@@deprecated "[since 2021-03] Use [Time_unix]"]
module Map : sig end [@@deprecated "[since 2021-03] Use [Time_unix]"]

module Ofday : sig
  include module type of struct
    include Ofday
  end

  val arg_type : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val now : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]

  module Zoned : sig end [@@deprecated "[since 2021-03] Use [Time_unix]"]
end

module Set : sig end [@@deprecated "[since 2021-03] Use [Time_unix]"]

module Span : sig
  include module type of struct
    include Span
  end

  val arg_type : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
end

module Stable : sig
  include module type of struct
    include Stable
  end

  module Ofday : sig
    include module type of struct
      include Ofday
    end

    module Zoned : sig end [@@deprecated "[since 2021-03] Use [Time_unix]"]
  end

  module Span : module type of struct
    include Span
  end

  module V1 : sig end [@@deprecated "[since 2021-03] Use [Time_unix]"]
  module With_t_of_sexp_abs : sig end [@@deprecated "[since 2021-03] Use [Time_unix]"]

  module With_utc_sexp : sig
    include module type of struct
      include With_utc_sexp
    end

    module V1 : sig end [@@deprecated "[since 2021-03] Use [Time_unix]"]

    module V2 : sig
      include module type of struct
        include V2
      end

      val comparator : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]

      module Map : sig end [@@deprecated "[since 2021-03] Use [Time_unix]"]
      module Set : sig end [@@deprecated "[since 2021-03] Use [Time_unix]"]
    end
  end

  module Zone : sig
    include module type of struct
      include Zone
    end

    module V1 : sig end [@@deprecated "[since 2021-03] Use [Time_unix]"]
  end
end

module Table : sig end [@@deprecated "[since 2021-03] Use [Time_unix]"]

module Zone : sig
  include module type of struct
    include Zone
  end

  module Hash_queue : sig end [@@deprecated "[since 2021-03] Use [Time_unix]"]
  module Hash_set : sig end [@@deprecated "[since 2021-03] Use [Time_unix]"]
  module Map : sig end [@@deprecated "[since 2021-03] Use [Time_unix]"]

  module Replace_polymorphic_compare : sig end
  [@@deprecated "[since 2021-03] Use [Time_unix]"]

  module Set : sig end [@@deprecated "[since 2021-03] Use [Time_unix]"]
  module Table : sig end [@@deprecated "[since 2021-03] Use [Time_unix]"]

  val ( < ) : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val ( <= ) : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val ( <> ) : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val ( = ) : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val ( > ) : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val ( >= ) : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val __bin_read_t__ : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val arg_type : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val ascending : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val between : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val bin_read_t : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val bin_reader_t : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val bin_shape_t : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val bin_size_t : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val bin_t : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val bin_write_t : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val bin_writer_t : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val clamp : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val clamp_exn : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val comparator : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val descending : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val equal : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val find : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val find_exn : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val hash : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val hash_fold_t : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val hashable : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val init : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]

  val initialized_zones : [ `Use_Time_unix ]
  [@@deprecated "[since 2021-03] Use [Time_unix]"]

  val local : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val max : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val min : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val of_string : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val pp : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val t_of_sexp : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val to_string : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
  val validate_bound : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]

  val validate_lbound : [ `Use_Time_unix ]
  [@@deprecated "[since 2021-03] Use [Time_unix]"]

  val validate_ubound : [ `Use_Time_unix ]
  [@@deprecated "[since 2021-03] Use [Time_unix]"]
end

val arg_type : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
val format : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
val get_sexp_zone : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
val hashable : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]

val interruptible_pause : [ `Use_Time_unix ]
[@@deprecated "[since 2021-03] Use [Time_unix]"]

val of_date_ofday_zoned : [ `Use_Time_unix ]
[@@deprecated "[since 2021-03] Use [Time_unix]"]

val of_string : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
val of_string_abs : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]

val of_string_fix_proto : [ `Use_Time_unix ]
[@@deprecated "[since 2021-03] Use [Time_unix]"]

val of_string_gen : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
val of_tm : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
val parse : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
val pause : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
val pause_forever : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
val pp : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
val set_sexp_zone : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
val sexp_of_t : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
val sexp_of_t_abs : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
val t_of_sexp : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
val t_of_sexp_abs : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]

val to_date_ofday_zoned : [ `Use_Time_unix ]
[@@deprecated "[since 2021-03] Use [Time_unix]"]

val to_ofday_zoned : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]
val to_string : [ `Use_Time_unix ] [@@deprecated "[since 2021-03] Use [Time_unix]"]

val to_string_fix_proto : [ `Use_Time_unix ]
[@@deprecated "[since 2021-03] Use [Time_unix]"]
