(** Provides a variant type for representing months (e.g., [Jan], [Feb], or [Nov]) and
    functions for converting them to other formats (like an int). *)

include Month_intf.Month (** @inline *)
