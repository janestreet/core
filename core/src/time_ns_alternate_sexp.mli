(** A [Time_ns] that uses its alternate sexp representation. **)

open! Import

include module type of struct
  include Time_ns
end

include module type of struct
  include Alternate_sexp
end
