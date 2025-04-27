@@ portable

(** @inline *)
include module type of struct
  include Base.Atomic
end

(*_ This module will soon be extended with more definitions, including stability and
  bin_io serialization *)
