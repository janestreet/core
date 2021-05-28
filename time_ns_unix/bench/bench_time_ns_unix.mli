open! Core

(* make sure we include a benchmark for everything in [Time_ns_unix] *)
include module type of struct
  include Time_ns_unix
end [@ocaml.remove_aliases]
