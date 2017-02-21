open! Import
open Core_kernel.Core_kernel_private

module  T = Core_time.Make (Time_float0) (Time_float)
include (T : module type of struct include T end
         with module Table      := T.Table
          and module Hash_set   := T.Hash_set
          and module Hash_queue := T.Hash_queue)

(* Previous versions rendered hash-based containers using float serialization rather than
   time serialization, so when reading hash-based containers in we accept either
   serialization. *)
include Hashable.Make_binable (struct
    include T

    let t_of_sexp sexp =
      match Float.t_of_sexp sexp with
      | float       -> of_span_since_epoch (Span.of_sec float)
      | exception _ -> t_of_sexp sexp
  end)
