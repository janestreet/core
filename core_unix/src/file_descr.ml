open! Core
open! Import

module M = struct
  type t = Unix.file_descr

  external to_int : t -> int = "%identity"
  external of_int : int -> t = "%identity"
  external of_int_exn : int -> t = "%identity"

  let of_string string = of_int (Int.of_string string)
  let to_string t = Int.to_string (to_int t)
  let hash t = Int.hash (to_int t)
  let compare t1 t2 = Int.compare (to_int t1) (to_int t2)
  let t_of_sexp sexp = of_int (Int.t_of_sexp sexp)

  let sexp_of_t t =
    (* File descriptors 0, 1, 2 (stdin, stdout, stderr) are stable, so we show them even
       in test. *)
    match am_running_test && Int.( > ) (to_int t) 2 with
    | false -> [%sexp (to_int t : int)]
    | true -> [%sexp "_"]
  ;;
end

include M
include Hashable.Make_plain_and_derive_hash_fold_t (M)

(* Given that [to_int] and [of_int] are set to "%identity", this is considerably more
   direct.  It's unfortunate, but despite [Caml_unix] using [type t = int] in the
   implementation, [Unix.file_descr] is abstract and cannot be tagged [@@immediate]. *)
let equal (t1 : t) t2 = phys_equal t1 t2
