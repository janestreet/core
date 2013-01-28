open Bin_prot.Std
open Sexplib.Std

include Binable0

(* [of_string] and [to_string] can't go in binable0.ml due to a cyclic dependency. *)
let of_string m string = of_bigstring m (Bigstring.of_string string)

let to_string m t = Bigstring.to_string (to_bigstring m t)

TEST_UNIT =
  let module M = struct type t = int with bin_io end in
  let m = (module M : S with type t = int) in
  List.iter [ min_int; min_int / 2; -1; 0; 1; max_int / 2; max_int; ]
    ~f:(fun i ->
      let check name of_x to_x =
        let i' = of_x m (to_x m i) in
        if i <> i' then
          Error.failwiths (Printf.sprintf "Binable.{of,to}_%s failure" name)
            (i, `Round_tripped_to i') <:sexp_of< int * [ `Round_tripped_to of int ] >>
      in
      check "string"    of_string    to_string;
      check "bigstring" of_bigstring to_bigstring;
    )
;;
