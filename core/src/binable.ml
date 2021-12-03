open! Import
include Binable_intf
include Binable0

(* [of_string] and [to_string] can't go in binable0.ml due to a cyclic dependency. *)
let of_string m string = of_bigstring m (Bigstring.of_string string)
let to_string m t = Bigstring.to_string (to_bigstring m t)

module Of_binable = Of_binable_without_uuid [@@alert "-legacy"]
module Of_binable1 = Of_binable1_without_uuid [@@alert "-legacy"]
module Of_binable2 = Of_binable2_without_uuid [@@alert "-legacy"]
module Of_binable3 = Of_binable3_without_uuid [@@alert "-legacy"]
module Of_sexpable = Of_sexpable_without_uuid [@@alert "-legacy"]
module Of_stringable = Of_stringable_without_uuid [@@alert "-legacy"]
