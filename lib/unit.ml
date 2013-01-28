open Sexplib.Std
open Bin_prot.Std

module T = struct
  type t = unit with sexp, bin_io

  let compare _ _ = 0
  let hash _ = 0
  let equal _ _ = true
end

include T
include Sexpable.To_stringable (T)
include Comparable.Make_binable (T)
include Hashable.Make_binable (T)

let pp ppf () = Format.fprintf ppf "()"
