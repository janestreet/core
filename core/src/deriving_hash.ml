open! Import
include Deriving_hash_intf

module%template.portable Of_deriving_hash
    (Repr : S)
    (M : sig
       type t

       val to_repr : t -> Repr.t
     end) =
struct
  let hash_fold_t state t = Repr.hash_fold_t state (M.to_repr t)
  let hash t = Ppx_hash_lib.Std.Hash.of_fold hash_fold_t t
end
