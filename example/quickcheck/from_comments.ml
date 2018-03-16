open Core
open Quickcheck.Observer

type 'a bst = Leaf | Node of 'a bst * 'a * 'a bst

let bst_obs key_obs =
  fixed_point (fun bst_of_key_obs ->
    unmap (Either.obs Unit.obs (tuple3 bst_of_key_obs key_obs bst_of_key_obs))
      ~f:(function
        | Leaf           -> First ()
        | Node (l, k, r) -> Second (l, k, r)))
