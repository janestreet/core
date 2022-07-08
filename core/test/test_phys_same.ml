open! Core
open! Import

let%test _ = phys_same 0 None
let%test _ = phys_same 1 true

let%test _ =
  let f () = "statically-allocated" in
  phys_same (f ()) (f ())
;;

let%test _ =
  let a = 1, 2 in
  phys_same a a
;;

type thing = Obscure : _ -> thing

let same_thing (Obscure a) (Obscure b) = phys_same a b

let%test _ =
  let a = 1, 2 in
  same_thing (Obscure a) (Obscure a)
;;
