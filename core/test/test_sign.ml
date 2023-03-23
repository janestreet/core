open! Core
open! Import
open! Sign

let ( < ) = Poly.( < )
let ( = ) = Poly.( = )
let%test _ = compare Neg Zero < 0 && compare Zero Pos < 0
let%test _ = List.for_all all ~f:(fun t -> t = (t |> to_int |> of_int))
let%test _ = List.for_all [ -1; 0; 1 ] ~f:(fun i -> i = (i |> of_int |> to_int))
