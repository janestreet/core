open Core
open OUnit

let esc_test i = int_of_string (Int.to_string_hum i) = i

let bound = 2 lsl 10

let rand state =
  let rec aux acc cnt =
    if cnt = 0 then
      acc
    else
      let bit = if Random.State.bool state then 1 else 0 in
      aux (2*acc + bit) (cnt -1)
  in
  let aval = aux 1 (Random.State.int state (Sys.word_size - 3)) in
  if Random.State.bool state then
    - aval
  else
    aval
(* Random with a distribution favouring small ones*)

let test =
  "core_int" >:::
  [ "to_string_hum" >::
    (fun () ->
       let state = Random.State.make [| 7; 4; 2 |] in
       "random" @? (
         List.init ~f:(fun _ -> rand state) 10_000
         |> List.for_all ~f:esc_test
       );
       "max_int" @? esc_test Int.max_value;
       "min_int" @? esc_test Int.min_value
    )
  ]
