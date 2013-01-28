open Core.Std
open OUnit

let () = Random.self_init ()
let esc_test i = int_of_string (Int.to_string_hum i) = i

let bound = 2 lsl 10

let rand () =
  let rec aux acc cnt =
    if cnt = 0 then
      acc
    else
      let bit = if Random.bool () then 1 else 0 in
      aux (2*acc + bit) (cnt -1)
  in
  let aval = aux 1 (Random.int (Sys.word_size - 3)) in
  if Random.bool () then
    - aval
  else
    aval
(* Random with a distribution favouring small ones*)

let test =
  "core_int" >:::
    [ "to_string_hum" >::
        (fun () ->
           "random" @? (
             List.init ~f:(fun _ -> rand ()) 10_000
           |! List.for_all ~f:esc_test
           );
           "max_int" @? esc_test max_int;
           "min_int" @? esc_test min_int
        )
    ]
