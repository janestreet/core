open Core
open OUnit

let of_string_test () =
  Char.of_string "c" = 'c'
  && Result.is_error (Result.try_with (fun () -> Char.of_string ""))
  && Result.is_error (Result.try_with (fun () -> Char.of_string "too long"))
;;

let test =
  "core_char" >:::
  [ "to_string_hum" >::
    (fun () ->
       "of_string" @? of_string_test ();
    )
  ]
