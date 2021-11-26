open Core

let f n =
  if n = 2 then (
    Printf.eprintf "%s%!" (Backtrace.to_string (Backtrace.get ()))
  )
  else Printf.printf "foo\n%!"

let g () =
  List.iter [1; 2; 3] ~f

let () =
  g ()
