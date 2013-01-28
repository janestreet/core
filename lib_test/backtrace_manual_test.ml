open Core.Std

let backtrace_get = Or_error.ok_exn Backtrace.get

let f n =
  if n = 2 then (
    Printf.eprintf "%s%!" (Backtrace.to_string (backtrace_get ()))
  )
  else Printf.printf "foo\n%!"

let g () =
  List.iter [1; 2; 3] ~f

let () =
  g ()
