open Core.Std
open Core_extended.Std

let buf = String.create 4096
let header = "\027[31m"
let footer = "\027[0m"

let run () =
  while true do
    let len = Unix.read Unix.stdin ~buf ~pos:0 ~len:4096 in
    if len = 0 then exit 0;
    print_string header;
    output stdout buf 0 len;
    print_string footer;
    flush stdout
  done

let usage_msg = "redcat.exe\n\
reads contents from stdin and prints it in red on stdout"
let specs = []

let main () =
  Arg.parse specs (fun _ -> Arg.usage specs usage_msg; exit 1) usage_msg;
  run  ()

let () = Exn.handle_uncaught ~exit:true main
