open Core.Std
open Core_extended.Std

let buf = String.create 4096
let header = "\027[31m"
let footer = "\027[0m"

let run () =
  while true do
    let len = Unix.read Unix.stdin ~buf ~pos:0 ~len:4096 in
    if len = 0 then exit 0;
    Out_channel.output_string Out_channel.stdout header;
    Out_channel.output Out_channel.stdout ~buf ~pos:0 ~len;
    Out_channel.output_string Out_channel.stdout footer;
    Out_channel.flush stdout
  done

let usage_msg = "redcat.exe\n\
reads contents from stdin and prints it in red on stdout"
let specs = []

let main () =
  Arg.parse specs (fun _ -> Arg.usage specs usage_msg; exit 1) usage_msg;
  run  ()

let () = Exn.handle_uncaught ~exit:true main
