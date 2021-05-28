open! Core

let group = Command.group ~summary:""

let command =
  Command.basic
    ~summary:""
    (let%map_open.Command a = anon ("A" %: string)
     and b = anon ("B" %: string) in
     fun () -> print_s [%message a b])
;;

let () = Command_unix.run (group [ "do", group [ "the", group [ "thing", command ] ] ])
