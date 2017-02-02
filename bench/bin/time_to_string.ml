open Core
open Core_bench.Std

let date = Date.create_exn ~d:14 ~m:Month.Jul ~y:1789

let () =
  Command.run
    (Bench.make_command
       [
         Bench.Test.create ~name:"Date.to_string"
           (fun () -> ignore (Date.to_string date : string))
       ])
;;
