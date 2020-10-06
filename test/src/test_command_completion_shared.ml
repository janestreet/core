open! Core
open! Import

module Simple_group = struct
  let basic =
    Command.basic
      ~summary:"dummy"
      (let%map_open.Command (false | true) = anon ("for-completion" %: bool) in
       fun () -> ())
  ;;

  let group = Command.group ~summary:"dummy group" [ "basic", basic ]
  let command = Command.group ~summary:"top level" [ "group", group ]
end
