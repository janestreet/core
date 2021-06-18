open! Core
module Unix = Core_unix

let gethostname = Memo.unit Unix.gethostname

let create () =
  Uuid.Private.create ~hostname:(gethostname ()) ~pid:(Unix.getpid () |> Pid.to_int)
;;
