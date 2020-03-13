open! Core_kernel
module Unix = Core.Unix

let gethostname = Memo.unit Unix.gethostname

let create () =
  Uuid.Private.create ~hostname:(gethostname ()) ~pid:(Unix.getpid () |> Pid.to_int)
;;
