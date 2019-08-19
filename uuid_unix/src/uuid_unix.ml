open! Core_kernel

let gethostname = Memo.unit Unix.gethostname
let create () = Uuid.Private.create ~hostname:(gethostname ()) ~pid:(Unix.getpid ())
