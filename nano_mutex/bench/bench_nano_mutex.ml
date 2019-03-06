open Core

module Bench = Core_bench.Bench

module type Mutex = sig
  type t
  val create : unit -> t
  val lock : t -> unit
  val unlock : t -> unit
end

let concat = String.concat

let make ~name (m : (module Mutex)) =
  let module M = (val m : Mutex) in
  [ concat [ name; " create"], (fun () -> ignore (M.create ()));
    concat [ name; " lock/unlock"],
    let l = M.create () in
    (fun () -> M.lock l; M.unlock l);
  ]
;;

module Nano_mutex : Mutex = struct
  include Nano_mutex

  let lock = lock_exn
  let unlock t = unlock_exn t
end

let () =
  Bench.bench
    (List.map ~f:(fun (name, thunk) -> Bench.Test.create ~name thunk)
       (
         make ~name:"Caml.Mutex" (module Caml.Mutex : Mutex)
         @ make ~name:"Error_checking_mutex" (module Error_checking_mutex : Mutex)
         @ make ~name:"Nano_mutex" (module Nano_mutex : Mutex)
       ))
;;
