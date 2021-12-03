open Core
open Linked_queue

let%test_module _ =
  (module struct
    let m =
      let module M = struct
        type 'a u = 'a t [@@deriving bin_io]
        type t = int u [@@deriving bin_io]
      end
      in
      (module M : Binable.S with type t = M.t)
    ;;

    let test list =
      let t = of_list list in
      let bigstring = Binable.to_bigstring m t in
      let list' = to_list (Binable.of_bigstring m bigstring) in
      [%compare.equal: int list] list list'
    ;;

    let%test _ = test []
    let%test _ = test [ 1 ]
    let%test _ = test [ 1; 2; 3 ]
    let%test _ = test (List.init 10_000 ~f:Fn.id)
  end)
;;
