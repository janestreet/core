open Sexplib.Std
open Bin_prot.Std

type 'a t = 'a lazy_t with bin_io, sexp

include (Lazy : module type of Lazy with type 'a t := 'a t)

include Monad.Make (struct
  type nonrec 'a t = 'a t

  let return x = lazy_from_val x

  let bind t f = lazy (force (f (force t)))
end)

TEST_MODULE = struct

  TEST_UNIT =
    let r = ref 0 in
    let t = return () >>= fun () -> incr r; return () in
    assert (!r = 0);
    force t;
    assert (!r = 1);
    force t;
    assert (!r = 1);
  ;;

  TEST_UNIT =
    let r = ref 0 in
    let t = return () >>= fun () -> lazy (incr r) in
    assert (!r = 0);
    force t;
    assert (!r = 1);
    force t;
    assert (!r = 1);
  ;;

end

let compare compare_a t1 t2 = compare_a (force t1) (force t2)
