open Core_kernel.Std

include Gc

module Expert = struct
  let add_finalizer x f = Caml.Gc.finalise f x

  (* [add_finalizer_exn] is the same as [add_finalizer].  However, their types in
     core_gc.mli are different, and the type of [add_finalizer] guarantees that it always
     receives a heap block, which ensures that it will not raise, while
     [add_finalizer_exn] accepts any type, and so may raise. *)
  let add_finalizer_exn = add_finalizer

  let finalize_release = Caml.Gc.finalise_release
end
