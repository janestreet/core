open! Import
open Std_internal
open Unique_id_intf

module type Id = Id

(* Only "make" can cause a context-switch that might lead to a race.
   Thus we have to check whether the contents of the cell remained
   unchanged across this call.  The subsequent comparison, dereferencing
   and assignment cannot cause context switches.  If the contents of the
   cell had changed, we will have to try again to obtain a unique id.
   This is essentially like a spin-lock and is virtually guaranteed to
   succeed quickly. *)
let rec race_free_create_loop cell make =
  let x = !cell in
  let new_x = make x in
  if phys_equal !cell x
  then (
    cell := new_x;
    x)
  else race_free_create_loop cell make
;;

module Int () = struct
  include Int

  let current = ref zero
  let create () = race_free_create_loop current succ
end

module Int63 () = struct
  include Int63

  let current = ref zero
  let create () = race_free_create_loop current succ
end
