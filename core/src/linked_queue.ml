open! Import
module Queue = Base.Linked_queue
include Queue

include Bin_prot.Utils.Make_iterable_binable1 (struct
  type 'a t = 'a Queue.t
  type 'a el = 'a [@@deriving bin_io]

  let caller_identity =
    Bin_prot.Shape.Uuid.of_string "800df9a0-4992-11e6-881d-ffe1a5c8aced"
  ;;

  let module_name = Some "Core.Linked_queue"
  let length = length
  let iter = iter

  (* Bin_prot reads the elements in the same order they were written out, as determined
       by [iter].  So, we can ignore the index and just enqueue each element as it is read
       in. *)
  let init ~len ~next =
    let t = create () in
    for _ = 1 to len do
      enqueue t (next ())
    done;
    t
  ;;
end)
