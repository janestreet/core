open! Import
include Base.Queue

include Test_binary_searchable.Make1_and_test (struct
  type nonrec 'a t = 'a t

  let get = get
  let length = length

  module For_test = struct
    let of_array a =
      let r = create () in
      (* We enqueue everything twice, and dequeue it once to ensure:
           - that the queue has the same content as the array.
           - that it has, in most cases, an interesting internal structure*)
      for i = 0 to Array.length a - 1 do
        enqueue r a.(i)
      done;
      for i = 0 to Array.length a - 1 do
        ignore (dequeue_exn r : bool);
        enqueue r a.(i)
      done;
      r
    ;;
  end
end)

module Serialization_v1 = struct
  let sexp_of_t = sexp_of_t
  let t_of_sexp = t_of_sexp
  let t_sexp_grammar = t_sexp_grammar

  include Bin_prot.Utils.Make_iterable_binable1 (struct
    type nonrec 'a t = 'a t
    type 'a el = 'a [@@deriving bin_io]

    let caller_identity =
      Bin_prot.Shape.Uuid.of_string "b4c84254-4992-11e6-9ba7-734e154027bd"
    ;;

    let module_name = Some "Core.Queue"
    let length = length
    let iter = iter
    let init ~len ~next = init len ~f:(fun _ -> next ())
  end)

  let stable_witness (_ : 'a Stable_witness.t) : 'a t Stable_witness.t =
    (* Serialization_v1 defines a stable serialization *)
    Stable_witness.assert_stable
  ;;
end

include Serialization_v1

module Stable = struct
  module V1 = struct
    type nonrec 'a t = 'a t [@@deriving compare, equal]

    include Serialization_v1

    (* We don't have a [%bin_digest] expect test here because the bin_io is mostly hand
       written, and [core_queue_unit_tests.ml] has unit tests for specific values. *)

    let map = map
  end
end
