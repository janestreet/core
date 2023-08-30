open! Import
include Base.Sequence

include Bin_prot.Utils.Make_binable1_without_uuid [@alert "-legacy"] (struct
  module Binable = struct
    type 'a t = 'a list [@@deriving bin_io]
  end

  type 'a t = 'a Base.Sequence.t

  let of_binable = Base.Sequence.of_list
  let to_binable = Base.Sequence.to_list
end)

module Step = struct
  include Step

  type ('a, 's) t = ('a, 's) Step.t =
    | Done
    | Skip of { state : 's }
    | Yield of
        { value : 'a
        ; state : 's
        }
  [@@deriving bin_io]
end

module Merge_with_duplicates_element = struct
  include Merge_with_duplicates_element

  type ('a, 'b) t = ('a, 'b) Merge_with_duplicates_element.t =
    | Left of 'a
    | Right of 'b
    | Both of 'a * 'b
  [@@deriving bin_io]
end

module type Heap = sig
  type 'a t

  val create : compare:('a -> 'a -> int) -> 'a t
  val add : 'a t -> 'a -> 'a t
  val pop_min : 'a t -> ('a * 'a t) option
end

let merge_all (module Heap : Heap) seqs ~compare =
  let module Merge_all_state = struct
    type 'a t =
      { heap : ('a * 'a Base.Sequence.t) Heap.t
      ; not_yet_in_heap : 'a Base.Sequence.t list
      }
    [@@deriving fields ~iterators:create]

    let create = Fields.create
  end
  in
  unfold_step
    ~init:
      (Merge_all_state.create
         ~heap:(Heap.create ~compare:(fun a b -> Base.Comparable.lift compare ~f:fst a b))
         ~not_yet_in_heap:seqs)
    ~f:(fun { heap; not_yet_in_heap } ->
      match not_yet_in_heap with
      | seq :: not_yet_in_heap ->
        (match Expert.next_step seq with
         | Done -> Skip { state = { not_yet_in_heap; heap } }
         | Skip { state = seq } ->
           Skip { state = { not_yet_in_heap = seq :: not_yet_in_heap; heap } }
         | Yield { value = elt; state = seq } ->
           Skip { state = { not_yet_in_heap; heap = Heap.add heap (elt, seq) } })
      | [] ->
        (match Heap.pop_min heap with
         | None -> Done
         | Some ((elt, seq), heap) ->
           Yield { value = elt; state = { heap; not_yet_in_heap = [ seq ] } }))
;;
