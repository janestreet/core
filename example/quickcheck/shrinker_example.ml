open Core

module Shrinker = Quickcheck.Shrinker
module Generator = Quickcheck.Generator

module Sorted_list = struct

  type t = int list
  [@@deriving sexp]

  let of_list list =
    List.stable_sort ~compare:Int.compare list

  let to_list t = t

  let gen elt =
    let open Generator.Monad_infix in
    List.gen elt >>| of_list

  let custom_int_shrinker =
    Shrinker.create (fun n ->
      if n = 0
      then Sequence.empty
      else Sequence.singleton (n / 2))

  let shrinker =
    let list_shrinker = List.shrinker custom_int_shrinker in
    Shrinker.map list_shrinker ~f:of_list ~f_inverse:to_list

  let invariant t =
    if List.is_sorted t ~compare:Int.compare then
      ()
    else
      failwiths "sorted_list isn't sorted" t sexp_of_t

  let invalid_merge t_a t_b =
    List.append t_a t_b

  let merge t_a t_b =
    List.merge t_a t_b ~compare:Int.compare
end

let%test_module "sorted list" =
  (module struct

    let sorted_list_tuple_gen =
      let int_gen = Int.gen_incl (-100) 100 in
      let sorted_list_gen = Sorted_list.gen int_gen in
      Generator.tuple2 sorted_list_gen sorted_list_gen

    let sorted_list_tuple_shrinker =
      Shrinker.tuple2 Sorted_list.shrinker Sorted_list.shrinker

    let test f (a,b) =
      f a b |> Sorted_list.invariant

    let sexp_of_sorted_list_tuple =
      [%sexp_of: Sorted_list.t * Sorted_list.t ]

    let%test_unit
      "Invalid merge \"should\" produce a valid sorted list (without shrinking)"
      =
      let run () =
        Quickcheck.test
          ~sexp_of:sexp_of_sorted_list_tuple
          sorted_list_tuple_gen
          ~f:(test Sorted_list.invalid_merge)
      in
      (* Swap which line is commented below to see error message with shrinking. *)
      assert (does_raise run)
    (* run () *)

    let%test_unit
      "Invalid merge \"should\" produce a valid sorted list (with shrinking)"
      =
      let run () =
        Quickcheck.test
          ~shrinker:sorted_list_tuple_shrinker
          ~sexp_of:sexp_of_sorted_list_tuple
          sorted_list_tuple_gen
          ~f:(test Sorted_list.invalid_merge)
      in
      (* Swap which line is commented below to see error message with shrinking. *)
      assert (does_raise run)
    (* run () *)

    let%test_unit "Valid merge should produce a valid sorted list (with shrinking)" =
      Quickcheck.test
        ~shrinker:sorted_list_tuple_shrinker
        ~sexp_of:sexp_of_sorted_list_tuple
        sorted_list_tuple_gen
        ~f:(test Sorted_list.merge)

  end)
