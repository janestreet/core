open! Base
open! Binary_searchable
include Test_binary_searchable_intf

module type S_gen = sig
  open Binary_searchable

  type 'a t
  type 'a elt

  val binary_search : ('a t, 'a elt, 'a elt) binary_search
  val binary_search_segmented : ('a t, 'a elt) binary_search_segmented
end

module type Indexable_gen_and_for_test = sig
  include S_gen

  module For_test : sig
    val compare : bool elt -> bool elt -> int
    val small : bool elt
    val big : bool elt
    val of_array : bool elt array -> bool t
  end
end

module Test_gen (M : Indexable_gen_and_for_test) = struct
  open M

  let%test_module "test_binary_searchable" =
    (module struct
      let compare = For_test.compare
      let elt_compare = For_test.compare
      let s = For_test.small
      let b = For_test.big

      let binary_search ?pos ?len ~compare t how v =
        binary_search ?pos ?len ~compare (For_test.of_array t) how v
      ;;

      let ( = ) = Poly.equal
      let%test _ = binary_search ~compare [||] `First_equal_to s = None
      let%test _ = binary_search ~compare [| s |] `First_equal_to s = Some 0
      let%test _ = binary_search ~compare [| s |] `First_equal_to b = None
      let%test _ = binary_search ~compare [| s; b |] `First_equal_to s = Some 0
      let%test _ = binary_search ~compare [| s; b |] `First_equal_to b = Some 1
      let%test _ = binary_search ~compare [| b; b |] `First_equal_to s = None
      let%test _ = binary_search ~compare [| s; s |] `First_equal_to b = None
      let%test _ = binary_search ~compare [| s; b; b |] `First_equal_to b = Some 1
      let%test _ = binary_search ~compare [| s; s; b |] `First_equal_to s = Some 0
      let%test _ = binary_search ~compare [| b; b; b |] `First_equal_to s = None
      let%test _ = binary_search ~compare [||] `Last_equal_to s = None
      let%test _ = binary_search ~compare [| s |] `Last_equal_to s = Some 0
      let%test _ = binary_search ~compare [| s |] `Last_equal_to b = None
      let%test _ = binary_search ~compare [| s; b |] `Last_equal_to b = Some 1
      let%test _ = binary_search ~compare [| s; b |] `Last_equal_to s = Some 0
      let%test _ = binary_search ~compare [| b; b |] `Last_equal_to s = None
      let%test _ = binary_search ~compare [| s; s |] `Last_equal_to b = None
      let%test _ = binary_search ~compare [| s; b; b |] `Last_equal_to b = Some 2
      let%test _ = binary_search ~compare [| s; s; b |] `Last_equal_to s = Some 1
      let%test _ = binary_search ~compare [| b; b; b |] `Last_equal_to s = None
      let%test _ = binary_search ~compare [||] `First_greater_than_or_equal_to s = None

      let%test _ =
        binary_search ~compare [| b |] `First_greater_than_or_equal_to s = Some 0
      ;;

      let%test _ =
        binary_search ~compare [| s |] `First_greater_than_or_equal_to s = Some 0
      ;;

      let%test _ = binary_search ~compare [| s |] `First_strictly_greater_than s = None
      let%test _ = binary_search ~compare [||] `Last_less_than_or_equal_to s = None
      let%test _ = binary_search ~compare [| b |] `Last_less_than_or_equal_to s = None
      let%test _ = binary_search ~compare [| s |] `Last_less_than_or_equal_to s = Some 0
      let%test _ = binary_search ~compare [| s |] `Last_strictly_less_than s = None

      let create_test_case (num_s, num_b) =
        let arr = Array.create b ~len:(num_s + num_b) in
        for i = 0 to num_s - 1 do
          arr.(i) <- s
        done;
        arr
      ;;

      let only_small = 10_000, 0
      let only_big = 0, 10_000
      let both = 2531, 4717

      let%test _ =
        match binary_search (create_test_case only_small) ~compare `First_equal_to s with
        | None -> false
        | Some _ -> true
      ;;

      let%test _ =
        let arr = create_test_case both in
        match binary_search arr ~compare `First_equal_to b with
        | None -> false
        | Some v -> v = 2531
      ;;

      let%test _ =
        let arr = create_test_case only_small in
        binary_search arr ~compare `First_equal_to b = None
      ;;

      let create_deterministic_test () =
        Array.init 100_000 ~f:(fun i -> if i > 50_000 then b else s)
      ;;

      let%test _ =
        let arr = create_deterministic_test () in
        binary_search arr ~compare `First_equal_to s = Some 0
      ;;

      let%test _ =
        let arr = create_deterministic_test () in
        binary_search arr ~compare `Last_equal_to s = Some 50_000
      ;;

      let%test _ =
        let arr = create_deterministic_test () in
        binary_search arr ~compare `First_greater_than_or_equal_to s = Some 0
      ;;

      let%test _ =
        let arr = create_deterministic_test () in
        binary_search arr ~compare `Last_less_than_or_equal_to s = Some 50_000
      ;;

      let%test _ =
        let arr = create_deterministic_test () in
        binary_search arr ~compare `First_strictly_greater_than s = Some 50_001
      ;;

      let%test _ =
        let arr = create_deterministic_test () in
        binary_search arr ~compare `Last_strictly_less_than b = Some 50_000
      ;;

      (* tests around a gap*)
      let%test _ =
        let arr = create_deterministic_test () in
        binary_search arr ~compare `First_equal_to b = Some 50_001
      ;;

      let%test _ =
        let arr = create_deterministic_test () in
        binary_search arr ~compare `Last_equal_to b = Some 99_999
      ;;

      let%test _ =
        let arr = create_deterministic_test () in
        binary_search arr ~compare `First_greater_than_or_equal_to b = Some 50_001
      ;;

      let%test _ =
        let arr = create_deterministic_test () in
        binary_search arr ~compare `Last_less_than_or_equal_to b = Some 99_999
      ;;

      let%test _ =
        let arr = create_deterministic_test () in
        binary_search arr ~compare `First_strictly_greater_than b = None
      ;;

      let%test _ =
        let arr = create_deterministic_test () in
        binary_search arr ~compare `Last_strictly_less_than b = Some 50_000
      ;;

      (* test beginning of array *)

      let%test _ =
        let arr = create_test_case only_big in
        binary_search arr ~compare `First_equal_to s = None
      ;;

      let%test _ =
        let arr = create_test_case only_big in
        binary_search arr ~compare `Last_equal_to s = None
      ;;

      let%test _ =
        let arr = create_test_case only_big in
        binary_search arr ~compare `First_greater_than_or_equal_to s = Some 0
      ;;

      let%test _ =
        let arr = create_test_case only_big in
        binary_search arr ~compare `Last_less_than_or_equal_to s = None
      ;;

      let%test _ =
        let arr = create_test_case only_big in
        binary_search arr ~compare `First_strictly_greater_than s = Some 0
      ;;

      let%test _ =
        let arr = create_test_case only_big in
        binary_search arr ~compare `Last_strictly_less_than b = None
      ;;

      (* test end of array *)

      let%test _ =
        let arr = create_test_case only_small in
        binary_search arr ~compare `First_equal_to b = None
      ;;

      let%test _ =
        let arr = create_test_case only_small in
        binary_search arr ~compare `Last_equal_to b = None
      ;;

      let%test _ =
        let arr = create_test_case only_small in
        binary_search arr ~compare `First_greater_than_or_equal_to b = None
      ;;

      let%test _ =
        let arr = create_test_case only_small in
        binary_search arr ~compare `Last_less_than_or_equal_to b = Some 9_999
      ;;

      let%test _ =
        let arr = create_test_case only_small in
        binary_search arr ~compare `First_strictly_greater_than s = None
      ;;

      let%test _ =
        let arr = create_test_case only_small in
        binary_search arr ~compare `Last_strictly_less_than b = Some 9_999
      ;;

      let%test_unit _ =
        for length = 0 to 5 do
          for num_s = 0 to length do
            let arr = Array.init length ~f:(fun i -> if i < num_s then s else b) in
            for pos = -1 to length do
              for len = -1 to length + 1 do
                (*try*)
                let should_raise =
                  Exn.does_raise (fun () ->
                    Ordered_collection_common.check_pos_len_exn
                      ~pos
                      ~len
                      ~total_length:length)
                in
                let result =
                  Result.try_with (fun () ->
                    binary_search arr ~pos ~len ~compare:elt_compare `Last_equal_to s)
                in
                match should_raise, result with
                | true, Error _ -> ()
                | true, Ok _ -> failwith "expected it to raise but it didn't"
                | false, Error _ -> failwith "expected it to not raise, but it raised"
                | false, Ok result ->
                  let searched = num_s - 1 in
                  let correct_result =
                    if searched < pos
                    then None
                    else if len = 0
                    then None
                    else if searched >= pos + len
                    then Some (pos + len - 1)
                    else Some searched
                  in
                  if not (correct_result = result) then failwith "Wrong result"
                (*with exn ->
                    failwiths "binary_search bug"
                    (exn, `length length, `search_key search_key, `pos pos, `len len)
                    <:sexp_of< exn * [ `length of int ] * [ `search_key of int ]
                 * [ `pos of int ] * [ `len of int ] >>*)
              done
            done
          done
        done
      ;;

      let binary_search_segmented a = binary_search_segmented (For_test.of_array a)

      (*test for binary_search_segmented*)
      let%test _ =
        let arr = create_deterministic_test () in
        let segment_of x = if x = b then `Right else `Left in
        binary_search_segmented arr ~segment_of `Last_on_left = Some 50_000
        && binary_search_segmented arr ~segment_of `First_on_right = Some 50_001
      ;;

      let%test _ =
        let arr = create_deterministic_test () in
        let segment_of _ = `Right in
        binary_search_segmented arr ~segment_of `Last_on_left = None
        && binary_search_segmented arr ~segment_of `First_on_right = Some 0
      ;;

      let%test _ =
        let arr = create_deterministic_test () in
        let segment_of _ = `Left in
        binary_search_segmented arr ~segment_of `Last_on_left = Some 99_999
        && binary_search_segmented arr ~segment_of `First_on_right = None
      ;;
    end)
  ;;
end

module Test (M : Binary_searchable_and_for_test) = Test_gen (struct
  type 'a t = M.t
  type 'a elt = M.elt

  let binary_search = M.binary_search
  let binary_search_segmented = M.binary_search_segmented

  module For_test = M.For_test
end)

module Test1 (M : Binary_searchable1_and_for_test) = Test_gen (struct
  type 'a t = 'a M.t
  type 'a elt = 'a

  let binary_search = M.binary_search
  let binary_search_segmented = M.binary_search_segmented

  module For_test = struct
    let of_array = M.For_test.of_array
    let compare = Bool.compare
    let small = false
    let big = true
  end
end)

module Make_and_test (M : Indexable_and_for_test) = struct
  module B = Binary_searchable.Make (M)
  include B

  include Test (struct
    type t = M.t
    type elt = M.elt

    include B
    module For_test = M.For_test
  end)
end

module Make1_and_test (M : Indexable1_and_for_test) = struct
  module B = Binary_searchable.Make1 (M)
  include B

  include Test1 (struct
    type 'a t = 'a M.t

    include B
    module For_test = M.For_test
  end)
end
