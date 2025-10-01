open! Core
open! Import
open! Base_quickcheck
open! Base_quickcheck.Export

(* Make sure we test all exports from Iarray. *)
module _ : module type of struct
  include Iarray
  module Private = Base.Iarray.Private [@alert "-private_iarray"]
end [@ocaml.remove_aliases] [@warning "-unused-module"] = struct
  type ('a : any mod separable) t = 'a Iarray.t

  [%%rederive.portable
    type 'a t = 'a Iarray.t [@@deriving bin_io ~localize, quickcheck ~portable, typerep]]

  let%expect_test "unstable (pseudo-nondeterministic) sample" =
    Test.with_sample_exn
      (quickcheck_generator Generator.small_positive_or_zero_int)
      ~config:{ Test.default_config with test_count = 64 }
      ~f:(fun seq ->
        Sequence.to_list seq
        |> List.dedup_and_sort ~compare:[%compare: int Iarray.t]
        |> List.iter ~f:(fun iarray -> print_s [%sexp (iarray : int Iarray.t)]));
    [%expect
      {|
      ()
      (0)
      (3)
      (4)
      (5)
      (7)
      (11)
      (13)
      (17)
      (20)
      (23)
      (24)
      (0 5)
      (1 0)
      (2 1)
      (13 0)
      (1 1 1)
      (1 2 2)
      (3 0 2)
      (3 2 4)
      (3 5 0)
      (5 5 1)
      (8 7 1)
      (0 1 0 1)
      (0 6 3 1)
      (1 1 0 2)
      (2 0 7 8)
      (3 2 6 1 0)
      (1 2 2 4 1 1)
      (2 3 2 0 11 4)
      (4 1 0 2 4 0)
      (0 6 3 0 1 3 0)
      (2 1 1 0 1 0 1)
      (1 1 1 0 1 2 0 1)
      (1 0 0 0 0 1 1 1 0)
      (1 3 2 0 0 0 0 3 1)
      (2 1 4 1 0 1 3 1 2)
      (1 0 1 1 1 0 0 1 0 0)
      (1 0 0 3 0 1 0 0 0 2 3 1)
      (0 1 1 3 3 1 0 0 2 1 0 0 2)
      (0 1 0 0 0 0 0 1 0 1 1 0 0 0)
      (1 1 2 1 0 0 0 1 0 0 1 0 0 1 0 1 1 1 0 0)
      (0 1 0 1 1 3 2 1 0 1 1 0 2 1 1 1 1 1 1 0 1 1)
      (0 2 1 1 1 1 0 1 1 0 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0)
      |}]
  ;;

  let%expect_test "examples where elements can also shrink" =
    let test iarray =
      Iarray.unsafe_of_array__promise_no_mutation iarray
      |> Shrinker.shrink [%shrinker: string Iarray.t]
      |> Sequence.iter ~f:(fun iarray -> print_s [%sexp (iarray : string Iarray.t)])
    in
    test [||];
    [%expect {| |}];
    test [| "" |];
    [%expect {| () |}];
    test [| "cat" |];
    [%expect
      {|
      ()
      (at)
      (ct)
      (ca)
      |}];
    test [| "in"; "the"; "hat" |];
    [%expect
      {|
      (the hat)
      (n the hat)
      (in hat)
      (i the hat)
      (in he hat)
      (in the)
      (in te hat)
      (in the at)
      (in th hat)
      (in the ht)
      (in the ha)
      |}]
  ;;

  open struct
    (* Test helper for hash functions, including observers *)

    let samples_for_hash =
      let extend sample =
        List.concat_map [ 1; 2; 3; 4 ] ~f:(fun elt ->
          List.map sample ~f:(fun list -> elt :: list))
      in
      let sample0 = [ [] ] in
      let sample1 = extend sample0 in
      let sample2 = extend sample1 in
      let sample3 = extend sample2 in
      let sample4 = extend sample3 in
      List.concat [ sample0; sample1; sample2; sample3; sample4 ]
      |> List.map ~f:Iarray.of_list
    ;;

    let test_hash_can_distinguish_sample hash_fn =
      let compare x y =
        Comparable.lift Int.compare x y ~f:(fun iarray ->
          hash_fn (Hash.alloc ()) iarray |> Hash.get_hash_value)
      in
      samples_for_hash
      |> List.sort_and_group ~compare
      |> List.iter ~f:(function
        | [] -> assert false
        | [ _ ] -> ()
        | multiple ->
          print_cr [%message "did not distinguish values" (multiple : int Iarray.t list)])
    ;;
  end

  let%expect_test "make sure observer can distinguish all values from a sample" =
    test_hash_can_distinguish_sample (fun hash iarray ->
      Observer.observe [%observer: int Iarray.t] ~size:1 ~hash iarray)
  ;;

  open struct
    (* Quickcheck helpers, now that we have tested quickcheck for [Iarray]. *)

    module Int_t = struct
      type t = (int[@generator Generator.small_positive_or_zero_int]) Iarray.t
      [@@deriving equal, globalize, quickcheck, sexp]
    end
  end

  let globalize = Iarray.globalize

  let%expect_test _ =
    quickcheck_m (module Int_t) ~f:(fun iarray ->
      require_equal (module Int_t) iarray (globalize Int.globalize iarray))
  ;;

  let compare = Iarray.compare

  let%expect_test "reflexive" =
    quickcheck_m (module Int_t) ~f:(fun iarray ->
      require_equal (module Int) (compare Int.compare iarray iarray) 0)
  ;;

  let%expect_test "anti-symmetric" =
    quickcheck_m
      (module struct
        type t = Int_t.t * Int_t.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (a, b) ->
        require_equal
          (module Sign)
          (Int.sign (compare Int.compare a b))
          (Sign.flip (Int.sign (compare Int.compare b a))))
  ;;

  let ordering_is_transitive a b c ~(f : _ -> _ -> Ordering.t) =
    match f a b, f b c with
    | Equal, Equal -> Ordering.equal (f a c) Equal
    | Equal, Less | Less, Equal | Less, Less -> Ordering.equal (f a c) Less
    | Equal, Greater | Greater, Equal | Greater, Greater -> Ordering.equal (f a c) Greater
    | Less, Greater -> true
    | Greater, Less -> true
  ;;

  let%expect_test "transitive" =
    let order x y = Ordering.of_int (compare Int.compare x y) in
    quickcheck_m
      (module struct
        type t = Int_t.t * Int_t.t * Int_t.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (a, b, c) ->
        require
          (ordering_is_transitive a b c ~f:order
           && ordering_is_transitive b c a ~f:order
           && ordering_is_transitive c a b ~f:order))
  ;;

  let equal = Iarray.equal

  let%expect_test "consistent with [compare]" =
    quickcheck_m
      (module struct
        type t = Int_t.t * Int_t.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (a, b) ->
        require_equal (module Bool) (equal Int.equal a b) (compare Int.compare a b = 0))
  ;;

  let compare__local = Iarray.compare__local

  let%expect_test "consistent with [compare]" =
    quickcheck_m
      (module struct
        type t = Int_t.t * Int_t.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (a, b) ->
        require_equal
          (module Ordering)
          (Ordering.of_int (compare__local Int.compare__local a b))
          (Ordering.of_int (compare Int.compare a b)))
  ;;

  let equal__local = Iarray.equal__local

  let%expect_test "consistent with [equal]" =
    quickcheck_m
      (module struct
        type t = Int_t.t * Int_t.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (a, b) ->
        require_equal
          (module Bool)
          (equal__local Int.equal__local a b)
          (equal Int.equal a b))
  ;;

  let hash_fold_t = Iarray.hash_fold_t

  let%expect_test "make sure hash function can distinguish all values from a sample" =
    test_hash_can_distinguish_sample (hash_fold_t Int.hash_fold_t)
  ;;

  let t_of_sexp = Iarray.t_of_sexp
  let sexp_of_t = Iarray.sexp_of_t
  let sexp_of_t__stack = Iarray.sexp_of_t__stack

  let%expect_test "test round-trip" =
    print_and_check_sexpable (module Int_t) (10 |> List.init ~f:(Iarray.init ~f:Int.succ));
    [%expect
      {|
      ()
      (1)
      (1 2)
      (1 2 3)
      (1 2 3 4)
      (1 2 3 4 5)
      (1 2 3 4 5 6)
      (1 2 3 4 5 6 7)
      (1 2 3 4 5 6 7 8)
      (1 2 3 4 5 6 7 8 9)
      |}];
    let module Int_t__local = struct
      include Int_t

      let sexp_of_t t =
        Sexp.globalize
          (sexp_of_t__stack (fun elt -> exclave_ sexp_of_int__stack elt) t) [@nontail]
      ;;
    end
    in
    print_and_check_sexpable
      (module Int_t__local)
      (10 |> List.init ~f:(Iarray.init ~f:Int.succ));
    [%expect
      {|
      ()
      (1)
      (1 2)
      (1 2 3)
      (1 2 3 4)
      (1 2 3 4 5)
      (1 2 3 4 5 6)
      (1 2 3 4 5 6 7)
      (1 2 3 4 5 6 7 8)
      (1 2 3 4 5 6 7 8 9)
      |}]
  ;;

  let empty = Iarray.empty

  let%expect_test _ =
    print_s [%sexp (empty : _ t)];
    [%expect {| () |}]
  ;;

  let singleton = Iarray.singleton

  let%expect_test _ =
    print_s [%sexp (singleton () : unit t)];
    print_s [%sexp (singleton 1 : int t)];
    print_s [%sexp (singleton "hello, world" : string t)];
    [%expect
      {|
      (())
      (1)
      ("hello, world")
      |}]
  ;;

  let create = Iarray.create

  let%expect_test _ =
    quickcheck_m (module Int_t) ~f:(fun t ->
      require_equal
        (module Int_t)
        (create ~len:(Iarray.length t) 0 ~mutate:(fun array ->
           for pos = 0 to Iarray.length t - 1 do
             Array.set array pos (Iarray.get t pos)
           done))
        t);
    [%expect {| |}]
  ;;

  let invariant = Iarray.invariant

  let%expect_test _ =
    require_does_raise (fun () ->
      invariant (fun () -> raise_s [%message "boom"]) (singleton ()));
    [%expect {| boom |}];
    quickcheck_m (module Int_t) ~f:(fun t -> invariant Int.invariant t)
  ;;

  [%%template
  [@@@mode.default c = (uncontended, shared, contended), p = (portable, nonportable)]

  external unsafe_get
    :  ('a t[@local_opt]) @ c p
    -> int
    -> ('a[@local_opt]) @ c p
    @@ portable
    = "%array_unsafe_get"]

  let%expect_test _ =
    quickcheck_m (module Int_t) ~f:(fun t ->
      require_equal (module Int_t) t (Iarray.init (Iarray.length t) ~f:(unsafe_get t)))
  ;;

  [%%template
  [@@@mode.default c = (uncontended, shared, contended), p = (portable, nonportable)]

  external get
    :  ('a t[@local_opt]) @ c p
    -> int
    -> ('a[@local_opt]) @ c p
    @@ portable
    = "%array_safe_get"]

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = Int_t.t * int [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, i) ->
        require_equal
          (module struct
            type t = int option [@@deriving equal, sexp_of]
          end)
          (Option.try_with (fun () -> get t i))
          (if 0 <= i && i < Iarray.length t then Some (Iarray.unsafe_get t i) else None))
  ;;

  [%%template
  [@@@mode.default c = (uncontended, shared, contended)]
  [@@@alloc.default a = (heap, stack)]

  let get_opt = (Iarray.get_opt [@mode c] [@alloc a])]

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = Int_t.t * int [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, i) ->
        require_equal
          (module struct
            type t = int option [@@deriving equal, sexp_of]
          end)
          (Option.try_with (fun () -> get t i))
          (Iarray.get_opt t i))
  ;;

  let set = Iarray.set

  let%expect_test _ =
    let module Args = struct
      type t =
        { iarray : Int_t.t
        ; index : (int[@generator Generator.small_positive_or_zero_int])
        ; value : (int[@generator Generator.small_positive_or_zero_int])
        }
      [@@deriving sexp_of, quickcheck]
    end
    in
    quickcheck_m (module Args) ~f:(fun { iarray; index; value } ->
      require_equal
        (module struct
          type t = Int_t.t option [@@deriving equal, sexp_of]
        end)
        (Option.try_with (fun () -> set iarray index value))
        (Option.try_with (fun () ->
           Iarray.concat
             (Iarray.unsafe_of_array__promise_no_mutation
                [| Iarray.prefix iarray ~len:index
                 ; Iarray.unsafe_of_array__promise_no_mutation [| value |]
                 ; Iarray.drop_prefix iarray ~len:(index + 1)
                |]))))
  ;;

  let update = Iarray.update

  let%expect_test _ =
    let module Args = struct
      type t =
        { iarray : Int_t.t
        ; index : (int[@generator Generator.small_positive_or_zero_int])
        ; add : (int[@generator Generator.small_positive_or_zero_int])
        }
      [@@deriving sexp_of, quickcheck]
    end
    in
    quickcheck_m (module Args) ~f:(fun { iarray; index; add } ->
      require_equal
        (module struct
          type t = Int_t.t option [@@deriving equal, sexp_of]
        end)
        (Option.try_with (fun () -> update iarray index ~f:(( + ) add)))
        (Option.try_with (fun () ->
           Iarray.concat
             (Iarray.unsafe_of_array__promise_no_mutation
                [| Iarray.prefix iarray ~len:index
                 ; Iarray.unsafe_of_array__promise_no_mutation
                     [| add + Iarray.get iarray index |]
                 ; Iarray.drop_prefix iarray ~len:(index + 1)
                |]))))
  ;;

  let of_sequence = Iarray.of_sequence
  let to_sequence = Iarray.to_sequence

  let%expect_test _ =
    quickcheck_m (module Int_t) ~f:(fun t ->
      require_equal (module Int_t) t (of_sequence (to_sequence t)))
  ;;

  let%template[@mode m = (global, local)] binary_search = (Iarray.binary_search [@mode m])

  let%template[@mode m = (global, local)] binary_search_segmented =
    (Iarray.binary_search_segmented [@mode m])
  ;;

  module%test [@name "run binary search tests"] _ =
  Base_for_tests.Test_binary_searchable.Test1 (struct
      type nonrec 'a t = 'a t

      let binary_search = binary_search
      let binary_search_segmented = binary_search_segmented

      module For_test = struct
        let of_array = Iarray.of_array
      end
    end)

  include Blit_helpers.Make (Iarray) (Iarray)

  let sub = Iarray.sub

  let%expect_test _ =
    test_sub ~sub:{ sub };
    [%expect {| |}]
  ;;

  let subo = Iarray.subo

  let%expect_test _ =
    test_subo ~subo:{ subo };
    [%expect {| |}]
  ;;

  module Of_array = struct
    open Blit_helpers.Make (Array) (Iarray)

    let sub = Iarray.Of_array.sub

    let%expect_test _ =
      test_sub ~sub:{ sub = (fun src ~pos ~len -> sub src ~pos ~len) };
      [%expect {| |}]
    ;;

    let subo = Iarray.Of_array.subo

    let%expect_test _ =
      test_subo ~subo:{ subo = (fun ?pos ?len src -> subo ?pos ?len src) };
      [%expect {| |}]
    ;;
  end

  module To_array = struct
    include Blit_helpers.Make (Iarray) (Array)

    let sub = Iarray.To_array.sub

    let%expect_test _ =
      test_sub ~sub:{ sub };
      [%expect {| |}]
    ;;

    let subo = Iarray.To_array.subo

    let%expect_test _ =
      test_subo ~subo:{ subo };
      [%expect {| |}]
    ;;

    let blit = Iarray.To_array.blit

    let%expect_test _ =
      test_blit
        ~blit:
          { blit =
              (fun ~src ~src_pos ~dst ~dst_pos ~len ->
                blit ~src ~src_pos ~dst ~dst_pos ~len)
          };
      [%expect {| |}]
    ;;

    let blito = Iarray.To_array.blito

    let%expect_test _ =
      test_blito
        ~blito:
          { blito =
              (fun ~src ?src_pos ?src_len ~dst ?dst_pos () ->
                blito ~src ?src_pos ?src_len ~dst ?dst_pos ())
          };
      [%expect {| |}]
    ;;

    let unsafe_blit = Iarray.To_array.unsafe_blit

    let%expect_test _ =
      test_unsafe_blit
        ~unsafe_blit:
          { unsafe_blit =
              (fun ~src ~src_pos ~dst ~dst_pos ~len ->
                unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len)
          };
      [%expect {| |}]
    ;;
  end

  let prefix = Iarray.prefix
  let suffix = Iarray.suffix
  let drop_prefix = Iarray.drop_prefix
  let drop_suffix = Iarray.drop_suffix

  let%expect_test _ =
    let module With_len = struct
      type t =
        { t : Int_t.t
        ; len : (int[@generator Generator.small_positive_or_zero_int])
        }
      [@@deriving quickcheck, sexp_of]
    end
    in
    let module Optional = struct
      type t = Int_t.t option [@@deriving equal, sexp_of]
    end
    in
    quickcheck_m (module With_len) ~f:(fun { t; len } ->
      require_equal
        (module Optional)
        (Option.try_with (fun () -> prefix t ~len))
        (Option.try_with (fun () -> sub t ~pos:0 ~len));
      require_equal
        (module Optional)
        (Option.try_with (fun () -> suffix t ~len))
        (Option.try_with (fun () -> sub t ~pos:(Iarray.length t - len) ~len));
      require_equal
        (module Optional)
        (Option.try_with (fun () -> drop_prefix t ~len))
        (Option.try_with (fun () -> sub t ~pos:len ~len:(Iarray.length t - len)));
      require_equal
        (module Optional)
        (Option.try_with (fun () -> drop_suffix t ~len))
        (Option.try_with (fun () -> sub t ~pos:0 ~len:(Iarray.length t - len))))
  ;;

  let group = Iarray.group

  let%expect_test _ =
    quickcheck_m (module Int_t) ~f:(fun t ->
      require_equal
        (module struct
          type t = int Iarray.t Iarray.t [@@deriving equal, sexp_of]
        end)
        (group ~break:( > ) t)
        (List.group ~break:( > ) (Iarray.to_list t)
         |> List.map ~f:Iarray.of_list
         |> Iarray.of_list))
  ;;

  let split_n = Iarray.split_n

  let%expect_test "[split_n]" =
    quickcheck_m (module Int_t) ~f:(fun t ->
      let list = Iarray.to_list t in
      for n = -2 to Iarray.length t + 2 do
        let first_arr, second_arr = split_n t n in
        let first_list, second_list = List.split_n list n in
        require_equal (module Int_t) first_arr (Iarray.of_list first_list);
        require_equal (module Int_t) second_arr (Iarray.of_list second_list)
      done)
  ;;

  let chunks_of = Iarray.chunks_of

  let%expect_test "[chunks_of]" =
    quickcheck_m (module Int_t) ~f:(fun t ->
      let list = Iarray.to_list t in
      for length = 1 to max 1 (Iarray.length t + 2) do
        let chunks_arr = chunks_of t ~length in
        let chunks_list = List.chunks_of list ~length in
        require_equal
          (module struct
            type t = int Iarray.t Iarray.t [@@deriving equal, sexp_of]
          end)
          chunks_arr
          (List.map chunks_list ~f:Iarray.of_list |> Iarray.of_list)
      done);
    (* Test that [chunks_of] raises for invalid length. *)
    require_does_raise (fun () -> chunks_of (Iarray.of_list [ 1; 2; 3 ]) ~length:0);
    require_does_raise (fun () -> chunks_of (Iarray.of_list [ 1; 2; 3 ]) ~length:(-1));
    [%expect
      {|
      (Invalid_argument "Iarray.chunks_of: Expected length > 0, got 0")
      (Invalid_argument "Iarray.chunks_of: Expected length > 0, got -1")
      |}]
  ;;

  let rev = Iarray.rev

  let%expect_test _ =
    quickcheck_m (module Int_t) ~f:(fun t ->
      require_equal
        (module Int_t)
        (rev t)
        (Iarray.init (Iarray.length t) ~f:(fun i ->
           Iarray.get t (Iarray.length t - i - 1))))
  ;;

  let sort = Iarray.sort

  let%expect_test _ =
    quickcheck_m (module Int_t) ~f:(fun t ->
      require_equal
        (module Int_t)
        (sort ~compare:Int.compare t)
        (Iarray.of_list (List.sort ~compare:Int.compare (Iarray.to_list t))))
  ;;

  let stable_sort = Iarray.stable_sort

  let%expect_test _ =
    quickcheck_m (module Int_t) ~f:(fun t ->
      let compare a b = Int.compare (a / 2) (b / 2) in
      require_equal
        (module Int_t)
        (stable_sort ~compare t)
        (Iarray.of_list (List.stable_sort ~compare (Iarray.to_list t))))
  ;;

  let dedup_and_sort = Iarray.dedup_and_sort

  let%expect_test _ =
    quickcheck_m (module Int_t) ~f:(fun t ->
      require_equal
        (module Int_t)
        (dedup_and_sort ~compare:Int.compare t)
        (Iarray.of_list (List.dedup_and_sort ~compare:Int.compare (Iarray.to_list t))))
  ;;

  let sort_and_group = Iarray.sort_and_group

  let%expect_test _ =
    quickcheck_m (module Int_t) ~f:(fun t ->
      require_equal
        (module struct
          type t = int Iarray.t Iarray.t [@@deriving equal, sexp_of]
        end)
        (sort_and_group ~compare:Int.compare t)
        (List.sort_and_group ~compare:Int.compare (Iarray.to_list t)
         |> List.map ~f:Iarray.of_list
         |> Iarray.of_list))
  ;;

  let is_sorted = Iarray.is_sorted

  let%expect_test _ =
    quickcheck_m (module Int_t) ~f:(fun t ->
      require_equal
        (module Bool)
        (is_sorted ~compare:Int.compare t)
        (List.is_sorted ~compare:Int.compare (Iarray.to_list t)));
    quickcheck_m (module Int_t) ~f:(fun t ->
      let sorted = sort ~compare:Int.compare t in
      require_equal (module Bool) true (is_sorted ~compare:Int.compare sorted))
  ;;

  let fold_right = Iarray.fold_right

  let%expect_test _ =
    quickcheck_m (module Int_t) ~f:(fun t ->
      require_equal
        (module Int)
        (fold_right t ~init:0 ~f:( - ))
        (List.fold_right (Iarray.to_list t) ~init:0 ~f:( - )))
  ;;

  let of_list_rev = Iarray.of_list_rev
  let of_list_map = Iarray.of_list_map
  let of_list_mapi = Iarray.of_list_mapi
  let of_list_rev_map = Iarray.of_list_rev_map

  let%expect_test _ =
    let module Int_list = struct
      type t = int list [@@deriving quickcheck, sexp_of]
    end
    in
    quickcheck_m (module Int_list) ~f:(fun list ->
      require_equal (module Int_t) (of_list_rev list) (Iarray.of_list (List.rev list));
      require_equal
        (module Int_t)
        (of_list_map list ~f:Int.succ)
        (Iarray.of_list (List.map list ~f:Int.succ));
      require_equal
        (module Int_t)
        (of_list_mapi list ~f:( + ))
        (Iarray.of_list (List.mapi list ~f:( + )));
      require_equal
        (module Int_t)
        (of_list_rev_map list ~f:Int.succ)
        (Iarray.of_list (List.rev_map list ~f:Int.succ)))
  ;;

  let reduce = Iarray.reduce
  let reduce_exn = Iarray.reduce_exn

  let%expect_test _ =
    let module Int_opt = struct
      type t = int option [@@deriving equal, sexp_of]
    end
    in
    quickcheck_m (module Int_t) ~f:(fun t ->
      require_equal
        (module Int_opt)
        (reduce t ~f:( - ))
        (List.reduce (Iarray.to_list t) ~f:( - ));
      require_equal
        (module Int_opt)
        (Option.try_with (fun () -> reduce_exn t ~f:( - )))
        (reduce t ~f:( - )))
  ;;

  let combine_errors = Iarray.combine_errors
  let combine_errors_unit = Iarray.combine_errors_unit

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = (int, Error.t) Result.t Iarray.t [@@deriving sexp_of]

        let quickcheck_generator =
          quickcheck_generator
            (Generator.result
               Generator.small_positive_or_zero_int
               (Generator.map Generator.string ~f:Error.of_string))
        ;;

        let quickcheck_shrinker = Shrinker.atomic
      end)
      ~f:(fun t ->
        require_equal
          (module struct
            type t = int Iarray.t Or_error.t [@@deriving equal, sexp_of]
          end)
          (combine_errors t)
          (Or_error.map ~f:Iarray.of_list (Or_error.combine_errors (Iarray.to_list t)));
        let unit_t = Iarray.map t ~f:(Or_error.map ~f:ignore) in
        require_equal
          (module struct
            type t = unit Or_error.t [@@deriving equal, sexp_of]
          end)
          (combine_errors_unit unit_t)
          (Or_error.combine_errors_unit (Iarray.to_list unit_t)))
  ;;

  let fold_map = Iarray.fold_map

  let%expect_test _ =
    quickcheck_m (module Int_t) ~f:(fun t ->
      let init = [] in
      let f acc x = x :: acc, -x in
      require_equal
        (module struct
          type t = int list * Int_t.t [@@deriving equal, sexp_of]
        end)
        (fold_map t ~init ~f)
        (List.rev (Iarray.to_list t), Iarray.map t ~f:Int.neg))
  ;;

  let fold_mapi = Iarray.fold_mapi

  let%expect_test _ =
    quickcheck_m (module Int_t) ~f:(fun t ->
      let init = [] in
      let f i acc x = x :: acc, i - x in
      require_equal
        (module struct
          type t = int list * Int_t.t [@@deriving equal, sexp_of]
        end)
        (fold_mapi t ~init ~f)
        (List.rev (Iarray.to_list t), Iarray.mapi t ~f:(fun i x -> i - x)))
  ;;

  let zip = Iarray.zip

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = Int_t.t * Int_t.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t1, t2) ->
        require_equal
          (module struct
            type t = (int * int) Iarray.t option [@@deriving equal, sexp_of]
          end)
          (zip t1 t2)
          (match List.zip (Iarray.to_list t1) (Iarray.to_list t2) with
           | Unequal_lengths -> None
           | Ok list -> Some (Iarray.of_list list)))
  ;;

  let zip_exn = Iarray.zip_exn

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = Int_t.t * Int_t.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t1, t2) ->
        require_equal
          (module struct
            type t = (int * int) Iarray.t option [@@deriving equal, sexp_of]
          end)
          (Option.try_with (fun () -> zip_exn t1 t2))
          (Option.try_with (fun () ->
             Iarray.of_list (List.zip_exn (Iarray.to_list t1) (Iarray.to_list t2)))))
  ;;

  let map2_exn = Iarray.map2_exn

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = Int_t.t * Int_t.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t1, t2) ->
        let f = ( + ) in
        require_equal
          (module struct
            type t = Int_t.t option [@@deriving equal, sexp_of]
          end)
          (Option.try_with (fun () -> map2_exn t1 t2 ~f))
          (Option.try_with (fun () ->
             Iarray.of_list (List.map2_exn (Iarray.to_list t1) (Iarray.to_list t2) ~f))))
  ;;

  let iter2_exn = Iarray.iter2_exn

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = Int_t.t * Int_t.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t1, t2) ->
        let f = ( - ) in
        require_equal
          (module struct
            type t = Int_t.t option [@@deriving equal, sexp_of]
          end)
          (Option.try_with (fun () ->
             let queue = Queue.create () in
             iter2_exn t1 t2 ~f:(fun x1 x2 -> Queue.enqueue queue (f x1 x2));
             Iarray.of_list (Queue.to_list queue)))
          (Option.try_with (fun () -> map2_exn t1 t2 ~f)))
  ;;

  let cartesian_product = Iarray.cartesian_product

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = Int_t.t * Int_t.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t1, t2) ->
        require_equal
          (module struct
            type nonrec t = (int * int) t [@@deriving equal, sexp_of]
          end)
          ([%globalize: (int * int) t] (cartesian_product t1 t2))
          (Iarray.of_list
             (List.cartesian_product (Iarray.to_list t1) (Iarray.to_list t2))))
  ;;

  let unzip = Iarray.unzip

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = (int * int) Iarray.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        require_equal
          (module struct
            type t = Int_t.t * Int_t.t [@@deriving equal, sexp_of]
          end)
          (unzip t)
          (List.unzip (Iarray.to_list t)
           |> Tuple2.map_both ~f1:Iarray.of_list ~f2:Iarray.of_list))
  ;;

  let is_sorted_strictly = Iarray.is_sorted_strictly

  let%expect_test _ =
    quickcheck_m (module Int_t) ~f:(fun t ->
      require_equal
        (module Bool)
        (is_sorted_strictly ~compare:Int.compare t)
        (List.is_sorted_strictly ~compare:Int.compare (Iarray.to_list t)));
    quickcheck_m (module Int_t) ~f:(fun t ->
      let sorted = dedup_and_sort ~compare:Int.compare t in
      require_equal (module Bool) true (is_sorted_strictly ~compare:Int.compare sorted))
  ;;

  let random_element = Iarray.random_element
  let random_element_exn = Iarray.random_element_exn

  let%expect_test _ =
    require_none [%sexp_of: int] (random_element empty);
    [%expect {| |}];
    require_does_raise (fun () -> random_element_exn empty);
    [%expect {| "Iarray.random_element_exn: empty array" |}];
    require ([%equal: int option] (random_element (singleton 0)) (Some 0));
    [%expect {| |}];
    require_equal (module Int) (random_element_exn (singleton 0)) 0;
    [%expect {| |}];
    quickcheck_m (module Int_t) ~f:(fun t ->
      match random_element t with
      | None -> require (Iarray.is_empty t)
      | Some x -> require (Iarray.mem t x ~equal:Int.equal))
  ;;

  let last_exn = Iarray.last_exn

  let%expect_test "Iarray.last" =
    require_does_raise (fun () -> last_exn empty);
    [%expect {| (Invalid_argument "index out of bounds") |}];
    require (Int.equal (last_exn (singleton 0)) 0);
    [%expect {| |}];
    quickcheck_m (module Int_t) ~f:(fun t ->
      if Iarray.is_empty t
      then ()
      else require (Int.equal (last_exn t) (List.last_exn (Iarray.to_list t))))
  ;;

  let%expect_test "Iarray.filter reserves physical equality" =
    let x = Iarray.of_list [ 1; 2; 3 ] in
    let y = Iarray.filter x ~f:(fun _ -> true) in
    require (phys_equal x y);
    [%expect {| |}]
  ;;

  let unsafe_to_array__promise_no_mutation = Iarray.unsafe_to_array__promise_no_mutation

  external unsafe_of_array__promise_no_mutation
    :  ('a array[@local_opt])
    -> ('a t[@local_opt])
    @@ portable
    = "%array_to_iarray"

  let%expect_test _ =
    quickcheck_m (module Int_t) ~f:(fun imm1 ->
      let arr1 = unsafe_to_array__promise_no_mutation (Sys.opaque_identity imm1) in
      let imm2 = unsafe_of_array__promise_no_mutation (Sys.opaque_identity arr1) in
      require (phys_equal imm1 imm2);
      let arr2 = unsafe_to_array__promise_no_mutation (Sys.opaque_identity imm2) in
      require (phys_equal arr1 arr2))
  ;;

  include (
    Iarray :
    sig
    @@ portable
      include Indexed_container.S1_with_creators with type 'a t := 'a t
    end)

  include struct
    open%template struct
      (* Accumulators just convert from an int and then do an int operation *)
      let[@kind bits64] of_int = Int64_u.of_int
      let[@kind bits32] of_int = Int32_u.of_int_trunc
      let[@kind word] of_int = Nativeint_u.of_int
      let[@kind float64] of_int = Float_u.of_int
      let[@kind bits64] to_int = Int64_u.to_int_trunc
      let[@kind bits32] to_int = Int32_u.to_int_trunc
      let[@kind word] to_int = Nativeint_u.to_int_trunc
      let[@kind float64] to_int = Float_u.to_int
    end

    [%%template
    [@@@kind.default ka = value, kacc = (bits64, bits32, word, float64)]

    let fold = (Iarray.fold [@kind ka kacc])
    let foldi = (Iarray.foldi [@kind ka kacc])
    let fold_right = (Iarray.fold_right [@kind ka kacc])

    let%expect_test _ =
      let iarr = List.range 2 7 |> Iarray.of_list_rev in
      let test fold ~f =
        fold iarr ~init:((of_int [@kind kacc]) 6) ~f
        |> (to_int [@kind kacc])
        |> [%sexp_of: int]
        |> print_s
      in
      test (fold [@kind ka kacc]) ~f:(fun acc a ->
        (to_int [@kind kacc]) acc + a |> (of_int [@kind kacc]));
      [%expect {| 26 |}];
      test (foldi [@kind ka kacc]) ~f:(fun i acc a ->
        (to_int [@kind kacc]) acc + a - i |> (of_int [@kind kacc]));
      [%expect {| 16 |}];
      test (fold_right [@kind ka kacc]) ~f:(fun a acc ->
        (to_int [@kind kacc]) acc + a |> (of_int [@kind kacc]));
      [%expect {| 26 |}]
    ;;]
  end

  (* Tested as [Local.init] below *)
  let%template[@alloc stack] init = (Iarray.init [@alloc stack])

  external length
    : ('a : any mod separable).
    ('a t[@local_opt]) @ immutable -> int
    @@ portable
    = "%array_length"
  [@@layout_poly]

  let%expect_test "Ensure [Iarray.length] accepts [local_]s" =
    let global_iarray = Iarray.unsafe_of_array__promise_no_mutation [| 1; 2; 3; 4 |] in
    printf "%d " (Iarray.length global_iarray);
    let local_ local_iarray =
      Iarray.unsafe_of_array__promise_no_mutation [| 1; 2; 3; 4 |]
    in
    printf "%d\n" (Iarray.length local_iarray);
    [%expect {| 4 4 |}]
  ;;

  let%expect_test _ =
    Base_container_tests.test_indexed_container_s1_with_creators (module Iarray);
    [%expect
      {|
      Container: testing [length]
      Container: testing [is_empty]
      Container: testing [mem]
      Container: testing [iter]
      Container: testing [iter_until]
      Container: testing [fold]
      Container: testing [fold_result]
      Container: testing [fold_until]
      Container: testing [exists]
      Container: testing [for_all]
      Container: testing [count]
      Container: testing [sum]
      Container: testing [find]
      Container: testing [find_map]
      Container: testing [to_list]
      Container: testing [to_array]
      Container: testing [min_elt]
      Container: testing [max_elt]
      Container: testing [of_list]
      Container: testing [of_array]
      Container: testing [append]
      Container: testing [concat]
      Container: testing [map]
      Container: testing [filter]
      Container: testing [filter_map]
      Container: testing [concat_map]
      Container: testing [partition_tf]
      Container: testing [partition_map]
      Container: testing [foldi]
      Container: testing [foldi_until]
      Container: testing [iteri]
      Container: testing [iteri_until]
      Container: testing [existsi]
      Container: testing [for_alli]
      Container: testing [counti]
      Container: testing [findi]
      Container: testing [find_mapi]
      Container: testing [init]
      Container: testing [mapi]
      Container: testing [filteri]
      Container: testing [filter_mapi]
      Container: testing [concat_mapi]
      Container: testing [partitioni_tf]
      Container: testing [partition_mapi]
      |}]
  ;;

  let%expect_test "standard stable conversions" =
    print_and_check_stable_type
      (module struct
        type t = int Iarray.t [@@deriving bin_io, compare, sexp]
      end)
      (List.init 10 ~f:(fun len -> Iarray.init len ~f:Fn.id));
    [%expect
      {|
      (bin_shape_digest 4c138035aa69ec9dd8b7a7119090f84a)
      ((sexp ()) (bin_io "\000"))
      ((sexp (0)) (bin_io "\001\000"))
      ((sexp (0 1)) (bin_io "\002\000\001"))
      ((sexp (0 1 2)) (bin_io "\003\000\001\002"))
      ((sexp (0 1 2 3)) (bin_io "\004\000\001\002\003"))
      ((sexp (0 1 2 3 4)) (bin_io "\005\000\001\002\003\004"))
      ((sexp (0 1 2 3 4 5)) (bin_io "\006\000\001\002\003\004\005"))
      ((sexp (0 1 2 3 4 5 6)) (bin_io "\007\000\001\002\003\004\005\006"))
      ((sexp (0 1 2 3 4 5 6 7)) (bin_io "\b\000\001\002\003\004\005\006\007"))
      ((sexp (0 1 2 3 4 5 6 7 8)) (bin_io "\t\000\001\002\003\004\005\006\007\b"))
      |}]
  ;;

  let%expect_test "local bin-io" =
    let module M = struct
      type t = int Iarray.t [@@deriving bin_io ~localize, equal, sexp_of]
    end
    in
    print_and_check_round_trip
      (module M)
      [ (module%template struct
          type t = int Iarray.t
          type repr = string [@@deriving sexp_of]

          let repr_name = "bin_io__local"
          let to_repr = (Binable.to_string [@mode local]) (module M)
          let of_repr = (Binable.of_string [@mode local]) (module M)
        end)
      ]
      (List.init 10 ~f:(fun len -> Iarray.init len ~f:Fn.id));
    [%expect
      {|
      "\000"
      "\001\000"
      "\002\000\001"
      "\003\000\001\002"
      "\004\000\001\002\003"
      "\005\000\001\002\003\004"
      "\006\000\001\002\003\004\005"
      "\007\000\001\002\003\004\005\006"
      "\b\000\001\002\003\004\005\006\007"
      "\t\000\001\002\003\004\005\006\007\b"
      |}]
  ;;

  let%expect_test "local bin-io for iarrays with float hack representation" =
    let module M = struct
      type t = float Iarray.t [@@deriving bin_io ~localize, equal, sexp_of]
    end
    in
    print_and_check_round_trip
      (module M)
      [ (module struct
          type t = float Iarray.t
          type repr = string [@@deriving sexp_of]

          let repr_name = "bin_io"
          let to_repr = Binable.to_string (module M)
          let of_repr = Binable.of_string (module M)
        end)
      ; (module%template struct
          type t = float Iarray.t
          type repr = string [@@deriving sexp_of]

          let repr_name = "bin_io__local"
          let to_repr = (Binable.to_string [@mode local]) (module M)
          let of_repr = (Binable.of_string [@mode local]) (module M)
        end)
      ]
      (List.init 4 ~f:(fun len -> Iarray.init len ~f:Float.of_int));
    [%expect
      {|
      ((bin_io        "\000")
       (bin_io__local "\000"))
      ((bin_io "\001\000\000\000\000\000\000\000\000")
       (bin_io__local "\001\000\000\000\000\000\000\000\000"))
      ((bin_io "\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\240?")
       (bin_io__local
        "\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\240?"))
      ((bin_io
        "\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\240?\000\000\000\000\000\000\000@")
       (bin_io__local
        "\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\240?\000\000\000\000\000\000\000@"))
      |}]
  ;;

  module O = struct
    (* already tested as [get] *)
    external ( .:() )
      :  ('a t[@local_opt])
      -> int
      -> ('a[@local_opt])
      @@ portable
      = "%array_safe_get"
  end

  include O

  module Stable = struct
    module V1 = struct
      type 'a t = 'a Iarray.Stable.V1.t
      [@@deriving compare ~localize, equal ~localize, globalize, hash]

      include (
        Iarray.Stable.V1 :
        sig
        @@ portable
          include%template Stable1_with_witness [@mode local] with type 'a t := 'a t
        end)

      let%expect_test _ =
        print_and_check_stable_type
          (module struct
            type nonrec t = int t [@@deriving bin_io, compare, sexp]
          end)
          (List.init 10 ~f:(fun len -> Iarray.init len ~f:Fn.id));
        [%expect
          {|
          (bin_shape_digest 4c138035aa69ec9dd8b7a7119090f84a)
          ((sexp ()) (bin_io "\000"))
          ((sexp (0)) (bin_io "\001\000"))
          ((sexp (0 1)) (bin_io "\002\000\001"))
          ((sexp (0 1 2)) (bin_io "\003\000\001\002"))
          ((sexp (0 1 2 3)) (bin_io "\004\000\001\002\003"))
          ((sexp (0 1 2 3 4)) (bin_io "\005\000\001\002\003\004"))
          ((sexp (0 1 2 3 4 5)) (bin_io "\006\000\001\002\003\004\005"))
          ((sexp (0 1 2 3 4 5 6)) (bin_io "\007\000\001\002\003\004\005\006"))
          ((sexp (0 1 2 3 4 5 6 7)) (bin_io "\b\000\001\002\003\004\005\006\007"))
          ((sexp (0 1 2 3 4 5 6 7 8)) (bin_io "\t\000\001\002\003\004\005\006\007\b"))
          |}]
      ;;

      let t_sexp_grammar = Iarray.Stable.V1.t_sexp_grammar

      let%expect_test "sexp_grammar" =
        require_ok
          (Sexp_grammar_validation.validate_grammar_poly1
             (module struct
               type nonrec 'a t = 'a t [@@deriving sexp, sexp_grammar]

               [%%rederive type 'a t = 'a Iarray.t [@@deriving quickcheck]]
             end));
        [%expect {| (List (Many (Any A))) |}]
      ;;
    end
  end

  (* We test [Local] functions against their non-local counterparts. *)
  module Local = struct
    open Test_container_with_local

    let singleton = Iarray.Local.singleton

    let%expect_test _ =
      print_s [%sexp ([%globalize: int t] (singleton 1) : int t)];
      [%expect {| (1) |}]
    ;;

    let create = Iarray.Local.create

    let%expect_test _ =
      quickcheck_m (module Int_t) ~f:(fun t ->
        require_equal
          (module Int_t)
          (create ~len:(Iarray.length t) 0 ~mutate:(fun array ->
             for pos = 0 to Iarray.length t - 1 do
               Array.set array pos (Iarray.get t pos)
             done)
           |> Int_t.globalize)
          t);
      [%expect {| |}]
    ;;

    let rev = Iarray.Local.rev

    let%expect_test _ =
      test
        (module Int_t)
        (module Int_t)
        (fun t -> exclave_ Iarray.Local.rev t)
        (fun t -> Iarray.rev t);
      [%expect {| |}]
    ;;

    let prefix = Iarray.Local.prefix

    let%expect_test _ =
      test
        (module struct
          type t = Int_t.t * (int[@generator Generator.small_positive_or_zero_int])
          [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = Int_t.t option [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, len) -> exclave_
          match Iarray.Local.prefix t ~len with
          | t -> Some t
          | exception _ -> None)
        (fun (t, len) -> Option.try_with (fun () -> Iarray.prefix t ~len))
        ~noalloc:(fun (t, len) -> exclave_
          if len < 0 || len > Iarray.Local.length t
          then None
          else Some (Iarray.Local.prefix t ~len));
      [%expect {| |}]
    ;;

    let suffix = Iarray.Local.suffix

    let%expect_test _ =
      test
        (module struct
          type t = Int_t.t * (int[@generator Generator.small_positive_or_zero_int])
          [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = Int_t.t option [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, len) -> exclave_
          match Iarray.Local.suffix t ~len with
          | t -> Some t
          | exception _ -> None)
        (fun (t, len) -> Option.try_with (fun () -> Iarray.suffix t ~len))
        ~noalloc:(fun (t, len) -> exclave_
          if len < 0 || len > Iarray.Local.length t
          then None
          else Some (Iarray.Local.suffix t ~len));
      [%expect {| |}]
    ;;

    let drop_prefix = Iarray.Local.drop_prefix

    let%expect_test _ =
      test
        (module struct
          type t = Int_t.t * (int[@generator Generator.small_positive_or_zero_int])
          [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = Int_t.t option [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, len) -> exclave_
          match Iarray.Local.drop_prefix t ~len with
          | t -> Some t
          | exception _ -> None)
        (fun (t, len) -> Option.try_with (fun () -> Iarray.drop_prefix t ~len))
        ~noalloc:(fun (t, len) -> exclave_
          if len < 0 || len > Iarray.Local.length t
          then None
          else Some (Iarray.Local.drop_prefix t ~len));
      [%expect {| |}]
    ;;

    let drop_suffix = Iarray.Local.drop_suffix

    let%expect_test _ =
      test
        (module struct
          type t = Int_t.t * (int[@generator Generator.small_positive_or_zero_int])
          [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = Int_t.t option [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, len) -> exclave_
          match Iarray.Local.drop_suffix t ~len with
          | t -> Some t
          | exception _ -> None)
        (fun (t, len) -> Option.try_with (fun () -> Iarray.drop_suffix t ~len))
        ~noalloc:(fun (t, len) -> exclave_
          if len < 0 || len > Iarray.Local.length t
          then None
          else Some (Iarray.Local.drop_prefix t ~len));
      [%expect {| |}]
    ;;

    let sub = Iarray.Local.sub

    let%expect_test _ =
      test
        (module struct
          type t =
            Int_t.t
            * (int[@generator Generator.small_positive_or_zero_int])
            * (int[@generator Generator.small_positive_or_zero_int])
          [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = Int_t.t option [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, pos, len) -> exclave_
          match Iarray.Local.sub t ~pos ~len with
          | t -> Some t
          | exception _ -> None)
        (fun (t, pos, len) -> Option.try_with (fun () -> Iarray.sub t ~pos ~len))
        ~noalloc:(fun (t, pos, len) -> exclave_
          if pos < 0 || len < 0 || pos + len > Iarray.Local.length t
          then None
          else Some (Iarray.Local.sub t ~pos ~len));
      [%expect {| |}]
    ;;

    let subo = Iarray.Local.subo

    let%expect_test _ =
      test
        (module struct
          type t =
            Int_t.t
            * (int[@generator Generator.small_positive_or_zero_int]) option
            * (int[@generator Generator.small_positive_or_zero_int]) option
          [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = Int_t.t option [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t, pos, len) -> exclave_
          match Iarray.Local.subo t ?pos ?len with
          | t -> Some t
          | exception _ -> None)
        (fun (t, pos, len) -> Option.try_with (fun () -> Iarray.subo t ?pos ?len))
        ~noalloc:(fun (t, pos, len) -> exclave_
          let length = Iarray.Local.length t in
          match local_ pos, len with
          | Some pos, _ when pos < 0 || pos > length -> None
          | _, Some len when len < 0 || len > length -> None
          | Some pos, Some len when pos + len > length -> None
          | _ -> Some (Iarray.Local.subo t ?pos ?len));
      [%expect {| |}]
    ;;

    let init_with_globals = Iarray.Local.init_with_globals

    let%expect_test _ =
      test
        (module Int_t)
        (module Int_t)
        (fun t -> exclave_
          init_with_globals (Iarray.Local.length t) ~f:(fun pos ->
            Iarray.get t pos [@nontail])
          [@nontail])
        Fn.id;
      [%expect {| |}]
    ;;

    let of_list_rev = Iarray.Local.of_list_rev

    let%expect_test _ =
      test
        (module struct
          type t = int list [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = Int_t.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun list -> exclave_ of_list_rev list)
        (fun list -> Iarray.of_list_rev list)
    ;;

    let of_list_map = Iarray.Local.of_list_map

    let%expect_test _ =
      test
        (module struct
          type t = int list [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = Int_t.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun list -> exclave_ of_list_map list ~f:(fun x -> x + 1))
        (fun list -> Iarray.of_list_map list ~f:(fun x -> x + 1))
    ;;

    let of_list_mapi = Iarray.Local.of_list_mapi

    let%expect_test _ =
      test
        (module struct
          type t = int list [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = Int_t.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun list -> exclave_ of_list_mapi list ~f:(fun i x -> x + i))
        (fun list -> Iarray.of_list_mapi list ~f:(fun i x -> x + i))
    ;;

    let of_list_rev_map = Iarray.Local.of_list_rev_map

    let%expect_test _ =
      test
        (module struct
          type t = int list [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = Int_t.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun list -> exclave_ of_list_rev_map list ~f:(fun x -> x + 1))
        (fun list -> Iarray.of_list_rev_map list ~f:(fun x -> x + 1))
    ;;

    let iter2_exn = Iarray.Local.iter2_exn

    let%expect_test _ =
      test
        (module struct
          type t = int list * int list [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = Int_t.t option [@@deriving equal, globalize, sexp_of]
        end)
        (fun (list1, list2) -> exclave_
          let local_ array =
            Array.create_local ~len:(Int.max (List.length list1) (List.length list2)) 0
          in
          let local_ pos = ref 0 in
          match
            iter2_exn
              (Iarray.Local.of_list list1)
              (Iarray.Local.of_list list2)
              ~f:(fun x y ->
                array.(!pos) <- x - y;
                pos := !pos + 1)
          with
          | () -> Some (Iarray.unsafe_of_array__promise_no_mutation array)
          | exception _ -> None)
        (fun (list1, list2) ->
          Option.try_with (fun () ->
            let queue = Queue.create () in
            Iarray.iter2_exn (Iarray.of_list list1) (Iarray.of_list list2) ~f:(fun x y ->
              Queue.enqueue queue (x - y));
            Iarray.of_list (Queue.to_list queue)))
    ;;

    let map2_exn = Iarray.Local.map2_exn

    let%expect_test _ =
      test
        (module struct
          type t = int list * int list [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type t = Int_t.t option [@@deriving equal, globalize, sexp_of]
        end)
        (fun (list1, list2) -> exclave_
          match
            map2_exn
              (Iarray.Local.of_list list1)
              (Iarray.Local.of_list list2)
              ~f:(fun x y -> x - y)
          with
          | array -> Some array
          | exception _ -> None)
        (fun (list1, list2) ->
          Option.try_with (fun () ->
            Iarray.map2_exn (Iarray.of_list list1) (Iarray.of_list list2) ~f:(fun x y ->
              x - y)))
    ;;

    let cartesian_product = Iarray.Local.cartesian_product

    let%expect_test _ =
      test
        (module struct
          type t = Int_t.t * Int_t.t [@@deriving quickcheck, sexp_of]
        end)
        (module struct
          type nonrec t = (int * int) t [@@deriving equal, globalize, sexp_of]
        end)
        (fun (t1, t2) -> exclave_ cartesian_product t1 t2)
        (fun (t1, t2) -> Iarray.cartesian_product t1 t2)
    ;;

    let fold_right = Iarray.Local.fold_right

    let%expect_test _ =
      quickcheck_m (module Int_t) ~f:(fun t ->
        require_equal
          (module Int)
          (fold_right t ~init:0 ~f:( - ))
          (List.fold_right (Iarray.to_list t) ~init:0 ~f:( - )))
    ;;

    let fold_map = Iarray.Local.fold_map

    let%expect_test _ =
      test
        (module Int_t)
        (module struct
          type t = int list * Int_t.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun t -> exclave_ fold_map t ~init:[] ~f:(fun acc x -> exclave_ x :: acc, -x))
        (fun t -> Iarray.fold_map t ~init:[] ~f:(fun acc x -> x :: acc, -x))
    ;;

    let fold_mapi = Iarray.Local.fold_mapi

    let%expect_test _ =
      test
        (module Int_t)
        (module struct
          type t = int list * Int_t.t [@@deriving equal, globalize, sexp_of]
        end)
        (fun t -> exclave_
          fold_mapi t ~init:[] ~f:(fun i acc x -> exclave_ x :: acc, i - x))
        (fun t -> Iarray.fold_mapi t ~init:[] ~f:(fun i acc x -> x :: acc, i - x))
    ;;

    let last_exn = Iarray.Local.last_exn

    let%expect_test _ =
      test
        (module Int_t)
        (module struct
          type t = int option [@@deriving equal, globalize, sexp_of]
        end)
        (fun t -> exclave_
          try Some (last_exn t) with
          | _ -> None)
        (fun t -> Option.try_with (fun () -> Iarray.last_exn t))
    ;;

    module Let_syntax = Iarray.Local.Let_syntax

    let%expect_test _ =
      quickcheck_m
        (module struct
          type t = Int_t.t * Int_t.t [@@deriving quickcheck, sexp_of]
        end)
        ~f:(fun (t1, t2) ->
          let f_map = ( + ) in
          let f_bind x = Iarray.unsafe_of_array__promise_no_mutation [| x; x * x |] in
          require_equal
            (module struct
              type nonrec t = Int_t.t [@@deriving equal, sexp_of]
            end)
            ([%globalize: Int_t.t]
               (let open Let_syntax in
                let%bindl x =
                  let%mapl x1 = t1
                  and x2 = t2 in
                  f_map x1 x2
                in
                f_bind x))
            (Iarray.of_list
               (let open List.Let_syntax in
                let%bind x =
                  let%map x1 = Iarray.to_list t1
                  and x2 = Iarray.to_list t2 in
                  f_map x1 x2
                in
                Iarray.to_list (f_bind x))))
    ;;

    include (
      Iarray.Local :
      sig
      @@ portable
        include Container_with_local.S1_indexed_with_creators with type 'a t := 'a iarray
      end)

    include struct
      open%template struct
        (* Accumulators just convert from an int and then do an int operation *)
        let[@kind bits64] of_int = Int64_u.of_int
        let[@kind bits32] of_int = Int32_u.of_int_trunc
        let[@kind word] of_int = Nativeint_u.of_int
        let[@kind float64] of_int = Float_u.of_int
        let[@kind bits64] to_int = Int64_u.to_int_trunc
        let[@kind bits32] to_int = Int32_u.to_int_trunc
        let[@kind word] to_int = Nativeint_u.to_int_trunc
        let[@kind float64] to_int = Float_u.to_int
      end

      [%%template
      [@@@kind.default ka = value, kacc = (bits64, bits32, word, float64)]

      let fold = (Iarray.Local.fold [@kind ka kacc])
      let foldi = (Iarray.Local.foldi [@kind ka kacc])
      let fold_right = (Iarray.Local.fold_right [@kind ka kacc])

      let%expect_test _ =
        let iarr = List.range 2 7 |> Iarray.Local.of_list_rev in
        let test fold ~f =
          fold iarr ~init:((of_int [@kind kacc]) 6) ~f
          |> (to_int [@kind kacc])
          |> [%sexp_of: int]
          |> print_s
        in
        test (fold [@kind ka kacc]) ~f:(fun acc a ->
          (to_int [@kind kacc]) acc + a |> (of_int [@kind kacc]));
        [%expect {| 26 |}];
        test (foldi [@kind ka kacc]) ~f:(fun i acc a ->
          (to_int [@kind kacc]) acc + a - i |> (of_int [@kind kacc]));
        [%expect {| 16 |}];
        test (fold_right [@kind ka kacc]) ~f:(fun a acc ->
          (to_int [@kind kacc]) acc + a |> (of_int [@kind kacc]));
        [%expect {| 26 |}]
      ;;]
    end

    let%expect_test _ =
      test_indexed_container_with_creators
        (module struct
          type 'a t = 'a Iarray.t [@@deriving equal, globalize, quickcheck, sexp_of]

          type 'a elt = 'a
          [@@deriving compare ~localize, equal ~localize, globalize, quickcheck, sexp_of]

          type 'a concat = 'a Iarray.t [@@deriving quickcheck, sexp_of]

          module Global = Iarray
          module Local = Iarray.Local
        end);
      [%expect
        {|
        Container_with_local: length
        Container_with_local: is_empty
        Container_with_local: mem
        Container_with_local: iter
        Container_with_local: fold
        Container_with_local: fold_result
        Container_with_local: fold_until
        Container_with_local: exists
        Container_with_local: for_all
        Container_with_local: count
        Container_with_local: sum
        Container_with_local: find
        Container_with_local: find_map
        Container_with_local: to_list
        Container_with_local: min_elt
        Container_with_local: max_elt
        Container_with_local: of_list
        Container_with_local: append
        Container_with_local: concat
        Container_with_local: map
        Container_with_local: map_to_global
        Container_with_local: map_of_global
        Container_with_local: filter
        Container_with_local: filter_map
        Container_with_local: concat_map
        Container_with_local: partition_tf
        Container_with_local: partition_map
        Container_with_local: foldi
        Container_with_local: iteri
        Container_with_local: existsi
        Container_with_local: for_alli
        Container_with_local: counti
        Container_with_local: findi
        Container_with_local: find_mapi
        Container_with_local: init
        Container_with_local: mapi
        Container_with_local: mapi_to_global
        Container_with_local: mapi_of_global
        Container_with_local: filteri
        Container_with_local: filter_mapi
        Container_with_local: concat_mapi
        Container_with_local: partitioni_tf
        Container_with_local: partition_mapi
        |}]
    ;;
  end

  module Unique = struct
    (* We test the Unique module by implementing all functions using only one bit of
       magic, to convert to and from a unique list. This is slower and involves more
       allocations than the implementations in Array, which are implemented in terms of
       magic *)
    open struct
      let of_unique_list : 'a list @ unique -> 'a iarray @ unique =
        fun l -> Obj.magic_unique (Iarray.of_list l)
      ;;

      let to_unique_list : 'a iarray @ unique -> 'a list @ unique =
        fun a ->
        let[@tail_mod_cons] rec go i =
          if i < Iarray.length a then Obj.magic_unique a.:(i) :: go (i + 1) else []
        in
        go 0
      ;;
    end

    (* ---- No magic after this point! ---- *)

    (* Implementation without magic - potentially less efficient but correct *)
    let init len ~f =
      let[@tail_mod_cons] rec go i = if i < len then f i :: go (i + 1) else [] in
      of_unique_list (go 0)
    ;;

    let mapi t ~f =
      let[@tail_mod_cons] rec go i = function
        | [] -> []
        | x :: xs -> f i x :: go (i + 1) xs
      in
      of_unique_list (go 0 (to_unique_list t))
    ;;

    let map (t @ unique) ~f = mapi t ~f:(fun _ x -> f x) [@nontail]
    let iteri (t @ unique) ~f = ignore (mapi t ~f : unit t)
    let iter (t @ unique) ~f = iteri t ~f:(fun _ x -> f x) [@nontail]

    let unzip (t @ unique) =
      let rec go = function
        | [] -> [], []
        | (x, y) :: l ->
          let l1, l2 = go l in
          x :: l1, y :: l2
      in
      let l1, l2 = go (to_unique_list t) in
      of_unique_list l1, of_unique_list l2
    ;;

    let zip_exn (t1 @ unique) (t2 @ unique) =
      let[@tail_mod_cons] rec go = function
        | #([], []) -> []
        | #(x :: xs, y :: ys) -> (x, y) :: go #(xs, ys)
        | _ -> invalid_arg "zip_exn"
      in
      of_unique_list (go #(to_unique_list t1, to_unique_list t2))
    ;;
  end

  module Private = struct
    (* We test that uses of the [iarray] functions labeled "VERY UNSAFE" are in fact safe.
       This relies on the tests being run in debug mode, i.e. with [-runtime_variant d]. *)
    module Test_unsafe_local_implementations = struct
      open struct
        (* Testing helpers. *)

        let ignore_local (local_ x) =
          let local_ _ignored = Sys.opaque_identity x in
          ()
        ;;

        let overhead =
          let count1 = Gc.allocated_bytes () in
          let count2 = Gc.allocated_bytes () in
          count2 -. count1
        ;;

        let run (f : local_ _ -> local_ _) (local_ x) =
          let f = Sys.opaque_identity f in
          let x = Sys.opaque_identity x in
          let x = Iarray.unsafe_of_array__promise_no_mutation x in
          let before = Gc.allocated_bytes () in
          let local_ y = f x in
          let after = Gc.allocated_bytes () in
          let delta =
            int_of_float (after -. before -. overhead) / (Sys.word_size_in_bits / 8)
          in
          if delta = 0
          then print_endline "No allocation."
          else print_cr [%message "Allocated unexpectedly."];
          if phys_same x y
          then print_cr [%message "input == output, not a good test of allocation"];
          (* In debug mode, Gc.minor () checks for forwards local pointers and minor
             heap->local pointers (though we're more concerned about the former) *)
          Gc.minor ();
          ignore_local y [@nontail]
        ;;

        (* In order to properly excercise the no forwards local pointers checks, we need to
           actually try to make some pointers. Specifically, if we create a value
           [a] which 'points to' [b], but [b] happens to be an immedate, then this is
           not caught as a forwards pointer even if [b] was morally 'allocated' later. *)
        let alloc_local_non_immediate (local_ i) = exclave_ `A i
      end

      module Iarray = Base.Iarray

      let concat = Iarray.Local.concat

      let%expect_test _ =
        run
          Iarray.Local.concat
          [| Iarray.unsafe_of_array__promise_no_mutation [||]
           ; Iarray.unsafe_of_array__promise_no_mutation [| 1 |]
           ; Iarray.unsafe_of_array__promise_no_mutation [| 1; 2 |]
           ; Iarray.unsafe_of_array__promise_no_mutation [| 1; 2; 3 |]
          |];
        [%expect {| No allocation. |}]
      ;;

      let concat_map = Iarray.Local.concat_map

      let%expect_test _ =
        run
          (fun iarray -> exclave_
            concat_map iarray ~f:(fun x -> exclave_
              Iarray.Local.init x ~f:alloc_local_non_immediate))
          [| 0; 1; 2; 3 |];
        [%expect {| No allocation. |}]
      ;;

      let concat_mapi = Iarray.Local.concat_mapi

      let%expect_test _ =
        run
          (fun iarray -> exclave_
            concat_mapi iarray ~f:(fun _ x -> exclave_
              Iarray.Local.init x ~f:alloc_local_non_immediate))
          [| 0; 1; 2; 3 |];
        [%expect {| No allocation. |}]
      ;;

      let filter = Iarray.Local.filter

      let%expect_test _ =
        run (fun iarray -> exclave_ filter iarray ~f:(fun _ -> true)) [| 1; 2; 3; 4 |];
        [%expect {| No allocation. |}]
      ;;

      let filteri = Iarray.Local.filteri

      let%expect_test _ =
        run (fun iarray -> exclave_ filteri iarray ~f:(fun _ _ -> true)) [| 1; 2; 3; 4 |];
        [%expect {| No allocation. |}]
      ;;

      let filter_map = Iarray.Local.filter_map

      let%expect_test _ =
        run
          (fun iarray -> exclave_
            filter_map iarray ~f:(fun x -> exclave_ Some (alloc_local_non_immediate x)))
          [| 1; 2; 3; 4 |];
        [%expect {| No allocation. |}]
      ;;

      let filter_mapi = Iarray.Local.filter_mapi

      let%expect_test _ =
        run
          (fun iarray -> exclave_
            filter_mapi iarray ~f:(fun _ x -> exclave_ Some (alloc_local_non_immediate x)))
          [| 1; 2; 3; 4 |];
        [%expect {| No allocation. |}]
      ;;

      let partition_tf = Iarray.Local.partition_tf

      let%expect_test _ =
        run
          (fun iarray -> exclave_ partition_tf iarray ~f:(fun x -> x > 2))
          [| 1; 2; 3; 4 |];
        [%expect {| No allocation. |}]
      ;;

      let partition_map = Iarray.Local.partition_map

      let%expect_test _ =
        run
          (fun iarray -> exclave_
            partition_map iarray ~f:(fun x -> exclave_
              let y = alloc_local_non_immediate x in
              if x > 2 then First y else Second y))
          [| 1; 2; 3; 4 |];
        [%expect {| No allocation. |}]
      ;;

      let fold_mapi = Iarray.Local.fold_mapi

      let%expect_test _ =
        run
          (fun iarray -> exclave_
            fold_mapi
              iarray
              ~init:(alloc_local_non_immediate 0)
              ~f:(fun i acc x -> exclave_
                let (`A acc) = acc in
                alloc_local_non_immediate (i - x + acc), alloc_local_non_immediate (x - i)))
          [| 1; 2; 3; 4 |];
        [%expect {| No allocation. |}]
      ;;

      let fold_map = Iarray.Local.fold_map

      let%expect_test _ =
        run
          (fun iarray -> exclave_
            fold_map iarray ~init:(alloc_local_non_immediate 0) ~f:(fun acc x -> exclave_
              let (`A acc) = acc in
              alloc_local_non_immediate (x + acc), alloc_local_non_immediate (x - acc)))
          [| 1; 2; 3; 4 |];
        [%expect {| No allocation. |}]
      ;;
    end
  end

  (* This test uses allocations as a proxy for checking for inlining. In particular,
     [min_elt] and [max_elt] are implemented by using a helper function that accepts
     a predicate [first_is_better_than_second] (used to abstract over the direction of
     comparison). This test ensures that the helper function is inlined; if it weren't,
     then the predicates constructed by [min_elt] and [max_elt] would allocate a closure.
     The two allocated minor words are for the returned option. *)
  let%expect_test ("[min_elt] and [max_elt] don't allocate" [@tags "fast-flambda2"]) =
    let iarr =
      Sys.opaque_identity (Iarray.unsafe_of_array__promise_no_mutation [| 1; 2; 3; 4 |])
    in
    let #(_, allocs) =
      Gc.For_testing.measure_allocation (fun () -> min_elt ~compare:Int.compare iarr)
    in
    print_s [%message (allocs : Gc.For_testing.Allocation_report.t)];
    [%expect
      {|
      (allocs (
        (major_words_allocated 0)
        (minor_words_allocated 2)))
      |}];
    let #(_, allocs) =
      Gc.For_testing.measure_allocation (fun () -> max_elt ~compare:Int.compare iarr)
    in
    print_s [%message (allocs : Gc.For_testing.Allocation_report.t)];
    [%expect
      {|
      (allocs (
        (major_words_allocated 0)
        (minor_words_allocated 2)))
      |}]
  ;;

  let t_sexp_grammar = Iarray.t_sexp_grammar

  let%expect_test "sexp_grammar" =
    require_ok
      (Sexp_grammar_validation.validate_grammar_poly1
         (module struct
           type nonrec 'a t = 'a t [@@deriving quickcheck, sexp, sexp_grammar]
         end));
    [%expect {| (List (Many (Any A))) |}]
  ;;
end
