open! Core
open! List
module Shrinker = Quickcheck.Shrinker

let%test_module "shrinker" =
  (module struct
    let t0 =
      Shrinker.create (fun v ->
        if Poly.( = ) 0 v then Sequence.empty else Sequence.singleton 0)
    ;;

    let test_list = [ 1; 2; 3 ]

    let expect =
      [ [ 2; 3 ]; [ 0; 2; 3 ]; [ 1; 3 ]; [ 1; 0; 3 ]; [ 1; 2 ]; [ 1; 2; 0 ] ]
      |> List.sort ~compare:[%compare: int list]
    ;;

    let rec recursive_list = 1 :: 5 :: recursive_list

    let%test_unit "shrinker produces expected outputs" =
      let shrunk =
        Shrinker.shrink (quickcheck_shrinker t0) test_list
        |> Sequence.to_list
        |> List.sort ~compare:[%compare: int list]
      in
      [%test_result: int list list] ~expect shrunk
    ;;

    let%test_unit "shrinker on infinite lists produces values" =
      let shrunk = Shrinker.shrink (quickcheck_shrinker t0) recursive_list in
      let result_length = Sequence.take shrunk 5 |> Sequence.to_list |> List.length in
      [%test_result: int] ~expect:5 result_length
    ;;
  end)
;;

let%test_module "random" =
  (module struct
    module G = Quickcheck.Generator

    module type T = sig
      type t [@@deriving sexp_of]

      include Quickcheckable with type t := t
      include Comparable with type t := t

      val module_name : string
    end

    module type Math = sig
      type t

      include T with type t := t
      include Container.Summable with type t := t
    end

    module Make (T : T) (Math : Math) = struct
      open T
      open G.Monad_infix

      module Q = Quickcheck.Configure (struct
        include Quickcheck

        let default_trial_count = 1_000
        let default_sizes = Sequence.cycle_list_exn (List.range 0 10 ~stop:`inclusive)

        let default_seed =
          `Deterministic
            (sprintf
               "%s values with %s operators."
               (String.capitalize T.module_name)
               (String.capitalize Math.module_name))
        ;;
      end)

      let%test_unit "duplicates" =
        Q.test_distinct_values
          (List.quickcheck_generator quickcheck_generator)
          ~trials:1_000
          ~distinct_values:500
          ~sexp_of:[%sexp_of: t list]
          ~compare:[%compare: t list]
      ;;

      let%test_unit "mem true" =
        Q.test
          ~sexp_of:[%sexp_of: t * t list]
          (List.gen_non_empty quickcheck_generator
           >>= fun list -> G.of_list list >>| fun elt_of_list -> elt_of_list, list)
          ~f:(fun (elt_of_list, list) ->
            [%test_result: bool] (List.mem list elt_of_list ~equal) ~expect:true)
      ;;

      let%test_unit "mem false" =
        Q.test
          ~sexp_of:[%sexp_of: t * t list]
          (quickcheck_generator
           >>= fun x ->
           let not_x_gen = G.filter quickcheck_generator ~f:(fun y -> y <> x) in
           List.quickcheck_generator not_x_gen >>| fun list_of_not_x -> x, list_of_not_x)
          ~f:(fun (x, list_of_not_x) ->
            [%test_result: bool] (List.mem list_of_not_x x ~equal) ~expect:false)
      ;;

      let%test_unit "len" =
        Q.test
          ~sexp_of:[%sexp_of: int * t list]
          (G.small_non_negative_int
           >>= fun len ->
           List.gen_with_length len quickcheck_generator >>| fun list -> len, list)
          ~f:(fun (len, list) -> [%test_result: int] (List.length list) ~expect:len)
      ;;

      let%test_unit "is_empty true" =
        Q.test ~sexp_of:[%sexp_of: t list] (G.singleton []) ~f:(fun empty ->
          [%test_result: bool] (List.is_empty empty) ~expect:true)
      ;;

      let%test_unit "is_empty false" =
        Q.test
          ~sexp_of:[%sexp_of: t list]
          (G.tuple2 quickcheck_generator (List.quickcheck_generator quickcheck_generator)
           >>| fun (x, list) -> x :: list)
          ~f:(fun non_empty ->
            [%test_result: bool] (List.is_empty non_empty) ~expect:false)
      ;;

      let%test_unit "iter" =
        Q.test
          ~sexp_of:[%sexp_of: t list]
          (List.quickcheck_generator quickcheck_generator)
          ~f:(fun list ->
          let q = Queue.create () in
          List.iter list ~f:(Queue.enqueue q);
          [%test_result: t list] (Queue.to_list q) ~expect:list)
      ;;

      let%test_unit "sum vs fold" =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> Math.t)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.fn quickcheck_observer Math.quickcheck_generator))
          ~f:(fun (list, f) ->
            [%test_eq: Math.t]
              (List.fold list ~init:Math.zero ~f:(fun m x -> Math.( + ) m (f x)))
              (List.sum (module Math) list ~f))
      ;;

      let%test_unit "for_all vs exists" =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> bool)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.fn quickcheck_observer Bool.quickcheck_generator))
          ~f:(fun (list, f) ->
            [%test_eq: bool] (List.for_all list ~f) (not (List.exists list ~f:(Fn.non f))))
      ;;

      let%test_unit "exists vs mem" =
        Q.test
          ~sexp_of:[%sexp_of: t * t list]
          (G.tuple2 quickcheck_generator (List.quickcheck_generator quickcheck_generator))
          ~f:(fun (x, list) ->
            [%test_eq: bool]
              (List.exists list ~f:(fun y -> equal x y))
              (List.mem list x ~equal))
      ;;

      let%test_unit "exists vs find" =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> bool)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.fn quickcheck_observer Bool.quickcheck_generator))
          ~f:(fun (list, f) ->
            [%test_eq: bool] (List.exists list ~f) (Option.is_some (List.find list ~f)))
      ;;

      let%test_unit "count vs length/filter" =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> bool)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.fn quickcheck_observer Bool.quickcheck_generator))
          ~f:(fun (list, f) ->
            [%test_eq: int] (List.count list ~f) (List.length (List.filter list ~f)))
      ;;

      let%test_unit "find vs find_map" =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> bool)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.fn quickcheck_observer Bool.quickcheck_generator))
          ~f:(fun (list, f) ->
            [%test_eq: t option]
              (List.find list ~f)
              (List.find_map list ~f:(fun x -> if f x then Some x else None)))
      ;;

      let%test_unit "to_list" =
        Q.test
          ~sexp_of:[%sexp_of: t list]
          (List.quickcheck_generator quickcheck_generator)
          ~f:(fun list -> [%test_result: t list] (List.to_list list) ~expect:list)
      ;;

      let%test_unit "to_array + Array.to_list" =
        Q.test
          ~sexp_of:[%sexp_of: t list]
          (List.quickcheck_generator quickcheck_generator)
          ~f:(fun list ->
          [%test_result: t list] (Array.to_list (List.to_array list)) ~expect:list)
      ;;

      let%test_unit "max_elt vs min_elt" =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> t -> int)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.compare_fn quickcheck_observer))
          ~f:(fun (list, cmp) ->
            [%test_eq: t option]
              (List.min_elt list ~compare:cmp)
              (List.max_elt list ~compare:(fun x y -> cmp y x)))
      ;;

      let%test_unit "return" =
        Q.test ~sexp_of:[%sexp_of: t] quickcheck_generator ~f:(fun x ->
          [%test_result: t list] (List.return x) ~expect:[ x ])
      ;;

      let%test_unit "map vs bind" =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> t)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.fn quickcheck_observer quickcheck_generator))
          ~f:(fun (list, f) ->
            [%test_eq: t list] (List.map list ~f) (List.bind list ~f:(fun x -> [ f x ])))
      ;;

      let%test_unit "monad left identity" =
        Q.test
          ~sexp_of:[%sexp_of: t * (t -> t list)]
          (G.tuple2
             quickcheck_generator
             (G.fn quickcheck_observer (List.quickcheck_generator quickcheck_generator)))
          ~f:(fun (x, f) -> [%test_eq: t list] (List.bind (List.return x) ~f) (f x))
      ;;

      let%test_unit "monad right identity" =
        Q.test
          ~sexp_of:[%sexp_of: t list]
          (List.quickcheck_generator quickcheck_generator)
          ~f:(fun list ->
          [%test_result: t list] (List.bind list ~f:List.return) ~expect:list)
      ;;

      let%test_unit ("monad associativity" [@tags "no-js"]) =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> t list) * (t -> t list)]
          (G.tuple3
             (List.quickcheck_generator quickcheck_generator)
             (G.fn quickcheck_observer (List.quickcheck_generator quickcheck_generator))
             (G.fn quickcheck_observer (List.quickcheck_generator quickcheck_generator)))
          ~f:(fun (list, f, g) ->
            [%test_eq: t list]
              (List.bind (List.bind list ~f) ~f:g)
              (List.bind list ~f:(fun x -> List.bind (f x) ~f:g)))
      ;;

      let%test_unit "join" =
        Q.test
          ~sexp_of:[%sexp_of: t list list]
          (List.quickcheck_generator (List.quickcheck_generator quickcheck_generator))
          ~f:(fun list -> [%test_eq: t list] (List.join list) (List.bind list ~f:Fn.id))
      ;;

      let%test_unit "ignore" =
        Q.test
          ~sexp_of:[%sexp_of: t list]
          (List.quickcheck_generator quickcheck_generator)
          ~f:(fun list ->
          [%test_eq: unit list] (List.ignore_m list) (List.map list ~f:ignore))
      ;;

      let%test_unit "of_list + to_list" =
        Q.test
          ~sexp_of:[%sexp_of: t list]
          (List.quickcheck_generator quickcheck_generator)
          ~f:(fun list -> [%test_result: t list] (List.of_list list) ~expect:list)
      ;;

      let%test_unit "nth vs nth_exn" =
        Q.test
          ~sexp_of:[%sexp_of: int * t list]
          (G.tuple2
             G.small_non_negative_int
             (List.quickcheck_generator quickcheck_generator))
          ~f:(fun (i, list) ->
            [%test_eq: t option]
              (List.nth list i)
              (Option.try_with (fun () -> List.nth_exn list i)))
      ;;

      let%test_unit "init + nth_exn" =
        Q.test
          ~sexp_of:[%sexp_of: int * int * (int -> t)]
          (G.small_non_negative_int
           >>= fun size ->
           let size = size + 1 in
           G.tuple3
             (G.return size)
             (Int.gen_incl 0 (size - 1))
             (G.fn Int.quickcheck_observer quickcheck_generator))
          ~f:(fun (size, i, f) ->
            [%test_result: t] (List.nth_exn (List.init size ~f) i) ~expect:(f i))
      ;;

      let%test_unit "rev^2" =
        Q.test
          ~sexp_of:[%sexp_of: t list]
          (List.quickcheck_generator quickcheck_generator)
          ~f:(fun list -> [%test_result: t list] (List.rev (List.rev list)) ~expect:list)
      ;;

      let%test_unit "rev_append vs rev + append" =
        Q.test
          ~sexp_of:[%sexp_of: t list * t list]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (List.quickcheck_generator quickcheck_generator))
          ~f:(fun (list1, list2) ->
            [%test_eq: t list]
              (List.rev_append list1 list2)
              (List.append (List.rev list1) list2))
      ;;

      let%test_unit "unordered_append vs append" =
        Q.test
          ~sexp_of:[%sexp_of: t list * t list * t]
          (G.tuple3
             (List.quickcheck_generator quickcheck_generator)
             (List.quickcheck_generator quickcheck_generator)
             quickcheck_generator)
          ~f:(fun (list1, list2, x) ->
            [%test_eq: bool]
              (List.mem (List.append list1 list2) x ~equal)
              (List.mem (List.unordered_append list1 list2) x ~equal))
      ;;

      let%test_unit "rev_map vs map + rev" =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> t)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.fn quickcheck_observer quickcheck_generator))
          ~f:(fun (list, f) ->
            [%test_eq: t list] (List.rev_map list ~f) (List.rev (List.map list ~f)))
      ;;

      let%test_unit "fold vs fold_left" =
        Q.test
          ~sexp_of:[%sexp_of: t list * t * (t -> t -> t)]
          (G.tuple3
             (List.quickcheck_generator quickcheck_generator)
             quickcheck_generator
             (G.fn2 quickcheck_observer quickcheck_observer quickcheck_generator))
          ~f:(fun (list, init, f) ->
            [%test_eq: t] (List.fold list ~init ~f) (List.fold_left list ~init ~f))
      ;;

      let%test_unit "unzip + iter2_exn vs iter" =
        Q.test
          ~sexp_of:[%sexp_of: (t * t) list * (t -> t -> t)]
          (G.tuple2
             (List.quickcheck_generator
                (G.tuple2 quickcheck_generator quickcheck_generator))
             (G.fn2 quickcheck_observer quickcheck_observer quickcheck_generator))
          ~f:(fun (pair_list, f) ->
            [%test_eq: t list]
              (let q = Queue.create () in
               let list1, list2 = List.unzip pair_list in
               List.iter2_exn list1 list2 ~f:(fun x y -> Queue.enqueue q (f x y));
               Queue.to_list q)
              (let q = Queue.create () in
               List.iter pair_list ~f:(fun (x, y) -> Queue.enqueue q (f x y));
               Queue.to_list q))
      ;;

      let%test_unit "rev_map2_exn vs rev + map2_exn" =
        Q.test
          ~sexp_of:[%sexp_of: (t * t) list * (t -> t -> t)]
          (G.tuple2
             (List.quickcheck_generator
                (G.tuple2 quickcheck_generator quickcheck_generator))
             (G.fn2 quickcheck_observer quickcheck_observer quickcheck_generator))
          ~f:(fun (pair_list, f) ->
            let list1, list2 = List.unzip pair_list in
            [%test_eq: t list]
              (List.rev_map2_exn list1 list2 ~f)
              (List.rev (List.map2_exn list1 list2 ~f)))
      ;;

      let%test_unit "unzip + fold2_exn + fold" =
        Q.test
          ~sexp_of:[%sexp_of: (t * t) list * t * (t -> t -> t -> t)]
          (G.tuple3
             (List.quickcheck_generator
                (G.tuple2 quickcheck_generator quickcheck_generator))
             quickcheck_generator
             (G.fn3
                quickcheck_observer
                quickcheck_observer
                quickcheck_observer
                quickcheck_generator))
          ~f:(fun (pair_list, init, f) ->
            let list1, list2 = List.unzip pair_list in
            [%test_eq: t]
              (List.fold2_exn list1 list2 ~init ~f)
              (List.fold (List.zip_exn list1 list2) ~init ~f:(fun acc (x, y) -> f acc x y)))
      ;;

      let%test_unit "unzip + for_all2_exn vs for_all" =
        Q.test
          ~sexp_of:[%sexp_of: (t * t) list * (t -> t -> bool)]
          (G.tuple2
             (List.quickcheck_generator
                (G.tuple2 quickcheck_generator quickcheck_generator))
             (G.fn2 quickcheck_observer quickcheck_observer Bool.quickcheck_generator))
          ~f:(fun (pair_list, f) ->
            [%test_eq: bool]
              (let list1, list2 = List.unzip pair_list in
               List.for_all2_exn list1 list2 ~f)
              (List.for_all pair_list ~f:(fun (x, y) -> f x y)))
      ;;

      let%test_unit "unzip + exists2_exn vs exists" =
        Q.test
          ~sexp_of:[%sexp_of: (t * t) list * (t -> t -> bool)]
          (G.tuple2
             (List.quickcheck_generator
                (G.tuple2 quickcheck_generator quickcheck_generator))
             (G.fn2 quickcheck_observer quickcheck_observer Bool.quickcheck_generator))
          ~f:(fun (pair_list, f) ->
            [%test_eq: bool]
              (let list1, list2 = List.unzip pair_list in
               List.exists2_exn list1 list2 ~f)
              (List.exists pair_list ~f:(fun (x, y) -> f x y)))
      ;;

      let%test_unit "filter vs for_all" =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> bool)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.fn quickcheck_observer Bool.quickcheck_generator))
          ~f:(fun (list, f) ->
            [%test_result: bool] (List.for_all ~f (List.filter ~f list)) ~expect:true)
      ;;

      let%test_unit "filter true" =
        Q.test
          ~sexp_of:[%sexp_of: t list]
          (List.quickcheck_generator quickcheck_generator)
          ~f:(fun list ->
          [%test_result: t list] (List.filter list ~f:(const true)) ~expect:list)
      ;;

      let%test_unit "filter vs rev_filter" =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> bool)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.fn quickcheck_observer Bool.quickcheck_generator))
          ~f:(fun (list, f) ->
            [%test_eq: t list] (List.rev (List.filter list ~f)) (List.rev_filter list ~f))
      ;;

      let%test_unit "filteri vs filter" =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> bool)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.fn quickcheck_observer Bool.quickcheck_generator))
          ~f:(fun (list, f) ->
            [%test_eq: t list]
              (List.filter list ~f)
              (List.filteri list ~f:(fun _ x -> f x)))
      ;;

      let%test_unit "partition_map vs filter_map" =
        let partition_of_variant = function
          | `A a -> First a
          | `B b -> Second b
        in
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> (t, t) Either.t)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.fn
                quickcheck_observer
                (G.variant2 quickcheck_generator quickcheck_generator
                 >>| partition_of_variant)))
          ~f:(fun (list, f) ->
            [%test_eq: t list * t list]
              (List.partition_map list ~f)
              ( List.filter_map list ~f:(fun x ->
                  match f x with
                  | First x -> Some x
                  | Second _ -> None)
              , List.filter_map list ~f:(fun x ->
                  match f x with
                  | First _ -> None
                  | Second x -> Some x) ))
      ;;

      let%test_unit "partition3_map vs filter_map" =
        let partition_of_variant = function
          | `A a -> `Fst a
          | `B b -> `Snd b
          | `C c -> `Trd c
        in
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> [ `Fst of t | `Snd of t | `Trd of t ])]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.fn
                quickcheck_observer
                (G.variant3 quickcheck_generator quickcheck_generator quickcheck_generator
                 >>| partition_of_variant)))
          ~f:(fun (list, f) ->
            [%test_eq: t list * t list * t list]
              (List.partition3_map list ~f)
              ( List.filter_map list ~f:(fun x ->
                  match f x with
                  | `Fst x -> Some x
                  | `Snd _ -> None
                  | `Trd _ -> None)
              , List.filter_map list ~f:(fun x ->
                  match f x with
                  | `Fst _ -> None
                  | `Snd x -> Some x
                  | `Trd _ -> None)
              , List.filter_map list ~f:(fun x ->
                  match f x with
                  | `Fst _ -> None
                  | `Snd _ -> None
                  | `Trd x -> Some x) ))
      ;;

      let%test_unit "partition_tf vs partition_map" =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> bool)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.fn quickcheck_observer Bool.quickcheck_generator))
          ~f:(fun (list, f) ->
            [%test_eq: t list * t list]
              (List.partition_tf list ~f)
              (List.partition_map list ~f:(fun x -> if f x then First x else Second x)))
      ;;

      let%test_unit "append + split_n" =
        Q.test
          ~sexp_of:[%sexp_of: t list * t list]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (List.quickcheck_generator quickcheck_generator))
          ~f:(fun (list1, list2) ->
            [%test_result: t list * t list]
              (List.split_n (List.append list1 list2) (List.length list1))
              ~expect:(list1, list2))
      ;;

      let%test_unit "sort vs stable_sort" =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> t -> int)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.compare_fn quickcheck_observer))
          ~f:(fun (list, cmp) ->
            (* When comparing [t] using [cmp], [sort] and [stable_sort] should be
               indistinguishable. *)
            let compare = cmp in
            [%test_eq: t list]
              (List.sort list ~compare:cmp)
              (List.stable_sort list ~compare:cmp))
      ;;

      let%test_unit "stable_sort + merge vs append + stable_sort" =
        Q.test
          ~sexp_of:[%sexp_of: t list * t list * (t -> t -> int)]
          (G.tuple3
             (List.quickcheck_generator quickcheck_generator)
             (List.quickcheck_generator quickcheck_generator)
             (G.compare_fn quickcheck_observer))
          ~f:(fun (list1, list2, cmp) ->
            [%test_eq: t list]
              (List.merge
                 ~compare:cmp
                 (List.stable_sort ~compare:cmp list1)
                 (List.stable_sort ~compare:cmp list2))
              (List.stable_sort ~compare:cmp (List.append list1 list2)))
      ;;

      let%test_unit "hd vs hd_exn" =
        Q.test
          ~sexp_of:[%sexp_of: t list]
          (List.quickcheck_generator quickcheck_generator)
          ~f:(fun list ->
          [%test_eq: t option]
            (List.hd list)
            (Option.try_with (fun () -> List.hd_exn list)))
      ;;

      let%test_unit "tl vs tl_exn" =
        Q.test
          ~sexp_of:[%sexp_of: t list]
          (List.quickcheck_generator quickcheck_generator)
          ~f:(fun list ->
          [%test_eq: t list option]
            (List.tl list)
            (Option.try_with (fun () -> List.tl_exn list)))
      ;;

      let%test_unit "find vs find_exn" =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> bool)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.fn quickcheck_observer Bool.quickcheck_generator))
          ~f:(fun (list, f) ->
            [%test_eq: t option]
              (List.find list ~f)
              (Option.try_with (fun () -> List.find_exn list ~f)))
      ;;

      let%test_unit "find vs findi" =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> bool)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.fn quickcheck_observer Bool.quickcheck_generator))
          ~f:(fun (list, f) ->
            [%test_eq: t option]
              (List.find list ~f)
              (Option.map ~f:snd (List.findi list ~f:(fun _ x -> f x))))
      ;;

      let%test_unit "append + rev" =
        Q.test
          ~sexp_of:[%sexp_of: t list * t list]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (List.quickcheck_generator quickcheck_generator))
          ~f:(fun (list1, list2) ->
            [%test_eq: t list]
              (List.rev (List.append list1 list2))
              (List.append (List.rev list2) (List.rev list1)))
      ;;

      let%test_unit "append associativity" =
        Q.test
          ~sexp_of:[%sexp_of: t list * t list * t list]
          (G.tuple3
             (List.quickcheck_generator quickcheck_generator)
             (List.quickcheck_generator quickcheck_generator)
             (List.quickcheck_generator quickcheck_generator))
          ~f:(fun (list1, list2, list3) ->
            [%test_eq: t list]
              (List.append list1 (List.append list2 list3))
              (List.append (List.append list1 list2) list3))
      ;;

      let%test_unit "map + rev vs rev + map" =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> t)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.fn quickcheck_observer quickcheck_generator))
          ~f:(fun (list, f) ->
            [%test_eq: t list] (List.rev (List.map list ~f)) (List.map (List.rev list) ~f))
      ;;

      let%test_unit "map + append vs append + map" =
        Q.test
          ~sexp_of:[%sexp_of: t list * t list * (t -> t)]
          (G.tuple3
             (List.quickcheck_generator quickcheck_generator)
             (List.quickcheck_generator quickcheck_generator)
             (G.fn quickcheck_observer quickcheck_generator))
          ~f:(fun (list1, list2, f) ->
            [%test_eq: t list]
              (List.append (List.map list1 ~f) (List.map list2 ~f))
              (List.map (List.append list1 list2) ~f))
      ;;

      let%test_unit "map vs concat_map" =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> t)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.fn quickcheck_observer quickcheck_generator))
          ~f:(fun (list, f) ->
            [%test_eq: t list]
              (List.map list ~f)
              (List.concat_map list ~f:(fun x -> [ f x ])))
      ;;

      let%test_unit "concat_mapi vs concat_map" =
        Q.test
          ~sexp_of:[%sexp_of: t list * (t -> t list)]
          (G.tuple2
             (List.quickcheck_generator quickcheck_generator)
             (G.fn quickcheck_observer (List.quickcheck_generator quickcheck_generator)))
          ~f:(fun (list, f) ->
            [%test_eq: t list]
              (List.concat_map list ~f)
              (List.concat_mapi list ~f:(fun _ x -> f x)))
      ;;

      let%test_unit "unzip + map2_exn vs map" =
        Q.test
          ~sexp_of:[%sexp_of: (t * t) list * (t -> t -> t)]
          (G.tuple2
             (List.quickcheck_generator
                (G.tuple2 quickcheck_generator quickcheck_generator))
             (G.fn2 quickcheck_observer quickcheck_observer quickcheck_generator))
          ~f:(fun (pair_list, f) ->
            [%test_eq: t list]
              (let list1, list2 = List.unzip pair_list in
               List.map2_exn list1 list2 ~f)
              (List.map pair_list ~f:(fun (x, y) -> f x y)))
      ;;

      let%test_unit "unzip + map3_exn vs map" =
        Q.test
          ~sexp_of:[%sexp_of: (t * (t * t)) list * (t -> t -> t -> t)]
          (G.tuple2
             (List.quickcheck_generator
                (G.tuple2
                   quickcheck_generator
                   (G.tuple2 quickcheck_generator quickcheck_generator)))
             (G.fn3
                quickcheck_observer
                quickcheck_observer
                quickcheck_observer
                quickcheck_generator))
          ~f:(fun (triple_list, f) ->
            [%test_eq: t list]
              (let list1, pair_list = List.unzip triple_list in
               let list2, list3 = List.unzip pair_list in
               List.map3_exn list1 list2 list3 ~f)
              (List.map triple_list ~f:(fun (x, (y, z)) -> f x y z)))
      ;;

      let%test_unit "rev + map3_exn vs rev_map3_exn" =
        Q.test
          ~sexp_of:[%sexp_of: (t * (t * t)) list * (t -> t -> t -> t)]
          (G.tuple2
             (List.quickcheck_generator
                (G.tuple2
                   quickcheck_generator
                   (G.tuple2 quickcheck_generator quickcheck_generator)))
             (G.fn3
                quickcheck_observer
                quickcheck_observer
                quickcheck_observer
                quickcheck_generator))
          ~f:(fun (triple_list, f) ->
            let list1, pair_list = List.unzip triple_list in
            let list2, list3 = List.unzip pair_list in
            [%test_eq: t list]
              (List.rev_map3_exn list1 list2 list3 ~f)
              (List.rev (List.map3_exn list1 list2 list3 ~f)))
      ;;
    end

    module String' = struct
      include String

      let module_name = "String"
    end

    module Int' = struct
      include Int

      let module_name = "Int"
    end

    let%test_module "string w/ int" = (module Make (String') (Int'))

    let%test_unit "List.fold_right doesn't allocate on empty lists" =
      let initial_words = Gc.minor_words () in
      let (_ : int) = List.fold_right [] ~f:( + ) ~init:0 in
      let allocated = Gc.minor_words () - initial_words in
      [%test_result: int] allocated ~expect:0
    ;;
  end)
;;

let%test_module "compare does not allocate" =
  (module struct
    let test_compare_alloc a b =
      let minor_words = Gc.minor_words () in
      ignore (List.compare Int.compare a b : int);
      Int.equal minor_words (Gc.minor_words ())
    ;;

    let%test _ = test_compare_alloc [] []
    let%test _ = test_compare_alloc [] [ 0 ]
    let%test _ = test_compare_alloc [ 1 ] []
    let%test _ = test_compare_alloc [ 2 ] [ 2 ]
    let%test _ = test_compare_alloc [ 1; 2; 3 ] [ 4 ]
    let%test _ = test_compare_alloc [ 4 ] [ 1; 2; 3 ]
  end)
;;

let%test_module "equal does not allocate" =
  (module struct
    let test_equal_alloc a b =
      let minor_words = Gc.minor_words () in
      ignore (List.equal Int.equal a b : bool);
      Int.equal minor_words (Gc.minor_words ())
    ;;

    let%test _ = test_equal_alloc [] []
    let%test _ = test_equal_alloc [] [ 0 ]
    let%test _ = test_equal_alloc [ 1 ] []
    let%test _ = test_equal_alloc [ 2 ] [ 2 ]
    let%test _ = test_equal_alloc [ 1; 2; 3 ] [ 4 ]
    let%test _ = test_equal_alloc [ 4 ] [ 1; 2; 3 ]
  end)
;;
