open! Import
open Quickcheck_intf
open Base_quickcheck
module Float = Base.Float
module Int = Base.Int
module List = Base.List
module Option = Base.Option
module Set = Base.Set
module Sexp = Base.Sexp

module Polymorphic_types = struct
  type ('a, 'b) variant2 =
    [ `A of 'a
    | `B of 'b
    ]
  [@@deriving quickcheck]

  type ('a, 'b, 'c) variant3 =
    [ `A of 'a
    | `B of 'b
    | `C of 'c
    ]
  [@@deriving quickcheck]

  type ('a, 'b, 'c, 'd) variant4 =
    [ `A of 'a
    | `B of 'b
    | `C of 'c
    | `D of 'd
    ]
  [@@deriving quickcheck]

  type ('a, 'b, 'c, 'd, 'e) variant5 =
    [ `A of 'a
    | `B of 'b
    | `C of 'c
    | `D of 'd
    | `E of 'e
    ]
  [@@deriving quickcheck]

  type ('a, 'b, 'c, 'd, 'e, 'f) variant6 =
    [ `A of 'a
    | `B of 'b
    | `C of 'c
    | `D of 'd
    | `E of 'e
    | `F of 'f
    ]
  [@@deriving quickcheck]

  type ('a, 'b) tuple2 = 'a * 'b [@@deriving quickcheck]
  type ('a, 'b, 'c) tuple3 = 'a * 'b * 'c [@@deriving quickcheck]
  type ('a, 'b, 'c, 'd) tuple4 = 'a * 'b * 'c * 'd [@@deriving quickcheck]
  type ('a, 'b, 'c, 'd, 'e) tuple5 = 'a * 'b * 'c * 'd * 'e [@@deriving quickcheck]

  type ('a, 'b, 'c, 'd, 'e, 'f) tuple6 = 'a * 'b * 'c * 'd * 'e * 'f
  [@@deriving quickcheck]

  type (-'a, -'b, 'r) fn2 = 'a -> 'b -> 'r [@@deriving quickcheck]
  type (-'a, -'b, -'c, 'r) fn3 = 'a -> 'b -> 'c -> 'r [@@deriving quickcheck]
  type (-'a, -'b, -'c, -'d, 'r) fn4 = 'a -> 'b -> 'c -> 'd -> 'r [@@deriving quickcheck]

  type (-'a, -'b, -'c, -'d, -'e, 'r) fn5 = 'a -> 'b -> 'c -> 'd -> 'e -> 'r
  [@@deriving quickcheck]

  type (-'a, -'b, -'c, -'d, -'e, -'f, 'r) fn6 = 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'r
  [@@deriving quickcheck]
end

module Observer = struct
  include Observer

  let of_hash (type a) (module M : Deriving_hash with type t = a) =
    of_hash_fold M.hash_fold_t
  ;;

  let variant2 = Polymorphic_types.quickcheck_observer_variant2
  let variant3 = Polymorphic_types.quickcheck_observer_variant3
  let variant4 = Polymorphic_types.quickcheck_observer_variant4
  let variant5 = Polymorphic_types.quickcheck_observer_variant5
  let variant6 = Polymorphic_types.quickcheck_observer_variant6
  let tuple2 = Polymorphic_types.quickcheck_observer_tuple2
  let tuple3 = Polymorphic_types.quickcheck_observer_tuple3
  let tuple4 = Polymorphic_types.quickcheck_observer_tuple4
  let tuple5 = Polymorphic_types.quickcheck_observer_tuple5
  let tuple6 = Polymorphic_types.quickcheck_observer_tuple6
  let of_predicate a b ~f = unmap (variant2 a b) ~f:(fun x -> if f x then `A x else `B x)
  let singleton () = opaque
  let doubleton f = of_predicate (singleton ()) (singleton ()) ~f
  let enum _ ~f = unmap int ~f

  let of_list list ~equal =
    let f x =
      match List.findi list ~f:(fun _ y -> equal x y) with
      | None -> failwith "Quickcheck.Observer.of_list: value not found"
      | Some (i, _) -> i
    in
    enum (List.length list) ~f
  ;;

  let of_fun f = create (fun x ~size ~hash -> observe (f ()) x ~size ~hash)

  let comparison ~compare ~eq ~lt ~gt =
    unmap
      (variant3 lt (singleton ()) gt)
      ~f:(fun x ->
        let c = compare x eq in
        if c < 0 then `A x else if c > 0 then `C x else `B x)
  ;;
end

module Generator = struct
  include Generator
  open Let_syntax

  let singleton = return

  let doubleton x y =
    create (fun ~size:_ ~random -> if Splittable_random.bool random then x else y)
  ;;

  let of_fun f = create (fun ~size ~random -> generate (f ()) ~size ~random)

  let of_sequence ~p seq =
    if Float.( <= ) p 0. || Float.( > ) p 1.
    then
      failwith (Printf.sprintf "Generator.of_sequence: probability [%f] out of bounds" p);
    Sequence.delayed_fold
      seq
      ~init:()
      ~finish:(fun () -> failwith "Generator.of_sequence: ran out of values")
      ~f:(fun () x ~k -> weighted_union [ p, singleton x; 1. -. p, of_fun k ])
  ;;

  let geometric = Generator.int_geometric
  let small_non_negative_int = small_positive_or_zero_int
  let small_positive_int = small_strictly_positive_int
  let list_with_length length t = list_with_length t ~length
  let variant2 = Polymorphic_types.quickcheck_generator_variant2
  let variant3 = Polymorphic_types.quickcheck_generator_variant3
  let variant4 = Polymorphic_types.quickcheck_generator_variant4
  let variant5 = Polymorphic_types.quickcheck_generator_variant5
  let variant6 = Polymorphic_types.quickcheck_generator_variant6
  let tuple2 = Polymorphic_types.quickcheck_generator_tuple2
  let tuple3 = Polymorphic_types.quickcheck_generator_tuple3
  let tuple4 = Polymorphic_types.quickcheck_generator_tuple4
  let tuple5 = Polymorphic_types.quickcheck_generator_tuple5
  let tuple6 = Polymorphic_types.quickcheck_generator_tuple6
  let fn2 = Polymorphic_types.quickcheck_generator_fn2
  let fn3 = Polymorphic_types.quickcheck_generator_fn3
  let fn4 = Polymorphic_types.quickcheck_generator_fn4
  let fn5 = Polymorphic_types.quickcheck_generator_fn5
  let fn6 = Polymorphic_types.quickcheck_generator_fn6

  let compare_fn dom =
    fn dom int >>| fun get_index x y -> [%compare: int] (get_index x) (get_index y)
  ;;

  let equal_fn dom = compare_fn dom >>| fun cmp x y -> Int.( = ) (cmp x y) 0
end

module Shrinker = struct
  include Shrinker

  let empty () = atomic
  let variant2 = Polymorphic_types.quickcheck_shrinker_variant2
  let variant3 = Polymorphic_types.quickcheck_shrinker_variant3
  let variant4 = Polymorphic_types.quickcheck_shrinker_variant4
  let variant5 = Polymorphic_types.quickcheck_shrinker_variant5
  let variant6 = Polymorphic_types.quickcheck_shrinker_variant6
  let tuple2 = Polymorphic_types.quickcheck_shrinker_tuple2
  let tuple3 = Polymorphic_types.quickcheck_shrinker_tuple3
  let tuple4 = Polymorphic_types.quickcheck_shrinker_tuple4
  let tuple5 = Polymorphic_types.quickcheck_shrinker_tuple5
  let tuple6 = Polymorphic_types.quickcheck_shrinker_tuple6
end

module Let_syntax = struct
  module Let_syntax = struct
    include Generator
    module Open_on_rhs = Generator
  end

  include Generator.Monad_infix

  let return = Generator.return
end

module Configure (Config : Quickcheck_config) = struct
  include Config

  let nondeterministic_state = lazy (Random.State.make_self_init ())

  let random_state_of_seed seed =
    match seed with
    | `Nondeterministic -> Splittable_random.create (force nondeterministic_state)
    | `Deterministic str -> Splittable_random.of_int ([%hash: string] str)
  ;;

  let make_seed seed : Test.Config.Seed.t =
    match seed with
    | `Nondeterministic -> Nondeterministic
    | `Deterministic string -> Deterministic string
  ;;

  let make_shrink_count = function
    | `Exhaustive -> Int.max_value
    | `Limit n -> n
  ;;

  let make_config ~seed ~sizes ~trials ~shrink_attempts : Test.Config.t =
    { seed = make_seed (Option.value seed ~default:default_seed)
    ; sizes = Option.value sizes ~default:default_sizes
    ; test_count = Option.value trials ~default:default_trial_count
    ; shrink_count =
        make_shrink_count (Option.value shrink_attempts ~default:default_shrink_attempts)
    }
  ;;

  let make_test_m (type a) ~gen ~shrinker ~sexp_of : (module Test.S with type t = a) =
    let module M = struct
      type t = a

      let quickcheck_generator = gen
      let quickcheck_shrinker = Option.value shrinker ~default:Shrinker.atomic
      let sexp_of_t = Option.value sexp_of ~default:[%sexp_of: _]
    end
    in
    (module M)
  ;;

  let random_value ?(seed = default_seed) ?(size = 30) gen =
    let random = random_state_of_seed seed in
    Generator.generate gen ~size ~random
  ;;

  let random_sequence ?seed ?sizes gen =
    let config =
      make_config ~seed ~sizes ~trials:(Some Int.max_value) ~shrink_attempts:None
    in
    let return = ref Sequence.empty in
    Test.with_sample_exn ~config gen ~f:(fun sequence -> return := sequence);
    !return
  ;;

  let iter ?seed ?sizes ?trials gen ~f =
    let config = make_config ~seed ~sizes ~trials ~shrink_attempts:None in
    Test.with_sample_exn ~config gen ~f:(fun sequence -> Sequence.iter sequence ~f)
  ;;

  let test ?seed ?sizes ?trials ?shrinker ?shrink_attempts ?sexp_of ?examples gen ~f =
    let config = make_config ~seed ~sizes ~trials ~shrink_attempts in
    let test_m = make_test_m ~gen ~shrinker ~sexp_of in
    Test.run_exn ~config ?examples ~f test_m
  ;;

  let test_or_error
    ?seed
    ?sizes
    ?trials
    ?shrinker
    ?shrink_attempts
    ?sexp_of
    ?examples
    gen
    ~f
    =
    let config = make_config ~seed ~sizes ~trials ~shrink_attempts in
    let test_m = make_test_m ~gen ~shrinker ~sexp_of in
    Test.run ~config ?examples ~f test_m
  ;;

  let test_distinct_values
    (type key)
    ?seed
    ?sizes
    ?sexp_of
    gen
    ~trials
    ~distinct_values
    ~compare
    =
    let module M = struct
      type t = key

      let (compare : t -> t -> int) = compare

      let sexp_of_t =
        match sexp_of with
        | Some sexp_of -> sexp_of
        | None -> sexp_of_opaque
      ;;

      include (val Comparator.make ~compare ~sexp_of_t)
    end
    in
    let fail set =
      let expect_count = distinct_values in
      let actual_count = Set.length set in
      let values =
        match sexp_of with
        | None -> None
        | Some sexp_of_elt -> Some [%sexp (Set.to_list set : elt list)]
      in
      raise_s
        [%message
          "insufficient distinct values"
            (trials : int)
            (expect_count : int)
            (actual_count : int)
            (values : (Sexp.t option[@sexp.option]))]
    in
    with_return (fun r ->
      let set = ref (Set.empty (module M)) in
      iter ?seed ?sizes ~trials gen ~f:(fun elt ->
        set := Set.add !set elt;
        if Set.length !set >= distinct_values then r.return ());
      fail !set)
  ;;

  let test_can_generate
    ?seed
    ?sizes
    ?(trials = default_can_generate_trial_count)
    ?sexp_of
    gen
    ~f
    =
    let r = ref [] in
    let f_and_enqueue return x = if f x then return `Can_generate else r := x :: !r in
    match
      With_return.with_return (fun return ->
        iter ?seed ?sizes ~trials gen ~f:(f_and_enqueue return.return);
        `Cannot_generate)
    with
    | `Can_generate -> ()
    | `Cannot_generate ->
      (match sexp_of with
       | None -> failwith "cannot generate"
       | Some sexp_of_value ->
         Error.raise_s [%message "cannot generate" ~attempts:(!r : value list)])
  ;;
end

include Configure (struct
  let default_seed = `Deterministic "an arbitrary but deterministic string"

  let default_trial_count =
    match Word_size.word_size with
    | W64 -> 10_000
    | W32 -> 1_000
  ;;

  let default_can_generate_trial_count = 10_000
  let default_shrink_attempts = `Limit 1000
  let default_sizes = Sequence.cycle_list_exn (List.range 0 30 ~stop:`inclusive)
end)

module type S = S
module type S1 = S1
module type S2 = S2
module type S_int = S_int
module type S_range = S_range

type nonrec seed = seed
type nonrec shrink_attempts = shrink_attempts

module type Quickcheck_config = Quickcheck_config
module type Quickcheck_configured = Quickcheck_configured
