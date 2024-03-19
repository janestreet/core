open! Core
open Poly
open! Expect_test_helpers_core
open! Quickcheck

let%expect_test ("Quickcheck.Let_syntax" [@tags "64-bits-only"]) =
  let open Quickcheck.Let_syntax in
  let quickcheck_generator =
    [%map_open
      let triple =
        tuple3
          Bool.quickcheck_generator
          Char.quickcheck_generator
          Float.quickcheck_generator
      and choice = variant2 String.quickcheck_generator Int.quickcheck_generator in
      [%sexp (triple : bool * char * float), (choice : [ `A of string | `B of int ])]]
  in
  Quickcheck.iter quickcheck_generator ~trials:10 ~f:print_s;
  [%expect
    {|
    ((false r -3.950862943457765E-284) (A ""))
    ((true R -3.3813765048980713) (B 15_568_213_580))
    ((false C -624423578.84277344) (B 24_202_329_494_286))
    ((false Y -1.0281888693571091) (A ""))
    ((true "\206" 1.1222031873105373E-250) (A cnU))
    ((false 8 -1.43279037293961E-322) (B -115_610_275))
    ((true B 14980.765357503886) (A "tq\210H7LQ"))
    ((false 4 -4.94065645841247E-324) (A ""))
    ((false X -287262765.50439453) (A "yP\000xv\134uV\197"))
    ((true q -8.6367588215150434E-19) (A A))
    |}]
;;

let%expect_test "ppx_quickcheck" =
  let module M = struct
    (* include some top-level type names like [int] and some module-exported type names
       like [Option.t] *)
    type t =
      | A
      | B of int list * Unit.t Option.t
      | C of { x : float }
      | D of int Map.M(String).t
      | E of Set.M(String).t
    [@@deriving compare, quickcheck, sexp_of]
  end
  in
  List.iter
    ~f:(fun predicate -> Quickcheck.test_can_generate M.quickcheck_generator ~f:predicate)
    [ (function
       | M.A -> true
       | _ -> false)
    ; (function
       | M.B _ -> true
       | _ -> false)
    ; (function
       | M.C _ -> true
       | _ -> false)
    ; (function
       | M.D _ -> true
       | _ -> false)
    ; (function
       | M.E _ -> true
       | _ -> false)
    ];
  [%expect {| |}]
;;

module Test (S : sig
  val default_seed : Quickcheck.seed
end) : sig end = struct
  let int_middle_bits =
    match Word_size.word_size with
    | W64 -> Int.of_string "0x0000_ffff_ffff_0000"
    | W32 -> Int.of_string "0x00ff_ff00"
  ;;

  module Q = Quickcheck.Configure (struct
    include Quickcheck
    include S
  end)

  open Q
  module G = Quickcheck.Generator
  module O = Quickcheck.Observer

  let memo k =
    let hashable = Hashtbl_intf.Hashable.of_key k in
    let memoize f = Memo.general f ~hashable in
    fun quickcheck_generator -> G.map quickcheck_generator ~f:memoize
  ;;

  let%test_module "examples" =
    (module struct
      let example = "some silly string that is unlikely to be generated randomly"

      let example_occurs ~examples =
        let occurs = ref false in
        test String.quickcheck_generator ~examples ~f:(fun str ->
          if String.equal str example then occurs := true);
        !occurs
      ;;

      let%test_unit _ = [%test_result: bool] (example_occurs ~examples:[]) ~expect:false

      let%test_unit _ =
        [%test_result: bool] (example_occurs ~examples:[ example ]) ~expect:true
      ;;
    end)
  ;;

  let%test_module "duplicates" =
    (module struct
      let quickcheck_generator = G.map Int.quickcheck_generator ~f:ignore
      let sexp_of = Unit.sexp_of_t
      let compare = Unit.compare

      let%test_unit _ =
        assert (
          Exn.does_raise (fun () ->
            test_distinct_values
              quickcheck_generator
              ~sexp_of
              ~compare
              ~trials:1_000
              ~distinct_values:2))
      ;;

      let%test_unit _ =
        test_distinct_values
          quickcheck_generator
          ~sexp_of
          ~compare
          ~trials:1_000
          ~distinct_values:1
      ;;
    end)
  ;;

  let%test_module "unit" =
    (module struct
      let sexp_of = Unit.sexp_of_t
      let quickcheck_generator = Unit.quickcheck_generator
      let can_generate f = test_can_generate quickcheck_generator ~sexp_of ~f
      let%test_unit _ = can_generate (fun () -> true)
    end)
  ;;

  let%test_module "bool" =
    (module struct
      let sexp_of = Bool.sexp_of_t
      let quickcheck_generator = Bool.quickcheck_generator
      let can_generate f = test_can_generate quickcheck_generator ~sexp_of ~f
      let%test_unit _ = can_generate (fun x -> x = true)
      let%test_unit _ = can_generate (fun x -> x = false)
    end)
  ;;

  let%test_module "int" =
    (module struct
      let sexp_of = Int.Hex.sexp_of_t
      let compare = Int.compare
      let quickcheck_generator = Int.quickcheck_generator
      let can_generate f = test_can_generate quickcheck_generator ~sexp_of ~f

      let%test_unit _ =
        test_distinct_values
          quickcheck_generator
          ~sexp_of
          ~compare
          ~trials:1_000
          ~distinct_values:500
      ;;

      let%test_unit _ = can_generate (fun x -> Int.popcount x < Int.num_bits / 2)
      let%test_unit _ = can_generate (fun x -> Int.popcount x > Int.num_bits / 2)

      let%test_unit _ =
        for i = 0 to Int.num_bits - 1 do
          can_generate (fun x -> x land (1 lsl i) = 0)
        done
      ;;

      let%test_unit _ =
        for i = 0 to Int.num_bits - 1 do
          can_generate (fun x -> x land (1 lsl i) <> 0)
        done
      ;;
    end)
  ;;

  let%test_module "float" =
    (module struct
      let bits_compare x y = Int64.compare (Int64.bits_of_float x) (Int64.bits_of_float y)
      let bits_equal x y = bits_compare x y = 0
      let sexp_of = Float.sexp_of_t
      let compare = bits_compare
      let quickcheck_generator = Float.quickcheck_generator
      let can_generate f = test_can_generate quickcheck_generator ~sexp_of ~f

      let has_class x c =
        match Float.classify x, (c : Float.Class.t) with
        | Infinite, Infinite
        | Nan, Nan
        | Normal, Normal
        | Subnormal, Subnormal
        | Zero, Zero -> true
        | _ -> false
      ;;

      let%test_unit _ =
        test_distinct_values
          quickcheck_generator
          ~sexp_of
          ~compare
          ~trials:1_000
          ~distinct_values:500
      ;;

      let%test_unit _ = can_generate (fun x -> has_class x Infinite)
      let%test_unit _ = can_generate (fun x -> has_class x Nan)
      let%test_unit _ = can_generate (fun x -> has_class x Normal)
      let%test_unit _ = can_generate (fun x -> has_class x Subnormal)
      let%test_unit _ = can_generate (fun x -> has_class x Zero)
      let%test_unit _ = can_generate (fun x -> Float.( < ) x 0.)
      let%test_unit _ = can_generate (fun x -> Float.( > ) x 0.)
      let%test_unit _ = can_generate (fun x -> Float.( = ) x 0. && bits_equal x 0.)
      let%test_unit _ = can_generate (fun x -> Float.( = ) x 0. && not (bits_equal x 0.))
      let%test_unit _ = can_generate (fun x -> Float.( = ) x Float.neg_infinity)

      let%test_unit _ =
        test_can_generate ~sexp_of Float.gen_without_nan ~f:(fun f ->
          Float.equal f Float.infinity)
      ;;

      let%test_unit _ =
        test_can_generate ~sexp_of Float.gen_without_nan ~f:(fun f ->
          Float.equal f Float.neg_infinity)
      ;;

      let%test_unit _ =
        test_can_generate ~sexp_of Float.gen_without_nan ~f:(fun f -> Float.is_finite f)
      ;;

      let%test_unit _ =
        test ~sexp_of Float.gen_without_nan ~f:(fun f -> assert (not (Float.is_nan f)))
      ;;

      let%test_unit _ =
        test_can_generate Float.gen_finite ~f:(fun f -> Float.equal (f +. 1.0) f)
      ;;

      let%test_unit _ =
        test_can_generate Float.gen_finite ~f:(fun f ->
          (not (Float.equal f 0.)) && Float.equal (f +. 1.0) 1.0)
      ;;

      let%test_unit _ =
        test ~sexp_of Float.gen_finite ~f:(fun f -> assert (not (Float.is_nan f)))
      ;;

      let%test_unit _ =
        test ~sexp_of Float.gen_finite ~f:(fun f -> assert (not (Float.is_inf f)))
      ;;

      let%test_unit _ = test ~sexp_of Float.gen_positive ~f:(fun f -> assert (f > 0.))

      let%test_unit _ =
        test_can_generate ~sexp_of Float.gen_positive ~f:(fun x -> has_class x Subnormal)
      ;;

      let%test_unit _ =
        test_can_generate ~sexp_of Float.gen_positive ~f:(fun x -> has_class x Normal)
      ;;

      let%test_unit _ = test ~sexp_of Float.gen_negative ~f:(fun f -> assert (f < 0.))

      let%test_unit _ =
        test_can_generate ~sexp_of Float.gen_negative ~f:(fun x -> has_class x Subnormal)
      ;;

      let%test_unit _ =
        test_can_generate ~sexp_of Float.gen_negative ~f:(fun x -> has_class x Normal)
      ;;

      let%test_unit _ =
        test ~sexp_of (Float.gen_uniform_excl (-1.) 1.) ~f:(fun f ->
          assert (f > -1. && f < 1.))
      ;;

      let%test_unit _ =
        test_can_generate ~sexp_of (Float.gen_uniform_excl (-1.) 1.) ~f:(fun f -> f < 0.)
      ;;

      let%test_unit _ =
        test_can_generate ~sexp_of (Float.gen_uniform_excl (-1.) 1.) ~f:(fun f -> f > 0.)
      ;;
    end)
  ;;

  let%test_module "string" =
    (module struct
      let sexp_of = String.sexp_of_t
      let compare = String.compare
      let quickcheck_generator = String.quickcheck_generator
      let can_generate f = test_can_generate quickcheck_generator ~sexp_of ~f

      let%test_unit _ =
        test_distinct_values
          quickcheck_generator
          ~sexp_of
          ~compare
          ~trials:1_000
          ~distinct_values:500
      ;;

      let%test_unit _ = can_generate (fun x -> String.length x = 0)
      let%test_unit _ = can_generate (fun x -> String.length x = 1)
      let%test_unit _ = can_generate (fun x -> String.length x = 2)
      let%test_unit _ = can_generate (fun x -> String.length x > 2)
      let%test_unit _ = can_generate (fun x -> String.uppercase x <> x)
      let%test_unit _ = can_generate (fun x -> String.lowercase x <> x)

      let%test_unit _ =
        can_generate (fun x ->
          match Int.of_string x with
          | _ -> true
          | exception _ -> false)
      ;;
    end)
  ;;

  let%test_module "char" =
    (module struct
      let sexp_of = Char.sexp_of_t
      let quickcheck_generator = Char.quickcheck_generator
      let can_generate f = test_can_generate quickcheck_generator ~sexp_of ~f
      let%test_unit _ = can_generate Char.is_digit
      let%test_unit _ = can_generate Char.is_lowercase
      let%test_unit _ = can_generate Char.is_uppercase
      let%test_unit _ = can_generate Char.is_print
      let%test_unit _ = can_generate Char.is_whitespace

      let%test_unit _ =
        can_generate (fun c ->
          (not (Char.is_digit c))
          && (not (Char.is_lowercase c))
          && (not (Char.is_uppercase c))
          && (not (Char.is_print c))
          && not (Char.is_whitespace c))
      ;;

      let test_coverage quickcheck_generator ~f =
        let all = Char.Set.of_list Char.all in
        (* repeat to make sure changing random seed doesn't affect the outcome *)
        for _ = 1 to 10 do
          let expect = Set.filter all ~f in
          let actual =
            let set = ref Char.Set.empty in
            with_return (fun return ->
              Sequence.iter (Quickcheck.random_sequence quickcheck_generator) ~f:(fun t ->
                set := Set.add !set t;
                if Set.equal !set expect then return.return ()));
            !set
          in
          [%test_result: Char.Set.t] actual ~expect
        done
      ;;

      (* exported generators: *)
      let%test_unit "default" = test_coverage Char.quickcheck_generator ~f:(fun _ -> true)
      let%test_unit "digit" = test_coverage Char.gen_digit ~f:Char.is_digit
      let%test_unit "lowercase" = test_coverage Char.gen_lowercase ~f:Char.is_lowercase
      let%test_unit "uppercase" = test_coverage Char.gen_uppercase ~f:Char.is_uppercase
      let%test_unit "alpha" = test_coverage Char.gen_alpha ~f:Char.is_alpha
      let%test_unit "alphanum" = test_coverage Char.gen_alphanum ~f:Char.is_alphanum
      let%test_unit "print" = test_coverage Char.gen_print ~f:Char.is_print
      let%test_unit "whitespace" = test_coverage Char.gen_whitespace ~f:Char.is_whitespace
    end)
  ;;

  let%test_module "tuple2" =
    (module struct
      let sexp_of = [%sexp_of: char * char]
      let compare = [%compare: char * char]

      let quickcheck_generator =
        G.tuple2 Char.quickcheck_generator Char.quickcheck_generator
      ;;

      let can_generate ?trials f =
        test_can_generate quickcheck_generator ~sexp_of ~f ?trials
      ;;

      let%test_unit _ =
        test_distinct_values
          quickcheck_generator
          ~sexp_of
          ~compare
          ~trials:1_000
          ~distinct_values:500
      ;;

      let%test_unit _ = can_generate (fun (x, y) -> Char.( = ) x y) ~trials:2_000
      let%test_unit _ = can_generate (fun (x, y) -> Char.( < ) x y)
      let%test_unit _ = can_generate (fun (x, y) -> Char.( > ) x y)
    end)
  ;;

  let%test_module "option" =
    (module struct
      let sexp_of = [%sexp_of: Int.Hex.t option]
      let compare = [%compare: int option]
      let quickcheck_generator = Option.quickcheck_generator Int.quickcheck_generator
      let can_generate f = test_can_generate quickcheck_generator ~sexp_of ~f

      let%test_unit _ =
        test_distinct_values
          (G.filter quickcheck_generator ~f:Option.is_some)
          ~sexp_of
          ~compare
          ~trials:1_000
          ~distinct_values:500
      ;;

      let%test_unit _ = can_generate Option.is_none
      let%test_unit _ = can_generate Option.is_some
    end)
  ;;

  let%test_module "function" =
    (module struct
      module F =
        Fn_for_testing.Make (Int) (Int)
          (struct
            let examples =
              [ Int.min_value
              ; Int.bit_not int_middle_bits
              ; -2
              ; -1
              ; 0
              ; 1
              ; 2
              ; int_middle_bits
              ; Int.max_value
              ]
            ;;
          end)

      let sexp_of = [%sexp_of: F.t]
      let compare = [%compare: F.t]

      let quickcheck_generator =
        (* memoizing these functions makes [test_no_duplicates] run much faster *)
        G.(fn Int.quickcheck_observer Int.quickcheck_generator) |> memo (module Int)
      ;;

      let can_generate f = test_can_generate quickcheck_generator ~sexp_of ~f

      let%test_unit _ =
        test_distinct_values
          quickcheck_generator
          ~sexp_of
          ~compare
          ~trials:1_000
          ~distinct_values:500
      ;;

      let%test_unit _ = can_generate (fun f -> f 0 < f (-1))
      let%test_unit _ = can_generate (fun f -> f 0 > f (-1))
      let%test_unit _ = can_generate (fun f -> f 1 < f 0)
      let%test_unit _ = can_generate (fun f -> f 1 > f 0)
      let%test_unit _ = can_generate (fun f -> f 2 < f 1)
      let%test_unit _ = can_generate (fun f -> f 2 > f 1)

      let%test_unit _ =
        can_generate (fun f -> f (-1) <> f 0 && f 0 <> f 1 && f 1 <> f (-1))
      ;;

      let%test_unit _ = can_generate (fun f -> f int_middle_bits <> f 0)
    end)
  ;;

  let%test_module "higher-order function" =
    (module struct
      (* [First_order] defines a flat representation for [int -> int] functions that has
         [sexp_of] and [compare], but which can map to functions and use [Observer.fn] on
         below. *)
      module First_order = struct
        type t =
          | Id
          | Neg
          | Abs
          | Succ
          | Pred
        [@@deriving sexp_of, compare, enumerate, hash]

        let apply = function
          | Id -> Fn.id
          | Neg -> Int.neg
          | Abs -> Int.abs
          | Succ -> Int.succ
          | Pred -> Int.pred
        ;;
      end

      module Higher_order =
        Fn_for_testing.Make (First_order) (Int)
          (struct
            let examples = First_order.all
          end)

      let sexp_of = [%sexp_of: Higher_order.t]
      let compare = [%compare: Higher_order.t]

      let quickcheck_generator =
        (* memoizing these functions makes [test_no_duplicates] run much faster *)
        G.(
          fn
            O.(
              fn Int.quickcheck_generator Int.quickcheck_observer
              |> unmap ~f:First_order.apply)
            Int.quickcheck_generator)
        |> memo (module First_order)
      ;;

      let can_generate f = test_can_generate quickcheck_generator ~f ~sexp_of

      let%test_unit _ =
        test_distinct_values
          quickcheck_generator
          ~sexp_of
          ~compare
          ~trials:1_000
          ~distinct_values:500
      ;;

      let%test_unit _ = can_generate (fun f -> f Succ = f Pred)
      let%test_unit _ = can_generate (fun f -> f Succ > f Pred)
      let%test_unit _ = can_generate (fun f -> f Succ < f Pred)
      let%test_unit _ = can_generate (fun f -> f Neg <> f Abs)
      let%test_unit _ = can_generate (fun f -> f Neg <> f Id)
      let%test_unit _ = can_generate (fun f -> f Abs <> f Id)

      let%test_unit _ =
        can_generate (fun f ->
          let x = f Id in
          let y = f Neg in
          let z = f Abs in
          x <> y && y <> z && z <> x)
      ;;
    end)
  ;;

  let%test_module "list" =
    (module struct
      let sexp_of = [%sexp_of: char list]
      let compare = [%compare: char list]
      let quickcheck_generator = List.quickcheck_generator Char.quickcheck_generator
      let can_generate f = test_can_generate quickcheck_generator ~sexp_of ~f

      let%test_unit _ =
        test_distinct_values
          quickcheck_generator
          ~sexp_of
          ~compare
          ~trials:1_000
          ~distinct_values:500
      ;;

      let%test_unit _ = can_generate List.is_empty

      let%test_unit _ =
        can_generate (function
          | [ _ ] -> true
          | _ -> false)
      ;;

      let%test_unit _ =
        can_generate (function
          | [ x; y ] -> Char.( < ) x y
          | _ -> false)
      ;;

      let%test_unit _ =
        can_generate (function
          | [ x; y ] -> Char.( > ) x y
          | _ -> false)
      ;;

      let%test_unit _ =
        can_generate (fun list ->
          List.length list > 2 && List.is_sorted_strictly list ~compare:Char.compare)
      ;;

      let%test_unit _ =
        can_generate (fun list ->
          List.length list > 2
          && List.is_sorted_strictly (List.rev list) ~compare:Char.compare)
      ;;
    end)
  ;;

  let%test_module "sexp" =
    (module struct
      let sexp_of = [%sexp_of: Sexp.t]
      let compare = [%compare: Sexp.t]
      let quickcheck_generator = Sexp.quickcheck_generator
      let can_generate f = test_can_generate quickcheck_generator ~sexp_of ~f

      let%test_unit _ =
        test_distinct_values
          quickcheck_generator
          ~sexp_of
          ~compare
          ~trials:1_000
          ~distinct_values:500
      ;;

      let%test_unit _ =
        can_generate (function
          | Sexp.Atom _ -> true
          | _ -> false)
      ;;

      let%test_unit _ =
        can_generate (function
          | Sexp.List _ -> true
          | _ -> false)
      ;;

      let%test_unit _ =
        can_generate (function
          | Sexp.Atom "" -> true
          | _ -> false)
      ;;

      let%test_unit _ =
        can_generate (function
          | Sexp.List [] -> true
          | _ -> false)
      ;;

      let%test_unit _ =
        can_generate (function
          | Sexp.List [ _ ] -> true
          | _ -> false)
      ;;

      let%test_unit _ =
        can_generate (function
          | Sexp.List [ _; _ ] -> true
          | _ -> false)
      ;;

      let%test_unit _ =
        can_generate (function
          | Sexp.List [ _; _; _ ] -> true
          | _ -> false)
      ;;

      let%test_unit _ =
        can_generate (function
          | Sexp.Atom _ -> false
          | Sexp.List list ->
            let is_atom = function
              | Sexp.Atom _ -> true
              | Sexp.List _ -> false
            in
            List.length list >= 2 && List.for_all list ~f:is_atom)
      ;;
    end)
  ;;

  let%test_module "function on recursive data" =
    (module struct
      module Bool_list = struct
        type t = bool list [@@deriving sexp_of, compare, hash]
      end

      module F =
        Fn_for_testing.Make (Bool_list) (Char)
          (struct
            let examples =
              [ []
              ; [ true ]
              ; [ false ]
              ; [ true; true ]
              ; [ true; false ]
              ; [ false; true ]
              ; [ false; false ]
              ; [ true; true; true ]
              ; [ true; true; false ]
              ; [ true; true; true; true ]
              ; [ true; true; true; false ]
              ]
            ;;
          end)

      let sexp_of = [%sexp_of: F.t]
      let compare = [%compare: F.t]

      let quickcheck_generator =
        (* memoizing these functions makes [test_no_duplicates] run much faster *)
        G.(
          fn (List.quickcheck_observer Bool.quickcheck_observer) Char.quickcheck_generator)
        |> memo (module Bool_list)
      ;;

      let can_generate f = test_can_generate quickcheck_generator ~sexp_of ~f

      let%test_unit _ =
        test_distinct_values
          quickcheck_generator
          ~sexp_of
          ~compare
          ~trials:1_000
          ~distinct_values:500
      ;;

      let%test_unit _ = can_generate (fun f -> f [] <> f [ true ])
      let%test_unit _ = can_generate (fun f -> f [] <> f [ false ])
      let%test_unit _ = can_generate (fun f -> f [ true ] <> f [ false ])
      let%test_unit _ = can_generate (fun f -> f [ true; true ] <> f [ true; false ])

      let%test_unit _ =
        can_generate (fun f -> f [ true; true; true ] <> f [ true; true; false ])
      ;;
    end)
  ;;

  let%test_module "deep recursion" =
    (module struct
      let test length =
        test
          ~trials:1
          (List.gen_with_length length Char.quickcheck_generator)
          ~f:(fun input -> [%test_result: int] (List.length input) ~expect:length)
      ;;

      let%test_unit "used to cause a stack overflow" = test 100_000
    end)
  ;;
end

let%test_module _ = (module Test (Quickcheck))

let%test_module _ =
  (module Test (struct
    let default_seed = `Deterministic "foo"
  end))
;;

let%test_module _ =
  (module Test (struct
    let default_seed = `Deterministic "bar"
  end))
;;

(* let%test_module _ = (module Test (struct let default_seed = `Deterministic "baz" end))
 * let%test_module _ = (module Test (struct let default_seed = `Deterministic "quux" end))
 * let%test_module _ = (module Test (struct let default_seed = `Deterministic "zanzibar" end))
 * let%test_module _ = (module Test (struct let default_seed = `Deterministic "lorem ipsum" end)) *)

module _ = struct
  open Shrinker

  module Test_data = struct
    let singleton equal min =
      create (fun v -> if equal min v then Sequence.empty else Sequence.singleton min)
    ;;

    let%test_module "singleton" =
      (module struct
        let t = singleton Poly.( = ) 42

        let%test_unit "singleton produces values" =
          let shrunk = shrink t 2 |> Sequence.to_list in
          let expect = [ 42 ] in
          [%test_result: int list] ~expect shrunk
        ;;

        let%test_unit "singleton doesn't produce the input" =
          let shrunk = shrink t 42 |> Sequence.to_list in
          let expect = [] in
          [%test_result: int list] ~expect shrunk
        ;;
      end)
    ;;

    let t0 = singleton Poly.( = ) 0
    let t1 = singleton Poly.( = ) 1
    let t2 = singleton Poly.( = ) 2
    let t3 = singleton Poly.( = ) 3
    let t4 = singleton Poly.( = ) 4
    let t5 = singleton Poly.( = ) 5
  end

  let%test_module "tuple shrinkers" =
    (module struct
      open Test_data

      let%test_unit "tuple2 shrinker" =
        let sort = List.sort ~compare:[%compare: int * int] in
        let expect = [ 0, 5; 5, 1 ] |> sort in
        let results = shrink (tuple2 t0 t1) (5, 5) |> Sequence.to_list |> sort in
        [%test_result: (int * int) list] ~expect results
      ;;

      let%test_unit "tuple3 shrinker" =
        let sort = List.sort ~compare:[%compare: int * int * int] in
        let expect = [ 0, 5, 5; 5, 1, 5; 5, 5, 2 ] |> sort in
        let results = shrink (tuple3 t0 t1 t2) (5, 5, 5) |> Sequence.to_list |> sort in
        [%test_result: (int * int * int) list] results ~expect
      ;;

      let%test_unit "tuple4 shrinker" =
        let sort = List.sort ~compare:[%compare: int * int * int * int] in
        let expect = [ 0, 5, 5, 5; 5, 1, 5, 5; 5, 5, 2, 5; 5, 5, 5, 3 ] |> sort in
        let results =
          shrink (tuple4 t0 t1 t2 t3) (5, 5, 5, 5) |> Sequence.to_list |> sort
        in
        [%test_result: (int * int * int * int) list] results ~expect
      ;;

      let%test_unit "tuple5 shrinker" =
        let sort = List.sort ~compare:[%compare: int * int * int * int * int] in
        let expect =
          [ 0, 5, 5, 5, 5; 5, 1, 5, 5, 5; 5, 5, 2, 5, 5; 5, 5, 5, 3, 5; 5, 5, 5, 5, 4 ]
          |> sort
        in
        let results =
          shrink (tuple5 t0 t1 t2 t3 t4) (5, 5, 5, 5, 5) |> Sequence.to_list |> sort
        in
        [%test_result: (int * int * int * int * int) list] results ~expect
      ;;

      let%test_unit "tuple6 shrinker" =
        let sort = List.sort ~compare:[%compare: int * int * int * int * int * int] in
        let expect =
          [ 0, 9, 9, 9, 9, 9
          ; 9, 1, 9, 9, 9, 9
          ; 9, 9, 2, 9, 9, 9
          ; 9, 9, 9, 3, 9, 9
          ; 9, 9, 9, 9, 4, 9
          ; 9, 9, 9, 9, 9, 5
          ]
          |> sort
        in
        let results =
          shrink (tuple6 t0 t1 t2 t3 t4 t5) (9, 9, 9, 9, 9, 9) |> Sequence.to_list |> sort
        in
        [%test_result: (int * int * int * int * int * int) list] results ~expect
      ;;
    end)
  ;;

  let%test_module "variant shrinkers" =
    (module struct
      open Test_data

      type var2 =
        [ `A of int
        | `B of int
        ]
      [@@deriving sexp, compare]

      type var3 =
        [ `A of int
        | `B of int
        | `C of int
        ]
      [@@deriving sexp, compare]

      type var4 =
        [ `A of int
        | `B of int
        | `C of int
        | `D of int
        ]
      [@@deriving sexp, compare]

      type var5 =
        [ `A of int
        | `B of int
        | `C of int
        | `D of int
        | `E of int
        ]
      [@@deriving sexp, compare]

      type var6 =
        [ `A of int
        | `B of int
        | `C of int
        | `D of int
        | `E of int
        | `F of int
        ]
      [@@deriving sexp, compare]

      let%test_unit "variant2 shrinker" =
        let t = variant2 t0 t1 in
        let shrunk_a = shrink t (`A 1) |> Sequence.to_list in
        [%test_result: var2 list] ~expect:[ `A 0 ] shrunk_a;
        let shrunk_b = shrink t (`B 0) |> Sequence.to_list in
        [%test_result: var2 list] ~expect:[ `B 1 ] shrunk_b
      ;;

      let%test_unit "variant3 shrinker" =
        let t = variant3 t0 t1 t2 in
        let shrunk_a = shrink t (`A 1) |> Sequence.to_list in
        [%test_result: var3 list] ~expect:[ `A 0 ] shrunk_a;
        let shrunk_b = shrink t (`B 0) |> Sequence.to_list in
        [%test_result: var3 list] ~expect:[ `B 1 ] shrunk_b;
        let shrunk_c = shrink t (`C 1) |> Sequence.to_list in
        [%test_result: var3 list] ~expect:[ `C 2 ] shrunk_c
      ;;

      let%test_unit "variant4 shrinker" =
        let t = variant4 t0 t1 t2 t3 in
        let shrunk_a = shrink t (`A 1) |> Sequence.to_list in
        [%test_result: var4 list] ~expect:[ `A 0 ] shrunk_a;
        let shrunk_b = shrink t (`B 0) |> Sequence.to_list in
        [%test_result: var4 list] ~expect:[ `B 1 ] shrunk_b;
        let shrunk_c = shrink t (`C 1) |> Sequence.to_list in
        [%test_result: var4 list] ~expect:[ `C 2 ] shrunk_c;
        let shrunk_d = shrink t (`D 1) |> Sequence.to_list in
        [%test_result: var4 list] ~expect:[ `D 3 ] shrunk_d
      ;;

      let%test_unit "variant5 shrinker" =
        let t = variant5 t0 t1 t2 t3 t4 in
        let shrunk_a = shrink t (`A 1) |> Sequence.to_list in
        [%test_result: var5 list] ~expect:[ `A 0 ] shrunk_a;
        let shrunk_b = shrink t (`B 0) |> Sequence.to_list in
        [%test_result: var5 list] ~expect:[ `B 1 ] shrunk_b;
        let shrunk_c = shrink t (`C 1) |> Sequence.to_list in
        [%test_result: var5 list] ~expect:[ `C 2 ] shrunk_c;
        let shrunk_d = shrink t (`D 1) |> Sequence.to_list in
        [%test_result: var5 list] ~expect:[ `D 3 ] shrunk_d;
        let shrunk_e = shrink t (`E 1) |> Sequence.to_list in
        [%test_result: var5 list] ~expect:[ `E 4 ] shrunk_e
      ;;

      let%test_unit "variant6 shrinker" =
        let t = variant6 t0 t1 t2 t3 t4 t5 in
        let shrunk_a = shrink t (`A 1) |> Sequence.to_list in
        [%test_result: var6 list] ~expect:[ `A 0 ] shrunk_a;
        let shrunk_b = shrink t (`B 0) |> Sequence.to_list in
        [%test_result: var6 list] ~expect:[ `B 1 ] shrunk_b;
        let shrunk_c = shrink t (`C 1) |> Sequence.to_list in
        [%test_result: var6 list] ~expect:[ `C 2 ] shrunk_c;
        let shrunk_d = shrink t (`D 1) |> Sequence.to_list in
        [%test_result: var6 list] ~expect:[ `D 3 ] shrunk_d;
        let shrunk_e = shrink t (`E 1) |> Sequence.to_list in
        [%test_result: var6 list] ~expect:[ `E 4 ] shrunk_e;
        let shrunk_f = shrink t (`F 1) |> Sequence.to_list in
        [%test_result: var6 list] ~expect:[ `F 5 ] shrunk_f
      ;;
    end)
  ;;

  let%test_module "list shrinkers" =
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

      let%test_unit "shrinker produces expected outputs" =
        let shrunk =
          Shrinker.shrink (List.quickcheck_shrinker t0) test_list
          |> Sequence.to_list
          |> List.sort ~compare:[%compare: int list]
        in
        [%test_result: int list list] ~expect shrunk
      ;;

      let rec recursive_list = 1 :: 5 :: recursive_list

      let%test_unit "shrinker on infinite lists produces values" =
        let shrunk = Shrinker.shrink (List.quickcheck_shrinker t0) recursive_list in
        let result_length = Sequence.take shrunk 5 |> Sequence.to_list |> List.length in
        [%test_result: int] ~expect:5 result_length
      ;;
    end)
  ;;
end
