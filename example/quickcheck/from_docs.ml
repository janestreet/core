open Core
open Quickcheck

module Initial_example = struct

  let%test_unit "fold_left vs fold_right" =
    Quickcheck.test
      (List.gen Int.gen)
      ~sexp_of:[%sexp_of: int list]
      ~f:(fun list ->
        [%test_eq: int]
          (List.fold_left  ~init:0 ~f:(+) list)
          (List.fold_right ~init:0 ~f:(+) list))

end

module Generator_examples = struct

  let (_ : _ Generator.t) =
    Generator.singleton "An arbitrary value."
  let (_ : _ Generator.t) =
    String.gen     (* any string, including weird strings like "\000" *)
  let (_ : _ Generator.t) =
    Int.gen        (* any int, from [min_value] to [max_value] *)
  let (_ : _ Generator.t) =
    Float.gen      (* any float, from [neg_infinity] to [infinity] plus [nan] *)
  let (_ : _ Generator.t) =
    Generator.small_non_negative_int
  let (_ : _ Generator.t) =
    Generator.small_positive_int
  let (_ : _ Generator.t) =
    Int.gen_incl 0 99
  let (_ : _ Generator.t) =
    Int.gen_uniform_incl 0 99
  let (_ : _ Generator.t) =
    Float.gen_incl 1. 100.
  let (_ : _ Generator.t) =
    Float.gen_finite
  let (_ : _ Generator.t) =
    Generator.tuple2 Int.gen Float.gen
  let (_ : _ Generator.t) =
    List.gen (Generator.tuple2 Int.gen Float.gen)
  let (_ : _ Generator.t) =
    List.gen_with_length 12 (Generator.tuple2 Int.gen Float.gen)
  let (_ : _ Generator.t) =
    Either.gen Int.gen Float.gen
  let (_ : _ Generator.t) =
    Option.gen String.gen
  let (_ : _ Generator.t) =
    Generator.map Char.gen ~f:Char.to_int
  let (_ : _ Generator.t) =
    Generator.filter Float.gen ~f:Float.is_finite (* use [filter] sparingly! *)
  let (_ : _ Generator.t) =
    Generator.fn Int.obs Bool.gen
  let (_ : _ Generator.t) =
    Generator.(union [ singleton (Ok ()) ; singleton (Or_error.error_string "fail") ])

  module Monadic = struct

    let (_ : _ Generator.t) =
      let open Generator in
      String.gen
      >>= fun str ->
      Int.gen_incl 0 (String.length str - 1)
      >>| fun i ->
      str, i, String.get str i

  end

  module Recursive = struct

    let (_ : _ Generator.t) =
      Generator.(fixed_point (fun self ->
        size >>= function
        | 0 -> String.gen    >>| fun atom -> Sexp.Atom atom
        | _ -> List.gen self >>| fun list -> Sexp.List list))

    let rec binary_subtree lower_bound upper_bound =
      let open Generator in
      if lower_bound > upper_bound
      then singleton `Leaf
      else
        union
          [ singleton `Leaf
          ; Int.gen_incl lower_bound upper_bound >>= fun key   ->
            binary_subtree  lower_bound (key - 1)   >>= fun left  ->
            binary_subtree  (key + 1)   upper_bound >>| fun right ->
            `Node (left, key, right)
          ]

    let _binary_tree : _ Generator.t =
      binary_subtree Int.min_value Int.max_value

    let rec powers_of_two_starting_from x =
      let open Generator in
      union
        [ singleton x
        ; of_fun (fun () -> powers_of_two_starting_from (x *. 2.))
        ]

    let _powers_of_two : _ Generator.t =
      powers_of_two_starting_from 1.

  end

end

module Observer_examples = struct

  let (_ : _ Observer.t) = Observer.singleton ()
  let (_ : _ Observer.t) = String.obs
  let (_ : _ Observer.t) = Int.obs
  let (_ : _ Observer.t) = Float.obs
  let (_ : _ Observer.t) = Observer.tuple2 Int.obs Float.obs
  let (_ : _ Observer.t) = List.obs (Observer.tuple2 Int.obs Float.obs)
  let (_ : _ Observer.t) = Either.obs Int.obs Float.obs
  let (_ : _ Observer.t) = Option.obs String.obs
  let (_ : _ Observer.t) = Observer.fn Int.gen Bool.obs
  let (_ : _ Observer.t) = Observer.unmap Char.obs ~f:Char.of_int_exn

end

module Example_1_functional = struct

  module Functional_stack : sig
    type 'a t [@@deriving sexp, compare]
    val empty : _ t
    val is_empty : _ t -> bool
    val push : 'a t -> 'a -> 'a t
    val top_exn : 'a t -> 'a
    val pop_exn : 'a t -> 'a t
  end = struct
    type 'a t = 'a list [@@deriving sexp, compare]
    let empty = []
    let is_empty = List.is_empty
    let push t x = x :: t
    let top_exn = function
      | [] -> failwith "empty stack"
      | x :: _ -> x
    let pop_exn = function
      | [] -> failwith "empty stack"
      | _ :: t -> t
  end

  let of_list list =
    List.fold list ~init:Functional_stack.empty ~f:Functional_stack.push

  let stack elt =
    Generator.map (List.gen elt) ~f:of_list

  open Functional_stack

  let%test_unit "push + is_empty" =
    Quickcheck.test (Generator.tuple2 Int.gen (stack Int.gen)) ~f:(fun (x, t) ->
      [%test_result: bool] (is_empty (push t x)) ~expect:false)

  let%test_unit "push + top_exn" =
    Quickcheck.test (Generator.tuple2 Int.gen (stack Int.gen)) ~f:(fun (x, t) ->
      [%test_result: int] (top_exn (push t x)) ~expect:x)

  let%test_unit "push + pop_exn" =
    Quickcheck.test (Generator.tuple2 Int.gen (stack Int.gen)) ~f:(fun (x, t) ->
      [%test_result: int t] (pop_exn (push t x)) ~expect:t)

end

module Example_2_imperative = struct

  module Imperative_stack : sig
    type 'a t [@@deriving sexp, compare]
    val create : unit -> _ t
    val is_empty : _ t -> bool
    val push : 'a t -> 'a -> unit
    val pop_exn : 'a t -> 'a
    val iter : 'a t -> f:('a -> unit) -> unit
    val to_list : 'a t -> 'a list
  end = struct
    type 'a t = 'a list ref [@@deriving sexp, compare]
    let create () = ref []
    let is_empty t = List.is_empty !t
    let push t x = t := x :: !t
    let pop_exn t =
      match !t with
      | [] -> failwith "empty stack"
      | x :: list -> t := list; x
    let to_list t = !t
    let iter t ~f = List.iter !t ~f
  end

  let stack elt =
    let open Generator in
    List.gen elt
    >>| fun xs ->
    let t = Imperative_stack.create () in
    List.iter xs ~f:(fun x -> Imperative_stack.push t x);
    t

  open Imperative_stack

  let%test_unit "push + is_empty" =
    Quickcheck.test (Generator.tuple2 String.gen (stack String.gen)) ~f:(fun (x, t) ->
      [%test_result: bool] (push t x; is_empty t) ~expect:false)

  let%test_unit "push + pop_exn" =
    Quickcheck.test (Generator.tuple2 String.gen (stack String.gen)) ~f:(fun (x, t) ->
      push t x;
      let y = pop_exn t in
      [%test_result: string] y ~expect:x)

  let%test_unit "push + to_list" =
    Quickcheck.test (Generator.tuple2 String.gen (stack String.gen)) ~f:(fun (x, t) ->
      let list1 = to_list t in
      push t x;
      let list2 = to_list t in
      [%test_result: string list] list2 ~expect:(x :: list1))

  let%test_unit "push + pop_exn + to_list" =
    Quickcheck.test (Generator.tuple2 String.gen (stack String.gen)) ~f:(fun (x, t) ->
      let list1 = to_list t in
      push t x;
      let _ = pop_exn t in
      let list2 = to_list t in
      [%test_result: string list] list2 ~expect:list1)

  let%test_unit "iter" =
    Quickcheck.test (stack String.gen) ~f:(fun t ->
      let q = Queue.create () in
      iter t ~f:(fun x -> Queue.enqueue q x);
      [%test_result: string list] (Queue.to_list q) ~expect:(to_list t))

end

module Example_3_asynchronous = struct

  open Async

  module Async_stack : sig
    type 'a t [@@deriving sexp, compare]
    val create   : unit -> _ t
    val is_empty : 'a t -> bool
    val push     : 'a t -> 'a -> unit Deferred.t (* pushback until stack empties *)
    val pop      : 'a t -> 'a Deferred.t         (* wait until element is available *)
    val iter     : 'a t -> f:('a -> unit Deferred.t) -> unit Deferred.t
    val to_list  : 'a t -> 'a list
  end = struct
    type 'a t =
      { mutable elts : 'a list
      ; mutable push : unit Ivar.t
      ; mutable pops : 'a Ivar.t list
      }
    let of_list elts =
      { elts
      ; push = if List.is_empty elts then Ivar.create_full () else Ivar.create ()
      ; pops = []
      }
    let to_list t = t.elts
    let sexp_of_t sexp_of_elt t = [%sexp_of: elt list] (to_list t)
    let t_of_sexp elt_of_sexp sexp = of_list ([%of_sexp: elt list] sexp)
    let compare (type elt) compare_elt t1 t2 = [%compare: elt list] t1.elts t2.elts
    let create () = of_list []
    let is_empty t = List.is_empty t.elts
    let push_without_pushback t x =
      match t.pops with
      | ivar :: rest ->
        t.pops <- rest;
        Ivar.fill ivar x
      | [] ->
        if Ivar.is_full t.push then t.push <- Ivar.create ();
        t.elts <- x :: t.elts
    let push t x =
      push_without_pushback t x;
      Ivar.read t.push
    let pop t =
      match t.elts with
      | [] ->
        let ivar = Ivar.create () in
        t.pops <- ivar :: t.pops;
        Ivar.read ivar
      | x :: rest ->
        t.elts <- rest;
        if List.is_empty rest then Ivar.fill t.push ();
        Deferred.return x
    let iter t ~f = Deferred.List.iter t.elts ~f
  end

  let stack elt =
    let open Generator in
    List.gen elt
    >>| fun xs ->
    let t = Async_stack.create () in
    List.iter xs ~f:(fun x -> don't_wait_for (Async_stack.push t x));
    t

  open Async_stack

  let%test_unit "push + is_empty" =
    Quickcheck.test (Generator.tuple2 Char.gen (stack Char.gen)) ~f:(fun (x, t) ->
      don't_wait_for (push t x);
      [%test_result: bool] (is_empty t) ~expect:false)

  let%test_unit "push + to_list" =
    Quickcheck.test (Generator.tuple2 Char.gen (stack Char.gen)) ~f:(fun (x, t) ->
      let list1 = to_list t in
      don't_wait_for (push t x);
      let list2 = to_list t in
      [%test_result: char list] list2 ~expect:(x :: list1))

  let%test_unit "push + pushback" =
    Quickcheck.test (Generator.tuple2 Char.gen (stack Char.gen)) ~f:(fun (x, t) ->
      let pushback = push t x in
      [%test_result: bool] (Deferred.is_determined pushback) ~expect:false)

  let%test_unit "push + pop" =
    Thread_safe.block_on_async_exn (fun () ->
      Quickcheck.async_test (Generator.tuple2 Char.gen (stack Char.gen)) ~f:(fun (x, t) ->
        don't_wait_for (push t x);
        pop t >>| fun y ->
        [%test_result: char] y ~expect:x))

  let%test_unit "push + pop + to_list" =
    Thread_safe.block_on_async_exn (fun () ->
      Quickcheck.async_test (Generator.tuple2 Char.gen (stack Char.gen)) ~f:(fun (x, t) ->
        let list1 = to_list t in
        don't_wait_for (push t x);
        pop t >>| fun _ ->
        let list2 = to_list t in
        [%test_result: char list] list2 ~expect:list1))

  let%test_unit "iter" =
    Thread_safe.block_on_async_exn (fun () ->
      Quickcheck.async_test (stack Char.gen) ~f:(fun t ->
        let q = Queue.create () in
        iter t ~f:(fun x -> Queue.enqueue q x; Deferred.unit)
        >>| fun () ->
        [%test_result: char list] (Queue.to_list q) ~expect:(to_list t)))

  let%test_unit "push + pop + pushback" =
    Thread_safe.block_on_async_exn (fun () ->
      Quickcheck.async_test (Generator.tuple2 Char.gen (stack Char.gen)) ~f:(fun (x, t) ->
        let pushback = push t x in
        pop t >>| fun _ ->
        [%test_result: bool] (Deferred.is_determined pushback) ~expect:(is_empty t)))

end
