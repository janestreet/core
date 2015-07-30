open Core.Std
open Quickcheck

module Initial_example = struct

  TEST_UNIT "fold_left vs fold_right" =
    Quickcheck.test
      Generator.(list int)
      ~sexp_of:<:sexp_of< int list >>
      ~f:(fun list ->
        <:test_eq< int >>
          (List.fold_left  ~init:0 ~f:(+) list)
          (List.fold_right ~init:0 ~f:(+) list))

end

module Generator_examples = struct

  let (_ : _ Generator.t) =
    Generator.singleton "An arbitrary value."
  let (_ : _ Generator.t) =
    Generator.string (* any string, including weird strings like "\000" *)
  let (_ : _ Generator.t) =
    Generator.int    (* any int, from [min_value] to [max_value] *)
  let (_ : _ Generator.t) =
    Generator.float  (* any float, from [neg_infinity] to [infinity] plus [nan] *)
  let (_ : _ Generator.t) =
    Generator.size   (* small non-negative ints *)
  let (_ : _ Generator.t) =
    Generator.int_between ~lower_bound:(Incl 0) ~upper_bound:(Excl 100)
  let (_ : _ Generator.t) =
    Generator.float_between
      ~lower_bound:(Incl 1.)
      ~upper_bound:Unbounded
      ~nan:Without
  let (_ : _ Generator.t) =
    Generator.(tuple2 int float)
  let (_ : _ Generator.t) =
    Generator.(list (tuple2 int float))
  let (_ : _ Generator.t) =
    Generator.(list (tuple2 int float)
                 ~unique:true
                 ~length:(`At_most 12)
                 ~sorted:(`By <:compare< int * float >>))
  let (_ : _ Generator.t) =
    Generator.(either int float)
  let (_ : _ Generator.t) =
    Generator.(option string)
  let (_ : _ Generator.t) =
    Generator.(map char ~f:Char.to_int)
  let (_ : _ Generator.t) =
    Generator.(filter float ~f:Float.is_finite) (* use [filter] sparingly! *)
  let (_ : _ Generator.t) =
    Generator.(fn Observer.int bool)
  let (_ : _ Generator.t) =
    Generator.(union [ singleton (Ok ()) ; singleton (Or_error.error_string "fail") ])

  module Monadic = struct

    let (_ : _ Generator.t) =
      let open Generator in
      string
      >>= fun str ->
      int_between ~lower_bound:(Incl 0) ~upper_bound:(Excl (String.length str))
      >>| fun i ->
      str, i, String.get str i

  end

  module Recursive = struct

    let (_ : _ Generator.t) =
      Generator.(recursive (fun sexp ->
        either string (list sexp)
        >>| function
        | First  a -> Sexp.Atom a
        | Second l -> Sexp.List l))

    let rec binary_subtree ~lower_bound ~upper_bound =
      let open Generator in
      union
        [ singleton `Leaf
        ; int_between    ~lower_bound ~upper_bound            >>= fun key   ->
          binary_subtree ~lower_bound ~upper_bound:(Excl key) >>= fun left  ->
          binary_subtree ~lower_bound:(Excl key) ~upper_bound >>| fun right ->
          `Node (left, key, right)
        ]

    let _binary_tree () : _ Generator.t =
      binary_subtree ~lower_bound:Unbounded ~upper_bound:Unbounded

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
  let (_ : _ Observer.t) = Observer.string
  let (_ : _ Observer.t) = Observer.int
  let (_ : _ Observer.t) = Observer.float
  let (_ : _ Observer.t) = Observer.(tuple2 int float)
  let (_ : _ Observer.t) = Observer.(list (tuple2 int float))
  let (_ : _ Observer.t) = Observer.(either int float)
  let (_ : _ Observer.t) = Observer.(option string)
  let (_ : _ Observer.t) = Observer.(fn Generator.int bool ~sexp_of_dom:<:sexp_of< int >>)
  let (_ : _ Observer.t) =
    Observer.(unmap char
                ~f:Char.of_int_exn
                ~f_sexp:(fun () -> Sexp.Atom "Char.of_int_exn"))

end

module Example_1_functional = struct

  module Functional_stack : sig
    type 'a t with sexp, compare
    val empty : _ t
    val is_empty : _ t -> bool
    val push : 'a t -> 'a -> 'a t
    val top_exn : 'a t -> 'a
    val pop_exn : 'a t -> 'a t
  end = struct
    type 'a t = 'a list with sexp, compare
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

  let stack elt =
    Generator.(recursive (fun self ->
      either unit (tuple2 elt self)
      >>| function
      | First  ()     -> Functional_stack.empty
      | Second (x, t) -> Functional_stack.push t x))

  open Functional_stack

  TEST_UNIT "push + is_empty" =
    Quickcheck.test Generator.(tuple2 int (stack int)) ~f:(fun (x, t) ->
      <:test_result< bool >> (is_empty (push t x)) ~expect:false)

  TEST_UNIT "push + top_exn" =
    Quickcheck.test Generator.(tuple2 int (stack int)) ~f:(fun (x, t) ->
      <:test_result< int >> (top_exn (push t x)) ~expect:x)

  TEST_UNIT "push + pop_exn" =
    Quickcheck.test Generator.(tuple2 int (stack int)) ~f:(fun (x, t) ->
      <:test_result< int t >> (pop_exn (push t x)) ~expect:t)

end

module Example_2_imperative = struct

  module Imperative_stack : sig
    type 'a t with sexp, compare
    val create : unit -> _ t
    val is_empty : _ t -> bool
    val push : 'a t -> 'a -> unit
    val pop_exn : 'a t -> 'a
    val iter : 'a t -> f:('a -> unit) -> unit
    val to_list : 'a t -> 'a list
  end = struct
    type 'a t = 'a list ref with sexp, compare
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
    list elt
    >>| fun xs ->
    let t = Imperative_stack.create () in
    List.iter xs ~f:(fun x -> Imperative_stack.push t x);
    t

  open Imperative_stack

  TEST_UNIT "push + is_empty" =
    Quickcheck.test Generator.(tuple2 string (stack string)) ~f:(fun (x, t) ->
      <:test_result< bool >> (push t x; is_empty t) ~expect:false)

  TEST_UNIT "push + pop_exn" =
    Quickcheck.test Generator.(tuple2 string (stack string)) ~f:(fun (x, t) ->
      push t x;
      let y = pop_exn t in
      <:test_result< string >> y ~expect:x)

  TEST_UNIT "push + to_list" =
    Quickcheck.test Generator.(tuple2 string (stack string)) ~f:(fun (x, t) ->
      let list1 = to_list t in
      push t x;
      let list2 = to_list t in
      <:test_result< string list >> list2 ~expect:(x :: list1))

  TEST_UNIT "push + pop_exn + to_list" =
    Quickcheck.test Generator.(tuple2 string (stack string)) ~f:(fun (x, t) ->
      let list1 = to_list t in
      push t x;
      let _ = pop_exn t in
      let list2 = to_list t in
      <:test_result< string list >> list2 ~expect:list1)

  TEST_UNIT "iter" =
    Quickcheck.test Generator.(stack string) ~f:(fun t ->
      let q = Queue.create () in
      iter t ~f:(fun x -> Queue.enqueue q x);
      <:test_result< string list >> (Queue.to_list q) ~expect:(to_list t))

end

module Example_3_asynchronous = struct

  open Async.Std

  module Async_stack : sig
    type 'a t with sexp, compare
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
    let sexp_of_t sexp_of_elt t = <:sexp_of< elt list >> (to_list t)
    let t_of_sexp elt_of_sexp sexp = of_list (<:of_sexp< elt list >> sexp)
    let compare (type elt) compare_elt t1 t2 = <:compare< elt list >> t1.elts t2.elts
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
    list elt
    >>| fun xs ->
    let t = Async_stack.create () in
    List.iter xs ~f:(fun x -> don't_wait_for (Async_stack.push t x));
    t

  open Async_stack

  TEST_UNIT "push + is_empty" =
    Quickcheck.test Generator.(tuple2 char (stack char)) ~f:(fun (x, t) ->
      don't_wait_for (push t x);
      <:test_result< bool >> (is_empty t) ~expect:false)

  TEST_UNIT "push + to_list" =
    Quickcheck.test Generator.(tuple2 char (stack char)) ~f:(fun (x, t) ->
      let list1 = to_list t in
      don't_wait_for (push t x);
      let list2 = to_list t in
      <:test_result< char list >> list2 ~expect:(x :: list1))

  TEST_UNIT "push + pushback" =
    Quickcheck.test Generator.(tuple2 char (stack char)) ~f:(fun (x, t) ->
      let pushback = push t x in
      <:test_result< bool >> (Deferred.is_determined pushback) ~expect:false)

  TEST_UNIT "push + pop" =
    Thread_safe.block_on_async_exn (fun () ->
      Quickcheck.async_test Generator.(tuple2 char (stack char)) ~f:(fun (x, t) ->
        don't_wait_for (push t x);
        pop t >>| fun y ->
        <:test_result< char >> y ~expect:x))

  TEST_UNIT "push + pop + to_list" =
    Thread_safe.block_on_async_exn (fun () ->
      Quickcheck.async_test Generator.(tuple2 char (stack char)) ~f:(fun (x, t) ->
        let list1 = to_list t in
        don't_wait_for (push t x);
        pop t >>| fun _ ->
        let list2 = to_list t in
        <:test_result< char list >> list2 ~expect:list1))

  TEST_UNIT "iter" =
    Thread_safe.block_on_async_exn (fun () ->
      Quickcheck.async_test Generator.(stack char) ~f:(fun t ->
        let q = Queue.create () in
        iter t ~f:(fun x -> Queue.enqueue q x; Deferred.unit)
        >>| fun () ->
        <:test_result< char list >> (Queue.to_list q) ~expect:(to_list t)))

  TEST_UNIT "push + pop + pushback" =
    Thread_safe.block_on_async_exn (fun () ->
      Quickcheck.async_test Generator.(tuple2 char (stack char)) ~f:(fun (x, t) ->
        let pushback = push t x in
        pop t >>| fun _ ->
        <:test_result< bool >> (Deferred.is_determined pushback) ~expect:(is_empty t)))

end
