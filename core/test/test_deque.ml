open! Core
open! Import
open! Deque

let%test_unit _ = ignore (create ~initial_length:0 () : _ t)
let%test _ = length (create ()) = 0

let%test_module _ =
  (module struct
    let binary_search = binary_search ~compare:Int.compare
    let t = of_array [| 1; 2; 3; 4 |]
    let%test _ = [%equal: int option] (binary_search t `First_equal_to 2) (Some 1)
    let%test _ = [%equal: int option] (binary_search t `First_equal_to 5) None
    let%test _ = [%equal: int option] (binary_search t `First_equal_to 0) None
    let%test _ = [%equal: int option] (binary_search t ~pos:2 `First_equal_to 2) None
    let%test _ = [%equal: int option] (binary_search t ~pos:2 `First_equal_to 3) (Some 2)
    let (_ : int option) = dequeue_front t
    let (_ : int option) = dequeue_front t
    let%test _ = [%equal: int option] (binary_search t `First_equal_to 2) None
    let%test _ = [%equal: int option] (binary_search t `First_equal_to 3) (Some 2)
    let%test _ = [%equal: int option] (binary_search t `First_equal_to 5) None
    let%test _ = [%equal: int option] (binary_search t `First_equal_to 0) None
    let%test _ = [%equal: int option] (binary_search t ~pos:2 `First_equal_to 2) None
    let%test _ = [%equal: int option] (binary_search t ~pos:2 `First_equal_to 3) (Some 2)
  end)
;;

let%test_module _ =
  (module struct
    let%test_unit _ =
      let q = create () in
      let bin_alpha _ = assert false in
      let pos_ref = ref 0 in
      assert (Int.( = ) (length q) 0);
      let bigstring = Bigstring.create (bin_size_t bin_alpha q) in
      ignore (bin_write_t bin_alpha bigstring ~pos:0 q : int);
      let q' = bin_read_t bin_alpha bigstring ~pos_ref in
      assert (Int.( = ) (length q') 0)
    ;;

    module type Deque_intf = sig
      type 'a t

      val create : unit -> 'a t
      val enqueue : 'a t -> [ `back | `front ] -> 'a -> unit
      val dequeue : 'a t -> [ `back | `front ] -> 'a option
      val to_array : 'a t -> 'a array
      val clear : 'a t -> unit
      val length : 'a t -> int
      val iter : 'a t -> f:('a -> unit) -> unit

      val fold'
        :  'a t
        -> [ `front_to_back | `back_to_front ]
        -> init:'b
        -> f:('b -> 'a -> 'b)
        -> 'b
    end

    module That_dequeue : Deque_intf = struct
      type 'a t = 'a Doubly_linked.t

      let create = Doubly_linked.create

      let enqueue t back_or_front v =
        match back_or_front with
        | `back -> ignore (Doubly_linked.insert_last t v : _ Doubly_linked.Elt.t)
        | `front -> ignore (Doubly_linked.insert_first t v : _ Doubly_linked.Elt.t)
      ;;

      let dequeue t back_or_front =
        match back_or_front with
        | `back -> Doubly_linked.remove_last t
        | `front -> Doubly_linked.remove_first t
      ;;

      let fold' t dir ~init ~f =
        match dir with
        | `front_to_back -> Doubly_linked.fold t ~init ~f
        | `back_to_front -> Doubly_linked.fold_right t ~init ~f:(fun x acc -> f acc x)
      ;;

      let to_array = Doubly_linked.to_array
      let clear = Doubly_linked.clear
      let iter = Doubly_linked.iter
      let length = Doubly_linked.length
    end

    module This_dequeue : Deque_intf = struct
      type nonrec 'a t = 'a t

      let create () = create ()
      let enqueue = enqueue
      let dequeue = dequeue
      let to_array = to_array
      let clear = clear
      let length = length
      let iter = iter
      let fold' = fold'
    end

    let enqueue (t_a, t_b) back_or_front v =
      let start_a = This_dequeue.to_array t_a in
      let start_b = That_dequeue.to_array t_b in
      This_dequeue.enqueue t_a back_or_front v;
      That_dequeue.enqueue t_b back_or_front v;
      let end_a = This_dequeue.to_array t_a in
      let end_b = That_dequeue.to_array t_b in
      if not ([%equal: int array] end_a end_b)
      then
        failwithf
          "enqueue transition failure of: %s -> %s vs. %s -> %s"
          (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t start_a))
          (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t end_a))
          (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t start_b))
          (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t end_b))
          ()
    ;;

    let dequeue (t_a, t_b) back_or_front =
      let start_a = This_dequeue.to_array t_a in
      let start_b = That_dequeue.to_array t_b in
      let a, b =
        This_dequeue.dequeue t_a back_or_front, That_dequeue.dequeue t_b back_or_front
      in
      let end_a = This_dequeue.to_array t_a in
      let end_b = That_dequeue.to_array t_b in
      if (not ([%equal: int option] a b)) || not ([%equal: int array] end_a end_b)
      then
        failwithf
          "error in dequeue: %s (%s -> %s) <> %s (%s -> %s)"
          (Option.value ~default:"None" (Option.map a ~f:Int.to_string))
          (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t start_a))
          (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t end_a))
          (Option.value ~default:"None" (Option.map b ~f:Int.to_string))
          (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t start_b))
          (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t end_b))
          ()
    ;;

    let clear (t_a, t_b) =
      This_dequeue.clear t_a;
      That_dequeue.clear t_b
    ;;

    let create () =
      let t_a = This_dequeue.create () in
      let t_b = That_dequeue.create () in
      t_a, t_b
    ;;

    let this_to_string this_t =
      Sexp.to_string ([%sexp_of: int array] (This_dequeue.to_array this_t))
    ;;

    let that_to_string that_t =
      Sexp.to_string ([%sexp_of: int array] (That_dequeue.to_array that_t))
    ;;

    let fold_check (t_a, t_b) dir =
      let make_list fold t = fold t dir ~init:[] ~f:(fun acc x -> x :: acc) in
      let this_l = make_list This_dequeue.fold' t_a in
      let that_l = make_list That_dequeue.fold' t_b in
      if not ([%equal: int list] this_l that_l)
      then
        failwithf
          "error in fold:  %s (from %s) <> %s (from %s)"
          (Sexp.to_string ([%sexp_of: int list] this_l))
          (this_to_string t_a)
          (Sexp.to_string ([%sexp_of: int list] that_l))
          (that_to_string t_b)
          ()
    ;;

    let iter_check (t_a, t_b) =
      let make_rev_list iter t =
        let r = ref [] in
        iter t ~f:(fun x -> r := x :: !r);
        !r
      in
      let this_l = make_rev_list This_dequeue.iter t_a in
      let that_l = make_rev_list That_dequeue.iter t_b in
      if not ([%equal: int list] this_l that_l)
      then
        failwithf
          "error in iter:  %s (from %s) <> %s (from %s)"
          (Sexp.to_string ([%sexp_of: int list] this_l))
          (this_to_string t_a)
          (Sexp.to_string ([%sexp_of: int list] that_l))
          (that_to_string t_b)
          ()
    ;;

    let length_check (t_a, t_b) =
      let this_len = This_dequeue.length t_a in
      let that_len = That_dequeue.length t_b in
      if this_len <> that_len
      then
        failwithf
          "error in length: %i (for %s) <> %i (for %s)"
          this_len
          (this_to_string t_a)
          that_len
          (that_to_string t_b)
          ()
    ;;

    let test () =
      let t = create () in
      let rec loop ops =
        if ops = 0
        then (
          let t_a, t_b = t in
          let arr_a = This_dequeue.to_array t_a in
          let arr_b = That_dequeue.to_array t_b in
          if not ([%equal: int array] arr_a arr_b)
          then
            failwithf
              "dequeue final states not equal: %s vs. %s"
              (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t arr_a))
              (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t arr_b))
              ())
        else (
          let r = Random.int 110 in
          if r < 20
          then enqueue t `front (Random.int 10_000)
          else if r < 40
          then enqueue t `back (Random.int 10_000)
          else if r < 50
          then dequeue t `front
          else if r < 60
          then dequeue t `back
          else if r < 70
          then clear t
          else if r < 80
          then fold_check t `front_to_back
          else if r < 90
          then fold_check t `back_to_front
          else if r < 100
          then iter_check t
          else length_check t;
          loop (ops - 1))
      in
      loop 1_000
    ;;

    let%test_unit _ = test ()
  end)
;;
