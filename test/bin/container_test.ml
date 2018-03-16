open Core

module Test_S1
    (M : sig
       include Container.S1
       val of_list : 'a list -> 'a  t
     end) : sig
  val test : unit -> unit
end = struct
  let lists = List.init 10 ~f:(fun i -> List.init i ~f:ident)

  let test () =
    List.iter lists ~f:(fun l ->
      let m = M.of_list l in
      assert (M.length m = List.length l);
      assert (M.is_empty m = List.is_empty l);
      let is_l_unsorted l' = l = List.sort l' ~compare:Int.compare in
      assert (is_l_unsorted (M.fold m ~init:[] ~f:(fun ac x -> x :: ac)));
      assert (is_l_unsorted (M.to_list m));
      let r = ref [] in
      M.iter m ~f:(fun x -> r := x :: !r);
      assert (is_l_unsorted !r);
      assert (is_l_unsorted (Array.to_list (M.to_array m)));
      List.iter l ~f:(fun x -> assert (M.exists m ~f:(fun x' -> x = x')));
      assert (M.for_all m ~f:(fun x -> List.exists l ~f:(fun x' -> x = x')));
      List.iter l ~f:(fun x -> assert (Some x = M.find m ~f:(fun x' -> x = x')));
    )
  ;;
end
