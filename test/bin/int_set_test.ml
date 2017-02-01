open OUnit
open Core

module ISet = struct
  include Set.Make(Int)

  let of_int_set iset =
    List.fold (Int_set.ranges iset) ~init:empty ~f:(fun st (lo,hi) ->
      assert (lo <= hi);
      let st = ref st in
      for i = lo to hi do
        st := add !st i
      done;
      !st)
end

(* add random int between [0..99] 100 times and check the result is correct *)
let test_random_0_99 () =
  let f () =
    let set = ref ISet.empty in
    let int_set = ref Int_set.empty in
    for _ = 0 to 99 do
      let n = Random.int 100 in
      set := ISet.add !set n;
      int_set := Int_set.add !int_set n;
    done;
    assert (ISet.equal !set (ISet.of_int_set !int_set));
    (* mem *)
    ISet.iter !set ~f:(fun n -> assert (Int_set.mem !int_set n));
    (* ranges returns normalized *)
    let ranges = Int_set.ranges !int_set in
    let discrete (x1,x2) (y1,y2) =  x2 + 1 < y1 || y2 + 1 < x1 in
    let rec f = function
      | [] -> ()
      | x::xs ->
        List.iter xs ~f:(fun y -> assert (discrete x y));
        f xs
    in
    f ranges;
    (* max and min *)
    assert (Int_set.max !int_set = ISet.max_elt !set);
    assert (Int_set.min !int_set = ISet.min_elt !set)
  in
  for _ = 0 to 999 do f () done;
  (* max and min for empty *)
  assert (Int_set.max Int_set.empty = Int_set.min Int_set.empty)

(* test for add_range, checking some sensitive cases *)
let test_add_range () =
  let targets = [ [ (0, 100) ], [ (0, 50); (51, 100) ];
                  [ (0, 100) ], [ (51, 100); (0, 50) ];
                  [ (0, 100) ], [ (0, 49); (51, 100); (50, 50) ] ]
  in
  let iset_of_ranges = List.fold ~init:Int_set.empty
                         ~f:(fun st (x,y) -> Int_set.add_range st x y)
  in
  List.iter targets ~f:(fun (answer, subranges) ->
    let answer = iset_of_ranges answer in
    let sum = iset_of_ranges subranges in
    let sum' = iset_of_ranges (List.rev subranges) in
    assert (answer = sum);
    assert (answer = sum'))

let test_min_and_max () =
  let set = Int_set.empty in
  assert_equal None (Int_set.min set);
  assert_equal None (Int_set.max set);
  let set = Int_set.add_range set 20 30 in
  let set = Int_set.add_range set 10 12 in
  let set = Int_set.add_range set 13 42 in
  let set = Int_set.add_range set 25 35 in
  assert_equal (Some 10) (Int_set.min set);
  assert_equal (Some 42) (Int_set.max set)

let test =
  "int_set" >::: [
    "random_0_99" >:: test_random_0_99;
    "add_range" >:: test_add_range;
    "test_min_and_max" >:: test_min_and_max;
  ]



