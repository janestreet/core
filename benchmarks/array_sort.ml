(** Demonstrates how [Obj.magic] can be used to speed up sorting. *)

module Array = StdLabels.Array

module List = Core_kernel.Core_list

let invalid_argf = Core_kernel.Core_printf.invalid_argf

let failwiths = Core_kernel.Error.failwiths
let does_raise = Core_kernel.Common.does_raise

type 'a t = 'a array

module Sort = struct
  let swap arr i j =
    let arr : int array = Obj.magic arr in
    let tmp = Array.unsafe_get arr i in
    Array.unsafe_set arr i (Array.unsafe_get arr j);
    Array.unsafe_set arr j tmp
  ;;

  module type Sort = sig
    val sort
      :  'a t
      -> cmp:('a -> 'a -> int)
      -> left:int (* leftmost index of sub-array to sort *)
      -> right:int (* rightmost index of sub-array to sort *)
      -> unit
  end

  (* http://en.wikipedia.org/wiki/Insertion_sort *)
  module Insertion_sort : Sort = struct
    let sort arr ~cmp ~left ~right =
      let arr : int array = Obj.magic arr in
      let insert pos v =
        (* loop invariants:
           1.  the subarray arr[left .. i-1] is sorted
           2.  the subarray arr[i+1 .. pos] is sorted and contains only elements > v
           3.  arr[i] may be thought of as containing v
        *)
        let rec loop i =
          let i_next = i - 1 in
          if i_next >= left && cmp (Obj.magic (Array.unsafe_get arr i_next)) v > 0 then
          begin
            Array.unsafe_set arr i (Array.unsafe_get arr i_next);
            loop i_next
          end else
            i
        in
        let final_pos = loop pos in
        Array.unsafe_set arr final_pos (Obj.magic v)
      in
      (* loop invariant:
         arr is sorted from left to i-1, inclusive
      *)
      for i = left + 1 to right do
        insert i (Obj.magic (Array.unsafe_get arr i))
      done
    ;;
  end

  (* http://en.wikipedia.org/wiki/Heapsort *)
  module Heap_sort : Sort = struct
    (* loop invariant:
       root's children are both either roots of max-heaps or > right
    *)
    let rec heapify arr ~(cmp:'a -> 'a -> int) root ~left ~right =
      let arr : int array = Obj.magic arr in
      let relative_root = root - left in
      let left_child    = (2 * relative_root) + left + 1 in
      let right_child   = (2 * relative_root) + left + 2 in
      let largest =
        if left_child <= right
          && cmp (Obj.magic (Array.unsafe_get arr left_child))
            (Obj.magic (Array.unsafe_get arr root)) > 0
        then left_child
        else root
      in
      let largest =
        if right_child <= right
          && cmp (Obj.magic (Array.unsafe_get arr right_child))
            (Obj.magic (Array.unsafe_get arr largest)) > 0
        then right_child
        else largest
      in
      if largest <> root then begin
        swap arr root largest;
        heapify arr ~cmp largest ~left ~right
      end;
    ;;

    let build_heap arr ~cmp ~left ~right =
      let arr : int array = Obj.magic arr in
      (* Elements in the second half of the array are already heaps of size 1.  We move
         through the first half of the array from back to front examining the element at
         hand, and the left and right children, fixing the heap property as we go. *)
      for i = (left + right) / 2 downto left do
        heapify arr ~cmp i ~left ~right;
      done;
    ;;

    let sort arr ~cmp ~left ~right =
      let arr : int array = Obj.magic arr in
      build_heap arr ~cmp ~left ~right;
      (* loop invariants:
         1.  the subarray arr[left ... i] is a max-heap H
         2.  the subarray arr[i+1 ... right] is sorted (call it S)
         3.  every element of H is less than every element of S
      *)
      for i = right downto left + 1 do
        swap arr left i;
        heapify arr ~cmp left ~left ~right:(i - 1);
      done;
    ;;
  end

  (* http://en.wikipedia.org/wiki/Introsort *)
  module Intro_sort : Sort = struct

    let five_element_sort arr ~cmp m1 m2 m3 m4 m5 =
      let arr : int array = Obj.magic arr in
      let compare_and_swap i j =
        let x = Obj.magic (Array.unsafe_get arr i) in
        let y = Obj.magic (Array.unsafe_get arr j) in
        if cmp x y > 0 then swap arr i j
      in
      (* optimal 5-element sorting network *)
      compare_and_swap m1 m2;  (* 1--o-----o-----o--------------1 *)
      compare_and_swap m4 m5;  (*    |     |     |                *)
      compare_and_swap m1 m3;  (* 2--o-----|--o--|-----o--o-----2 *)
      compare_and_swap m2 m3;  (*          |  |  |     |  |       *)
      compare_and_swap m1 m4;  (* 3--------o--o--|--o--|--o-----3 *)
      compare_and_swap m3 m4;  (*                |  |  |          *)
      compare_and_swap m2 m5;  (* 4-----o--------o--o--|-----o--4 *)
      compare_and_swap m2 m3;  (*       |              |     |    *)
      compare_and_swap m4 m5;  (* 5-----o--------------o-----o--5 *)
    ;;

    (* choose pivots for the array by sorting 5 elements and examining the center three
        elements.  The goal is to choose two pivots that will either:
        - break the range up into 3 even partitions
        or
        - eliminate a commonly appearing element by sorting it into the center partition
          by itself
        To this end we look at the center 3 elements of the 5 and return pairs of equal
        elements or the widest range *)
    let choose_pivots arr ~cmp ~left ~right =
      let sixth = (right - left) / 6 in
      let m1 = left + sixth in
      let m2 = m1 + sixth in
      let m3 = m2 + sixth in
      let m4 = m3 + sixth in
      let m5 = m4 + sixth in
      five_element_sort arr ~cmp m1 m2 m3 m4 m5;
      let m2_val = Array.unsafe_get arr m2 in
      let m3_val = Array.unsafe_get arr m3 in
      let m4_val = Array.unsafe_get arr m4 in
      if cmp m2_val m3_val = 0      then (m2_val, m3_val, true)
      else if cmp m3_val m4_val = 0 then (m3_val, m4_val, true)
      else                               (m2_val, m4_val, false)
    ;;

    let dual_pivot_partition arr ~cmp ~left ~right =
      let pivot1, pivot2, pivots_equal = choose_pivots arr ~cmp ~left ~right in
      let arr : int array = Obj.magic arr in
      (* loop invariants:
         1.  left <= l < r <= right
         2.  l <= p <= r
         3.  l <= x < p     implies arr[x] >= pivot1
                                and arr[x] <= pivot2
         4.  left <= x < l  implies arr[x] < pivot1
         5.  r < x <= right implies arr[x] > pivot2
      *)
      let rec loop l p r =
        let pv = Obj.magic (Array.unsafe_get arr p) in
        if cmp pv pivot1 < 0 then begin
          swap arr p l;
          cont (l + 1) (p + 1) r
        end else if cmp pv pivot2 > 0 then begin
          (* loop invariants:  same as those of the outer loop *)
          let rec scan_backwards r =
            if r > p && cmp (Obj.magic (Array.unsafe_get arr r)) pivot2 > 0
            then scan_backwards (r - 1)
            else r
          in
          let r = scan_backwards r in
          swap arr r p;
          cont l p (r - 1)
        end else
          cont l (p + 1) r
      and cont l p r =
        if p > r then (l, r) else loop l p r
      in
      let (l, r) = cont left left right in
      (l, r, pivots_equal)
    ;;

    let rec intro_sort arr ~max_depth ~cmp ~left ~right =
      let len = right - left + 1 in
      (* This takes care of some edge cases, such as left > right or very short arrays,
         since Insertion_sort.sort handles these cases properly.  Thus we don't need to
         make sure that left and right are valid in recursive calls. *)
      if len <= 32 then begin
        Insertion_sort.sort arr ~cmp ~left ~right
      end else if max_depth < 0 then begin
        Heap_sort.sort arr ~cmp ~left ~right;
      end else begin
        let max_depth = max_depth - 1 in
        let (l, r, middle_sorted) = dual_pivot_partition arr ~cmp ~left ~right in
        intro_sort arr ~max_depth ~cmp ~left ~right:(l - 1);
        if not middle_sorted then intro_sort arr ~max_depth ~cmp ~left:l ~right:r;
        intro_sort arr ~max_depth ~cmp ~left:(r + 1) ~right;
      end
    ;;

    let log10_of_3 = log10 3.

    let log3 x = log10 x /. log10_of_3

    let sort arr ~cmp ~left ~right =
      let len = right - left + 1 in
      let heap_sort_switch_depth =
        (* with perfect 3-way partitioning, this is the recursion depth *)
        int_of_float (log3 (float_of_int len))
      in
      intro_sort arr ~max_depth:heap_sort_switch_depth ~cmp ~left ~right;
    ;;
  end
end

let sort ?pos ?len arr ~cmp =
  let pos, len =
    Core_kernel.Ordered_collection_common.get_pos_len_exn ?pos ?len ~length:(Array.length arr)
  in
  Sort.Intro_sort.sort arr ~cmp ~left:pos ~right:(pos + len - 1)


let () =
  let n = int_of_string Sys.argv.(1) in
  let iter = int_of_string Sys.argv.(2) in


  let r = Array.init n ~f:(fun _ -> Random.int n) in
  let a = Array.make n 0 in
  let start = Unix.gettimeofday () in
  let cmp (x : int) (x' : int) = compare x x' in
  for _i = 0 to iter do
    Array.blit ~src:r ~src_pos:0 ~dst:a ~dst_pos:0 ~len:n;
    sort a ~cmp
  done;
  Printf.printf "%f\n" (Unix.gettimeofday () -. start);
  for i = 0 to n - 2 do
    assert (a.(i) <= a.(i + 1))
  done;

  let start = Unix.gettimeofday () in
  for _i = 0 to iter do
    Array.blit ~src:r ~src_pos:0 ~dst:a ~dst_pos:0 ~len:n;
    Core.Std.Array.sort a ~cmp
  done;
  Printf.printf "%f\n" (Unix.gettimeofday () -. start);

  
  let r = Array.init n ~f:(fun _ -> ref (Random.int n)) in
  let a = Array.make n (ref 0) in

  let start = Unix.gettimeofday () in
  let cmp (x : int ref) (x' : int ref) = compare !x !x' in
  for _i = 0 to iter do
    Array.blit ~src:r ~src_pos:0 ~dst:a ~dst_pos:0 ~len:n;
    sort a ~cmp
  done;
  Printf.printf "%f\n" (Unix.gettimeofday () -. start);
  for i = 0 to n - 2 do
    assert (!(a.(i)) <= !(a.(i + 1)))
  done;

  let start = Unix.gettimeofday () in
  for _i = 0 to iter do
    Array.blit ~src:r ~src_pos:0 ~dst:a ~dst_pos:0 ~len:n;
    Core.Std.Array.sort a ~cmp
  done;
  Printf.printf "%f\n" (Unix.gettimeofday () -. start);

