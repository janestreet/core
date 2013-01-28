module List = StdLabels.List
module String = StdLabels.String
open Sexplib.Std
open Bin_prot.Std

module Random = Core_random

let invalid_argf = Core_printf.invalid_argf

module T = struct
  type 'a t = 'a list with sexp, bin_io
end

include T

let range ?(stride=1) ?(start=`inclusive) ?(stop=`exclusive) start_i stop_i =
  if stride = 0 then
    invalid_arg "Core_list.range: stride must be non-zero";
  (* Generate the range from the last element, so that we do not need to rev it *)
  let rec loop last counter accum =
    if counter <= 0 then accum
    else loop (last - stride) (counter - 1) (last :: accum)
  in
  let stride_sign = if stride > 0 then 1 else -1 in
  let start =
    match start with
    | `inclusive -> start_i
    | `exclusive -> start_i + stride
  in
  let stop =
    match stop with
    | `inclusive -> stop_i + stride_sign
    | `exclusive -> stop_i
  in
  let num_elts = (stop - start + stride - stride_sign) / stride in
  loop (start + (stride * (num_elts - 1))) num_elts []
;;

TEST_MODULE "range symmetries" = struct

  let basic ~stride ~start ~stop ~start_n ~stop_n ~result =
    range ~stride ~start ~stop start_n stop_n = result

  let test stride (start_n, start) (stop_n, stop) result =
    basic ~stride ~start ~stop ~start_n ~stop_n ~result
    && (* works for negative [start] and [stop] *)
      basic ~stride:(-stride)
        ~start_n:(-start_n)
        ~stop_n:(-stop_n)
        ~start
        ~stop
        ~result:(List.map result ~f:(fun x -> -x))

  TEST = test 1    ( 3, `inclusive) ( 1, `exclusive) []
  TEST = test 1    ( 3, `inclusive) ( 3, `exclusive) []
  TEST = test 1    ( 3, `inclusive) ( 4, `exclusive) [3]
  TEST = test 1    ( 3, `inclusive) ( 8, `exclusive) [3;4;5;6;7]
  TEST = test 3    ( 4, `inclusive) (10, `exclusive) [4;7]
  TEST = test 3    ( 4, `inclusive) (11, `exclusive) [4;7;10]
  TEST = test 3    ( 4, `inclusive) (12, `exclusive) [4;7;10]
  TEST = test 3    ( 4, `inclusive) (13, `exclusive) [4;7;10]
  TEST = test 3    ( 4, `inclusive) (14, `exclusive) [4;7;10;13]

  TEST = test (-1) ( 1, `inclusive) ( 3, `exclusive) []
  TEST = test (-1) ( 3, `inclusive) ( 3, `exclusive) []
  TEST = test (-1) ( 4, `inclusive) ( 3, `exclusive) [4]
  TEST = test (-1) ( 8, `inclusive) ( 3, `exclusive) [8;7;6;5;4]
  TEST = test (-3) (10, `inclusive) ( 4, `exclusive) [10;7]
  TEST = test (-3) (10, `inclusive) ( 3, `exclusive) [10;7;4]
  TEST = test (-3) (10, `inclusive) ( 2, `exclusive) [10;7;4]
  TEST = test (-3) (10, `inclusive) ( 1, `exclusive) [10;7;4]
  TEST = test (-3) (10, `inclusive) ( 0, `exclusive) [10;7;4;1]

  TEST = test 1    ( 3, `exclusive) ( 1, `exclusive) []
  TEST = test 1    ( 3, `exclusive) ( 3, `exclusive) []
  TEST = test 1    ( 3, `exclusive) ( 4, `exclusive) []
  TEST = test 1    ( 3, `exclusive) ( 8, `exclusive) [4;5;6;7]
  TEST = test 3    ( 4, `exclusive) (10, `exclusive) [7]
  TEST = test 3    ( 4, `exclusive) (11, `exclusive) [7;10]
  TEST = test 3    ( 4, `exclusive) (12, `exclusive) [7;10]
  TEST = test 3    ( 4, `exclusive) (13, `exclusive) [7;10]
  TEST = test 3    ( 4, `exclusive) (14, `exclusive) [7;10;13]

  TEST = test (-1) ( 1, `exclusive) ( 3, `exclusive) []
  TEST = test (-1) ( 3, `exclusive) ( 3, `exclusive) []
  TEST = test (-1) ( 4, `exclusive) ( 3, `exclusive) []
  TEST = test (-1) ( 8, `exclusive) ( 3, `exclusive) [7;6;5;4]
  TEST = test (-3) (10, `exclusive) ( 4, `exclusive) [7]
  TEST = test (-3) (10, `exclusive) ( 3, `exclusive) [7;4]
  TEST = test (-3) (10, `exclusive) ( 2, `exclusive) [7;4]
  TEST = test (-3) (10, `exclusive) ( 1, `exclusive) [7;4]
  TEST = test (-3) (10, `exclusive) ( 0, `exclusive) [7;4;1]

  TEST = test 1    ( 3, `inclusive) ( 1, `inclusive) []
  TEST = test 1    ( 3, `inclusive) ( 3, `inclusive) [3]
  TEST = test 1    ( 3, `inclusive) ( 4, `inclusive) [3;4]
  TEST = test 1    ( 3, `inclusive) ( 8, `inclusive) [3;4;5;6;7;8]
  TEST = test 3    ( 4, `inclusive) (10, `inclusive) [4;7;10]
  TEST = test 3    ( 4, `inclusive) (11, `inclusive) [4;7;10]
  TEST = test 3    ( 4, `inclusive) (12, `inclusive) [4;7;10]
  TEST = test 3    ( 4, `inclusive) (13, `inclusive) [4;7;10;13]
  TEST = test 3    ( 4, `inclusive) (14, `inclusive) [4;7;10;13]

  TEST = test (-1) ( 1, `inclusive) ( 3, `inclusive) []
  TEST = test (-1) ( 3, `inclusive) ( 3, `inclusive) [3]
  TEST = test (-1) ( 4, `inclusive) ( 3, `inclusive) [4;3]
  TEST = test (-1) ( 8, `inclusive) ( 3, `inclusive) [8;7;6;5;4;3]
  TEST = test (-3) (10, `inclusive) ( 4, `inclusive) [10;7;4]
  TEST = test (-3) (10, `inclusive) ( 3, `inclusive) [10;7;4]
  TEST = test (-3) (10, `inclusive) ( 2, `inclusive) [10;7;4]
  TEST = test (-3) (10, `inclusive) ( 1, `inclusive) [10;7;4;1]
  TEST = test (-3) (10, `inclusive) ( 0, `inclusive) [10;7;4;1]

  TEST = test 1    ( 3, `exclusive) ( 1, `inclusive) []
  TEST = test 1    ( 3, `exclusive) ( 3, `inclusive) []
  TEST = test 1    ( 3, `exclusive) ( 4, `inclusive) [4]
  TEST = test 1    ( 3, `exclusive) ( 8, `inclusive) [4;5;6;7;8]
  TEST = test 3    ( 4, `exclusive) (10, `inclusive) [7;10]
  TEST = test 3    ( 4, `exclusive) (11, `inclusive) [7;10]
  TEST = test 3    ( 4, `exclusive) (12, `inclusive) [7;10]
  TEST = test 3    ( 4, `exclusive) (13, `inclusive) [7;10;13]
  TEST = test 3    ( 4, `exclusive) (14, `inclusive) [7;10;13]

  TEST = test (-1) ( 1, `exclusive) ( 3, `inclusive) []
  TEST = test (-1) ( 3, `exclusive) ( 3, `inclusive) []
  TEST = test (-1) ( 4, `exclusive) ( 3, `inclusive) [3]
  TEST = test (-1) ( 8, `exclusive) ( 3, `inclusive) [7;6;5;4;3]
  TEST = test (-3) (10, `exclusive) ( 4, `inclusive) [7;4]
  TEST = test (-3) (10, `exclusive) ( 3, `inclusive) [7;4]
  TEST = test (-3) (10, `exclusive) ( 2, `inclusive) [7;4]
  TEST = test (-3) (10, `exclusive) ( 1, `inclusive) [7;4;1]
  TEST = test (-3) (10, `exclusive) ( 0, `inclusive) [7;4;1]

  let test_start_inc_exc stride start (stop, stop_inc_exc) result =
    test stride (start, `inclusive) (stop, stop_inc_exc) result
    && begin
        match result with
        | [] -> true
        | head :: tail ->
          head = start && test stride (start, `exclusive) (stop, stop_inc_exc) tail
      end

  let test_inc_exc stride start stop result =
    test_start_inc_exc stride start (stop, `inclusive) result
    && begin
        match List.rev result with
        | [] -> true
        | last :: all_but_last ->
          let all_but_last = List.rev all_but_last in
          if last = stop then
            test_start_inc_exc stride start (stop, `exclusive) all_but_last
          else
            true
      end

  TEST = test_inc_exc 1 4 10 [4;5;6;7;8;9;10]
  TEST = test_inc_exc 3 4 10 [4;7;10]
  TEST = test_inc_exc 3 4 11 [4;7;10]
  TEST = test_inc_exc 3 4 12 [4;7;10]
  TEST = test_inc_exc 3 4 13 [4;7;10;13]
  TEST = test_inc_exc 3 4 14 [4;7;10;13]

end

module Test_values = struct
  let long1 =
    let v = lazy (range 1 100_000) in
    fun () -> Lazy.force v

  let l1 = [1;2;3;4;5;6;7;8;9;10]
end

(* Standard functions *)
let length = List.length
let hd_exn = List.hd
let tl_exn = List.tl

let hd t =
  match t with
  | [] -> None
  | x :: _ -> Some x
;;

let tl t =
  match t with
  | [] -> None
  | _ :: t' -> Some t'
;;

let nth t n =
  if n < 0 then None else
  let rec nth_aux t n =
    match t with
    | [] -> None
    | a :: t -> if n = 0 then Some a else nth_aux t (n-1)
  in nth_aux t n
;;

let nth_exn t n =
  match nth t n with
  | None ->
      invalid_argf "List.nth_exn %d called on list of length %d"
        n (length t) ()
  | Some a -> a
;;

let rev_append = List.rev_append

TEST = rev_append [1;2;3] [4;5;6] = [3;2;1;4;5;6]
TEST = rev_append [] [4;5;6] = [4;5;6]
TEST = rev_append [1;2;3] [] = [3;2;1]
TEST = rev_append [1] [2;3] = [1;2;3]
TEST = rev_append [1;2] [3] = [2;1;3]
TEST =
  let long = Test_values.long1 () in
  ignore (rev_append long long:int list);
  true

let rev = function
  | [] | [_] as res -> res
  | x :: y :: rest -> rev_append rest [y; x]

let unordered_append l1 l2 =
  match l1, l2 with
  | [], l | l, [] -> l
  | _             -> List.rev_append l1 l2

let iter = List.iter

let rev_map t ~f = List.rev_map t ~f

exception Length_mismatch of string * int * int with sexp

let check_length2 name l1 l2 =
  let n1 = length l1 in
  let n2 = length l2 in
  if n1 <> n2 then
    raise (invalid_argf "length mismatch in %s: %d <> %d " name n1 n2 ())
;;

let check_length3 name l1 l2 l3 =
  let n1 = length l1 in
  let n2 = length l2 in
  let n3 = length l3 in
  if n1 <> n2 || n2 <> n3 then
    raise (invalid_argf "length mismatch in %s: %d <> %d || %d <> %d"
             name n1 n2 n2 n3 ())
;;

let iter2_exn l1 l2 ~f =
  check_length2 "iter2_exn" l1 l2;
  List.iter2 l1 l2 ~f;
;;

let rev_map2_exn l1 l2 ~f  =
  check_length2 "rev_map2_exn" l1 l2;
  List.rev_map2 l1 l2 ~f;
;;

let fold2_exn l1 l2 ~init ~f =
  check_length2 "fold2_exn" l1 l2;
  List.fold_left2 l1 l2 ~init ~f;
;;

let for_all2_exn l1 l2 ~f =
  check_length2 "for_all2_exn" l1 l2;
  List.for_all2 l1 l2 ~f;
;;

let exists2_exn l1 l2 ~f =
  check_length2 "exists2_exn" l1 l2;
  List.exists2 l1 l2 ~f;
;;

let mem ?(equal = (=)) t a = List.exists t ~f:(equal a)

(* This is a copy of the code from the standard library, with an extra eta-expansion to
   avoid creating partial closures (showed up for List.filter in profiling). *)
let rev_filter t ~f =
  let rec find ~f accu = function
    | [] -> accu
    | x :: l -> if f x then find ~f (x :: accu) l else find ~f accu l
  in
  find ~f [] t
;;

let filter t ~f = rev (rev_filter t ~f)

let sort = List.sort
let stable_sort = List.stable_sort

let find_map t ~f =
  let rec loop = function
    | [] -> None
    | x :: l ->
        match f x with
        | None -> loop l
        | Some _ as r -> r
  in
  loop t
;;

let find t ~f =
  let rec loop = function
    | [] -> None
    | x :: l -> if f x then Some x else loop l
  in
  loop t
;;

let find_exn t ~f = List.find t ~f

let findi t ~f =
  let rec loop i t =
    match t with
    | [] -> None
    | x :: l -> if f i x then Some (i, x) else loop (i + 1) l
  in
  loop 0 t
;;

(** changing the order of arguments on some standard [List] functions. *)
let exists t ~f = List.exists t ~f
let for_all t ~f = List.for_all t ~f
let iter t ~f = List.iter t ~f

(** For the container interface. *)
let fold t ~init ~f = List.fold_left t ~f ~init
let fold_left = fold
let to_array = Caml.Array.of_list
let to_list t = t

(** Tail recursive versions of standard [List] module *)

let slow_append l1 l2 = List.rev_append (List.rev l1) l2

(* There are a few optimized list operations, here, including append and map.  There are
   basically two optimizations in play: loop unrolling, and dynamic switching between
   stack and heap allocation.

   The loop-unrolling is straight-forward, we just unroll 5 levels of the loop.  This
   makes each iteration faster, and also reduces the number of stack frames consumed per
   list element.

   The dynamic switching is done by counting the number of stack frames, and then
   switching to the "slow" implementation when we exceed a given limit.  This means that
   short lists use the fast stack-allocation method, and long-lists use a slower one that
   doesn't require stack space.
*)
let rec count_append l1 l2 count =
  match l1 with
  | []               ->                         l2
  | [x1]             -> x1                   :: l2
  | [x1; x2]         -> x1 :: x2             :: l2
  | [x1; x2; x3]     -> x1 :: x2 :: x3       :: l2
  | [x1; x2; x3; x4] -> x1 :: x2 :: x3 :: x4 :: l2
  | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
    x1 :: x2 :: x3 :: x4 :: x5 ::
      (if count > 1000
       then slow_append tl l2
       else count_append tl l2 (count + 1))

let append l1 l2 = count_append l1 l2 0

TEST = append [1;2;3] [4;5;6] = [1;2;3;4;5;6]
TEST = append [] [4;5;6] = [4;5;6]
TEST = append [1;2;3] [] = [1;2;3]
TEST = append [1] [2;3] = [1;2;3]
TEST = append [1;2] [3] = [1;2;3]
TEST_UNIT =
  let long = Test_values.long1 () in
  ignore (append long long:int list)

(* Rebind [@] so that uses below get our tail-recursive version rather than
   Pervasive's nontail version. *)
let (@) = append

let map_slow l ~f = List.rev (List.rev_map ~f l)

let rec count_map ~f l ctr =
  match l with
  | [] -> []
  | [x1] ->
    let f1 = f x1 in
    [f1]
  | [x1; x2] ->
    let f1 = f x1 in
    let f2 = f x2 in
    [f1; f2]
  | [x1; x2; x3] ->
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    [f1; f2; f3]
  | [x1; x2; x3; x4] ->
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    let f4 = f x4 in
    [f1; f2; f3; f4]
  | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    let f4 = f x4 in
    let f5 = f x5 in
    f1 :: f2 :: f3 :: f4 :: f5 ::
      (if ctr > 1000
        then map_slow ~f tl
        else count_map ~f tl (ctr + 1))

let map l ~f = count_map ~f l 0

TEST = map ~f:(fun x -> x) Test_values.l1 = Test_values.l1
TEST = map ~f:(fun x -> x) [] = []
TEST = map ~f:(fun x -> x +. 5.) [1.;2.;3.] = [6.;7.;8.]
TEST_UNIT =
  ignore (map ~f:(fun x -> x) (Test_values.long1 ()):int list)

let (>>|) l f = map l ~f

let map2_exn l1 l2 ~f = List.rev (rev_map2_exn l1 l2 ~f)

TEST = map2_exn ~f:(fun a b -> a, b) [1;2;3] ['a';'b';'c']
    = [(1,'a'); (2,'b'); (3,'c')]
TEST = map2_exn ~f:(fun _ _ -> ()) [] [] = []
TEST_UNIT =
  let long = Test_values.long1 () in
  ignore (map2_exn ~f:(fun _ _ -> ()) long long:unit list)

let rev_map3_exn l1 l2 l3 ~f =
  check_length3 "rev_map3" l1 l2 l3;
  let rec loop l1 l2 l3 ac =
    match (l1, l2, l3) with
    | ([], [], []) -> ac
    | (x1 :: l1, x2 :: l2, x3 :: l3) -> loop l1 l2 l3 (f x1 x2 x3 :: ac)
    | _ -> assert false
  in
  loop l1 l2 l3 []
;;

let map3_exn l1 l2 l3 ~f = List.rev (rev_map3_exn l1 l2 l3 ~f)

let rec rev_map_append l1 l2 ~f =
  match l1 with
  | [] -> l2
  | h :: t -> rev_map_append ~f t (f h :: l2)

TEST = rev_map_append [1;2;3;4;5] [6] ~f:(fun x -> x) = [5;4;3;2;1;6]
TEST = rev_map_append [1;2;3;4;5] [6] ~f:(fun x -> 2 * x) = [10;8;6;4;2;6]
TEST = rev_map_append [] [6] ~f:(fun _ -> failwith "bug!") = [6]

let fold_right l ~f ~init =
  fold ~f:(fun a b -> f b a) ~init (List.rev l)

TEST = fold_right ~f:(fun e acc -> e :: acc) Test_values.l1 ~init:[] =
  Test_values.l1
TEST = fold_right ~f:(fun e acc -> e ^ acc) ["1";"2"] ~init:"3" = "123"
TEST = fold_right ~f:(fun _ _ -> ()) [] ~init:() = ()
TEST_UNIT =
  let long = Test_values.long1 () in
  ignore (fold_right ~f:(fun e acc -> e :: acc) long ~init:[])

let fold_right2_exn l1 l2 ~f ~init =
  fold2_exn (List.rev l1) (List.rev l2) ~init ~f:(fun a b c -> f b c a)
;;

let unzip list =
  let rec loop list l1 l2 =
    match list with
    | [] -> (List.rev l1, List.rev l2)
    | (x, y) :: tl -> loop tl (x :: l1) (y :: l2)
  in
  loop list [] []

let zip_exn l1 l2 = map2_exn ~f:(fun a b -> (a, b)) l1 l2

TEST  =
  let l1 = Test_values.l1 in
  unzip (zip_exn l1 (List.rev l1)) = (l1, List.rev l1)
;;

TEST_UNIT =
  let long = Test_values.long1 () in
  ignore (unzip (zip_exn long long))
;;

let zip l1 l2 = try Some (zip_exn l1 l2) with _ -> None
TEST = zip [1;2;3] [4;5;6] = Some [1,4;2,5;3,6]
TEST = zip [1] [4;5;6]     = None

(** Additional list operations *)

let rev_mapi l ~f =
  let rec loop i acc = function
    | [] -> acc
    | h :: t -> loop (i + 1) (f i h :: acc) t
  in
  loop 0 [] l

let mapi l ~f = List.rev (rev_mapi l ~f)


TEST = mapi ~f:(fun i x -> (i,x))
  ["one";"two";"three";"four"] = [0,"one";1,"two";2,"three";3,"four"]
TEST = mapi ~f:(fun i x -> (i,x)) [] = []

let iteri l ~f =
  ignore (fold l ~init:0 ~f:(fun i x -> f i x; i + 1));
;;

let foldi t ~f ~init =
  snd (fold t ~init:(0, init) ~f:(fun (i, acc) v -> (i + 1, f i acc v)))
;;

let filteri l ~f =
  List.rev (foldi l
               ~f:(fun pos acc x ->
                 if f pos x then x :: acc else acc)
               ~init:[])

let reduce l ~f = match l with
  | [] -> None
  | hd :: tl -> Some (fold ~init:hd ~f tl)

let reduce_exn l ~f =
  match reduce l ~f with
  | None -> raise (Invalid_argument "List.reduce_exn")
  | Some v -> v

let groupi l ~break =
  let groups =
    foldi l ~init:[] ~f:(fun i acc x ->
      match acc with
      | [] -> [[x]]
      | current_group :: tl ->
        if break i (hd_exn current_group) x then
          [x] :: current_group :: tl  (* start new group *)
        else
          (x :: current_group) :: tl) (* extend current group *)
  in
  match groups with
  | [] -> []
  | l -> rev_map l ~f:rev

let group l ~break = groupi l ~break:(fun _ x y -> break x y)

TEST_MODULE "group" = struct
  TEST = (group [1;2;3;4] ~break:(fun _ x -> x = 3) = [[1;2];[3;4]])

  TEST = (group [] ~break:(fun _ -> assert false)) = []

  let mis = ['M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i']
  let equal_letters =
    [['M'];['i'];['s';'s'];['i'];['s';'s'];['i'];['p';'p'];['i']]
  let single_letters =
    [['M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i']]
  let every_three =
    [['M'; 'i'; 's']; ['s'; 'i'; 's']; ['s'; 'i'; 'p']; ['p'; 'i' ]]

  TEST = (group ~break:(<>) mis) = equal_letters
  TEST = (group ~break:(fun _ _ -> false) mis) = single_letters
  TEST = (groupi ~break:(fun i _ _ -> i mod 3 = 0) mis) = every_three
end

let concat_map l ~f =
  let rec aux acc = function
    | [] -> List.rev acc
    | hd :: tl -> aux (rev_append (f hd) acc) tl
  in
  aux [] l

let concat_mapi l ~f =
  let rec aux cont acc = function
    | [] -> List.rev acc
    | hd :: tl -> aux (cont + 1) (rev_append (f cont hd) acc) tl
  in
  aux 0 [] l

let merge l1 l2 ~cmp =
  let rec loop acc l1 l2 =
    match l1,l2 with
    | [], l2 -> rev_append acc l2
    | l1, [] -> rev_append acc l1
    | h1 :: t1, h2 :: t2 ->
        if cmp h1 h2 <= 0
        then loop (h1 :: acc) t1 l2
        else loop (h2 :: acc) l1 t2
  in
  loop [] l1 l2
;;


include struct
  (* We are explicit about what we import from the general Monad functor so that
   * we don't accidentally rebind more efficient list-specific functions.
   *)
  module Monad = Monad.Make (struct
    type 'a t = 'a list
    let bind x f = concat_map x ~f
    let return x = [x]
  end)
  open Monad
  module Monad_infix = Monad_infix
  let ignore = ignore
  let join = join
  let bind = bind
  let (>>=) = bind
  let return = return
  let all = all
  let all_ignore = all_ignore
end

(** returns final element of list *)
let rec last_exn list = match list with
  | [x] -> x
  | _ :: tl -> last_exn tl
  | [] -> raise (Invalid_argument "Core_list.last")

TEST = last_exn [1;2;3] = 3
TEST = last_exn [1] = 1
TEST = last_exn (Test_values.long1 ()) = 99_999

(** optionally returns final element of list *)
let rec last list = match list with
  | [x] -> Some x
  | _ :: tl -> last tl
  | [] -> None

(* returns list without adjacent duplicates *)
let dedup_without_sorting ?(compare=Pervasives.compare) list =
  let rec loop list accum = match list with
    | [] -> accum
    | hd :: [] -> hd :: accum
    | hd1 :: hd2 :: tl ->
        if compare hd1 hd2 = 0
        then loop (hd2 :: tl) accum
        else loop (hd2 :: tl) (hd1 :: accum)
  in
  loop list []

(** returns sorted version of list with duplicates removed *)
let dedup ?(compare=Pervasives.compare) list =
  let sorted = List.sort ~cmp:(fun x y -> compare y x) list in
  dedup_without_sorting ~compare sorted

TEST = dedup [] = []
TEST = dedup [5;5;5;5;5] = [5]
TEST = length (dedup [2;1;5;3;4]) = 5
TEST = length (dedup [2;3;5;3;4]) = 4
TEST = length (dedup [(0,1);(2,2);(0,2);(4,1)] ~compare:(fun (a,_) (b,_) ->
  Pervasives.compare a b)) = 3
TEST = length (dedup [(0,1);(2,2);(0,2);(4,1)] ~compare:(fun (_,a) (_,b) ->
  Pervasives.compare a b)) = 2

let contains_dup ?compare lst = length (dedup ?compare lst) <> length lst

let find_a_dup ?(compare=Pervasives.compare) l =
  let sorted = List.sort ~cmp:compare l in
  let rec loop l = match l with
      [] | [_] -> None
    | hd1 :: hd2 :: tl ->
      if compare hd1 hd2 = 0 then Some (hd1) else loop (hd2 :: tl)
  in
  loop sorted

TEST = find_a_dup [] = None
TEST = find_a_dup [3] = None
TEST = find_a_dup [3;4] = None
TEST = find_a_dup [3;3] = Some 3
TEST = find_a_dup [3;5;4;6;12] = None
TEST = find_a_dup [3;5;4;5;12] = Some 5
TEST = find_a_dup [3;5;12;5;12] = Some 5
TEST = find_a_dup [(0,1);(2,2);(0,2);(4,1)] = None
TEST = Option.is_some
      (find_a_dup [(0,1);(2,2);(0,2);(4,1)]
         ~compare:(fun (_,a) (_,b) -> Pervasives.compare a b))
TEST = let dup = find_a_dup [(0,1);(2,2);(0,2);(4,1)]
         ~compare:(fun (a,_) (b,_) -> Pervasives.compare a b)
       in
       Option.map dup ~f:fst = Some 0


type sexp_thunk = unit -> Sexplib.Sexp.t
let sexp_of_sexp_thunk x = x ()
exception Duplicate_found of sexp_thunk * string with sexp

let exn_if_dup ?compare ?(context="exn_if_dup") t ~to_sexp =
  Option.iter (find_a_dup ?compare t) ~f:(fun dup ->
    raise (Duplicate_found ((fun () -> to_sexp dup),context))
  )

let count t ~f = Container.fold_count fold t ~f

let init n ~f =
  if n < 0 then invalid_argf "List.init %d" n ();
  let rec loop i accum =
    assert (i >= 0);
    if i = 0 then accum
    else loop (i-1) (f (i-1) :: accum)
  in
  loop n []
;;

let rev_filter_map l ~f =
  let rec loop l accum =
    match l with
    | [] -> accum
    | hd :: tl ->
      match f hd with
      | Some x -> loop tl (x :: accum)
      | None   -> loop tl accum
  in
  loop l []
;;

let filter_map l ~f = List.rev (rev_filter_map l ~f)

TEST = filter_map ~f:(fun x -> Some x) Test_values.l1 = Test_values.l1
TEST = filter_map ~f:(fun x -> Some x) [] = []
TEST = filter_map ~f:(fun _x -> None) [1.;2.;3.] = []
TEST = filter_map
    ~f:(fun x -> if (x > 0) then Some x else None) [1;-1;3] = [1;3]

let rev_filter_mapi l ~f =
  let rec loop i l accum =
    match l with
    | [] -> accum
    | hd :: tl ->
      match f i hd with
      | Some x -> loop (i + 1) tl (x :: accum)
      | None   -> loop (i + 1) tl accum
  in
  loop 0 l []
;;

let filter_mapi l ~f = List.rev (rev_filter_mapi l ~f)
TEST = filter_mapi ~f:(fun _i x -> Some x) Test_values.l1 = Test_values.l1
TEST = filter_mapi ~f:(fun _i x -> Some x) [] = []
TEST = filter_mapi ~f:(fun _i _x -> None) [1.;2.;3.] = []
TEST = filter_mapi ~f:(fun _i x -> if (x > 0) then Some x else None) [1;-1;3]
       = [1;3]
TEST = filter_mapi ~f:(fun i x -> if (i mod 2=0) then Some x else None)
  [1;-1;3] = [1;3]


let filter_opt l = filter_map l ~f:(fun x -> x)

let partition_map t ~f =
  let rec loop t fst snd =
    match t with
    | [] -> (rev fst, rev snd)
    | x :: t ->
      match f x with
      | `Fst y -> loop t (y :: fst) snd
      | `Snd y -> loop t fst (y :: snd)
  in
  loop t [] []
;;

let partition_tf t ~f =
  let f x = if f x then `Fst x else `Snd x in
  partition_map t ~f
;;

module Assoc = struct

  type ('a, 'b) t = ('a * 'b) list with sexp

  let equal x y = compare x y = 0

  let find t ?(equal=equal) key =
    Option.map (find t ~f:(fun (key', _) -> equal key key')) ~f:snd

  let find_exn t ?(equal=equal) key =
    match find t key ~equal with
    | None -> raise Not_found
    | Some value -> value

  let mem t ?(equal=equal) key = Option.is_some (find t ~equal key)

  let remove t ?(equal=equal) key =
    filter t ~f:(fun (key', _) -> not (equal key key'))

  let add t ?(equal=equal) key value =
    (* the remove doesn't change the map semantics, but keeps the list small *)
    (key, value) :: remove t ~equal key

  let inverse t = map t ~f:(fun (x, y) -> (y, x))

  let map t ~f = List.map t ~f:(fun (key, value) -> (key, f value))

end

let sub l ~pos ~len =
  (* We use [pos > length l - len] rather than [pos + len > length l] to avoid the
     possibility of overflow. *)
  if pos < 0 || len < 0 || pos > length l - len then invalid_arg "List.sub";
  List.rev
    (foldi l ~init:[]
       ~f:(fun i acc el ->
             if i >= pos && i < (pos + len)
             then el :: acc
             else acc
          )
    )
;;

let normalize a i =
  Ordered_collection_common.normalize ~length_fun:length a i
let slice a start stop =
  Ordered_collection_common.slice ~length_fun:length ~sub_fun:sub
    a start stop

let split_n t_orig n =
  if n <= 0 then
    ([], t_orig)
  else
    let rec loop n t accum =
      if n = 0 then
        (List.rev accum, t)
      else
        match t with
        | [] -> (t_orig, []) (* in this case, t_orig = List.rev accum *)
        | hd :: tl -> loop (n - 1) tl (hd :: accum)
    in
    loop n t_orig []

TEST = split_n [1;2;3;4;5;6] 3 = ([1;2;3],[4;5;6])
TEST = split_n [1;2;3;4;5;6] 100 = ([1;2;3;4;5;6],[])
TEST = split_n [1;2;3;4;5;6] 0 = ([],[1;2;3;4;5;6])
TEST = split_n [1;2;3;4;5;6] (-5) = ([],[1;2;3;4;5;6])

let take t n = fst (split_n t n)
let drop t n = snd (split_n t n)

let rec drop_while t ~f =
  match t with
  | h :: t when f h -> drop_while t ~f
  | _ -> t

let take_while t ~f =
  let rec loop acc = function
    | hd :: tl when f hd -> loop (hd :: acc) tl
    | _ -> rev acc
  in
  loop [] t
;;

let cartesian_product list1 list2 =
  if list2 = [] then [] else
    let rec loop l1 l2 accum = match l1 with
      | [] -> accum
      | (hd :: tl) ->
          loop tl l2
            (List.rev_append
               (map ~f:(fun x -> (hd,x)) l2)
               accum)
    in
    List.rev (loop list1 list2 [])

let concat l = fold_right l ~init:[] ~f:append
TEST = concat [] = []
TEST = concat [[]] = []
TEST = concat [[3]] = [3]
TEST = concat [[1;2;3;4]] = [1;2;3;4]
TEST = concat
  [[1;2;3;4];[5;6;7];[8;9;10];[];[11;12]]
  = [1;2;3;4;5;6;7;8;9;10;11;12]

let concat_no_order l = fold l ~init:[] ~f:rev_append

let cons x l = x :: l

let is_empty l = match l with [] -> true | _ -> false

let is_sorted l ~compare =
  let rec loop l =
    match l with
    | [] | [_] -> true
    | x1 :: ((x2 :: _) as rest) ->
        compare x1 x2 <= 0 && loop rest
  in loop l

TEST = is_sorted [] ~compare
TEST = is_sorted [1] ~compare
TEST = is_sorted [1; 2; 3; 4] ~compare
TEST = not (is_sorted [2; 1] ~compare)
TEST = not (is_sorted [1; 3; 2] ~compare)

module Infix = struct
  let ( @ ) = append
end

let permute ?(random_state = Random.State.default) list =
  match list with
  (* special cases to speed things up in trivial cases *)
  | [] | [_] -> list
  | [ x; y ] -> if Random.State.bool random_state then [ y; x ] else list
  | _ ->
    let arr = Array.of_list list in
    Array_permute.permute arr ~random_state;
    Array.to_list arr;
;;

let to_string ~f t =
  Sexplib.Sexp.to_string
    (sexp_of_t (fun x -> Sexplib.Sexp.Atom x) (List.map t ~f))
;;

let compare a b ~cmp =
  let rec loop a b =
    match a, b with
    | [], [] -> 0
    | [], _  -> -1
    | _ , [] -> 1
    | x :: xs, y :: ys ->
      let n = cmp x y in
      if n = 0 then loop xs ys
      else n
  in
  loop a b
;;

let equal t1 t2 ~equal =
  let rec loop t1 t2 =
    match t1, t2 with
    | [], [] -> true
    | x1 :: t1, x2 :: t2 -> equal x1 x2 && loop t1 t2
    | _ -> false
  in
  loop t1 t2
;;

let transpose =
  let rec transpose_aux t rev_columns =
    match partition_map t ~f:(function [] -> `Snd () | x :: xs -> `Fst (x, xs)) with
    | (_ :: _, _ :: _) -> None
    | ([], _) -> Some (rev_append rev_columns [])
    | (heads_and_tails, []) ->
      let (column, trimmed_rows) = unzip heads_and_tails in
      transpose_aux trimmed_rows (column :: rev_columns)
  in
  fun t ->
    transpose_aux t []

exception Transpose_got_lists_of_different_lengths of int list with sexp

let transpose_exn l =
  match transpose l with
  | Some l -> l
  | None ->
    raise (Transpose_got_lists_of_different_lengths (List.map l ~f:List.length))

TEST_MODULE "transpose" = struct

  let round_trip a b = transpose a = Some b && transpose b = Some a

  TEST = round_trip [] []

  TEST = transpose [[]] = Some []
  TEST = transpose [[]; []] = Some []
  TEST = transpose [[]; []; []] = Some []

  TEST = round_trip [[1]] [[1]]

  TEST = round_trip [[1];
                     [2]] [[1; 2]]

  TEST = round_trip [[1];
                     [2];
                     [3]] [[1; 2; 3]]

  TEST = round_trip [[1; 2];
                     [3; 4]] [[1; 3];
                              [2; 4]]

  TEST = round_trip [[1; 2; 3];
                     [4; 5; 6]] [[1; 4];
                                 [2; 5];
                                 [3; 6]]

  TEST = transpose [[]; [1]] = None

  TEST = transpose [[1;2];[3]] = None

end
