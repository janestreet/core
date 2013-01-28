
open Std_internal


let rec foldn ~f ~init:acc i =
  if i = 0 then acc else foldn ~f ~init:(f acc i) (i-1)

let sum_int = List.fold ~f:(+) ~init:0

type 'a gen = unit -> 'a

let pfg () = exp (Random.float 30. -. 15.)

let fg () =
  pfg () *. (if Random.bool () then 1. else -1.)

let nng () =
  let p = Random.float 1. in
  if p < 0.5 then Random.int 10
  else if p < 0.75 then Random.int 100
  else if p < 0.95 then Random.int 1_000
  else Random.int 10_000

(* Below uniform random in range min_int, min_int+1,...,max_int.  Here's why:
 *   bound = max_int + 1
 *   0 <= r <= max_int
 *   0 <= r <= max_int  &&  -max_int -1 <= -r - 1 <= -1
 *   -max_int -1 <= result <= max_int
 *   min_int <= result <= max_int
 *)
let uig =
  let bound = Int64.(+) 1L (Int64.of_int max_int) in
  fun () ->
    let r = Int64.to_int_exn (Random.int64 bound) in
    if Random.bool () then r else -r - 1

let lg gen ?(size_gen=nng) () =
  foldn ~f:(fun acc _ -> (gen ())::acc) ~init:[] (size_gen ())

let pg gen1 gen2 () = (gen1 (), gen2 ())

let tg g1 g2 g3 () = (g1 (),g2 (), g3 ())

let cg () = char_of_int (Random.int 256)

let sg ?(char_gen = cg) ?(size_gen = nng) () =
  let s = String.create (size_gen ()) in
  for i = 0 to String.length s - 1 do
    s.[i] <- char_gen ()
  done;
  s

let oneof xs =
  List.nth_exn xs (Random.int (List.length xs))

let always x () = x

let frequency xs =
  let sums = sum_int (List.map ~f:fst xs) in
  let i = Random.int sums in
  let rec aux acc = function
    | ((x,g)::xs) -> if i < acc+x then g else aux (acc+x) xs
    | _ -> failwith "frequency"
  in
  aux 0 xs

let frequencyl l = frequency (List.map ~f:(fun (i,e) -> (i,always e)) l)

let rec laws iter gen func =
  if iter <= 0 then None
  else
    let input = gen () in
    try
      if not (func input) then Some input
      else laws (iter-1) gen func
    with _ -> Some input

let laws_exn name iter gen func =
  match laws iter gen func with
    None -> ()
  | Some _ -> failwith (Printf.sprintf "law %s failed" name)

let repeat times test gen =
  for i = 1 to times do test (gen()) done
