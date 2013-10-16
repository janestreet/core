open Core.Std
open Core_bench.Std

(** A faster alternative implementation of heap. *)
module Array_heap = struct
  type 'a t = {
    a  : 'a array;
    is : int array;
    l  : int;
    le : 'a -> 'a -> bool;
  }

  let create ~le elements =
    let a = List.to_array elements in
    let l = Array.length a in
    let is = Array.init l ~f:(fun i -> i) in
    Array.sort is ~cmp:(fun i i' -> if le a.(i) a.(i') then -1 else 1);
    { a; is; l; le }

  let top t = Array.unsafe_get t.a (Array.unsafe_get t.is 0)

  let swap t i j =
    let is = t.is in
    let e = Array.unsafe_get is i in
    Array.unsafe_set is i (Array.unsafe_get is j);
    Array.unsafe_set is j e

  let update t =
    let rec loop t i l =
      let ir = (i + 1) lsl 1 in
      let il = ir - 1 in
      let a = t.a in
      let is = t.is in
      if ir < l then begin
        let e  = Array.unsafe_get a (Array.unsafe_get is i) in
        let el = Array.unsafe_get a (Array.unsafe_get is il) in
        let er = Array.unsafe_get a (Array.unsafe_get is ir) in
        let le = t.le in
        if le e el then begin
          if not (le e er) then begin
            swap t i ir;
            loop t ir l
          end
        end else begin
          if le e er then begin
            swap t i il;
            loop t il l
          end else begin
            let is = if le el er then il else ir in
            swap t i is;
            loop t is l
          end
        end
      end else if il < t.l then begin
        let e  = Array.unsafe_get a (Array.unsafe_get is i) in
        let el = Array.unsafe_get a (Array.unsafe_get is il) in
        if not (t.le e el) then
          swap t i il
      end
    in
    loop t 0 t.l
end

let () =
  let r = Array.init 1024 ~f:(fun _ -> Random.int 1024) in
  let ah = Array_heap.create ~le:(fun x y -> !x < !y)
    (Array.map r ~f:(fun x -> ref x) |> Array.to_list)
  in
  let array_heap_test = Bench.Test.create ~name:"Array_heap" (fun () ->
    let r = !(ref r) in
    let ah = !(ref ah) in
    for i = 0 to Array.length r - 1 do
      let top = Array_heap.top ah in
      top := !top + Array.unsafe_get r i;
      Array_heap.update ah
    done)
  in
  let h = Heap.create ~cmp:(fun x y -> Int.compare !x !y) () in
  for i = 0 to Array.length r - 1 do
    Heap.add h (ref r.(i));
  done;
  let heap_test = Bench.Test.create ~name:"Heap" (fun () ->
    let r = !(ref r) in
    let h = !(ref h) in
    for i = 0 to Array.length r - 1 do
      let top = Heap.pop_exn h in
      top := !top + Array.unsafe_get r i;
      Heap.add h top
    done)
  in
  Bench.bench ~time_quota:(Time.Span.of_sec 1.) [array_heap_test; heap_test]
