open Std_internal

type 'a t = 'a with sexp_of

external is_heap_block : Obj.t -> bool = "core_heap_block_is_heap_block" "noalloc"

let is_ok v = is_heap_block (Obj.repr v)

let create v = if is_ok v then Some v else None

let create_exn v =
  if is_ok v
  then v
  else failwith "Heap_block.create_exn called with non heap block"
;;

let value t = t

let bytes_per_word = Word_size.(num_bits word_size) / 8

let bytes (type a) (t : a t) = (Obj.size (Obj.repr (t : a t)) + 1) * bytes_per_word

TEST_UNIT =
  assert (create 13 = None);
  begin match Sys.execution_mode () with
  | `Bytecode -> ()
  | `Native   -> assert (create "foo" = None)
  end;
  let string = String.concat [ "foo"; "bar" ] in
  let t =
    match create string with
    | None -> assert false
    | Some t -> t
  in
  assert (phys_equal (Obj.magic t : string) string);
;;
