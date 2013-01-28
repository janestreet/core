open Core.Std
(* open Core_extended.Std *)

module X = Table_new_intf

module Bucket = struct
  (* This saves a word and an indirection vs a list of triples *)
  type ('k, 'v) t =
  | Empty
  | Cons of 'k * 'v * int * ('k, 'v) t

  let empty = Empty

  let remove =
    let rec loop removed compare acc t key =
      match t with
      | Empty -> acc
      | Cons (key', data', hashv', next) ->
          if compare key key' = 0 then begin
            removed := true;
            loop removed compare acc next key
          end else
            loop removed compare
              (Cons (key', data', hashv', next))
              acc key
    in
    fun t removed compare key -> loop removed compare Empty t key
  ;;

  let rec fold bucket ~init ~f =
    match bucket with
    | Empty -> init
    | Cons (key, data, hashv, next) ->
        fold next ~init:(f ~key ~data ~hashv init) ~f
  ;;

  let rec find t compare key =
    (* INRIA uses this recursion unrolling trick in their
       implementation, and it actually works (5 - 10 % improvement) *)
    match t with
    | Empty -> None
    | Cons (key', data, _hval, next) ->
        if compare key key' = 0 then Some data
        else
          match next with
          | Empty -> None
          | Cons (key', data, _hval, next) ->
              if compare key key' = 0 then Some data
              else
                match next with
                | Empty -> None
                | Cons (key', data, _hval, next) ->
                    if compare key key' = 0 then Some data
                    else find next compare key
  ;;

  let cons t key data hashv = Cons (key, data, hashv, t)
end

module T = struct
  type ('k, 'v) t = {
    mutable table: ('k, 'v) Bucket.t array;
    mutable array_length: int;
    mutable length: int;
    mutable params: X.params;
    hashable: 'k X.hashable;
  }

  let create ?(params = X.default_params) hashable =
    let s = Int.min (Int.max 1 params.X.initial_size) Sys.max_array_length in
    { table = Array.create s Bucket.empty;
      array_length = s;
      length = 0;
      params = params;
      hashable = hashable }
  ;;

  let rec add t ~key ~data =
    let hashv = t.hashable.X.hash key in
    let i = hashv mod t.array_length in
    let removed = ref false in
    let bucket = Bucket.remove t.table.(i) removed t.hashable.X.compare key in
    t.table.(i) <- Bucket.cons bucket key data hashv;
    if not !removed then
      t.length <- t.length + 1;
    if t.params.X.grow &&
      t.length > t.array_length * t.params.X.load_factor
    then resize t

  and resize t =
    let new_size =
      Int.min (t.array_length * t.params.X.load_factor) Sys.max_array_length
    in
    if new_size > t.array_length then begin
      let old_table = t.table in
      t.array_length <- new_size;
      t.table <- Array.create t.array_length Bucket.empty;
      let move ~key ~data ~hashv () =
        let i = hashv mod t.array_length in
        t.table.(i) <- Bucket.cons t.table.(i) key data hashv
      in
      for i = 0 to Array.length old_table - 1 do
        Bucket.fold old_table.(i) ~init:() ~f:move
      done
    end
  ;;

  let slot t key = t.hashable.X.hash key mod t.array_length

  let remove t key =
    let i = slot t key in
    let removed = ref false in
    t.table.(i) <- Bucket.remove t.table.(i) removed t.hashable.X.compare key;
    if !removed then
      t.length <- t.length - 1
  ;;

  let find t key =
    Bucket.find t.table.(slot t key) t.hashable.X.compare key
  ;;

  let length t = t.length

  let clear t =
    for i = 0 to t.array_length - 1 do
      t.table.(i) <- Bucket.empty
    done;
    t.length <- 0
  ;;

  let fold =
    (* this is done recursivly to avoid the write barrier in the case
       that the accumulator is a structured block, Array.fold does
       this with a for loop and a ref cell, when it is fixed, we can
       use it. *)
    let rec loop buckets i len init f =
      if i < len then
        loop buckets (i + 1) len (Bucket.fold buckets.(i) ~init ~f) f
      else
        init
    in
    fun t ~init ~f ->
      loop t.table 0 t.array_length init
        (fun ~key ~data ~hashv:_ acc -> f ~key ~data acc)
  ;;

  let invariant t =
    assert ((fold t ~init:0 ~f:(fun ~key:_ ~data:_ l -> l + 1)) = t.length);
    assert (Array.length t.table = t.array_length);
  ;;

  let hashable t = t.hashable
  let get_params t = t.params
  let set_params t params = t.params <- params

  let mem t key =
    match find t key with
    | None -> false
    | Some _ -> true
  ;;
end

include X.Make (T)
