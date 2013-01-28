open Core.Std
open Core_extended.Std

module X = Table_new_intf

(* Cache efficient seperate chaining using arrays instead of lists for
   hashtbl buckets. The idea is that when doing a lookup the entire
   bucket is in the same cache line, so iterating through it should be
   very very fast. In reality, caml (and generalized keys), require
   some indirection in the bucket. Still, the spine should be in the
   same cache line, meaning that at least we've removed one level of
   indirection compared to a list. Another (perhaps serious) problem
   is that in order to avoid adding yet more indirection we must pre
   allocate the bucket arrays (or be forced to make them an option,
   and add yet more indirection). This means that the GC must scan
   them, since they are blocks.

   If we were willing to use Obj.magic this situation could be
   significantly improved.
*)
  
module T = struct
  type ('k, 'v) t = {
    mutable table : ('k * 'v) option array array;
    mutable array_length: int;
    mutable length : int;
    mutable params : X.params;
    hashable: 'k X.hashable;
  }


  let mk_array size = Array.init size ~f:(fun _ -> Array.create 1 None)
    
  let create ?(params = X.default_params) hashable =
    let s = Int.min (Int.max 1 params.X.initial_size) Sys.max_array_length in
    { table = mk_array s;
      array_length = s;
      length = 0;
      params = params;
      hashable = hashable; }

  let invariant _t = ()

  let hashable t = t.hashable
    
  let slot t key = t.hashable.X.hash key mod t.array_length

  let really_add t ~key ~data =
    let slot = slot t key in
    let bucket = t.table.(slot) in
    let bucket_len = Array.length bucket in
    let added = ref true in
    let i = ref 0 in
    while (* find our bucket with no extra function calls! *)
      (!i < bucket_len &&
          match bucket.(!i) with
          | None -> false
          | Some (k, _) ->
              let c = t.hashable.X.compare k key in
              if c = 0 then begin
                added := false;
                false
              end else
                true)
    do
      incr i
    done;
    if !added then
      t.length <- t.length + 1;
    if !i < bucket_len then
      bucket.(!i) <- Some (key, data)
    else begin
      (* we reached the end of the array without finding a
         suitable bucket, so we must grow the array *)
      let new_bucket =
        Array.init (2 * bucket_len) ~f:(fun i ->
          if i < bucket_len then bucket.(i)
          else None)
      in
      new_bucket.(bucket_len) <- Some (key, data);
      t.table.(slot) <- new_bucket
    end
      
  let maybe_resize_table t =
    if t.params.X.grow &&
      t.length > t.array_length * t.params.X.load_factor
    then begin
      let new_array_length = t.array_length * t.params.X.load_factor in
      let new_table = mk_array new_array_length in
      let old_table = t.table in
      t.array_length <- new_array_length;
      t.table <- new_table;
      t.length <- 0;
      for i = 0 to Array.length old_table - 1 do
        Array.iter old_table.(i) ~f:(function
          | None -> ()
          | Some (key, data) -> really_add t ~key ~data)
      done
    end

  let add t ~key ~data =
    maybe_resize_table t;
    really_add t ~key ~data

  let clear t =
    for i = 0 to t.array_length do
      t.table.(i) <- Array.create 3 None
    done

  let find_bucket t key =
    let bucket = t.table.(slot t key) in
    let bucket_len = Array.length bucket in
    (* int refs don't hit the write barrier *)
    let i = ref 0 in
    (* while loop avoids function calls *)
    while
      (!i < bucket_len &&
          match bucket.(!i) with
          | None -> true
          | Some (k, _) -> t.hashable.X.compare k key = 0)
    do
      incr i
    done;
    if !i < bucket_len then
      Some (!i, bucket)
    else
      None
    
  let find t key =
    match find_bucket t key with
    | None -> None
    | Some (i, bucket) ->
        match bucket.(i) with
        | None -> None
        | Some (_, v) -> Some v

  let mem t key =
    match find_bucket t key with
    | None -> false
    | Some _ -> true
    
  let remove t key =
    match find_bucket t key with
    | None -> ()
    | Some (i, bucket) ->
        bucket.(i) <- None

  let get_params t = t.params
  let set_params t p = t.params <- p
          
  let length t = t.length

  let fold t ~init ~f =
    Array.fold t.table ~init ~f:(fun init b ->
      Array.fold b ~init ~f:(fun init x ->
        match x with
        | None -> init
        | Some (key, data) -> f ~key ~data init))
end

include X.Make (T)
