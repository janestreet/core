(* A substring is a contiguous sequence of characters in a string.  We use a
   functor because we want substrings of [string] and [bigstring].
*)

open! Import
open Std_internal
include Make_substring_intf

type bigstring = Bigstring.t

module Blit = struct
  type ('src, 'dst) t = ('src, 'dst) Blit.blito

  let string_bytes ~src ?src_pos ?src_len ~dst ?(dst_pos = 0) () =
    let src_pos, len =
      Ordered_collection_common.get_pos_len_exn
        ()
        ?pos:src_pos
        ?len:src_len
        ~total_length:(String.length src)
    in
    Bytes.From_string.blit ~src ~src_pos ~len ~dst ~dst_pos
  ;;

  let string_string = string_bytes
  let bytes_bytes = Bytes.blito
  let string_bigstring = Bigstring.From_string.blito
  let bytes_bigstring = Bigstring.From_bytes.blito
  let bigstring_bigstring = Bigstring.blito
  let bigstring_string = Bigstring.To_bytes.blito
  let bigstring_bytes = Bigstring.To_bytes.blito
end

(* We can't call the base module [Base] because [@@deriving quickcheck] wants to access
   the [Base] library directly, and we'd be shadowing it. *)
module F (Underlying : Base) : S with type base = Underlying.t = struct
  type base = Underlying.t

  type t =
    { base : Underlying.t
    ; pos : int
    ; len : int
    }
  [@@deriving quickcheck]

  (* note we override the generated [quickcheck_generator] below, once we've defined
     [create] *)

  (* {[
       let invariant t =
         assert (0 <= t.pos);
         assert (0 <= t.len);
         assert (t.pos + t.len <= Base.length t.base);
       ;; ]} *)

  let base t = t.base
  let pos t = t.pos
  let length t = t.len
  let is_empty t = Int.equal t.len 0

  let base_of_string s =
    let len = String.length s in
    let buf = Underlying.create len in
    Underlying.blit_from_string ~src:s ~dst:buf ();
    buf
  ;;

  let base_of_bigstring s =
    let len = Bigstring.length s in
    let buf = Underlying.create len in
    Underlying.blit_from_bigstring ~src:s ~dst:buf ();
    buf
  ;;

  let create ?pos ?len base =
    let pos, len =
      Ordered_collection_common.get_pos_len_exn
        ()
        ?pos
        ?len
        ~total_length:(Underlying.length base)
    in
    { base; pos; len }
  ;;

  let quickcheck_generator =
    let open Quickcheck.Let_syntax in
    let%bind base = Underlying.quickcheck_generator in
    let base_len = Underlying.length base in
    let%bind len = Int.gen_uniform_incl 0 base_len in
    let%bind pos = Int.gen_uniform_incl 0 (base_len - len) in
    return (create ~pos ~len base)
  ;;

  let get_no_bounds_check t i = Underlying.get (base t) (pos t + i)

  let get t i =
    if i >= 0 && i < length t
    then get_no_bounds_check t i
    else raise (Invalid_argument "index out of bounds")
  ;;

  let sub ?pos ?len t =
    let pos, len =
      Ordered_collection_common.get_pos_len_exn () ?pos ?len ~total_length:(length t)
    in
    { base = t.base; pos = t.pos + pos; len }
  ;;

  module Make_arg = struct
    type nonrec t = t

    module Elt = Char

    let fold t ~init ~f =
      let rec go acc i = if i >= length t then acc else go (f acc (get t i)) (i + 1) in
      go init 0 [@nontail]
    ;;

    let iter =
      `Custom
        (fun t ~f ->
          for i = 0 to length t - 1 do
            f (get t i)
          done)
    ;;

    let foldi =
      `Custom
        (fun t ~init ~f ->
          let rec go acc i =
            if i >= length t then acc else go (f i acc (get_no_bounds_check t i)) (i + 1)
          in
          go init 0 [@nontail])
    ;;

    let iteri =
      `Custom
        (fun t ~f ->
          for i = 0 to length t - 1 do
            f i (get_no_bounds_check t i)
          done)
    ;;

    let length = `Custom length
  end

  module C = Indexed_container.Make0 (Make_arg)

  let fold = C.fold
  let iter = C.iter
  let fold_result = C.fold_result
  let fold_until = C.fold_until

  (* [C.to_list] has to construct then reverse the list *)
  let to_list t = List.init (length t) ~f:(get t)
  let to_array = C.to_array
  let find_map = C.find_map
  let find = C.find
  let exists = C.exists
  let for_all = C.for_all
  let mem = C.mem
  let count = C.count
  let sum = C.sum
  let min_elt = C.min_elt
  let max_elt = C.max_elt
  let foldi = C.foldi
  let iteri = C.iteri
  let existsi = C.existsi
  let for_alli = C.for_alli
  let counti = C.counti
  let findi = C.findi
  let find_mapi = C.find_mapi

  let wrap_sub_n t n ~name ~pos ~len ~on_error =
    if n < 0
    then invalid_arg (name ^ " expecting nonnegative argument")
    else (
      try sub t ~pos ~len with
      | _ -> on_error)
  ;;

  let drop_prefix t n =
    wrap_sub_n
      ~name:"drop_prefix"
      t
      n
      ~pos:n
      ~len:(length t - n)
      ~on_error:{ t with len = 0 }
  ;;

  let drop_suffix t n =
    wrap_sub_n
      ~name:"drop_suffix"
      t
      n
      ~pos:0
      ~len:(length t - n)
      ~on_error:{ t with len = 0 }
  ;;

  let prefix t n = wrap_sub_n ~name:"prefix" t n ~pos:0 ~len:n ~on_error:t
  let suffix t n = wrap_sub_n ~name:"suffix" t n ~pos:(length t - n) ~len:n ~on_error:t

  let blit_to (type a) (blit : (Underlying.t, a) Blit.t) t ~dst ~dst_pos =
    blit ~src:t.base ~src_pos:t.pos ~src_len:t.len ~dst ~dst_pos ()
  ;;

  let blit_to_string = blit_to Underlying.blit_to_bytes
  let blit_to_bytes = blit_to Underlying.blit_to_bytes
  let blit_to_bigstring = blit_to Underlying.blit_to_bigstring
  let blit_base = blit_to Underlying.blit

  let blit_from ~name (type a) (blit : (a, base) Blit.t) t ~src ~src_pos ~len =
    if len > t.len
    then
      failwithf
        "Substring.blit_from_%s len > substring length : %d > %d"
        name
        len
        t.len
        ();
    blit ~src ~src_pos ~src_len:len ~dst:t.base ~dst_pos:t.pos ()
  ;;

  let blit_from_string = blit_from ~name:"string" Underlying.blit_from_string
  let blit_from_bigstring = blit_from ~name:"bigstring" Underlying.blit_from_bigstring
  let of_base base = { base; pos = 0; len = Underlying.length base }
  let of_string x = of_base (base_of_string x)
  let of_bigstring x = of_base (base_of_bigstring x)

  let make (type a) create (blit : (base, a) Blit.t) t =
    let dst = create t.len in
    blit ~src:t.base ~src_pos:t.pos ~src_len:t.len ~dst ~dst_pos:0 ();
    dst
  ;;

  let to_string x =
    Bytes.unsafe_to_string
      ~no_mutation_while_string_reachable:(make Bytes.create Underlying.blit_to_bytes x)
  ;;

  let to_bigstring = make Bigstring.create Underlying.blit_to_bigstring
  let sexp_of_t x = Sexp.Atom (to_string x)

  let concat_gen create_dst blit_dst ts =
    let len = List.fold ts ~init:0 ~f:(fun len t -> len + length t) in
    let dst = create_dst len in
    ignore
      (List.fold ts ~init:0 ~f:(fun dst_pos t ->
         blit_dst t ~dst ~dst_pos;
         dst_pos + length t)
        : int);
    dst
  ;;

  let concat ts = of_base (concat_gen Underlying.create blit_base ts)

  let concat_string ts =
    Bytes.unsafe_to_string
      ~no_mutation_while_string_reachable:(concat_gen Bytes.create blit_to_string ts)
  ;;

  let concat_bigstring ts = concat_gen Bigstring.create blit_to_bigstring ts
end
