open Std_internal

type 'a t = 'a ref = { mutable contents : 'a }

let sexp_of_t sexp_of_a t = sexp_of_a !t

let t_of_sexp a_of_sexp sexp = ref (a_of_sexp sexp)

include Bin_prot.Utils.Make_binable1 (struct
  module Binable = struct
    type 'a t = 'a with bin_io
  end

  type 'a t = 'a ref
  let to_binable t = !t
  let of_binable a = ref a
end)

let create x = ref x

let (!) = Pervasives.(!)
let (:=) = Pervasives.(:=)

let swap t1 t2 =
  let tmp = !t1 in
  t1 := !t2;
  t2 := tmp

let replace t f = t := f !t

(* container functions below *)
let length _ = 1

let is_empty _ = false

let iter t ~f = f !t

let fold t ~init ~f = f init !t

let count t ~f = if f !t then 1 else 0

let exists t ~f = f !t

let for_all t ~f = f !t

let mem ?(equal = (=)) t a = equal a !t

let find t ~f = let a = !t in if f a then Some a else None

let find_map t ~f = f !t

let to_list t = [ !t ]

let to_array t = [| !t |]
