module Hashtbl = Core_hashtbl
module Array = Core_array
module Int = Core_int
module List = Core_list
module Sexp = Core_sexp
module String = Core_string

let failwithf = Core_printf.failwithf

let num_months = 12

type t = int

let invariant t =
  assert (0 <= t && t < num_months);
;;

let is_valid_month i = 1 <= i && i <= num_months

let of_int i =
  if is_valid_month i then Some (i - 1)
  else None
;;

let of_int_exn i =
  if is_valid_month i then i - 1
  else failwithf "Month.of_int_exn %d" i ()
;;

let to_int t = t + 1

let shift t i = Int.Infix.( % ) (t + i) num_months

let jan = 0
let feb = 1
let mar = 2
let apr = 3
let may = 4
let jun = 5
let jul = 6
let aug = 7
let sep = 8
let oct = 9
let nov = 10
let dec = 11

let all = List.init num_months ~f:Fn.id

type variant = [ `Jan | `Feb | `Mar | `Apr | `May | `Jun
               | `Jul | `Aug | `Sep | `Oct | `Nov | `Dec ]
with sexp_of

let create = function
  | `Jan -> jan
  | `Feb -> feb
  | `Mar -> mar
  | `Apr -> apr
  | `May -> may
  | `Jun -> jun
  | `Jul -> jul
  | `Aug -> aug
  | `Sep -> sep
  | `Oct -> oct
  | `Nov -> nov
  | `Dec -> dec
;;

let all_variants =
  [| `Jan; `Feb; `Mar; `Apr; `May; `Jun; `Jul; `Aug; `Sep; `Oct; `Nov; `Dec |]
;;

let all_strings =
  Array.map all_variants ~f:(fun variant ->
    Sexp.to_string (sexp_of_variant variant))
;;

let get t = all_variants.(t)

let to_string t = all_strings.(t)

let of_string =
  let table = lazy (
    let module T = String.Table in
    let table = T.create ~size:num_months () in
    Array.iteri all_strings ~f:(fun t s ->
      Hashtbl.replace table ~key:s ~data:t;
      Hashtbl.replace table ~key:(String.lowercase s) ~data:t;
      Hashtbl.replace table ~key:(String.uppercase s) ~data:t);
    table)
  in
  fun str ->
    match Hashtbl.find (Lazy.force table) str with
    | Some x -> x
    | None   -> failwithf "Invalid month: %s" str ()
;;

include (Sexpable.Of_stringable (struct
  type t = int
  let of_string = of_string
  let to_string = to_string
end) : Sexpable.S with type t := t)

include (Int : sig
  include Binable.S with type t := t
  include Comparable.S with type t := t
  include Hashable.S with type t := t
end)
