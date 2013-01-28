open Std_internal

module type Key = sig
  type t with bin_io, sexp
  include Floatable with type t := t
end

module type S = sig
  type key

  type t with bin_io, sexp

  val create : (key * float) list -> (t, string) Result.t
  val get : t -> key -> float
  val to_knots : t -> (key * float) list
end

module Make (Key : Key) = struct

  (* if long lists are needed, consider rewriting with binary search in array *)
  type t = (float * float) list with bin_io

  type key = Key.t

  let create knots =
    let t = List.map knots ~f:(fun (x, y) -> (Key.to_float x, y)) in
    let x_values = List.map t ~f:fst in
    if t = [] then
      Error "no knots given"
    else if not (List.is_sorted x_values ~compare:Float.compare) then
      Error "knots are unsorted"
    else if List.exists t ~f:(fun (x, y) -> Float.is_nan x || Float.is_nan y) then
      Error "knots contain nan"
    else
      Ok t

  type knots = (Key.t * float) list with sexp

  let to_knots t =
    List.map t ~f:(fun (x, y) -> (Key.of_float x, y))

  let t_of_sexp sexp =
    let knots = knots_of_sexp sexp in
    match create knots with
    | Error str -> Sexplib.Conv.of_sexp_error str sexp
    | Ok t -> t

  let sexp_of_t t =
    sexp_of_knots (to_knots t)

  let linear x (x1, y1) (x2, y2) =
    let weight = (x -. x1) /. (x2 -. x1) in (* note: numerically unstable if x2=.x1 *)
    let weight = Float.max_inan 0. (Float.min_inan 1. weight) in
    (1. -. weight) *. y1 +. weight *. y2

  let get t x =
    let x = Key.to_float x in
    if Float.is_nan x
    then invalid_arg "Piecewise_linear.get on nan";
    let rec loop = function
      | left :: ((right :: _) as rest) ->
          if x <= fst left then snd left
          else if x <= fst right then linear x left right
          else loop rest
      | last :: [] -> snd last
      | [] -> failwith "Bug in Piecewise_linear.get"
    in
    loop t

end

module Time = Make (Time)
module Ofday = Make (Ofday)
module Span = Make (Span)
module Float = Make (Float)
module Int = Make (Int)
