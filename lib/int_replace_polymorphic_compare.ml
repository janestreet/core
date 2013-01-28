type t = int

(* Needed to avoid circular references *)

module P = Pervasives

let (>=) (x:int) y = P.(>=) x y
let (<=) (x:int) y = P.(<=) x y
let (= ) (x:int) y = P.(= ) x y
let (> ) (x:int) y = P.(> ) x y
let (< ) (x:int) y = P.(< ) x y
let (<>) (x:int) y = P.(<>) x y
let equal   (x:int) y = P.(=)     x y
let compare (x:int) y = P.compare x y
let min     (x:int) y = P.min     x y
let max     (x:int) y = P.max     x y

