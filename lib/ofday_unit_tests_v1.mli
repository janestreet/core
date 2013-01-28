(* [unit_tests ~create] creates unit tests of the form expected by
   [Stable_unit_test.Make], i.e.  Ofday.t * sexp-representation * bin-io representation.
   This includes both hand generated tests - a few cases which may be of special interest,
   e.g. the start and end of day - and auto-generated unit tests looking at 10,000
   randomly picked times of day.
*)
val unit_tests
  :  create:(hr:int -> min:int -> sec:int -> ms: int -> us:int -> 'a)
  -> ('a * string * string) list
