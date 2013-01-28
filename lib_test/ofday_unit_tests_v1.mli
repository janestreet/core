(* [test] includes both hand generated tests - a few cases which may be of special
   interest, e.g. the start and end of day - and auto-generated unit tests looking at
   10,000 randomly picked times of day. *)
val test : unit -> OUnit.test
