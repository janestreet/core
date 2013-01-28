(* An interface for creating unit tests to check stability of sexp and bin-io
   serializations *)

module type Arg = sig
  type t with sexp, bin_io
  val equal : t -> t -> bool
  (* [tests] is a list of (value, sexp-representation, bin-io-representation) triples.
     The ounit tests created by this functor check that the type properly serializes and
     de-serializes according to the given representations. *)
  val tests : (t * string * string) list
end

module type S = sig
  val ounit_tests : unit -> OUnit.test
end
