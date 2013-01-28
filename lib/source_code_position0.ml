open Bin_prot.Std
open Sexplib.Std

type t = Lexing.position =
  { pos_fname : string;
    pos_lnum : int;
    pos_bol : int;
    pos_cnum : int;
  }
with bin_io, compare, sexp

let to_sexp_hum t =
  <:sexp_of< [ `file of string ] * [ `line of int ] >>
    (`file t.pos_fname, `line t.pos_lnum)
;;

