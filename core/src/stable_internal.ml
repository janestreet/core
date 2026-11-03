open! Import
include Bin_prot.Std
include Hash.Builtin
include Stable_witness.Export

include (
  Base :
  sig
    [%%rederive: type nonrec 'a array = 'a array [@@deriving sexp, sexp_grammar]]
    [%%rederive: type nonrec bool = bool [@@deriving sexp ~unboxed, sexp_grammar]]
    [%%rederive: type nonrec char = char [@@deriving sexp ~unboxed, sexp_grammar]]
    [%%rederive: type nonrec exn = exn [@@deriving sexp_of]]
    [%%rederive: type nonrec float = float [@@deriving sexp ~unboxed, sexp_grammar]]
    [%%rederive: type nonrec int = int [@@deriving sexp, sexp_grammar]]
    [%%rederive: type nonrec int32 = int32 [@@deriving sexp, sexp_grammar]]
    [%%rederive: type nonrec int64 = int64 [@@deriving sexp, sexp_grammar]]
    [%%rederive: type nonrec 'a list = 'a list [@@deriving sexp, sexp_grammar]]
    [%%rederive: type nonrec nativeint = nativeint [@@deriving sexp, sexp_grammar]]
    [%%rederive: type nonrec 'a option = 'a option [@@deriving sexp, sexp_grammar]]
    [%%rederive: type nonrec 'a ref = 'a ref [@@deriving sexp, sexp_grammar]]
    [%%rederive: type nonrec string = string [@@deriving sexp, sexp_grammar]]
    [%%rederive: type nonrec bytes = bytes [@@deriving sexp, sexp_grammar]]
    [%%rederive: type nonrec unit = unit [@@deriving sexp ~unboxed, sexp_grammar]]

    [%%rederive:
      type 'a or_null = 'a Base.Or_null.t [@@deriving sexp] [@@or_null_reexport]]
  end)
