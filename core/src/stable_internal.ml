open! Import
include Bin_prot.Std
include Hash.Builtin
include Stable_witness.Export

include (
  Base :
  sig
  @@ portable
    [%%rederive:
      type nonrec 'a array = 'a array [@@deriving sexp ~stackify, sexp_grammar]]

    [%%rederive: type nonrec bool = bool [@@deriving sexp ~stackify, sexp_grammar]]
    [%%rederive: type nonrec char = char [@@deriving sexp ~stackify, sexp_grammar]]
    [%%rederive: type nonrec exn = exn [@@deriving sexp_of]]
    [%%rederive: type nonrec float = float [@@deriving sexp ~stackify, sexp_grammar]]
    [%%rederive: type nonrec int = int [@@deriving sexp ~stackify, sexp_grammar]]
    [%%rederive: type nonrec int32 = int32 [@@deriving sexp ~stackify, sexp_grammar]]
    [%%rederive: type nonrec int64 = int64 [@@deriving sexp ~stackify, sexp_grammar]]
    [%%rederive: type nonrec 'a list = 'a list [@@deriving sexp ~stackify, sexp_grammar]]

    [%%rederive:
      type nonrec nativeint = nativeint [@@deriving sexp ~stackify, sexp_grammar]]

    [%%rederive:
      type nonrec 'a option = 'a option [@@deriving sexp ~stackify, sexp_grammar]]

    [%%rederive: type nonrec 'a ref = 'a ref [@@deriving sexp ~stackify, sexp_grammar]]
    [%%rederive: type nonrec string = string [@@deriving sexp ~stackify, sexp_grammar]]
    [%%rederive: type nonrec bytes = bytes [@@deriving sexp ~stackify, sexp_grammar]]
    [%%rederive: type nonrec unit = unit [@@deriving sexp ~stackify, sexp_grammar]]

    [%%rederive:
      type 'a or_null = 'a Base.Or_null.t [@@deriving sexp ~stackify] [@@or_null_reexport]]
  end)
