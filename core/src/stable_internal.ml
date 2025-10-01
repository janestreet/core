open! Import
include Bin_prot.Std
include Hash.Builtin
include Stable_witness.Export

include (
  Base :
    sig
    @@ portable
      type nonrec 'a array = 'a array [@@deriving sexp ~stackify, sexp_grammar]
      type nonrec bool = bool [@@deriving sexp ~stackify, sexp_grammar]
      type nonrec char = char [@@deriving sexp ~stackify, sexp_grammar]
      type nonrec exn = exn [@@deriving sexp_of]
      type nonrec float = float [@@deriving sexp ~stackify, sexp_grammar]
      type nonrec int = int [@@deriving sexp ~stackify, sexp_grammar]
      type nonrec int32 = int32 [@@deriving sexp ~stackify, sexp_grammar]
      type nonrec int64 = int64 [@@deriving sexp ~stackify, sexp_grammar]
      type nonrec 'a list = 'a list [@@deriving sexp ~stackify, sexp_grammar]
      type nonrec nativeint = nativeint [@@deriving sexp ~stackify, sexp_grammar]
      type nonrec 'a option = 'a option [@@deriving sexp ~stackify, sexp_grammar]
      type nonrec 'a ref = 'a ref [@@deriving sexp ~stackify, sexp_grammar]
      type nonrec string = string [@@deriving sexp ~stackify, sexp_grammar]
      type nonrec bytes = bytes [@@deriving sexp ~stackify, sexp_grammar]
      type nonrec unit = unit [@@deriving sexp ~stackify, sexp_grammar]
    end
    with type 'a array := 'a array
    with type bool := bool
    with type char := char
    with type exn := exn
    with type float := float
    with type int := int
    with type int32 := int32
    with type int64 := int64
    with type 'a list := 'a list
    with type nativeint := nativeint
    with type 'a option := 'a option
    with type 'a ref := 'a ref
    with type string := string
    with type bytes := bytes
    with type unit := unit)
