(* We do not [include Base] here, and instead import modules that [Core] doesn't extend,
   because we want code in [Core] to be clear when it references a [Base] module that
   [Core] is overriding. *)
module Applicative = Base.Applicative
module Avltree = Base.Avltree
module Backtrace = Base.Backtrace
module Binary_search = Base.Binary_search
module Comparisons = Base.Comparisons
module Continue_or_stop = Base.Continue_or_stop
module Equal = Base.Equal
module Exn = Base.Exn
module Floatable = Base.Floatable
module Formatter = Base.Formatter
module Hash = Base.Hash
module Hasher = Base.Hasher
module Intable = Base.Intable
module Int_conversions = Base.Int_conversions
module Int_math = Base.Int_math
module Invariant = Base.Invariant
module Monad = Base.Monad
module Poly = Base.Poly
module Pretty_printer = Base.Pretty_printer
module Random = Base.Random
module Staged = Base.Staged
module Stringable = Base.Stringable
module Sys = Base.Sys
module With_return = Base.With_return
module Word_size = Base.Word_size

(* We do include [Base]'s top-level value and type bindings, because they don't cause
   any confusion, and duplicating them would be error prone. *)
include Base.Export
include Stdio
include Base_for_tests
include Bin_prot.Std
include Stable_witness.Export
module Field = Fieldslib.Field

module From_sexplib : sig
  type bigstring = Sexplib.Conv.bigstring [@@deriving sexp]
  type mat = Sexplib.Conv.mat [@@deriving sexp]
  type vec = Sexplib.Conv.vec [@@deriving sexp]

  (* [sexp_of_opaque] and [opaque_of_sexp] are used by the code generated from
     [[@@deriving sexp]], [[%sexp_of: ]], and [[%of_sexp: ]].  The type [_ sexp_opaque]
     expands to uses of [sexp_of_opaque] and [opaque_of_sexp]. *)

  val sexp_of_opaque : _ -> Base.Sexp.t
  val opaque_of_sexp : Base.Sexp.t -> _
  val sexp_of_pair : ('a -> Base.Sexp.t) -> ('b -> Base.Sexp.t) -> 'a * 'b -> Base.Sexp.t
  val pair_of_sexp : (Base.Sexp.t -> 'a) -> (Base.Sexp.t -> 'b) -> Base.Sexp.t -> 'a * 'b

  exception Of_sexp_error of exn * Base.Sexp.t

  val of_sexp_error : string -> Base.Sexp.t -> _
  val of_sexp_error_exn : exn -> Base.Sexp.t -> _
end =
  Sexplib.Conv

include From_sexplib

(* [sexp_opaque] indicates to [ppx_sexp_conv] that a value should be rendered as [_], i.e.
   [Sexp.Atom "_"].  Here we expose the [@@deriving] aspects of [sexp_opaque] so that
   other ppx's treat [sexp_opaque] correctly, by ignoring it and processing the underlying
   type. *)
include (
  struct
    type 'a sexp_opaque = 'a [@@deriving bin_io, compare, hash, typerep]
  end :
    sig
      type 'a sexp_opaque [@@deriving bin_io, compare, hash, typerep]
    end
    with type 'a sexp_opaque := 'a)

include (
  Typerep_lib.Std :
    module type of struct
      include Typerep_lib.Std
    end
    with module Type_equal := Typerep_lib.Std.Type_equal)

module Variant = Variantslib.Variant

let with_return = With_return.with_return

let am_running_test =
  try
    ignore (Stdlib.Sys.getenv "TESTING_FRAMEWORK" : string);
    true
  with
  (* [Caml.*] never raises [Not_found_s] *)
  | Stdlib.Not_found -> false
;;

type 'a identity = 'a

module Not_found = struct
  exception
    Not_found = Not_found
      [@deprecated
        {|[since 2018-02] Instead of raising [Not_found], consider using [raise_s] with an
informative error message.  If code needs to distinguish [Not_found] from other
exceptions, please change it to handle both [Not_found] and [Not_found_s].  Then, instead
of raising [Not_found], raise [Not_found_s] with an informative error message.|}]

  exception Not_found_s = Base.Not_found_s
end

include Not_found
