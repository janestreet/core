open Interfaces
open Sexplib

include Sexpable with type t := Sexp.t
include Stringable with type t := Sexp.t
include Binable with type t := Sexp.t

include Sexp_intf.S

val of_int_style : [ `Underscores | `No_underscores ] ref

(** [no_raise] is the identity, but by using ['a no_raise] in a sexpable type, the
    resulting use [sexp_of_no_raise] protects the conversion of ['a] to a sexp so that if
    it fails, one gets a sexp with an error message about the failure, rather than an
    exception being raised.

    The resulting [no_raise_of_sexp] can still raise. *)
type 'a no_raise = 'a with bin_io, sexp

(* Please refer to the Sexplib documentation in base/sexplib/doc to learn
   more about sexp_option, sexp_list, and sexp_array generators. *)

(* The purpose of these modules is to allow bin_io to work with these special
   sexp types.  The more direct method of adding "with bin_io" at the point of
   the initial declaration of the types is not possible because sexplib does not
   (should not) depend on bin_io. *)
module Sexp_option : sig
  open Bin_prot

  val bin_size_sexp_option   : ('a, 'a option) Size.sizer1
  val bin_write_sexp_option  : ('a, 'a option) Map_to_safe.writer1
  val bin_write_sexp_option_ : ('a, 'a option) Unsafe_write_c.writer1
  val bin_read_sexp_option   : ('a, 'a option) Map_to_safe.reader1
  val bin_read_sexp_option_  : ('a, 'a option) Unsafe_read_c.reader1
  val bin_read_sexp_option__ : ('a, int ->     'a option) Unsafe_read_c.reader1
  val bin_writer_sexp_option : ('a, 'a option) Type_class.S1.writer
  val bin_reader_sexp_option : ('a, 'a option) Type_class.S1.reader
  val bin_sexp_option        : ('a, 'a option) Type_class.S1.t
end

module Sexp_list : sig
  open Bin_prot

  val bin_size_sexp_list   : ('a, 'a list) Size.sizer1
  val bin_write_sexp_list  : ('a, 'a list) Map_to_safe.writer1
  val bin_write_sexp_list_ : ('a, 'a list) Unsafe_write_c.writer1
  val bin_read_sexp_list   : ('a, 'a list) Map_to_safe.reader1
  val bin_read_sexp_list_  : ('a, 'a list) Unsafe_read_c.reader1
  val bin_read_sexp_list__ : ('a, int ->   'a list) Unsafe_read_c.reader1
  val bin_writer_sexp_list : ('a, 'a list) Type_class.S1.writer
  val bin_reader_sexp_list : ('a, 'a list) Type_class.S1.reader
  val bin_sexp_list        : ('a, 'a list) Type_class.S1.t
end

module Sexp_array : sig
  open Bin_prot

  val bin_size_sexp_array   : ('a, 'a array) Size.sizer1
  val bin_write_sexp_array  : ('a, 'a array) Map_to_safe.writer1
  val bin_write_sexp_array_ : ('a, 'a array) Unsafe_write_c.writer1
  val bin_read_sexp_array   : ('a, 'a array) Map_to_safe.reader1
  val bin_read_sexp_array_  : ('a, 'a array) Unsafe_read_c.reader1
  val bin_read_sexp_array__ : ('a, int ->    'a array) Unsafe_read_c.reader1
  val bin_writer_sexp_array : ('a, 'a array) Type_class.S1.writer
  val bin_reader_sexp_array : ('a, 'a array) Type_class.S1.reader
  val bin_sexp_array        : ('a, 'a array) Type_class.S1.t
end

module Sexp_opaque : sig
  open Bin_prot

  val bin_size_sexp_opaque   : ('a, 'a)  Size.sizer1
  val bin_write_sexp_opaque  : ('a, 'a)  Map_to_safe.writer1
  val bin_write_sexp_opaque_ : ('a, 'a)  Unsafe_write_c.writer1
  val bin_read_sexp_opaque   : ('a, 'a)  Map_to_safe.reader1
  val bin_read_sexp_opaque_  : ('a, 'a)  Unsafe_read_c.reader1
  val bin_read_sexp_opaque__ : ('a, int -> 'a) Unsafe_read_c.reader1
  val bin_writer_sexp_opaque : ('a, 'a)  Type_class.S1.writer
  val bin_reader_sexp_opaque : ('a, 'a)  Type_class.S1.reader
  val bin_sexp_opaque        : ('a, 'a)  Type_class.S1.t
end

(* A type to use when you want a sexpable type as part of a larger sexpable
   type, and if the sexp parsing fails in the small part, the whole thing
   doesn't fail; you just get an Error there.  It doesn't quite round-trip
   cleanly in the case of Error, but at least it stays an Error. *)
module Sexp_maybe : sig
  type 'a t = ('a, Sexp.t) Result.t

  include Sexpable.S1 with type 'a t := 'a t
  include Binable.S1  with type 'a t := 'a t
end
