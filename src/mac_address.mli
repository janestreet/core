(** Functions for working with and formatting 48-bit MAC addresses. *)

open Core_kernel

(** [private Int63.t] to enable immediate-type optimizations on 64-bit platforms. *)
type t = private Int63.t

(** [of_string] and [t_of_sexp] accept any string-style notation (see [String_style]
    below).  They accept both lowercase and uppercase hex digits.  [to_string] and
    [sexp_of_t] use [Dash] style and lowercase hex digits. *)
include Identifiable   with type t := t
include Quickcheckable with type t := t

(** More significant bytes in the integer go first in the address:
    {v
      0x112233445566 <-> 11-22-33-44-55-66
    v} *)
val of_int_exn : int -> t
val to_int_exn : t -> int

val of_int63_exn : Int63.t -> t
val to_int63     : t -> Int63.t

module String_style : sig
  (** [Dash]-formatted strings look like "xx-xx-xx-xx-xx-xx", where the [x]'s are any hex
      digit.  [Dot]-formatted strings look like "xxxx.xxxx.xxxx".  [Colon]-formatted
      strings look like "xx:xx:xx:xx:xx:xx". *)
  type t = Dash | Dot | Colon [@@deriving compare, sexp, enumerate]

  val arg : t Command.Arg_type.t

  module Stable : sig
    module V1 : Stable_without_comparator with type t := t
  end
end

(** [to_string_with_style] produces the dash-, colon-, or dot-separated string
    representation of the given MAC.  It produces only lowercase hex digits. *)
val to_string_with_style : t -> style:String_style.t -> string

(** [broadcast] is ff-ff-ff-ff-ff-ff *)
val broadcast : t

(** [any] is 00-00-00-00-00-00 *)
val any : t

module Stable : sig
  module V1 : sig
    include Stable_comparable.V1
      with type t = t
       and type comparator_witness = comparator_witness

    include Core_kernel.Core_kernel_stable.Hashable.V1.S
      with type key := t
  end
end
