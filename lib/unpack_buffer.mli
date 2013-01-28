(* An [Unpack_buffer.t] is a buffer to which one can [feed] strings, and then [unpack] from
   the buffer to produce a queue of values. *)
open Std_internal

module Unpack_one : sig
  (** If [unpack_one : ('value, 'partial_unpack) unpack_one], then [unpack_one buf ?pos
      ?len ?partial_unpack] must unpack at most one value of type ['value] from [buf]
      starting at [pos], and not using more than [len] characters.  [unpack_one] must
      returns one the following:

      `Ok (value, n) -- unpacking succeeded and consumed [n] bytes
      `Not_enough_data (p, n) -- unpacking encountered a valid proper prefix of a packed
      value, and consumed [n] bytes (0 <= n <= len).  [p] is a "partial unpack" that can
      be supplied to a future call to [unpack_one] to continue unpacking
      `Invalid_data -- unpacking encountered an invalidly packed value

      A naive [unpack_one] that only succeeds on a fully packed value could lead to
      quadratic behavior if a packed value's bytes are input using a linear number of
      calls to [feed]. *)

  type ('value, 'partial_unpack) t =
    ?partial_unpack:'partial_unpack
    -> ?pos:int
    -> ?len:int
    -> Bigstring.t
    -> [ `Ok of 'value * int
       | `Not_enough_data of 'partial_unpack * int
       | `Invalid_data of Error.t
       ]

  val map : ('a, 'partial_unpack) t -> f:('a -> 'b) -> ('b, 'partial_unpack) t

  (** [create_bin_prot reader] returns an unpacker that reads the "size-prefixed" bin_prot
      encoding, in which a value is encoded by first writing the length of the bin_prot
      data as a 64-bit int, and then writing the data itself.  This encoding makes it
      trivial to know if enough data is available in the buffer, so there is no need to
      represent partially unpacked values, and hence ['partial_unpack = unit]. *)
  val create_bin_prot : 'a Bin_prot.Type_class.reader -> ('a, unit) t
end

(* Note that [t] doesn't expose its type ['partial_unpack].  It is only here to allow the
   chosen implementation strategy in the .ml. *)
type ('value, 'partial_unpack) t with sexp_of

val invariant : (_, _) t -> unit

val create
  :  ?partial_unpack:'partial_unpack
  -> ('value, 'partial_unpack) Unpack_one.t
  -> ('value, 'partial_unpack) t

(** [create_bin_prot reader] returns an unpack buffer that unpacks the "size-prefixed"
    bin_prot encoding, in which a value is encoded by first writing the length of the
    bin_prot data as a 64-bit int, and then writing the bin_prot data itself.  This
    encoding makes it trivial to know if enough data is available in the buffer, so there
    is no need to represent partially unpacked values, and hence ['partial_unpack =
    unit]. *)
val create_bin_prot : 'a Bin_prot.Type_class.reader -> ('a, unit) t

(** [is_empty t] returns [true] if [t] has no unconsumed bytes, and [false] if it does.
    [is_empty] returns an error if [t] has encountered an unpacking error. *)
val is_empty : (_, _) t -> bool Or_error.t

(** [feed t buf ?pos ?len] adds the specified substring of [buf] to [t]'s buffer.  It
    returns an error if [t] has encountered an unpacking error. *)
val feed        : ?pos:int -> ?len:int -> (_, _) t -> Bigstring.t -> unit Or_error.t
val feed_string : ?pos:int -> ?len:int -> (_, _) t -> string      -> unit Or_error.t

(** [unpack t] unpacks all the values that it can from [t].  The resulting queue will be
    empty if nothing could be unpacked.  If there is an unpacking error, [unpack] returns
    an error, and subsequent [feed] and [unpack] operations on [t] will return that same
    error.  I.e. no more data can be fed to or unpacked from [t]. *)
val unpack : ('value, _) t -> 'value Queue.t Or_error.t

(** [debug] controls whether invariants are checked at each call.  Setting this to [true]
    can make things very slow. *)
val debug : bool ref
