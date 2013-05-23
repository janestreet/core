(** A non-moving (in the gc sense) contiguous range of bytes, useful for I/O operations.

    An iobuf consists of:

    - bigstring
    - limits -- a subrange of the bigstring
    - window -- a subrange of the limits

    All iobuf operations are restricted to operate within the limits.  Initially, the
    window of an iobuf is identical to the limits.  A phantom type, "seek" permission,
    controls whether or not code is allowed to change the limits and window.  With seek
    permission, the limits can be [narrow]ed, but can never be widened, and the window can
    be set to an arbitrary subrange of the limits.

    A phantom type controls whether code can read and write bytes in the bigstring (within
    the limits) or can only read them.

    To present a restricted view of an iobuf to a client, one can create a sub-iobuf or
    add a type constraint.
*)

open Common
open Iobuf_intf

(** [no_seek] and [seek] are defined and used in a similar manner to
    [read_only] and [read_write]. *)
type no_seek with sexp_of                (** like [read_only] *)
type seek = private no_seek with sexp_of (** like [read_write] *)

(** The first type parameter controls whether the iobuf can be written to.
    The second type parameter controls whether the window and limits can be changed.

    To allow [read_write] or [read_only] access, a function's type uses [_] rather than
    [read_only] as the type argument to [t].  Analogously, to allow [no_seek] or [seek]
    access, a function's type uses [_] rather than [no_seek] as the type argument to [t].
    Using [_] allows the function to be directly applied to either permission.  Using
    a specific permission would require code to use coercion [:>]. *)
type (+'data_perm_read_write, +'seek_permission) t with sexp_of

include Invariant.S2 with type ('a, 'b) t := ('a, 'b) t

(** {1 Creation} *)

(** [create ~len] creates a new iobuf, backed by a bigstring of length [len],
    with the limits and window set to the entire bigstring. *)
val create : len:int -> (_, _) t

(** [of_bigstring bigstring ~pos ~len] returns an iobuf backed by [bigstring], with the
    window and limits specified starting at [pos] and of length [len]. *)
val of_bigstring
  :  ?pos:int  (** default is [0] *)
  -> ?len:int  (** default is [Bigstring.length bigstring - pos] *)
  -> Bigstring.t
  -> (_, _) t

(** [of_string s] returns a new iobuf whose contents are [s]. *)
val of_string : string -> (_, _) t

(** [sub t ~pos ~len] returns a new iobuf with limits and window set to the subrange of
    [t] specified by [pos] and [len].  [sub] preserves data permissions, but allows
    arbitrary seek permissions on the resulting iobuf. *)
val sub : ?pos:int -> ?len:int -> ('d, _) t -> ('d, _) t

(** {1 Accessors} *)

(** [capacity t] returns the size of [t]'s limits subrange.  The capacity of an iobuf can
    be reduced via [narrow]. *)
val capacity : (_, _) t -> int

(** [length t] returns the size of [t]'s window. *)
val length : (_, _) t -> int

(** [is_empty t] is [length t = 0]. *)
val is_empty : (_, _) t -> bool

(** {1 Changing the limits} *)

(** [narrow t] sets [t]'s limits to the current window. *)
val narrow : (_, seek) t -> unit

(** {1 Changing the window} *)

(** One can call [snapshot t] to get a snapshot of the front of the window, and then later
    restore that snapshot.  This is useful for speculatively parsing, and then rewinding
    when there isn't enough data to finish.

    Using a snapshot with a different iobuf, even a sub iobuf of the snapshotted one, has
    unspecified results.  An exception may be raised, or a silent error may occur.
    However, the safety guarantees of the iobuf will not be violated, i.e., the attempt
    will not enlarge the limits of the subject iobuf. *)
module Snapshot : sig
  type ('d, 'w) iobuf = ('d, 'w) t
  type t with sexp_of
  val restore : t -> (_, seek) iobuf -> unit
end with type ('d, 'w) iobuf := ('d, 'w) t
val snapshot : (_, seek) t -> Snapshot.t

(** [advance t amount] advances the front of the window by [amount].  It is an error to
    advance past the back of the window or the lower limit. *)
val advance : (_, seek) t -> int -> unit

(** [resize t] sets the length of [t]'s window, provided it does not exceed limits. *)
val resize : (_, seek) t -> len:int -> unit

(** [rewind t] sets the front of the window to the lower limit. *)
val rewind : (_, seek) t -> unit

(** [reset t] sets the window to the limits. *)
val reset : (_, seek) t -> unit

(** [flip t] sets the window to range from the lower limit to the front of the old window.
    This is typically called after a series of [Fill]s, to reposition the window in
    preparation to [Consume] the newly written data. *)
val flip : (_, seek) t -> unit

(** [compact t] copies data from the window to the lower limit of the iobuf and sets the
    window to range from the end of the copied data to the upper limit.  This is typically
    called after a series of [Consume]s to save unread data and prepare for the next
    series of [Fill]s and [flip]. *)
val compact : (read_write, seek) t -> unit

(** {1 Getting and setting data} *)
(** "consume" and "fill" functions access data at the front of the window and advance the
    front of the window.  "peek" and "poke" functions access data but do not advance the
    window. *)

(** [to_string t] returns the bytes in [t] as a string.  It does not alter the window. *)
val to_string : ?len:int -> (_, _) t -> string

(** [consume_into_string t s ~pos ~len] reads [len] bytes from [t], advancing [t]'s window
    accordingly, and writes them into [s] starting at [pos].  By default [pos = 0] and
    [len = String.length s - pos].  It is an error if [pos] and [len] don't specify a
    valid region of [s] or [len > length t]. *)
val consume_into_string    : ?pos:int -> ?len:int -> (_, seek) t ->    string   -> unit
val consume_into_bigstring : ?pos:int -> ?len:int -> (_, seek) t -> Bigstring.t -> unit

(** [Consume.string t ~len] reads [len] characters (all, by default) from [t] into a new
    string and advances the front of the window accordingly. *)
module Consume
  : Accessors with type ('a, 'd, 'w) t = ('d, seek) t -> 'a
module Fill
  : Accessors with type ('a, 'd, 'w) t = (read_write, seek) t -> 'a -> unit
(** [Peek] and [Poke] functions access a value at [pos] from the front of the window
    and do not advance. *)
module Peek
  : Accessors with type ('a, 'd, 'w) t = ('d, 'w) t -> pos:int -> 'a
module Poke
  : Accessors with type ('a, 'd, 'w) t = (read_write, 'w) t -> pos:int -> 'a -> unit

(** [Unsafe] has submodules that are like their corresponding module, except with no range
    checks.  Hence, mistaken uses can cause segfaults.  Be careful! *)
module Unsafe : sig
  module Consume : module type of Consume
  module Fill    : module type of Fill
  module Peek    : module type of Peek
  module Poke    : module type of Poke
end

(** [fill_bin_prot] writes a bin-prot value to the front of the window, prefixed by its
    length, and advances the front of the window by the amount written.  [fill_bin_prot]
    returns an error if the window is too small to write the value.

    [consume_bin_prot t reader] reads a bin-prot value from the front of the window, which
    should have been written using [fill_bin_prot], and advances the window by the amount
    read.  [consume_bin_prot] returns an error if there is not a complete message in the
    window and in that case the window is left unchanged. *)
val fill_bin_prot
  : (read_write, seek) t -> 'a Bin_prot.Type_class.writer -> 'a -> unit Or_error.t
val consume_bin_prot
  :          (_, seek) t -> 'a Bin_prot.Type_class.reader -> 'a Or_error.t

(** [transfer] blits [len] bytes from the front of [src] to the front of [dst], advancing
    both.  It is an error if [len > length src || len > length dst || phys_equal src
    dst]. *)
val transfer
  :  ?len:int  (** default is [min (length src) (length dst)] *)
  -> src:((_         , seek) t)
  -> dst:((read_write, seek) t)
  -> unit

(** [memmove] blits [len] bytes from [src_pos] to [dst_pos] in an iobuf, both relative to
    the front of the window.  The window is not advanced. *)
val memmove : (read_write, _) t -> src_pos:int -> dst_pos:int -> len:int -> unit

(** {1 I/O} *)

(** [Iobuf] has analogs of various [Bigstring] functions.  These analogs advance by the
    amount written/read. *)
val read_assume_fd_is_nonblocking
  : (read_write, seek) t -> Unix.File_descr.t -> int
val pread_assume_fd_is_nonblocking
  : (read_write, seek) t -> Unix.File_descr.t -> offset:int -> int
val recvfrom_assume_fd_is_nonblocking
  : (read_write, seek) t -> Unix.File_descr.t -> int * Unix.sockaddr
val recvmmsg_assume_fd_is_nonblocking
  : (Unix.File_descr.t
     -> ?count:int
     -> ?srcs:Unix.sockaddr array
     -> (read_write, seek) t array
     -> int)
      Or_error.t

val send_nonblocking_no_sigpipe
  : unit -> ((_, seek) t -> Unix.File_descr.t                  -> int option) Or_error.t
val sendto_nonblocking_no_sigpipe
  : unit -> ((_, seek) t -> Unix.File_descr.t -> Unix.sockaddr -> int option) Or_error.t
val write_assume_fd_is_nonblocking  : (_, seek) t -> Unix.File_descr.t -> int
val pwrite_assume_fd_is_nonblocking : (_, seek) t -> Unix.File_descr.t -> offset:int -> int
