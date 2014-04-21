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

open Core_kernel.Std
open Common
open Iobuf_intf

type nonrec seek    = seek    with sexp_of
type nonrec no_seek = no_seek with sexp_of

(** The first type parameter controls whether the iobuf can be written to.  The second
    type parameter controls whether the window and limits can be changed.

    To allow [read_write] or [read_only] access, a function's type uses [_] rather than
    [read_only] as the type argument to [t].  Analogously, to allow [no_seek] or [seek]
    access, a function's type uses [_] rather than [no_seek] as the type argument to [t].
    Using [_] allows the function to be directly applied to either permission.  Using a
    specific permission would require code to use coercion [:>]. *)
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

(** [set_bounds_and_buffer ~src ~dst] copies bounds (ie limits + window) and shallowly
    copies the buffer from [src] to [dst].  [read_write] access is required on [src]
    because the caller might have [read_write] access to [dst], and would after the call
    then effectively have [read_write] access to [src]. *)
val set_bounds_and_buffer : src:(read_write, _) t -> dst:(_, seek) t -> unit

(** [set_bounds_and_buffer_sub ?pos ?len ~src ~dst ()] is a more efficient version of:
    [set_bounds_and_buffer ~src:(Iobuf.sub ?pos ?len src) ~dst].

    [set_bounds_and_buffer ~src ~dst] is not the same as
    [set_bounds_and_buffer_sub ~dst ~src ()], because the limits are narrowed in the
    latter case. *)
val set_bounds_and_buffer_sub
  :  ?pos:int
  -> ?len:int
  -> src:(read_write, _) t
  -> dst:(_, seek) t
  -> unit -> unit

(** {1 Generalization}

    One may wonder why you'd want to call [read_only] or [no_seek], given that the casts
    are already possible, e.g. [t : (read_write, _) t :> (read_only, _) t].  It turns out
    that if you want to define some [f : (_, seek) t -> unit] of your own, which can be
    conveniently applied to [read_only] and [read_write] iobufs without the user having to
    cast [read_write] up, you need this [read_only] function.
*)
val read_only : (_, 's) t -> (read_only, 's) t
val no_seek : ('r, _) t -> ('r, no_seek) t

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

(** One can call [Lo_bound.window t] to get a snapshot of the lower bound of the
    window, and then later restore that snapshot with [Lo_bound.restore].  This is
    useful for speculatively parsing, and then rewinding when there isn't enough data to
    finish.

    Similarly for [Hi_bound.window] and [Lo_bound.restore].

    Using a snapshot with a different iobuf, even a sub iobuf of the snapshotted one, has
    unspecified results.  An exception may be raised, or a silent error may occur.
    However, the safety guarantees of the iobuf will not be violated, i.e., the attempt
    will not enlarge the limits of the subject iobuf.
*)

module type Bound = Bound with type ('d, 'w) iobuf := ('d, 'w) t
module Lo_bound : Bound
module Hi_bound : Bound

(** [advance t amount] advances the lower bound of the window by [amount].  It is an error
    to advance past the upper bound of the window or the lower limit. *)
val advance : (_, seek) t -> int -> unit

(** [unsafe_advance] is like [advance] but with no bounds checking, so incorrect usage can
    easily cause segfaults. *)
val unsafe_advance : (_, seek) t -> int -> unit

(** [resize t] sets the length of [t]'s window, provided it does not exceed limits. *)
val resize : (_, seek) t -> len:int -> unit

(** [unsafe_resize] is like [resize] but with no bounds checking, so incorrect usage can
    easily cause segfaults. *)
val unsafe_resize : (_, seek) t -> len:int -> unit

(** [rewind t] sets the lower bound of the window to the lower limit. *)
val rewind : (_, seek) t -> unit

(** [reset t] sets the window to the limits. *)
val reset : (_, seek) t -> unit

(** [flip_lo t] sets the window to range from the lower limit to the lower bound of the
    old window.  This is typically called after a series of [Fill]s, to reposition the
    window in preparation to [Consume] the newly written data.

    The bounded version narrows the effective limit.  This can preserve some data near the
    limit, such as an hypothetical packet header, in the case of [bounded_flip_lo] or
    unfilled suffix of a buffer, in [bounded_flip_hi]. *)
val flip_lo         : (_, seek) t -> unit
val bounded_flip_lo : (_, seek) t -> Lo_bound.t -> unit

(** [compact t] copies data from the window to the lower limit of the iobuf and sets the
    window to range from the end of the copied data to the upper limit.  This is typically
    called after a series of [Consume]s to save unread data and prepare for the next
    series of [Fill]s and [flip_lo]. *)
val compact : (read_write, seek) t -> unit
val bounded_compact : (read_write, seek) t -> Lo_bound.t -> Hi_bound.t -> unit

(** [flip_hi t] sets the window to range from the the upper bound of the current window to
    the upper limit.  This operation is dual to [flip_lo] and is typically called when the
    data in the current (narrowed) window has been processed and the window needs to be
    positioned over the remaining data in the buffer.  For example:

    {[
      (* ... determine initial_data_len ... *)
      Iobuf.resize buf ~len:initial_data_len;
      (* ... and process initial data ... *)
      Iobuf.flip_hi buf;
    ]}

    Now the window of [buf] ranges over the remainder of the data. *)
val flip_hi         : (_, seek) t -> unit
val bounded_flip_hi : (_, seek) t -> Hi_bound.t -> unit

(** [protect_window_and_bounds t ~f] applies [f] to [t] and restores [t]'s bounds
    afterwards. *)
val protect_window_and_bounds
  :  ('rw, no_seek) t
  -> f:(('rw, seek) t -> 'a)
  -> 'a

(** {1 Getting and setting data} *)
(** "consume" and "fill" functions access data at the lower bound of the window and
    advance lower bound of the window.  "peek" and "poke" functions access data but do not
    advance the window. *)

(** [to_string t] returns the bytes in [t] as a string.  It does not alter the window. *)
val to_string : ?len:int -> (_, _) t -> string

(** [to_string_hum t] produces a readable, multi-line representation of an iobuf.
    [bounds] defaults to [`Limits] and determines how much of the contents are shown. *)
val to_string_hum : ?bounds:[`Window | `Limits | `Whole] -> (_, _) t -> string


(** [Consume.string t ~len] reads [len] characters (all, by default) from [t] into a new
    string and advances the lower bound of the window accordingly.

    [Consume.bin_prot X.bin_read_t t] returns the initial [X.t] in [t], advancing past the
    bytes read. *)
module Consume : sig
  (** [To_string.blito ~src ~dst ~dst_pos ~src_len ()] reads [src_len] bytes from [src],
      advancing [src]'s window accordingly, and writes them into [dst] starting at
      [dst_pos].  By default [dst_pos = 0] and [src_len = length src].  It is an error if
      [dst_pos] and [src_len] don't specify a valid region of [dst] or [src_len > length
      src]. *)
  type src = (read_only, seek) t
  module To_string    : Consuming_blit with type src := src with type dst := string
  module To_bigstring : Consuming_blit with type src := src with type dst := Bigstring.t

  include
    Accessors
    with type ('a, 'r, 's) t = ('r, seek) t -> 'a
    with type 'a bin_prot := 'a Bin_prot.Type_class.reader
end

(** [Fill.bin_prot X.bin_write_t t x] writes [x] to [t] in bin-prot form, advancing past
    the bytes written. *)
module Fill
  : Accessors
    with type ('a, 'd, 'w) t = (read_write, seek) t -> 'a -> unit
    with type 'a bin_prot := 'a Bin_prot.Type_class.writer

(** [Peek] and [Poke] functions access a value at [pos] from the lower bound of the window
    and do not advance.

    [Peek.bin_prot X.bin_read_t t] returns the initial [X.t] in [t] without advancing.

    Following the [bin_prot] protocol, the representation of [x] is [X.bin_size_t x] bytes
    long.  [Peek.], [Poke.], [Consume.], and [Fill.bin_prot] do not add any size prefix or
    other framing to the [bin_prot] representation. *)
module Peek : sig
  (** Similar to [Consume.To_*], but do not advance the buffer. *)
  type src = (read_only, no_seek) t
  module To_string    : Blit.S_distinct with type src := src with type dst := string
  module To_bigstring : Blit.S_distinct with type src := src with type dst := Bigstring.t

  include
    Accessors
    with type ('a, 'd, 'w) t = ('d, 'w) t -> pos:int -> 'a
    with type 'a bin_prot := 'a Bin_prot.Type_class.reader
end

(** [Poke.bin_prot X.bin_write_t t x] writes [x] to the beginning of [t] in binary form
    without advancing.  You can use [X.bin_size_t] to tell how long it was.
    [X.bin_write_t] is only allowed to write that portion of the buffer to which you have
    access. *)
module Poke
  : Accessors
    with type ('a, 'd, 'w) t = (read_write, 'w) t -> pos:int -> 'a -> unit
    with type 'a bin_prot := 'a Bin_prot.Type_class.writer

(** [Unsafe] has submodules that are like their corresponding module, except with no range
    checks.  Hence, mistaken uses can cause segfaults.  Be careful! *)
module Unsafe : sig
  module Consume : module type of Consume
  module Fill    : module type of Fill
  module Peek    : module type of Peek
  module Poke    : module type of Poke
end

(** [fill_bin_prot] writes a bin-prot value to the lower bound of the window, prefixed by
    its length, and advances by the amount written.  [fill_bin_prot] returns an error if
    the window is too small to write the value.

    [consume_bin_prot t reader] reads a bin-prot value from the lower bound of the window,
    which should have been written using [fill_bin_prot], and advances the window by the
    amount read.  [consume_bin_prot] returns an error if there is not a complete message
    in the window and in that case the window is left unchanged.

    Don't use these without a good reason, as they are incompatible with similar functions
    in [Reader] and [Writer].  They use a 4-byte length rather than an 8-byte length. *)
val fill_bin_prot
  : (read_write, seek) t -> 'a Bin_prot.Type_class.writer -> 'a -> unit Or_error.t
val consume_bin_prot
  :          (_, seek) t -> 'a Bin_prot.Type_class.reader -> 'a Or_error.t

(** [transfer] blits [len] bytes from [src] to [dst], advancing the lower bounds of both
    windows.  It is an error if [len > length src || len > length dst || phys_equal src
    dst]. *)
val transfer
  :  ?len:int (** default is [min (length src) (length dst)] *)
  -> src:((_         , seek) t)
  -> dst:((read_write, seek) t)
  -> unit

(** [memmove] blits [len] bytes from [src_pos] to [dst_pos] in an iobuf, both relative to
    the lower bound of the window.  The window is not advanced. *)
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

(** When there are no packets awaiting reception, [recvmmsg_assume_fd_is_nonblocking]
    returns <0 (actually, -[EWOULDBLOCK]/-[EAGAIN]), rather than raising [Unix_error] with
    [EAGAIN]/[EWOULDBLOCK].  This saves the allocation of an exception (and backtrace) in
    common cases, at the cost of callers having to handle but the return code and possible
    exception. *)
val recvmmsg_assume_fd_is_nonblocking
  : (Unix.File_descr.t
     -> ?count:int
     -> ?srcs:Unix.sockaddr array
     -> (read_write, seek) t array
     -> int)
      Or_error.t
val recvmmsg_assume_fd_is_nonblocking_no_options
  : (Unix.File_descr.t
     -> count:int
     -> (read_write, seek) t array
     -> int)
      Or_error.t


val send_nonblocking_no_sigpipe
  : unit -> ((_, seek) t -> Unix.File_descr.t -> int option) Or_error.t
val sendto_nonblocking_no_sigpipe
  : unit -> ((_, seek) t -> Unix.File_descr.t -> Unix.sockaddr -> int option) Or_error.t
val write_assume_fd_is_nonblocking
  : (_, seek) t -> Unix.File_descr.t -> int
val pwrite_assume_fd_is_nonblocking
  : (_, seek) t -> Unix.File_descr.t -> offset:int -> int
