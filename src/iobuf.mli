(** A non-moving (in the GC sense) contiguous range of bytes, useful for I/O operations.

    An iobuf consists of:

    - bigstring
    - limits -- a subrange of the bigstring
    - window -- a subrange of the limits

    All iobuf operations are restricted to operate within the limits.  Initially, the
    window of an iobuf is identical to its limits.  A phantom type, the "seek" permission,
    controls whether or not code is allowed to change the limits and window.  With seek
    permission, the limits can be [narrow]ed, but can never be widened, and the window can
    be set to an arbitrary subrange of the limits.

    A phantom type controls whether code can read and write bytes in the bigstring (within
    the limits) or can only read them.

    To present a restricted view of an iobuf to a client, one can create a sub-iobuf or
    add a type constraint.

    Functions operate on the window unless the documentation or naming indicates
    otherwise. *)

open! Import
open Iobuf_intf

type nonrec seek    = seek    [@@deriving sexp_of]
type nonrec no_seek = no_seek [@@deriving sexp_of]

(** The first type parameter controls whether the iobuf can be written to.  The second
    type parameter controls whether the window and limits can be changed.

    See the [Perms] module for information on how the first type parameter is used.

    To allow [no_seek] or [seek] access, a function's type uses [_] rather than [no_seek]
    as the type argument to [t].  Using [_] allows the function to be directly applied to
    either permission.  Using a specific permission would require code to use coercion
    [:>].

    There is no [t_of_sexp].  One should use [Iobuf.Hexdump.t_of_sexp] or [sexp_opaque]
    as desired. *)
type (-'data_perm_read_write, +'seek_permission) t

include Invariant.S2 with type ('rw, 'seek) t := ('rw, 'seek) t

(** Provides a [Window.Hexdump] submodule that renders the contents of [t]'s window. *)
module Window : Hexdump.S2 with type ('rw, 'seek) t := ('rw, 'seek) t

(** Provides a [Window.Hexdump] submodule that renders the contents of [t]'s limits. *)
module Limits : Hexdump.S2 with type ('rw, 'seek) t := ('rw, 'seek) t

(** Provides a [Hexdump] submodule that renders the contents of [t]'s window and limits
    using indices relative to the limits. *)
include Compound_hexdump with type ('rw, 'seek) t := ('rw, 'seek) t

(** Provides a [Debug.Hexdump] submodule that renders the contents of [t]'s window,
    limits, and underlying bigstring using indices relative to the bigstring. *)
module Debug : Compound_hexdump with type ('rw, 'seek) t := ('rw, 'seek) t

(** {2 Creation} *)

(** [create ~len] creates a new iobuf, backed by a bigstring of length [len],
    with the limits and window set to the entire bigstring. *)
val create : len:int -> (_, _) t


(** [of_bigstring bigstring ~pos ~len] returns an iobuf backed by [bigstring], with the
    window and limits specified starting at [pos] and of length [len]. *)
val of_bigstring
  :  ?pos:int  (** default is [0] *)
  -> ?len:int  (** default is [Bigstring.length bigstring - pos] *)
  -> Bigstring.t
  -> ([< read_write], _) t  (** forbid [immutable] to prevent aliasing *)

(** [of_string s] returns a new iobuf whose contents are [s]. *)
val of_string : string -> (_, _) t

(** [sub_shared t ~pos ~len] returns a new iobuf with limits and window set to the
    subrange of [t]'s window specified by [pos] and [len].  [sub_shared] preserves data
    permissions, but allows arbitrary seek permissions on the resulting iobuf. *)
val sub_shared : ?pos:int -> ?len:int -> ('d, _) t -> ('d, _) t

(** [set_bounds_and_buffer ~src ~dst] copies bounds metadata (i.e., limits and window) and
    shallowly copies the buffer (data pointer) from [src] to [dst].  It does not access
    data, but does allow access through [dst].  This makes [dst] an alias of [src].

    Because [set_bounds_and_buffer] creates an alias, we disallow immutable [src] and
    [dst] using [[> write]].  Otherwise, one of [src] or [dst] could be [read_write :>
    read] and the other [immutable :> read], which would allow you to write the
    [immutable] alias's data through the [read_write] alias.

    [set_bounds_and_buffer] is typically used with a frame iobuf that need only be
    allocated once.  This frame can be updated repeatedly and handed to users, without
    further allocation.  Allocation-sensitive applications need this. *)
val set_bounds_and_buffer
  : src : ([> write] as 'data, _) t -> dst : ('data, seek) t -> unit

(** [set_bounds_and_buffer_sub ~pos ~len ~src ~dst] is a more efficient version of
    [set_bounds_and_buffer ~src:(Iobuf.sub_shared ~pos ~len src) ~dst].

    [set_bounds_and_buffer ~src ~dst] is not the same as [set_bounds_and_buffer_sub ~dst
    ~src ~len:(Iobuf.length src)] because the limits are narrowed in the latter case.

    [~len] and [~pos] are mandatory for performance reasons, in concert with [@@inline].
    If they were optional, allocation would be necessary when passing a non-default,
    non-constant value, which is an important use case. *)
val set_bounds_and_buffer_sub
  :  pos : int
  -> len : int
  -> src : ([> write] as 'data, _) t
  -> dst : ('data, seek) t
  -> unit
[@@inline]

(** {2 Generalization}

    One may wonder why you'd want to call [no_seek], given that a cast is already
    possible, e.g., [t : (_, seek) t :> (_, no_seek) t].  It turns out that if you want to
    define some [f : (_, _) t -> unit] of your own that can be conveniently applied to
    [seek] iobufs without the user having to cast [seek] up, you need this [no_seek]
    function.

    [read_only] is more of a historical convenience now that [read_write] is a polymorphic
    variant, as one can now explicitly specify the general type for an argument with
    something like [t : (_ perms, _) t :> (read, _) t]. *)
val read_only : ([> read], 's) t -> (read, 's) t
val no_seek : ('r, _) t -> ('r, no_seek) t

(** {2 Accessors} *)

(** [capacity t] returns the size of [t]'s limits subrange.  The capacity of an iobuf can
    be reduced via [narrow]. *)
val capacity : (_, _) t -> int

(** [length t] returns the size of [t]'s window. *)
val length : (_, _) t -> int

(** [is_empty t] is [length t = 0]. *)
val is_empty : (_, _) t -> bool

(** {2 Changing the limits} *)

(** [narrow t] sets [t]'s limits to the current window. *)
val narrow : (_, seek) t -> unit

(** [narrow_lo t] sets [t]'s lower limit to the beginning of the current window. *)
val narrow_lo : (_, seek) t -> unit

(** [narrow_hi t] sets [t]'s upper limit to the end of the current window. *)
val narrow_hi : (_, seek) t -> unit


(** {2 Changing the window} *)

(** One can call [Lo_bound.window t] to get a snapshot of the lower bound of the window,
    and then later restore that snapshot with [Lo_bound.restore].  This is useful for
    speculatively parsing, and then rewinding when there isn't enough data to finish.

    Similarly for [Hi_bound.window] and [Lo_bound.restore].

    Using a snapshot with a different iobuf, even a sub iobuf of the snapshotted one, has
    unspecified results.  An exception may be raised, or a silent error may occur.
    However, the safety guarantees of the iobuf will not be violated, i.e., the attempt
    will not enlarge the limits of the subject iobuf. *)

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
    limit, such as a hypothetical packet header (in the case of [bounded_flip_lo]) or
    unfilled suffix of a buffer (in [bounded_flip_hi]). *)
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

(** [protect_window_and_bounds t ~f] applies [f] to [t] and restores [t]'s bounds and
    buffer afterward. *)
val protect_window_and_bounds : ('rw, no_seek) t -> f:(('rw, seek) t -> 'a) -> 'a

(** {2 Getting and setting data}

    "consume" and "fill" functions access data at the lower bound of the window and
    advance the lower bound of the window. "peek" and "poke" functions access data but do
    not advance the window. *)

(** [to_string t] returns the bytes in [t] as a string.  It does not alter the window. *)
val to_string : ?len:int -> ([> read], _) t -> string

(** Equivalent to [Hexdump.to_string_hum].  Renders [t]'s windows and limits. *)
val to_string_hum : ?max_lines:int -> (_, _) t -> string


(** [Consume.string t ~len] reads [len] characters (all, by default) from [t] into a new
    string and advances the lower bound of the window accordingly.

    [Consume.bin_prot X.bin_read_t t] returns the initial [X.t] in [t], advancing past the
    bytes read. *)
module Consume : sig
  (** [To_bytes.blito ~src ~dst ~dst_pos ~src_len ()] reads [src_len] bytes from [src],
      advancing [src]'s window accordingly, and writes them into [dst] starting at
      [dst_pos].  By default [dst_pos = 0] and [src_len = length src].  It is an error if
      [dst_pos] and [src_len] don't specify a valid region of [dst] or if [src_len >
      length src]. *)
  type src = (read, seek) t
  module To_bytes     : Consuming_blit with type src := src with type dst := Bytes.t
  module To_bigstring : Consuming_blit with type src := src with type dst := Bigstring.t

  module To_string : sig
    val blito       : (src, Bytes.t) consuming_blito
    [@@deprecated "[since 2017-10] use [Consume.To_bytes.blito] instead"]
    val blit        : (src, Bytes.t) consuming_blit
    [@@deprecated "[since 2017-10] use [Consume.To_bytes.blit] instead"]
    val unsafe_blit : (src, Bytes.t) consuming_blit
    [@@deprecated "[since 2017-10] use [Consume.To_bytes.unsafe_blit] instead"]

    (** [subo] defaults to using [Iobuf.length src]. *)
    val subo : ?len:int -> src -> string
    val sub  : src -> len:int -> string
  end

  include
    Accessors
    with type ('a, 'r, 's) t = ([> read] as 'r, seek) t -> 'a
    with type 'a bin_prot := 'a Bin_prot.Type_class.reader
end

(** [Fill.bin_prot X.bin_write_t t x] writes [x] to [t] in bin-prot form, advancing past
    the bytes written. *)
module Fill : sig
  include
    Accessors
    with type ('a, 'd, 'w) t = (read_write, seek) t -> 'a -> unit
    with type 'a bin_prot := 'a Bin_prot.Type_class.writer

  (** [decimal t int] is equivalent to [Iobuf.Fill.string t (Int.to_string int)], but with
      improved efficiency and no intermediate allocation.

      In other words: It fills the decimal representation of [int] to [t].  [t] is
      advanced by the number of characters written and no terminator is added.  If
      sufficient space is not available, [decimal] will raise. *)
  val decimal : (int, _, _) t
end

(** [Peek] and [Poke] functions access a value at [pos] from the lower bound of the window
    and do not advance.

    [Peek.bin_prot X.bin_read_t t] returns the initial [X.t] in [t] without advancing.

    Following the [bin_prot] protocol, the representation of [x] is [X.bin_size_t x] bytes
    long.  [Peek.], [Poke.], [Consume.], and [Fill.bin_prot] do not add any size prefix or
    other framing to the [bin_prot] representation. *)
module Peek : sig
  (** Similar to [Consume.To_*], but do not advance the buffer. *)
  type src = (read, no_seek) t
  module To_bytes     : Blit.S_distinct with type src := src with type dst := Bytes.t
  module To_bigstring : Blit.S_distinct with type src := src with type dst := Bigstring.t

  module To_string : sig
    val blit        : (src, Bytes.t) Base.Blit.blit
    [@@deprecated "[since 2017-10] use [Peek.To_bytes.blit] instead"]
    val blito       : (src, Bytes.t) Base.Blit.blito
    [@@deprecated "[since 2017-10] use [Peek.To_bytes.blito] instead"]
    val unsafe_blit : (src, Bytes.t) Base.Blit.blit
    [@@deprecated "[since 2017-10] use [Peek.To_bytes.unsafe_blit] instead"]

    val sub  : (src, string) Base.Blit.sub
    val subo : (src, string) Base.Blit.subo
  end

  val index : ([> read], _) t -> ?pos:int -> ?len:int -> char -> int option

  include
    Accessors
    with type ('a, 'd, 'w) t = ('d, 'w) t -> pos:int -> 'a
    with type 'a bin_prot := 'a Bin_prot.Type_class.reader
end

(** [Poke.bin_prot X.bin_write_t t x] writes [x] to the beginning of [t] in binary form
    without advancing.  You can use [X.bin_size_t] to tell how long it was.
    [X.bin_write_t] is only allowed to write that portion of the buffer you have access
    to. *)
module Poke : sig
  (** [decimal t ~pos i] returns the number of bytes written at [pos]. *)
  val decimal : (read_write, 'w) t -> pos:int -> int -> int

  include
    Accessors
    with type ('a, 'd, 'w) t = (read_write, 'w) t -> pos:int -> 'a -> unit
    with type 'a bin_prot := 'a Bin_prot.Type_class.writer
end

(** [Unsafe] has submodules that are like their corresponding module, except with no range
    checks.  Hence, mistaken uses can cause segfaults.  Be careful! *)
module Unsafe : sig
  module Consume : module type of Consume
  module Fill    : module type of Fill
  module Peek    : module type of Peek
  module Poke    : module type of Poke
end

val crc32 : ([> read], _) t -> Int63.t

(** The number of bytes in the length prefix of [consume_bin_prot] and [fill_bin_prot]. *)
val bin_prot_length_prefix_bytes : int

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
  : ([> write], seek) t -> 'a Bin_prot.Type_class.writer -> 'a -> unit Or_error.t
val consume_bin_prot
  : ([> read] , seek) t -> 'a Bin_prot.Type_class.reader -> 'a Or_error.t

(** [Blit] copies between iobufs and advances neither [src] nor [dst]. *)
module Blit : sig
  type 'rw t_no_seek = ('rw, no_seek) t
  include Blit.S_permissions
    with type 'rw t := 'rw t_no_seek

  (** Override types of [sub] and [subo] to allow return type to have [seek] as needed. *)
  val sub
    :  ([> read], no_seek) t
    -> pos : int
    -> len : int
    -> (_, _) t
  val subo
    :  ?pos : int
    -> ?len : int
    -> ([> read], no_seek) t
    -> (_, _) t
end

(** [Blit_consume] copies between iobufs and advances [src] but does not advance [dst]. *)
module Blit_consume : sig
  val blit
    :  src     : ([> read],  seek)    t
    -> dst     : ([> write], no_seek) t
    -> dst_pos : int
    -> len     : int
    -> unit
  val blito
    :  src      : ([> read],  seek)    t
    -> ?src_len : int
    -> dst      : ([> write], no_seek) t
    -> ?dst_pos : int
    -> unit
    -> unit
  val unsafe_blit
    :  src     : ([> read],  seek)    t
    -> dst     : ([> write], no_seek) t
    -> dst_pos : int
    -> len     : int
    -> unit
  val sub
    :  ([> read], seek) t
    -> len : int
    -> (_, _) t
  val subo
    :  ?len : int
    -> ([> read], seek) t
    -> (_, _) t
end

(** [Blit_fill] copies between iobufs and advances [dst] but does not advance [src]. *)
module Blit_fill : sig
  val blit
    :  src     : ([> read],  no_seek) t
    -> src_pos : int
    -> dst     : ([> write], seek)    t
    -> len     : int
    -> unit
  val blito
    :  src      : ([> read],  no_seek) t
    -> ?src_pos : int
    -> ?src_len : int
    -> dst      : ([> write], seek)    t
    -> unit
    -> unit
  val unsafe_blit
    :  src     : ([> read],  no_seek) t
    -> src_pos : int
    -> dst     : ([> write], seek)    t
    -> len     : int
    -> unit
end

(** [Blit_consume_and_fill] copies between iobufs and advances both [src] and [dst]. *)
module Blit_consume_and_fill : sig
  val blit
    :  src : ([> read],  seek) t
    -> dst : ([> write], seek) t
    -> len : int
    -> unit
  val blito
    :  src      : ([> read],  seek) t
    -> ?src_len : int
    -> dst      : ([> write], seek) t
    -> unit
    -> unit
  val unsafe_blit
    :  src : ([> read],  seek) t
    -> dst : ([> write], seek) t
    -> len : int
    -> unit
end

(** {2 I/O} *)

type ok_or_eof = Ok | Eof [@@deriving compare, sexp_of]

(** [Iobuf] has analogs of various [Bigstring] functions.  These analogs advance by the
    amount written/read. *)
val input : ([> write], seek) t -> In_channel.t      -> ok_or_eof
val read  : ([> write], seek) t -> Unix.File_descr.t -> ok_or_eof

val read_assume_fd_is_nonblocking
  : ([> write], seek) t -> Unix.File_descr.t -> Syscall_result.Unit.t
val pread_assume_fd_is_nonblocking
  : ([> write], seek) t -> Unix.File_descr.t -> offset:int -> unit
val recvfrom_assume_fd_is_nonblocking
  : ([> write], seek) t -> Unix.File_descr.t -> Unix.sockaddr

(** [recvmmsg]'s context comprises data needed by the system call.  Setup can be
    expensive, particularly for many buffers.

    NOTE: Unlike most system calls involving iobufs, the lo offset is not respected.
    Instead, the iobuf is implicity [reset] (i.e., [lo <- lo_min] and [hi <- hi_max])
    prior to reading and a [flip_lo] applied afterward.  This is to prevent the
    memory-unsafe case where an iobuf's lo pointer is advanced and [recvmmsg] attempts to
    copy into memory exceeding the underlying [bigstring]'s capacity.  If any of the
    returned iobufs have had their underlying bigstring or limits changed (e.g., through a
    call to [set_bounds_and_buffer] or [narrow_lo]), the call will fail with [EINVAL]. *)
module Recvmmsg_context : sig
  type ('rw, 'seek) iobuf
  type t

  (** Do not change these [Iobuf]'s [buf]s or limits before calling
      [recvmmsg_assume_fd_is_nonblocking]. *)
  val create : ([> write], seek) iobuf array -> t
end with type ('rw, 'seek) iobuf := ('rw, 'seek) t

(** [recvmmsg_assume_fd_is_nonblocking fd context] returns the number of [context] iobufs
    read into (or [errno]).  [fd] must not block.  [THREAD_IO_CUTOFF] is ignored.

    [EINVAL] is returned if an [Iobuf] passed to [Recvmmsg_context.create] has its [buf]
    or limits changed. *)
val recvmmsg_assume_fd_is_nonblocking
  : (Unix.File_descr.t -> Recvmmsg_context.t -> Unix.Syscall_result.Int.t) Or_error.t

val send_nonblocking_no_sigpipe
  :  unit
  -> (([> read], seek) t
      -> Unix.File_descr.t
      -> Unix.Syscall_result.Unit.t)
       Or_error.t
val sendto_nonblocking_no_sigpipe
  :  unit
  -> (([> read], seek) t
      -> Unix.File_descr.t
      -> Unix.sockaddr
      -> Unix.Syscall_result.Unit.t)
       Or_error.t

val output : ([> read], seek) t -> Out_channel.t     -> unit
val write  : ([> read], seek) t -> Unix.File_descr.t -> unit

val write_assume_fd_is_nonblocking
  : ([> read], seek) t -> Unix.File_descr.t -> unit
val pwrite_assume_fd_is_nonblocking
  : ([> read], seek) t -> Unix.File_descr.t -> offset:int -> unit

(** {2 Expert} *)

(** The [Expert] module is for building efficient out-of-module [Iobuf] abstractions. *)
module Expert: sig
  (** These accessors will not allocate, and are mainly here to assist in building
      low-cost syscall wrappers.

      One must be careful to avoid writing out of the limits (between [lo_min] and
      [hi_max]) of the [buf].  Doing so would violate the invariants of the parent
      [Iobuf]. *)
  val buf    : (_, _) t -> Bigstring.t
  val hi_max : (_, _) t -> int
  val hi     : (_, _) t -> int
  val lo     : (_, _) t -> int
  val lo_min : (_, _) t -> int

  (** [to_bigstring_shared t] and [to_iobuf_shared t] allocate new wrappers around the
      storage of [buf t], relative to [t]'s current bounds.

      These operations allow access outside the bounds and limits of [t], and without
      respect to its read/write access.  Be careful not to violate [t]'s invariants. *)
  val to_bigstring_shared : ?pos:int -> ?len:int -> (_, _) t -> Bigstring.t
  val to_iovec_shared     : ?pos:int -> ?len:int -> (_, _) t -> Bigstring.t Unix.IOVec.t

  (** [reinitialize_of_bigstring t bigstring] reinitializes [t] with backing [bigstring],
      and the window and limits specified starting at [pos] and of length [len]. *)
  val reinitialize_of_bigstring : (_, _) t -> pos:int -> len:int -> Bigstring.t -> unit

  (** These versions of [set_bounds_and_buffer] allow [~src] to be read-only.  [~dst] will
      be writable through [~src] aliases even though the type does not reflect this! *)
  val set_bounds_and_buffer : src : ('data, _) t -> dst : ('data, seek) t -> unit
  val set_bounds_and_buffer_sub
    :  pos : int
    -> len : int
    -> src : ('data, _) t
    -> dst : ('data, seek) t
    -> unit
end
