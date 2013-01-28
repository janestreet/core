
open Unix
open Bigarray

(** {6 Types and exceptions} *)

(** Type of bigstrings *)
type t = (char, int8_unsigned_elt, c_layout) Array1.t

include Sexpable.S with type t := t
include Binable.S with type t := t

(** Type of I/O errors *)
exception IOError of
  int *  (** Number of bytes successfully read/written before error *)
  exn  (** The occurred exception (e.g. Unix_error, End_of_file) *)


(** {6 Creation and string conversion} *)

val create : int -> t
(** [create length] @return a new bigstring having [length]. *)

(** [init n ~f] creates a bigstring [t] of length [n], with [t.{i} = f i] *)
val init : int -> f:(int -> char) -> t

val of_string : ?pos : int -> ?len : int -> string -> t
(** [of_string ?pos ?len str] @return a new bigstring that is equivalent
    to the substring of length [len] in [str] starting at position [pos].

    @param pos default = 0
    @param len default = [String.length str - pos]
*)

val to_string : ?pos : int -> ?len : int -> t -> string
(** [to_string ?pos ?len bstr] @return a new string that is equivalent
    to the substring of length [len] in [bstr] starting at position [pos].

    @param pos default = 0
    @param len default = [length bstr - pos]

    @raise Invalid_argument if the string would exceed runtime limits.
*)


(** {6 Checking} *)

val check_args : loc : string -> pos : int -> len : int -> t -> unit
(** [check_args ~loc ~pos ~len bstr] checks the position and length
    arguments [pos] and [len] for bigstrings [bstr].  @raise
    Invalid_argument if these arguments are illegal for the given
    bigstring using [loc] to indicate the calling context. *)

val get_opt_len : t -> pos : int -> int option -> int
(** [get_opt_len bstr ~pos opt_len] @return the length of a subbigstring
    in [bstr] starting at position [pos] and given optional length
    [opt_len].  This function does not check the validity of its
    arguments.  Use {!check_args} for that purpose. *)


(** {6 Accessors} *)

val length : t -> int
(** [length bstr] @return the length of bigstring [bstr]. *)

val sub : ?pos : int -> ?len : int -> t -> t
(** [sub ?pos ?len bstr] @return the sub-bigstring in [bstr] that starts at
    position [pos] and has length [len].  The sub-bigstring is a unique copy
    of the memory region, i.e. modifying it will not modify the original
    bigstring.  Note that this is different than the behavior of the
    standard OCaml Array1.sub, which shares the memory.

    @param pos default = 0
    @param len default = [Bigstring.length bstr - pos]
*)

val sub_shared : ?pos : int -> ?len : int -> t -> t
(** [sub_shared ?pos ?len bstr] @return the sub-bigstring in [bstr]
    that starts at position [pos] and has length [len].  The sub-bigstring
    shares the same memory region, i.e. modifying it will modify the
    original bigstring.  Holding on to the sub-bigstring will also keep
    the (usually bigger) original one around.

    @param pos default = 0
    @param len default = [Bigstring.length bstr - pos]
*)

(** [get t pos] returns the character at [pos] *)
val get : t -> int -> char

external is_mmapped : t -> bool = "bigstring_is_mmapped_stub" "noalloc"
(** [is_mmapped bstr] @return whether the bigstring [bstr] is
    memory-mapped. *)

(** {6 Blitting} *)

(** [blit ~src ?src_pos ?src_len ~dst ?dst_pos ()] blits [src_len] characters
    from [src] starting at position [src_pos] to [dst] at position [dst_pos].

    @raise Invalid_argument if the designated ranges are out of bounds.
*)
type ('src, 'dst) blit
  =  src : 'src
  -> ?src_pos : int
  -> ?src_len : int
  -> dst : 'dst
  -> ?dst_pos : int
  -> unit
  -> unit

val blit                  : (t     , t     ) blit
val blit_string_bigstring : (string, t     ) blit
val blit_bigstring_string : (t     , string) blit

(** {6 Input functions} *)

val read : ?min_len : int -> file_descr -> ?pos : int -> ?len : int -> t -> int
(** [read ?min_len fd ?pos ?len bstr] reads at least [min_len] (must be
    greater than or equal zero) and at most [len] (must be greater than
    or equal to [min_len]) bytes from file descriptor [fd], and writes
    them to bigstring [bstr] starting at position [pos].  @return the
    number of bytes actually read.

    NOTE: even if [len] is zero, there may still be errors when reading
    from the descriptor!

    @raise Invalid_argument if the designated ranges are out of bounds.

    @raise IOError in the case of input errors, or on EOF if the
    minimum length could not be read.

    @param pos default = 0
    @param min_len default = 0
    @param len default = [length bstr - pos]
*)

val really_read : file_descr -> ?pos : int -> ?len : int -> t -> unit
(** [really_read fd ?pos ?len bstr] reads [len] bytes from file descriptor
    [fd], and writes them to bigstring [bstr] starting at position [pos].

    @raise Invalid_argument if the designated range is out of bounds.
    @raise IOError in the case of input errors, or on EOF.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

val really_recv : file_descr -> ?pos : int -> ?len : int -> t -> unit
(** [really_recv sock ?pos ?len bstr] receives [len] bytes from socket
    [sock], and writes them to bigstring [bstr] starting at position
    [pos].  If [len] is zero, the function returns immediately without
    performing the underlying system call.

    @raise Invalid_argument if the designated range is out of bounds.
    @raise IOError in the case of input errors, or on EOF.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

val recvfrom_assume_fd_is_nonblocking :
  file_descr -> ?pos : int -> ?len : int -> t -> int * sockaddr
(** [recvfrom_assume_fd_is_nonblocking sock ?pos ?len bstr] reads up to
    [len] bytes into bigstring [bstr] starting at position [pos] from
    socket [sock] without yielding to other OCaml-threads.

    @return the number of bytes actually read and the socket address of
    the client.

    @raise Unix_error in the case of input errors.
    @raise Invalid_argument if the designated range is out of bounds.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

val read_assume_fd_is_nonblocking :
  file_descr -> ?pos : int -> ?len : int -> t -> int
(** [read_assume_fd_is_nonblocking fd ?pos ?len bstr] reads up to
    [len] bytes into bigstring [bstr] starting at position [pos] from
    file descriptor [fd] without yielding to other OCaml-threads.
    @return the number of bytes actually read.

    @raise Unix_error in the case of input errors.
    @raise Invalid_argument if the designated range is out of bounds.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

val input : ?min_len : int -> in_channel -> ?pos : int -> ?len : int -> t -> int
(** [input ?min_len ic ?pos ?len bstr] tries to read [len] bytes
    (guarantees to read at least [min_len] bytes (must be greater than
    or equal to zero and smaller or equal to [len]), if possible, before
    returning) from input channel [ic], and writes them to bigstring
    [bstr] starting at position [pos].  @return the number of bytes
    actually read.

    NOTE: even if [len] is zero, there may still be errors when reading
    from the descriptor, which will be done if the internal buffer
    is empty!

    NOTE: if at least [len] characters are available in the input channel
    buffer and if [len] is not zero, data will only be fetched from the
    channel buffer.  Otherwise data will be read until at least [min_len]
    characters are available.

    @raise Invalid_argument if the designated range is out of bounds.
    @raise IOError in the case of input errors, or on premature EOF.

    @param pos default = 0
    @param min_len default = 0
    @param len default = [length bstr - pos]
*)

val really_input : in_channel -> ?pos : int -> ?len : int -> t -> unit
(** [really_input ic ?pos ?len bstr] reads exactly [len] bytes from
    input channel [ic], and writes them to bigstring [bstr] starting at
    position [pos].

    @raise Invalid_argument if the designated range is out of bounds.
    @raise IOError in the case of input errors, or on premature EOF.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)


(** {6 Output functions} *)

val really_write : file_descr -> ?pos : int -> ?len : int -> t -> unit
(** [really_write fd ?pos ?len bstr] writes [len] bytes in bigstring
    [bstr] starting at position [pos] to file descriptor [fd].

    @raise Invalid_argument if the designated range is out of bounds.
    @raise IOError in the case of output errors.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

(** [really_send_no_sigpipe sock ?pos ?len bstr] sends [len] bytes in
    bigstring [bstr] starting at position [pos] to socket [sock] without
    blocking and ignoring [SIGPIPE].

    @raise Invalid_argument if the designated range is out of bounds.
    @raise IOError in the case of output errors.

    @param pos default = 0
    @param len default = [length bstr - pos]

    [really_send_no_sigpipe] is not implemented on some platforms, in which
    case it is an [Error] value that indicates that it is unimplemented. *)
val really_send_no_sigpipe
  : (file_descr -> ?pos : int -> ?len : int -> t -> unit) Or_error.t

val send_nonblocking_no_sigpipe :
  (file_descr -> ?pos : int -> ?len : int -> t -> int option) Or_error.t
(** [send_nonblocking_no_sigpipe sock ?pos ?len bstr] tries to send
    [len] bytes in bigstring [bstr] starting at position [pos] to socket
    [sock].  @return [Some bytes_written], or [None] if the operation
    would have blocked.

    @raise Invalid_argument if the designated range is out of bounds.
    @raise Unix_error in the case of output errors.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

val sendto_nonblocking_no_sigpipe :
  (file_descr -> ?pos : int -> ?len : int -> t ->
    sockaddr -> int option) Or_error.t
(** [sendto_nonblocking_no_sigpipe sock ?pos ?len bstr sockaddr] tries
    to send [len] bytes in bigstring [bstr] starting at position [pos]
    to socket [sock] using address [addr].  @return [Some bytes_written],
    or [None] if the operation would have blocked.

    @raise Invalid_argument if the designated range is out of bounds.
    @raise Unix_error in the case of output errors.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

val write : file_descr -> ?pos : int -> ?len : int -> t -> int
(** [write fd ?pos ?len bstr] writes [len]
    bytes in bigstring [bstr] starting at position [pos] to file
    descriptor [fd].  @return the number of bytes actually written.

    @raise Unix_error in the case of output errors.
    @raise Invalid_argument if the designated range is out of bounds.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

val write_assume_fd_is_nonblocking :
  file_descr -> ?pos : int -> ?len : int -> t -> int
(** [write_assume_fd_is_nonblocking fd ?pos ?len bstr] writes [len]
    bytes in bigstring [bstr] starting at position [pos] to file
    descriptor [fd] without yielding to other OCaml-threads.  @return the
    number of bytes actually written.

    @raise Unix_error in the case of output errors.
    @raise Invalid_argument if the designated range is out of bounds.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

val writev :
  file_descr -> ?count : int -> t Core_unix.IOVec.t array -> int
(** [writev fd ?count iovecs] writes [count] [iovecs] of
    bigstrings to file descriptor [fd].  @return the number of bytes
    written.

    @raise Unix_error in the case of output errors.
    @raise Invalid_argument if count is out of range.

    @param count default = [Array.length iovecs]
*)

val writev_assume_fd_is_nonblocking :
  file_descr -> ?count : int -> t Core_unix.IOVec.t array -> int
(** [writev_assume_fd_is_nonblocking fd ?count iovecs] writes [count]
    [iovecs] of bigstrings to file descriptor [fd] without yielding to
    other OCaml-threads.  @return the number of bytes actually written.

    @raise Unix_error in the case of output errors.
    @raise Invalid_argument if the designated range is out of bounds.

    @param count default = [Array.length iovecs]
*)

val sendmsg_nonblocking_no_sigpipe :
  (file_descr -> ?count : int ->
    t Core_unix.IOVec.t array -> int option) Or_error.t
(** [sendmsg_nonblocking_no_sigpipe sock ?count iovecs] sends
    [count] [iovecs] of bigstrings to socket [sock].  @return [Some
    bytes_written], or [None] if the operation would have blocked.
    This system call will not cause signal [SIGPIPE] if an attempt is
    made to write to a socket that was closed by the other side.

    @raise Unix_error in the case of output errors.
    @raise Invalid_argument if [count] is out of range.

    @param count default = [Array.length iovecs]
*)

val output :
  ?min_len : int -> out_channel -> ?pos : int -> ?len : int -> t -> int
(** [output ?min_len oc ?pos ?len bstr] tries to output
    [len] bytes (guarantees to write at least [min_len] bytes (must be
    equal to or greater than zero), if possible, before returning) from
    bigstring [bstr] starting at position [pos] to output channel [oc].
    @return the number of bytes actually written.

    NOTE: you may need to flush [oc] to make sure that the data is
    actually sent.

    NOTE: if [len] characters fit into the channel buffer completely,
    they will be buffered.  Otherwise writes will be attempted until at
    least [min_len] characters have been sent.

    @raise Invalid_argument if the designated range is out of bounds.

    @raise IOError in the case of output errors.  The [IOError]-argument
    counting the number of successful bytes includes those that have
    been transferred to the channel buffer before the error.

    @param pos default = 0
    @param min_len default = 0
    @param len default = [length bstr - pos]
*)

val really_output :
  out_channel -> ?pos : int -> ?len : int -> t -> unit
(** [really_output oc ?pos ?len bstr] outputs exactly [len]
    bytes from bigstring [bstr] starting at position [pos] to output
    channel [oc].

    @raise Invalid_argument if the designated range is out of bounds.

    @raise IOError in the case of output errors.  The [IOError]-argument
    counting the number of successful bytes includes those that have
    been transferred to the channel buffer before the error.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)


(** {6 Memory mapping} *)

val map_file : shared : bool -> file_descr -> int -> t
(** [map_file shared fd n] memory-maps [n] characters of the data
    associated with descriptor [fd] to a bigstring.  Iff [shared] is
    [true], all changes to the bigstring will be reflected in the file. *)


(** {6 Unsafe functions} *)


external unsafe_blit :
  src : t -> src_pos : int -> dst : t -> dst_pos : int -> len : int -> unit
  = "bigstring_blit_stub"
(** [unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len] similar to
    {!Bigstring.blit}, but does not perform any bounds checks.  Will crash
    on bounds errors! *)

external unsafe_blit_string_bigstring :
  src : string -> src_pos : int -> dst : t -> dst_pos : int -> len : int -> unit
  = "bigstring_blit_string_bigstring_stub" "noalloc"
(** [unsafe_blit_string_bigstring ~src ~src_pos ~dst ~dst_pos ~len]
    similar to {!Bigstring.blit_string_bigstring}, but does not perform
    any bounds checks.  Will crash on bounds errors! *)

external unsafe_blit_bigstring_string :
  src : t -> src_pos : int -> dst : string -> dst_pos : int -> len : int -> unit
  = "bigstring_blit_bigstring_string_stub" "noalloc"
(** [unsafe_blit_bigstring_string ~src ~src_pos ~dst ~dst_pos ~len]
    similar to {!Bigstring.blit_bigstring_string}, but does not perform
    any bounds checks.  Will crash on bounds errors! *)

external unsafe_read_assume_fd_is_nonblocking :
  file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_read_assume_fd_is_nonblocking_stub"
(** [unsafe_read_assume_fd_is_nonblocking fd ~pos ~len bstr]
    similar to {!Bigstring.read_assume_fd_is_nonblocking}, but does
    not perform any bounds checks.  Will crash on bounds errors! *)

external unsafe_write :
  file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_write_stub"
(** [unsafe_write fd ~pos ~len bstr] similar to
    {!Bigstring.write}, but does not perform any bounds checks.
    Will crash on bounds errors! *)

external unsafe_write_assume_fd_is_nonblocking :
  file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_write_assume_fd_is_nonblocking_stub"
(** [unsafe_write_assume_fd_is_nonblocking fd ~pos ~len bstr]
    similar to {!Bigstring.write_assume_fd_is_nonblocking}, but does
    not perform any bounds checks.  Will crash on bounds errors! *)

external unsafe_read :
  min_len : int -> file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_read_stub"
(** [unsafe_read ~min_len fd ~pos ~len bstr] similar to
    {!Bigstring.read}, but does not perform any bounds checks.
    Will crash on bounds errors! *)

external unsafe_really_recv :
  file_descr -> pos : int -> len : int -> t -> unit
  = "bigstring_really_recv_stub"
(** [unsafe_really_recv sock ~pos ~len bstr] similar to
    {!Bigstring.really_recv}, but does not perform any
    bounds checks.  Will crash on bounds errors! *)

external unsafe_input :
  min_len : int -> in_channel -> pos : int -> len : int -> t -> int
  = "bigstring_input_stub"
(** [unsafe_input ~min_len ic ~pos ~len bstr] similar to
    {!Bigstring.input}, but does not perform any bounds checks.
    Will crash on bounds errors! *)

external unsafe_really_write :
  file_descr -> pos : int -> len : int -> t -> unit
  = "bigstring_really_write_stub"
(** [unsafe_really_write fd ~pos ~len bstr] similar to
    {!Bigstring.write}, but does not perform any bounds checks.
    Will crash on bounds errors! *)


(** [unsafe_really_send_no_sigpipe sock ~pos ~len bstr]
    similar to {!Bigstring.send}, but does not perform any
    bounds checks.  Will crash on bounds errors! *)
val unsafe_really_send_no_sigpipe
  : (file_descr -> pos : int -> len : int -> t -> unit) Or_error.t

(** [unsafe_send_nonblocking_no_sigpipe sock ~pos ~len bstr] similar to
    {!Bigstring.send_nonblocking_no_sigpipe}, but does not perform any
    bounds checks.  Will crash on bounds errors! *)
val unsafe_send_nonblocking_no_sigpipe
  : (file_descr -> pos : int -> len : int -> t -> int option) Or_error.t

external unsafe_output :
  min_len : int -> out_channel -> pos : int -> len : int -> t -> int
  = "bigstring_output_stub"
(** [unsafe_output ~min_len oc ~pos ~len bstr] similar to
    {!Bigstring.output}, but does not perform any bounds checks.
    Will crash on bounds errors! *)

external unsafe_writev :
  file_descr -> t Core_unix.IOVec.t array -> int -> int
  = "bigstring_writev_stub"
(** [unsafe_writev fd iovecs count] similar to
    {!Bigstring.writev}, but does not perform any bounds checks.
    Will crash on bounds errors! *)

(** [unsafe_sendmsg_nonblocking_no_sigpipe fd iovecs count]
    similar to {!Bigstring.sendmsg_nonblocking_no_sigpipe}, but
    does not perform any bounds checks.  Will crash on bounds errors! *)
val unsafe_sendmsg_nonblocking_no_sigpipe
  : (file_descr -> t Core_unix.IOVec.t array -> int -> int option) Or_error.t

(** {6 Search} *)

(** [find ?pos ?len char t] returns [Some i] for the smallest [i >= pos] such that
    [t.{i} = char], or [None] if there is no such [i].

    @param pos default = 0
    @param len default = [length bstr - pos] *)
val find :
  ?pos : int
  -> ?len : int
  -> char
  -> t
  -> int option

(** {6 Destruction} *)

external unsafe_destroy : t -> unit = "bigstring_destroy_stub"
(** [unsafe_destroy bstr] destroys the bigstring by deallocating its
    associated data or, if memory-mapped, unmapping the corresponding
    file, and setting all dimensions to zero.  This effectively frees
    the associated memory or address-space resources instantaneously.
    This feature helps working around a bug in the current OCaml runtime,
    which does not correctly estimate how aggressively to reclaim such
    resources.

    This operation is safe unless you have passed the bigstring to
    another thread that is performing operations on it at the same time.
    Access to the bigstring after this operation will yield array bounds
    exceptions.

    @raise Failure if the bigstring has already been deallocated (or
    deemed "external", which is treated equivalently), or if it has
    proxies, i.e. other bigstrings referring to the same data.
*)
