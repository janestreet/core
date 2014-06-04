(** String type based on [Bigarray], for use in I/O and C-bindings *)

open Core_kernel.Std
open Unix

include module type of Core_kernel.Std.Bigstring with type t = Core_kernel.Std.Bigstring.t

(** Type of I/O errors *)
exception IOError of
    int *  (** Number of bytes successfully read/written before error *)
      exn  (** The occurred exception (e.g. Unix_error, End_of_file) *)

(** {6 Input functions} *)

val read : ?min_len : int -> file_descr -> ?pos : int -> ?len : int -> t -> int
(** [read ?min_len fd ?pos ?len bstr] reads at least [min_len] (must be
    greater than or equal zero) and at most [len] (must be greater than
    or equal to [min_len]) bytes from file descriptor [fd], and writes
    them to bigstring [bstr] starting at position [pos].  @return the
    number of bytes actually read.

    [read] returns zero only if [len = 0].  If [len > 0] and there's nothing left to read,
    [read] raises to indicate EOF even if [min_len = 0].

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

val recvfrom_assume_fd_is_nonblocking
  : file_descr -> ?pos : int -> ?len : int -> t -> int * sockaddr
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

val read_assume_fd_is_nonblocking
  : file_descr -> ?pos : int -> ?len : int -> t -> int
(** [read_assume_fd_is_nonblocking fd ?pos ?len bstr] reads up to
    [len] bytes into bigstring [bstr] starting at position [pos] from
    file descriptor [fd] without yielding to other OCaml-threads.
    @return the number of bytes actually read.

    @raise Unix_error in the case of input errors.
    @raise Invalid_argument if the designated range is out of bounds.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

val pread_assume_fd_is_nonblocking
  : file_descr -> offset : int -> ?pos : int -> ?len : int -> t -> int
(** [pread_assume_fd_is_nonblocking fd ~offset ?pos ?len bstr] reads up to [len] bytes
    from file descriptor [fd] at offset [offset], and writes them to bigstring [bstr]
    starting at position [pos].  The fd must be capable of seeking, and the current file
    offset used for a regular [read()] is unchanged. Please see 'man pread' for more
    information. @return the number of bytes actually read.

    @raise Invalid_argument if the designated range is out of bounds.
    @raise Unix_error in the case of input errors.

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

val send_nonblocking_no_sigpipe
  : (file_descr -> ?pos : int -> ?len : int -> t -> int option) Or_error.t
(** [send_nonblocking_no_sigpipe sock ?pos ?len bstr] tries to send
    [len] bytes in bigstring [bstr] starting at position [pos] to socket
    [sock].  @return [Some bytes_written], or [None] if the operation
    would have blocked.

    @raise Invalid_argument if the designated range is out of bounds.
    @raise Unix_error in the case of output errors.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

val sendto_nonblocking_no_sigpipe
  : (file_descr -> ?pos : int -> ?len : int -> t ->
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

val pwrite_assume_fd_is_nonblocking
  : file_descr -> offset : int -> ?pos : int -> ?len : int -> t -> int
(** [pwrite_assume_fd_is_nonblocking fd ~offset ?pos ?len bstr] writes up to [len] bytes
    of bigstring [bstr] starting at position [pos] to file descriptor [fd] at position
    [offset].  The fd must be capable of seeking, and the current file offset used for
    non-positional [read()]/[write()] calls is unchanged.  @return the number of bytes
    written.

    @raise Invalid_argument if the designated range is out of bounds.
    @raise Unix_error in the case of input errors.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

val write_assume_fd_is_nonblocking
  : file_descr -> ?pos : int -> ?len : int -> t -> int
(** [write_assume_fd_is_nonblocking fd ?pos ?len bstr] writes [len]
    bytes in bigstring [bstr] starting at position [pos] to file
    descriptor [fd] without yielding to other OCaml-threads.  @return the
    number of bytes actually written.

    @raise Unix_error in the case of output errors.
    @raise Invalid_argument if the designated range is out of bounds.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

val writev
  : file_descr -> ?count : int -> t Core_unix.IOVec.t array -> int
(** [writev fd ?count iovecs] writes [count] [iovecs] of
    bigstrings to file descriptor [fd].  @return the number of bytes
    written.

    @raise Unix_error in the case of output errors.
    @raise Invalid_argument if count is out of range.

    @param count default = [Array.length iovecs]
*)

val writev_assume_fd_is_nonblocking
  : file_descr -> ?count : int -> t Core_unix.IOVec.t array -> int
(** [writev_assume_fd_is_nonblocking fd ?count iovecs] writes [count]
    [iovecs] of bigstrings to file descriptor [fd] without yielding to
    other OCaml-threads.  @return the number of bytes actually written.

    @raise Unix_error in the case of output errors.
    @raise Invalid_argument if the designated range is out of bounds.

    @param count default = [Array.length iovecs]
*)

val recvmmsg_assume_fd_is_nonblocking
  : (file_descr
     -> ?count : int
     -> ?srcs : sockaddr array
     -> t Core_unix.IOVec.t array
     -> lens : int array
     -> int) Or_error.t
(** [recvmmsg_assume_fd_is_nonblocking fd iovecs ~count ~lens] receives up to [count]
    messages into [iovecs] from file descriptor [fd] without yielding to other OCaml
    threads.  If [~count] is supplied, it must be that [0 <= count <= Array.length
    iovecs].  If [~srcs] is supplied, save the source addresses for corresponding recieved
    messages there.  If supplied, [Array.length srcs] must be [>= count].  Save the
    lengths of the received messages in [lens].  It is required that [Array.length lens >=
    count].

    If an IOVec isn't long enough for its corresponding message, excess bytes may be
    discarded, depending on the type of socket the message is received from.  While the
    [recvmmsg] system call itself does return details of such truncation, etc., those
    details are not (yet) passed through this interface.

    @see "recvmmsg(2)" re. the underlying system call.

    @return the number of messages actually read, or a negative number to indicate
    EWOULDBLOCK or EAGAIN. This is a compromise to mitigate the exception overhead for
    what ends up being a very common result with our use of [recvmmsg].

    @raise Unix_error in the case of output errors.
    @raise Invalid_argument if the designated range is out of bounds.

    @param count default = [Array.length iovecs]
*)

val sendmsg_nonblocking_no_sigpipe
  : (file_descr -> ?count : int ->
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

val output
  : ?min_len : int -> out_channel -> ?pos : int -> ?len : int -> t -> int
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

val really_output
  : out_channel -> ?pos : int -> ?len : int -> t -> unit
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

(** {6 Unsafe functions} *)

external unsafe_read_assume_fd_is_nonblocking
  : file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_read_assume_fd_is_nonblocking_stub"
(** [unsafe_read_assume_fd_is_nonblocking fd ~pos ~len bstr]
    similar to {!Bigstring.read_assume_fd_is_nonblocking}, but does
    not perform any bounds checks.  Will crash on bounds errors! *)

external unsafe_write
  : file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_write_stub"
(** [unsafe_write fd ~pos ~len bstr] similar to
    {!Bigstring.write}, but does not perform any bounds checks.
    Will crash on bounds errors! *)

external unsafe_write_assume_fd_is_nonblocking
  : file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_write_assume_fd_is_nonblocking_stub"
(** [unsafe_write_assume_fd_is_nonblocking fd ~pos ~len bstr]
    similar to {!Bigstring.write_assume_fd_is_nonblocking}, but does
    not perform any bounds checks.  Will crash on bounds errors! *)

external unsafe_read
  : min_len : int -> file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_read_stub"
(** [unsafe_read ~min_len fd ~pos ~len bstr] similar to
    {!Bigstring.read}, but does not perform any bounds checks.
    Will crash on bounds errors! *)

external unsafe_really_recv
  : file_descr -> pos : int -> len : int -> t -> unit
  = "bigstring_really_recv_stub"
(** [unsafe_really_recv sock ~pos ~len bstr] similar to
    {!Bigstring.really_recv}, but does not perform any
    bounds checks.  Will crash on bounds errors! *)

external unsafe_really_write
  : file_descr -> pos : int -> len : int -> t -> unit
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

external unsafe_writev
  : file_descr -> t Core_unix.IOVec.t array -> int -> int
  = "bigstring_writev_stub"
(** [unsafe_writev fd iovecs count] similar to
    {!Bigstring.writev}, but does not perform any bounds checks.
    Will crash on bounds errors! *)

(** [unsafe_sendmsg_nonblocking_no_sigpipe fd iovecs count]
    similar to {!Bigstring.sendmsg_nonblocking_no_sigpipe}, but
    does not perform any bounds checks.  Will crash on bounds errors! *)
val unsafe_sendmsg_nonblocking_no_sigpipe
  : (file_descr -> t Core_unix.IOVec.t array -> int -> int option) Or_error.t

external unsafe_input
  : min_len : int -> in_channel -> pos : int -> len : int -> t -> int
  = "bigstring_input_stub"
(** [unsafe_input ~min_len ic ~pos ~len bstr] similar to
    {!Bigstring.input}, but does not perform any bounds checks.
    Will crash on bounds errors! *)

external unsafe_output
  : min_len : int -> out_channel -> pos : int -> len : int -> t -> int
  = "bigstring_output_stub"
(** [unsafe_output ~min_len oc ~pos ~len bstr] similar to
    {!Bigstring.output}, but does not perform any bounds checks.
    Will crash on bounds errors! *)
