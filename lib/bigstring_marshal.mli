(** Utility functions for marshalling to and from bigstring

    @author Markus Mottl <mmottl\@janestreet.com>
*)


open Bigstring

(** {6 Marshalling to/from bigstrings} *)

(** [marshal_blit ?flags v ?pos ?len buf] marshals value [v] to bigstring
    [buf] starting at position [pos] and at most [len] bytes.

    @raise Invalid_argument if the designated range is out of bounds.
    @raise Failure if [buf] cannot hold enough data for marshalling.

    @param flags default = []
    @param pos default = 0
    @param len default = [length buf - pos]
*)
val marshal_blit :
  ?flags : Marshal.extern_flags list -> 'a ->
  ?pos : int -> ?len : int -> t -> int

(** [marshal ?flags v] marshals value [v] to a bigstring using marshalling
    flags [flags].  This function may need two times more memory than
    [marshal_blit].

    @param flags default = []
*)
val marshal : ?flags : Marshal.extern_flags list -> 'a -> t

(** [marshal_data_size ?pos buf] @return the length of marshalled data in
    bigstring [buf] starting at position [pos].

    @raise Invalid_argument if the position is out of bounds considering
    a valid marshal header.

    @param pos default = 0
*)
val marshal_data_size : ?pos : int -> t -> int

(** [unmarshal ?pos buf] unmarshals data contained in [buf] starting
    at position [pos].

    @raise Invalid_argument if the position is out of bounds, or if
    there is not enough data for unmarshalling.

    @param pos default = 0
*)
val unmarshal : ?pos : int -> t -> 'a

(** [unmarshal_next ?pos buf] unmarshals data contained in [buf] starting
    at position [pos].  @return [(v, next_pos)], where [v] is the
    unmarshalled value, and [next_pos] designates the start of the byte
    following the unmarshalled data.

    @raise Invalid_argument if the position is out of bounds, or if
    there is not enough data for unmarshalling.

    @param pos default = 0
*)
val unmarshal_next : ?pos : int -> t -> 'a * int

(** [skip ?pos buf] skips the marshalled data starting at position [pos].
    @return the start of the byte following the unmarshalled data.

    @raise Invalid_argument if the position is out of bounds, or if
    there is not enough data for unmarshalling.

    @param pos default = 0
*)
val skip : ?pos : int -> t -> int

(** [marshal_to_fd ?buf fd v] marshals data [v] to file descriptor [fd]
    using marshalling buffer [buf], and marshalling flags [flags].
    Raises input errors as in {!Bigstring.really_write}.

    @raise Failure if [buf] cannot hold enough data for marshalling.

    @param flags default = []
    @param buf default = determined dynamically
*)
val marshal_to_fd :
  ?buf : t -> ?flags : Marshal.extern_flags list ->
  Unix.file_descr -> 'a -> unit

(** [marshal_to_sock_no_sigpipe ?buf sock v] same as {!marshal_to_fd}, but
    writes to sockets only and uses {!Bigstring.really_send_no_sigpipe}
    to avoid [SIGPIPE] on sockets. *)
val marshal_to_sock_no_sigpipe
  : (?buf : t
     -> ?flags : Marshal.extern_flags list
     -> Unix.file_descr
     -> 'a
     -> unit) Or_error.t

(** [unmarshal_from_sock ?buf sock] unmarshals data from socket [sock]
    using unmarshalling buffer [buf].  Raises input errors as in
    {!Bigstring.really_recv}.

    @raise Failure if [buf] cannot hold enough data for unmarshalling.

    @param buf default = determined dynamically
*)
val unmarshal_from_sock : ?buf : t -> Unix.file_descr -> 'a
