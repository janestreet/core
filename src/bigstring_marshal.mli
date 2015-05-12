(** Utility functions for marshalling to and from bigstring

    @author Markus Mottl <mmottl\@janestreet.com>
*)

open Core_kernel.Std
open Bigstring

include module type of Core_kernel.Std.Bigstring_marshal

(** [marshal_to_fd ?buf fd v] marshals data [v] to file descriptor [fd]
    using marshalling buffer [buf], and marshalling flags [flags].
    Raises input errors as in {!Bigstring.really_write}.

    @raise Failure if [buf] cannot hold enough data for marshalling.

    @param flags default = []
    @param buf default = determined dynamically
*)
val marshal_to_fd
  :  ?buf : t
  -> ?flags : Marshal.extern_flags list
  -> Unix.file_descr
  -> 'a
  -> unit

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
