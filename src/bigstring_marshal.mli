(** Utility functions for marshalling to and from bigstring, extending
    {{!Core_kernel.Bigstring_marshal}[Core_kernel.Bigstring_marshal]}. *)

open! Import
open Bigstring

include module type of struct include Core_kernel.Bigstring_marshal end (** @open *)

(** [marshal_to_fd ?buf fd v] marshals data [v] to file descriptor [fd] using marshalling
    buffer [buf], and marshalling flags [flags].  Raises input errors as in
    {!Bigstring.really_write}.

    Raises [Failure] if [buf] cannot hold enough data for marshalling. *)
val marshal_to_fd
  :  ?buf : t (** default = determined dynamically *)
  -> ?flags : Marshal.extern_flags list (** default = [] *)
  -> Unix.file_descr
  -> 'a
  -> unit

(** [marshal_to_sock_no_sigpipe ?buf sock v] same as {!marshal_to_fd}, but writes to
    sockets only and uses {!Bigstring.really_send_no_sigpipe} to avoid [SIGPIPE] on
    sockets. *)
val marshal_to_sock_no_sigpipe
  : (?buf : t
     -> ?flags : Marshal.extern_flags list
     -> Unix.file_descr
     -> 'a
     -> unit) Or_error.t

(** [unmarshal_from_sock ?buf sock] unmarshals data from socket [sock] using unmarshalling
    buffer [buf].  Raises input errors as in {!Bigstring.really_recv}.

    Raises [Failure] if [buf] cannot hold enough data for unmarshalling. *)
val unmarshal_from_sock
  :  ?buf : t (** default = determined dynamically *)
  -> Unix.file_descr
  -> 'a
