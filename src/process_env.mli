(** Utility functions for dealing with the environment. *)

open! Import

(** [parse_ssh_client] reads the [SSH_CLIENT] environment variable, retrieving the IP from
    which you are currently sshing. *)
val parse_ssh_client : unit -> [ `From of Core_unix.Inet_addr.t | `Nowhere ] Or_error.t

(**/**)

(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

  https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  val parse_ssh_client_var
    : string option -> [ `From of Core_unix.Inet_addr.t | `Nowhere ] Or_error.t
end
