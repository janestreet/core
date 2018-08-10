(** Extends {{!Core_kernel.Bigbuffer}[Core_kernel.Bigbuffer]}. *)

open! Core
open! Import
open! Core_kernel.Bigbuffer

val add_channel : t -> In_channel.t -> int -> unit
(** [add_channel b ic n] reads exactly [n] characters from the input channel [ic] and
    stores them at the end of buffer [b].  Raises [End_of_file] if the channel contains
    fewer than [n] characters. *)

val output_buffer : Out_channel.t -> t -> unit
(** [output_buffer oc b] writes the current contents of buffer [b] on the output channel
    [oc]. *)

val md5 : t -> Md5.t
(** Digest the current contents of the buffer. *)
