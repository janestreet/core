(** Extends {{!Core_kernel.Bigbuffer}[Core_kernel.Bigbuffer]}. *)

open! Import

include module type of struct include Core_kernel.Bigbuffer end (** @open *)

val add_channel : t -> In_channel.t -> int -> unit
(** [add_channel b ic n] reads exactly [n] characters from the input channel [ic] and
    stores them at the end of buffer [b].  Raises [End_of_file] if the channel contains
    fewer than [n] characters. *)

val output_buffer : Out_channel.t -> t -> unit
(** [output_buffer oc b] writes the current contents of buffer [b] on the output channel
    [oc]. *)
