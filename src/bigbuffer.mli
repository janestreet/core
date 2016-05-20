open! Core_kernel.Std

include module type of struct include Core_kernel.Std.Bigbuffer end

val add_channel : t -> In_channel.t -> int -> unit
(** [add_channel b ic n] reads exactly [n] character from the
   input channel [ic] and stores them at the end of buffer [b].
   Raise [End_of_file] if the channel contains fewer than [n]
   characters. *)

val output_buffer : Out_channel.t -> t -> unit
(** [output_buffer oc b] writes the current contents of buffer [b]
   on the output channel [oc]. *)
