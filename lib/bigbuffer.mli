open Core_kernel.Std

include module type of Core_kernel.Std.Bigbuffer with type t = Core_kernel.Std.Bigbuffer.t

val add_channel : t -> in_channel -> int -> unit
(** [add_channel b ic n] reads exactly [n] character from the
   input channel [ic] and stores them at the end of buffer [b].
   Raise [End_of_file] if the channel contains fewer than [n]
   characters. *)

val output_buffer : out_channel -> t -> unit
(** [output_buffer oc b] writes the current contents of buffer [b]
   on the output channel [oc]. *)
