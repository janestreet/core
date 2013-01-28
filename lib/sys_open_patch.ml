(* core_sys_open is the same as caml_sys_open, except it does not
   re-acquire the runtime lock until after its call to fcntl.  this code
   should be removed when this fix makes it into the caml runtime
*)
external open_desc: string -> open_flag list -> int -> int = "core_sys_open"

external open_descriptor_in  : int ->  in_channel = "caml_ml_open_descriptor_in"
external open_descriptor_out : int -> out_channel = "caml_ml_open_descriptor_out"

let open_in_gen  mode perm name = open_descriptor_in  (open_desc name mode perm)
let open_out_gen mode perm name = open_descriptor_out (open_desc name mode perm)

