module List = Core_list

type t = out_channel

let seek = Pervasives.LargeFile.seek_out
let pos = Pervasives.LargeFile.pos_out
let length = Pervasives.LargeFile.out_channel_length

let stdout = Pervasives.stdout
let stderr = Pervasives.stderr

type 'a with_create_args = ?binary:bool -> ?append:bool -> ?perm:int -> 'a

let create ?(binary = true) ?(append = false) ?(perm = 0o666) file =
  let flags = [Open_wronly; Open_creat] in
  let flags = (if binary then Open_binary else Open_text) :: flags in
  let flags = (if append then Open_append else Open_trunc) :: flags in
  Sys_open_patch.open_out_gen flags perm file
;;

let set_binary_mode = Pervasives.set_binary_mode_out

let flush = Pervasives.flush

external close_out_channel : t -> unit = "fixed_close_channel";;
let close t = flush t; close_out_channel t
let close_noerr t =
  (try flush t with _ -> ());
  (try close_out_channel t with _ -> ())

let output t ~buf ~pos ~len = Pervasives.output t buf pos len
let output_string = Pervasives.output_string
let output_char = Pervasives.output_char
let output_byte = Pervasives.output_byte
let output_binary_int = Pervasives.output_binary_int
let output_value = Pervasives.output_value

let newline t = output_string t "\n"

let output_lines t lines =
  List.iter lines ~f:(fun line -> output_string t line; newline t)

let with_file ?(binary = true) ?(append=false) ?(perm = 0o666) file ~f =
  Exn.protectx (create ~binary ~append ~perm file) ~f ~finally:close

let write_lines file lines =
  with_file file ~f:(fun t -> output_lines t lines)

let write_all file ~data =
  with_file file ~f:(fun t -> output_string t data)
