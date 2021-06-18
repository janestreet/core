open! Core
open! Import
open Core.Sys
module LargeFile = Unix.LargeFile

let getenv_f ~f var =
  try Some (f var) with
  | Not_found_s _ | Caml.Not_found -> None
;;

let unsafe_getenv = getenv_f ~f:Unix.unsafe_getenv

let getenv_exn_f ~f_str ~f var =
  match f var with
  | Some x -> x
  | None -> Printf.failwithf "%s: environment variable %s is not set" f_str var ()
;;

let unsafe_getenv_exn = getenv_exn_f ~f_str:"Sys.unsafe_getenv_exn" ~f:unsafe_getenv


let stat_check_exn f ?(follow_symlinks = true) path =
  let rec loop () =
    try f (if follow_symlinks then LargeFile.stat path else LargeFile.lstat path) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
    | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> false
  in
  loop ()
;;

let stat_check f ?follow_symlinks path =
  try if stat_check_exn f ?follow_symlinks path then `Yes else `No with
  | Unix.Unix_error ((Unix.EACCES | Unix.ELOOP), _, _) -> `Unknown
;;

let file_exists = stat_check (fun _ -> true)
let file_exists_exn = stat_check_exn (fun _ -> true)
let is_directory = stat_check (fun stat -> Poly.equal stat.LargeFile.st_kind Unix.S_DIR)

let is_directory_exn =
  stat_check_exn (fun stat -> Poly.equal stat.LargeFile.st_kind Unix.S_DIR)
;;

let is_file = stat_check (fun stat -> Poly.equal stat.LargeFile.st_kind Unix.S_REG)

let is_file_exn =
  stat_check_exn (fun stat -> Poly.equal stat.LargeFile.st_kind Unix.S_REG)
;;

include struct
  open Caml.Sys

  let executable_name = executable_name
  let remove = remove
  let rename = rename
  let command = command
  let chdir = chdir
  let getcwd = getcwd
  let readdir = readdir

  exception Break = Break

  let catch_break = catch_break
end

exception Command_failed_with_status of Int.t * String.t [@@deriving sexp]

let command_exn string =
  let status = command string in
  if status <> 0 then raise (Command_failed_with_status (status, string))
;;

let fold_dir ~init ~f directory = Array.fold (readdir directory) ~f ~init
let ls_dir directory = Array.to_list (readdir directory)

(* This function takes six units to cause ocaml to call a different
   function when executing bytecode:
   http://caml.inria.fr/pub/docs/manual-ocaml/intfc.html#ss:c-prim-impl
*)
external executing_bytecode
  :  unit
  -> unit
  -> unit
  -> unit
  -> unit
  -> unit
  -> bool
  = "executing_bytecode" "not_executing_bytecode"
[@@noalloc]

let execution_mode () =
  if executing_bytecode () () () () () () then `Bytecode else `Native
;;

(* returns size, in bits, of an [int] type in C *)
external c_int_size : unit -> int = "c_int_size" [@@noalloc]

let home_directory () =
  match getenv "HOME" with
  | Some home -> home
  | None -> (Unix.getpwuid (Unix.geteuid ())).pw_dir
;;

[%%if ocaml_version < (4, 09, 0)]

let override_argv args =
  let len = Array.length args in
  assert (len <= Array.length Sys.argv);
  Array.blit ~src:args ~src_pos:0 ~dst:Sys.argv ~dst_pos:0 ~len;
  (Caml.Obj.truncate [@ocaml.alert "-deprecated"]) (Obj.repr Sys.argv) len;
  Arg.current := 0
;;

[%%else]

external caml_sys_modify_argv : string array -> unit = "caml_sys_modify_argv"

let override_argv new_argv =
  caml_sys_modify_argv new_argv;
  Arg.current := 0
;;

[%%endif]
