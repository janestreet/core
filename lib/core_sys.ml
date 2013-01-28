module LargeFile = Unix.LargeFile

let getenv var = try Some (Sys.getenv var) with Not_found -> None

let getenv_exn var =
  match getenv var with
  | Some x -> x
  | None ->
    Core_printf.failwithf
      "Sys.getenv_exn: environment variable %s is not set" var ()


let stat_check_exn f ?(follow_symlinks = true) path =
  let rec loop () =
    try
      f (if follow_symlinks then
          LargeFile.stat path
        else
          LargeFile.lstat path)
    with
    | Unix.Unix_error(Unix.EINTR,_,_) -> loop ()
    | Unix.Unix_error ((Unix.ENOENT|Unix.ENOTDIR), _, _) -> false
  in
  loop ()
;;

let stat_check f ?follow_symlinks path =
  try
    if stat_check_exn f ?follow_symlinks path then
      `Yes
    else
      `No
  with
  | Unix.Unix_error ((Unix.EACCES|Unix.ELOOP), _, _)   -> `Unknown
;;

let file_exists = stat_check (fun _ -> true)
let file_exists_exn = stat_check_exn (fun _ -> true)

let is_directory = stat_check (fun stat -> stat.LargeFile.st_kind = Unix.S_DIR)
let is_directory_exn = stat_check_exn
  (fun stat -> stat.LargeFile.st_kind = Unix.S_DIR)

let is_file = stat_check (fun stat -> stat.LargeFile.st_kind = Unix.S_REG)
let is_file_exn = stat_check_exn
  (fun stat -> stat.LargeFile.st_kind = Unix.S_REG)

include struct
  open Caml.Sys
  let argv = argv
  let executable_name = executable_name
  let remove = remove
  let rename = rename
  let command = command
  let chdir = chdir
  let getcwd = getcwd
  let readdir = readdir
  let interactive = interactive
  let os_type = os_type
  let word_size = word_size
  exception Break = Break
  let catch_break = catch_break
  let ocaml_version = ocaml_version
end

exception Command_failed_with_status of Core_int.t * Core_string.t with sexp

let command_exn string =
  let status = command string in
  if status <> 0 then raise (Command_failed_with_status (status, string))
;;

let fold_dir ~init ~f directory = Core_array.fold (readdir directory) ~f ~init

let ls_dir directory = Core_array.to_list (readdir directory)

(* This function takes six units to cause ocaml to call a different
   function when executing bytecode:
   http://caml.inria.fr/pub/docs/manual-ocaml/manual033.html#19.1.2
*)
external executing_bytecode
  : unit -> unit -> unit -> unit -> unit -> unit -> bool
  = "executing_bytecode" "not_executing_bytecode" "noalloc"

let execution_mode () =
  if executing_bytecode () () () () () () then `Bytecode else `Native

TEST = execution_mode () =
         (if Core_string.is_suffix argv.(0) ~suffix:".exe" ||
             Core_string.is_suffix argv.(0) ~suffix:".native"
          then `Native else `Bytecode)


(* returns size, in bits, of an [int] type in C *)
external c_int_size : unit -> int = "c_int_size" "noalloc"

TEST = let size = c_int_size () in size >= 16 && size <= Sys.word_size

