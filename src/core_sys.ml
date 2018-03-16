open! Import

module LargeFile = Unix.LargeFile

let getenv var = try Some (Sys.getenv var) with Not_found_s _ | Caml.Not_found -> None

let getenv_exn var =
  match getenv var with
  | Some x -> x
  | None ->
    Printf.failwithf
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
  let int_size = int_size
  let big_endian = big_endian
  exception Break = Break
  let catch_break = catch_break
  let ocaml_version = ocaml_version
end

exception Command_failed_with_status of Int.t * String.t [@@deriving sexp]

let command_exn string =
  let status = command string in
  if status <> 0 then raise (Command_failed_with_status (status, string))
;;

let unix_quote x =
  if not (String.is_empty x)
  && String.for_all x
       ~f:(function
         | 'a'..'z' | 'A'..'Z' | '0'..'9'
         | '_' | '-' | ':' | '.' | '/' | ',' | '+' | '=' | '%' | '@' -> true
         | _ -> false)
  then
    (* Shell keywords, as output by [compgen -k] for bash, [man dash] for dash, and [PATH=
       type -m '*' | grep reserved] for zsh, except for keywords that have special
       characters like [[. Note that builtins don't matter because 'alias' and alias
       behave the same, unlike 'if' and if. *)
    match x with
    | "if"
    | "then"
    | "else"
    | "elif"
    | "fi"
    | "case"
    | "esac"
    | "for"
    | "select"
    | "while"
    | "until"
    | "do"
    | "done"
    | "in"
    | "function"
    | "time"
    | "coproc"
    | "foreach"
    | "repeat"
    | "nocorrect" -> Filename.quote x
    | _ -> x
  else Filename.quote x
;;

let%test_unit _ =
  [%test_eq: string] (unix_quote "") {|''|};
  [%test_eq: string] (unix_quote "a ") {|'a '|};
  [%test_eq: string] (unix_quote "a'") {|'a'\'''|};
  [%test_eq: string] (unix_quote "a/b/+share+") {|a/b/+share+|};
  [%test_eq: string] (unix_quote "x\000y") "'x\000y'"
;;

let quote =
  match Caml.Sys.os_type with
  | "Unix" -> unix_quote
  | _ -> Filename.quote
;;

let fold_dir ~init ~f directory = Array.fold (readdir directory) ~f ~init

let ls_dir directory = Array.to_list (readdir directory)

(* This function takes six units to cause ocaml to call a different
   function when executing bytecode:
   http://caml.inria.fr/pub/docs/manual-ocaml/intfc.html#sec400
*)
external executing_bytecode
  : unit -> unit -> unit -> unit -> unit -> unit -> bool
  = "executing_bytecode" "not_executing_bytecode" [@@noalloc]

let execution_mode () =
  if executing_bytecode () () () () () () then `Bytecode else `Native

let%test _ = execution_mode () =
             (if String.is_suffix argv.(0) ~suffix:".exe" ||
                 String.is_suffix argv.(0) ~suffix:".native"
              then `Native else `Bytecode)


(* returns size, in bits, of an [int] type in C *)
external c_int_size : unit -> int = "c_int_size" [@@noalloc]

let%test _ = let size = c_int_size () in size >= 16 && size <= Sys.word_size

let home_directory () =
  match getenv "HOME" with
  | Some home -> home
  | None -> (Unix.getpwuid (Unix.geteuid ())).pw_dir

external opaque_identity : 'a -> 'a = "%opaque"
