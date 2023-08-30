open! Import
include Base.Sys

let unix_quote x =
  if (not (String.is_empty x))
     && String.for_all x ~f:(function
          | 'a' .. 'z'
          | 'A' .. 'Z'
          | '0' .. '9'
          | '_' | '-' | ':' | '.' | '/' | ',' | '+' | '=' | '%' | '@' -> true
          | _ -> false)
  then (
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
    | _ -> x)
  else Filename.quote x
;;

let quote =
  match Stdlib.Sys.os_type with
  | "Unix" -> unix_quote
  | _ -> Filename.quote
;;

let concat_quoted split_command =
  List.map ~f:quote split_command |> String.concat ~sep:" "
;;

let c_int_size = `Use_Sys_unix
let catch_break = `Use_Sys_unix
let chdir = `Use_Sys_unix
let command = `Use_Sys_unix
let command_exn = `Use_Sys_unix
let executable_name = `Use_Sys_unix
let execution_mode = `Use_Sys_unix
let file_exists = `Use_Sys_unix
let file_exists_exn = `Use_Sys_unix
let fold_dir = `Use_Sys_unix
let getcwd = `Use_Sys_unix
let home_directory = `Use_Sys_unix
let is_directory = `Use_Sys_unix
let is_directory_exn = `Use_Sys_unix
let is_file = `Use_Sys_unix
let is_file_exn = `Use_Sys_unix
let ls_dir = `Use_Sys_unix
let override_argv = `Use_Sys_unix
let readdir = `Use_Sys_unix
let remove = `Use_Sys_unix
let rename = `Use_Sys_unix
let unsafe_getenv = `Use_Sys_unix
let unsafe_getenv_exn = `Use_Sys_unix

exception Break = Stdlib.Sys.Break

module Private = struct
  let unix_quote = unix_quote
end
