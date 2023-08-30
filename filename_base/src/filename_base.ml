open! Base

include (
  String :
    sig
      type t = string [@@deriving compare, hash, sexp, sexp_grammar]

      include
        Comparable.S
          with type t := t
          with type comparator_witness = String.comparator_witness

      val comparator : (t, comparator_witness) Comparator.t
    end)

include struct
  open Stdlib.Filename

  let check_suffix = check_suffix
  let chop_extension = chop_extension
  let chop_suffix = chop_suffix
  let chop_suffix_opt = chop_suffix_opt
  let current_dir_name = current_dir_name
  let is_implicit = is_implicit
  let is_relative = is_relative
  let parent_dir_name = parent_dir_name
  let dir_sep = dir_sep
  let quote = quote
  let temp_dir_name = get_temp_dir_name ()
  let dirname = dirname
  let basename = basename
end

let is_absolute p = not (is_relative p)

let concat p1 p2 =
  if String.is_empty p1
  then
    Printf.failwithf
      "Filename.concat called with an empty string as its first argument (second \
       argument: %s)"
      p2
      ();
  let rec collapse_trailing s =
    match String.rsplit2 s ~on:'/' with
    | Some ("", ("." | "")) -> ""
    | Some (s, ("." | "")) -> collapse_trailing s
    | None | Some _ -> s
  in
  let rec collapse_leading s =
    match String.lsplit2 s ~on:'/' with
    | Some (("." | ""), s) -> collapse_leading s
    | Some _ | None -> s
  in
  collapse_trailing p1 ^ "/" ^ collapse_leading p2
;;

let to_absolute_exn p ~relative_to =
  if is_relative relative_to
  then
    Printf.failwithf
      "Filename.to_absolute_exn called with a [relative_to] that is a relative path: %s"
      relative_to
      ()
  else if is_absolute p
  then p
  else concat relative_to p
;;

let split s = dirname s, basename s

(* [max_pathname_component_size] comes from getconf _POSIX_NAME_MAX / *)
let max_pathname_component_size = 255

let is_posix_pathname_component s =
  let module S = String in
  s <> "."
  && s <> ".."
  && Int.(0 < S.length s)
  && Int.(S.length s <= max_pathname_component_size)
  && (not (S.contains s '/'))
  && not (S.contains s '\000')
;;

let root = "/"

let split_extension fn =
  let dir, fn =
    match String.rsplit2 ~on:'/' fn with
    | None -> None, fn
    | Some (path, fn) -> Some path, fn
  in
  let fn, ext =
    match String.rsplit2 ~on:'.' fn with
    | None -> fn, None
    | Some (base_fn, ext) -> base_fn, Some ext
  in
  let fn =
    match dir with
    | None -> fn
    | Some dir -> dir ^ "/" ^ fn
  in
  fn, ext
;;

let parts filename =
  let rec loop acc filename =
    match split filename with
    | ("." as base), "." -> base :: acc
    | ("/" as base), "/" -> base :: acc
    | rest, dir -> loop (dir :: acc) rest
  in
  loop [] filename
;;

let of_parts = function
  | [] -> failwith "Filename.of_parts: empty parts list"
  | root :: rest -> List.fold rest ~init:root ~f:Stdlib.Filename.concat
;;

let rec skip_common_prefix l1 l2 =
  match l1, l2 with
  | h1 :: t1, h2 :: t2 when String.equal h1 h2 -> skip_common_prefix t1 t2
  | _ -> l1, l2
;;

let of_absolute_exn a ~relative_to:b =
  if is_relative a
  then
    raise_s
      [%message
        "Filename.of_absolute_exn: first argument must be an absolute path"
          ~first_arg:(a : string)];
  if is_relative b
  then
    raise_s
      [%message
        "Filename.of_absolute_exn: [~relative_to] must be an absolute path"
          ~relative_to:(b : string)];
  let a_parts = parts a in
  let b_parts = parts b in
  let a_suffix, b_suffix = skip_common_prefix a_parts b_parts in
  let go_up = List.map ~f:(fun _ -> parent_dir_name) b_suffix in
  match go_up @ a_suffix with
  | [] -> current_dir_name
  | relpath -> of_parts relpath
;;

let arg_type = `Use_Filename_unix
let create_arg_type = `Use_Filename_unix
let open_temp_file = `Use_Filename_unix
let open_temp_file_fd = `Use_Filename_unix
let realpath = `Use_Filename_unix
let temp_dir = `Use_Filename_unix
let temp_file = `Use_Filename_unix
