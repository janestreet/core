open! Import

include Core_kernel.Filename

let create_arg_type ?key of_string =
  Core_kernel.Command.Arg_type.create ?key of_string ~complete:(fun _ ~part ->
    let completions =
      (* `compgen -f` handles some fiddly things nicely, e.g. completing "foo" and
         "foo/" appropriately. *)
      let command = sprintf "bash -c 'compgen -f %s'" part in
      let chan_in = Unix.open_process_in command in
      let completions = In_channel.input_lines chan_in in
      ignore (Unix.close_process_in chan_in);
      List.map (List.sort ~compare:String.compare completions) ~f:(fun comp ->
        if Caml.Sys.is_directory comp
        then comp ^ "/"
        else comp)
    in
    match completions with
    | [dir] when String.is_suffix dir ~suffix:"/" ->
      (* If the only match is a directory, we fake out bash here by creating a bogus
         entry, which the user will never see - it forces bash to push the completion
         out to the slash. Then when the user hits tab again, they will be at the end
         of the line, at the directory with a slash and completion will continue into
         the subdirectory.
      *)
      [dir; dir ^ "x"]
    | _ -> completions
  )

let arg_type = create_arg_type Fn.id

external realpath : string -> string = "core_unix_realpath"

let prng = Random.State.make_self_init ~allow_in_tests:true ()

(* try up to 1000 times to not get a Sys_error when opening a temp
   file / name: *)
let retry ?(in_dir=temp_dir_name) ~f prefix suffix =
  let escape s =
    String.map s ~f:(function
      | '/' | '\'' | '\000' | '\n' | '-' -> '_'
      | c -> c)
  in
  let prefix = escape prefix in
  let suffix = escape suffix in
  let rec try_name counter =
    let name =
      let rnd = Random.State.bits prng land 0xFF_FFFF in
      (Printf.sprintf "%s.tmp.%06x%s" prefix rnd suffix)
    in
    let name = concat in_dir name in
    try
      f name
    with Sys_error _ | Unix.Unix_error _ as e ->
      if Int.(counter >= 1000) then raise e else try_name (counter + 1)
  in
  try_name 0

(* these functions are the same as the ones in the std lib but you
   can override the temporary directory you are working in.  They also try the
   exact filename specified by the user before reverting to the "try with"
   machinery.
*)

let temp_dir ?(perm=0o700) ?in_dir prefix suffix =
  retry ?in_dir prefix suffix
    ~f:(fun name -> Unix.mkdir name perm; name)

let open_temp_file ?(perm=0o600) ?in_dir prefix suffix =
  retry ?in_dir prefix suffix
    ~f:(fun name -> (name, Out_channel.create ~perm ~fail_if_exists:true name))

let open_temp_file_fd ?(perm=0o600) ?in_dir prefix suffix =
  retry ?in_dir prefix suffix
    ~f:(fun name ->
      (name, UnixLabels.openfile ~perm ~mode:[O_EXCL; O_CREAT; O_RDWR] name))

let temp_file ?perm ?in_dir prefix suffix =
  let (name, oc) = open_temp_file ?perm ?in_dir prefix suffix in
  Out_channel.close oc;
  name
