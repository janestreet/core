(* OASIS_START *)
(* OASIS_STOP *)


let protectx x ~f ~finally =
  let r = try f x with exn -> finally x; raise exn in
  finally x; r

let rm_rf dir =
  ignore (Printf.ksprintf Sys.command "/bin/rm -rf %S" dir : int)

let temp_dir ?(in_dir = Filename.temp_dir_name) prefix suffix =
  let base = Filename.concat in_dir prefix in
  let rec loop i =
     let dir = base ^ string_of_int i ^ suffix in
     let ret = Printf.ksprintf Sys.command "/bin/mkdir %S 2>/dev/null" dir in
     if ret = 0 then dir
     else if Sys.file_exists dir then loop (i + 1)
     else failwith ("mkdir failed on " ^ dir)
   in loop 0

let read_lines ic =
  let rec loop acc =
    match try Some (input_line ic) with End_of_file -> None with
    | Some line -> loop (line :: acc)
    | None -> List.rev acc
  in loop []

let test cmd =
  match Sys.command cmd with
  | 0 -> true
  | 1 -> false
  | _ -> failwith ("command ^cmd^ failed.")

let sh_lines cmd =
  protectx (Filename.temp_file "ocamlbuild_cmd" ".txt")
    ~f:(fun fn ->
      ignore (Sys.command ("(" ^ cmd ^ ") >" ^ fn) : int);
      protectx (open_in fn) ~f:read_lines ~finally:close_in)
    ~finally:Sys.remove

let getconf var =
  let cmd = Printf.sprintf "getconf %S" var in
  match sh_lines cmd with
  | []  -> None
  | [x] -> Some x
  | _   -> failwith ("`"^cmd^"` returned multiple lines")

let endswith x s =
  let len_x = String.length x and len_s = String.length s in
  (len_x <= len_s) && x = String.sub s (len_s - len_x) len_x

let select_files dir ext =
  List.map (Filename.concat dir)
    (List.filter (endswith ext)
      (Array.to_list (Sys.readdir dir)))
;;


let setup_standard_build_flags () =
    begin match getconf "LFS64_CFLAGS" with
    | None -> ()
    | Some flags -> flag ["compile"; "c"] (S[A"-ccopt"; A flags])
    end;
    let cflags =
      let flags =
        [
          "-pipe";
          "-g";
          "-fPIC";
          "-O2";
          "-fomit-frame-pointer";
          "-fsigned-char";
          "-Wall";
          "-pedantic";
          "-Wextra";
          "-Wunused";
(*          "-Werror"; *)
          "-Wno-long-long";
        ]
      in
      let f flag = [A "-ccopt"; A flag] in
      List.concat (List.map f flags)
    in
    flag ["compile"; "c"] (S cflags);

    (* enable warnings; make sure the '@' character isn't in the beginning;
       ms-dos interprets that character specially *)
    flag ["compile"; "ocaml"] (S [A "-w"; A "A@Aemr-28"; A "-strict-sequence" ])
;;

let dispatch = function
  | After_rules as e ->
    setup_standard_build_flags ();

    pflag ["compile"; "ocaml"] "I" (fun x -> S [A "-I"; A x]);

    dep  ["ocaml"; "ocamldep"; "mlh"] (select_files "lib/" ".mlh");

    flag ["mlh"; "ocaml"; "ocamldep"] (S[A"-ppopt"; A"-Ilib/"]);
    flag ["mlh"; "ocaml"; "compile"]  (S[A"-ppopt"; A"-Ilib/"]);
    flag ["mlh"; "ocaml"; "doc"]      (S[A"-ppopt"; A"-Ilib/"]);

    dispatch_default e;

    if test "ld -lrt -shared -o /dev/null 2>/dev/null" then begin
      flag ["ocamlmklib"; "c"]                      (S[A"-lrt"]);
      flag ["use_libcore_stubs"; "link"] (S[A"-cclib"; A"-lrt"]);
    end
  | e -> dispatch_default e

let () = Ocamlbuild_plugin.dispatch dispatch
