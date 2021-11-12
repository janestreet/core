open! Core
open! Import
open! Unix

let%expect_test "[File_descr.sexp_of_t]" =
  print_s
    [%sexp
      (List.map [ -1; 0; 1; 2; 3 ] ~f:(fun i -> i, i |> File_descr.of_int)
       : (int * File_descr.t) list)];
  [%expect {|
    ((-1 -1)
     (0  0)
     (1  1)
     (2  2)
     (3  _)) |}]
;;

let%test_unit "[Error]" =
  for i = 1 to 10000 do
    [%test_result: int] ~expect:i (Error.Private.to_errno (Error.of_system_int ~errno:i))
  done
;;

let%expect_test "[mkdir_p ~perm name] sets the permissions on [name] and all other \
                 directories above it that it creates to [perm]"
  =
  let dir = "parent/child/grandchild" in
  let _ = Option.try_with (fun () -> remove dir) in
  let prev_umask = Unix.umask 0o000 in
  let perm = 0o755 in
  mkdir_p ~perm dir;
  let make_require_and_remove_dir lex dir =
    let dir_perms = (stat dir).st_perm in
    let lazy_sexp = lazy (Sexp.Atom (sprintf "%s has perms %o" dir dir_perms)) in
    require ~if_false_then_print_s:lazy_sexp lex (perm = dir_perms);
    remove dir
  in
  make_require_and_remove_dir [%here] dir;
  make_require_and_remove_dir [%here] (Filename.dirname dir);
  make_require_and_remove_dir [%here] (Filename.dirname (Filename.dirname dir));
  let () = [%expect {| |}] in
  let dir = "parent/child" in
  mkdir ~perm (Filename.dirname dir);
  mkdir_p ~perm dir;
  make_require_and_remove_dir [%here] dir;
  make_require_and_remove_dir [%here] (Filename.dirname dir);
  let (_ : int) = Unix.umask prev_umask in
  [%expect {| |}]
;;

let%expect_test "[mkdtemp] dir name contains [.tmp.]" =
  let dir = mkdtemp "foo" in
  rmdir dir;
  require
    [%here]
    (String.is_substring (Filename.basename dir) ~substring:".tmp.")
    ~if_false_then_print_s:(lazy [%message (dir : string)]);
  [%expect {| |}]
;;

let%expect_test "[mkstemp] file name contains [.tmp.]" =
  let file, fd = mkstemp "foo" in
  unlink file;
  close fd;
  require
    [%here]
    (String.is_substring (Filename.basename file) ~substring:".tmp.")
    ~if_false_then_print_s:(lazy [%message (file : string)]);
  [%expect {| |}]
;;

let%test _ =
  let dir = Filename_unix.temp_dir "remove_test" "" in
  let file = dir ^/ "test" in
  Out_channel.write_all (dir ^ "/test") ~data:"testing Core_unix.remove";
  remove file;
  remove dir;
  Result.is_error (access file [ `Exists ]) && Result.is_error (access dir [ `Exists ])
;;

let%expect_test "filename validation in [remove]" =
  show_raise (fun () -> remove "tmp-file-to-remove\000corrupted-filename");
  [%expect {| (raised (Invalid_argument "tmp-file-to-remove\000corrupted-filename")) |}]
;;

let%expect_test "expand ~base works" =
  let base = lazy [ "A=1"; "B=2"; "C=3" ] in
  print_s [%sexp (Unix.Env.expand ~base (`Replace [ "C", "2" ]) : string list)];
  [%expect {| (C=2) |}];
  print_s
    [%sexp (Unix.Env.expand ~base (`Override [ "A", None; "B", Some "4" ]) : string list)];
  [%expect {| (C=3 B=4) |}]
;;

let%test_unit "fork_exec ~env last binding takes precedence" =
  protectx
    ~finally:remove
    (Filename_unix.temp_file "test" "fork_exec.env.last-wins")
    ~f:(fun temp_file ->
      let var_in_child env =
        waitpid_exn
          (fork_exec
             ()
             ~env
             ~prog:"sh"
             ~argv:[ "sh"; "-c"; "echo -n ${VAR-undefined} > " ^ temp_file ]);
        In_channel.read_all temp_file
      in
      let env = [ "VAR", "first"; "VAR", "last" ] in
      List.iter
        [ `Replace_raw (List.map env ~f:(fun (v, s) -> v ^ "=" ^ s))
        ; `Replace env
        ; `Extend env
        ; `Override (List.map env ~f:(fun (v, s) -> v, Some s))
        ]
        ~f:(fun env -> [%test_result: string] ~expect:"last" (var_in_child env));
      Unix.putenv ~key:"VAR" ~data:"in-process";
      let env = `Override [ "VAR", None ] in
      [%test_result: string] ~expect:"undefined" (var_in_child env))
;;

let%test_module _ =
  (module struct
    open Open_flags

    let%test _ = can_read rdonly
    let%test _ = can_read rdwr
    let%test _ = not (can_read wronly)
    let%test _ = can_write wronly
    let%test _ = can_write rdwr
    let%test _ = not (can_write rdonly)

    let check t string =
      let sexp1 = sexp_of_t t in
      let sexp2 = Sexp.of_string string in
      if Sexp.( <> ) sexp1 sexp2
      then
        failwiths ~here:[%here] "unequal sexps" (sexp1, sexp2) [%sexp_of: Sexp.t * Sexp.t]
    ;;

    let%test_unit _ = check rdonly "(rdonly)"
    let%test_unit _ = check wronly "(wronly)"
    let%test_unit _ = check rdwr "(rdwr)"
    let%test_unit _ = check append "(rdonly append)"
    let%test_unit _ = check (wronly + append) "(wronly append)"
  end)
;;

let%test_unit _ =
  let test = "unix_test_file" in
  let rm_test () =
    try unlink test with
    | _ -> ()
  in
  rm_test ();
  let fd = openfile test ~mode:[ O_CREAT; O_WRONLY ] in
  let flags = fcntl_getfl fd in
  assert (Open_flags.do_intersect flags Open_flags.wronly);
  assert (Open_flags.are_disjoint flags Open_flags.append);
  fcntl_setfl fd (Open_flags.( + ) flags Open_flags.append);
  assert (Open_flags.do_intersect (fcntl_getfl fd) Open_flags.append);
  rm_test ()
;;

let%test_unit "record format hasn't changed" =
  (* Exclude the time zone (%Z) because it depends on the location. *)
  [%test_result: string]
    ~expect:"1907-07-05 04:03:08; wday=2; yday=010"
    (strftime
       { tm_sec = 8
       ; tm_min = 3
       ; tm_hour = 4
       ; tm_mday = 5
       ; tm_mon = 6
       ; tm_year = 7
       ; tm_wday = 2
       ; tm_yday = 9
       ; tm_isdst = true
       }
       "%F %T; wday=%u; yday=%j")
;;

module Unix_tm_for_testing = struct
  type t = Unix.tm =
    { tm_sec : int
    ; tm_min : int
    ; tm_hour : int
    ; tm_mday : int
    ; tm_mon : int
    ; tm_year : int
    ; tm_wday : int
    ; tm_yday : int
    ; tm_isdst : bool
    }
  [@@deriving fields, sexp_of]

  let equal t1 t2 =
    let eq_int f = Field.get f t1 = Field.get f t2 in
    let eq_bool f = Bool.( = ) (Field.get f t1) (Field.get f t2) in
    Fields.for_all
      ~tm_sec:eq_int
      ~tm_min:eq_int
      ~tm_hour:eq_int
      ~tm_mday:eq_int
      ~tm_mon:eq_int
      ~tm_year:eq_int
      ~tm_wday:eq_int
      ~tm_yday:eq_int
      ~tm_isdst:eq_bool
  ;;
end

let%expect_test "strptime match" =
  let res = strptime ~fmt:"%Y-%m-%d %H:%M:%S" "2012-05-23 10:14:23" in
  let res =
    (* fill in optional fields if they are missing *)
    let tm_wday = if res.Unix.tm_wday = 0 then 3 else res.Unix.tm_wday in
    let tm_yday = if res.Unix.tm_yday = 0 then 143 else res.Unix.tm_yday in
    { res with Unix.tm_wday; tm_yday }
  in
  require_equal
    [%here]
    (module Unix_tm_for_testing)
    res
    { Unix.tm_sec = 23
    ; tm_min = 14
    ; tm_hour = 10
    ; tm_mday = 23
    ; tm_mon = 4
    ; tm_year = 2012 - 1900
    ; tm_wday = 3
    ; tm_yday = 143
    ; tm_isdst = false
    }
;;

let%expect_test "strptime match failed" =
  require_does_raise [%here] (fun () -> strptime ~fmt:"%Y-%m-%d" "2012-05-");
  [%expect {| (Failure "unix_strptime: match failed") |}]
;;

let%expect_test "strptime trailing input" =
  print_s
    [%sexp
      (strptime ~allow_trailing_input:true ~fmt:"%Y-%m-%d" "2012-05-23 10:14:23"
       : Unix_tm_for_testing.t)];
  [%expect
    {|
    ((tm_sec   0)
     (tm_min   0)
     (tm_hour  0)
     (tm_mday  23)
     (tm_mon   4)
     (tm_year  112)
     (tm_wday  3)
     (tm_yday  143)
     (tm_isdst false)) |}];
  require_does_raise [%here] (fun () -> strptime ~fmt:"%Y-%m-%d" "2012-05-23 10:14:23");
  [%expect {| (Failure "unix_strptime: did not consume entire input") |}]
;;

module _ = struct
  open Inet_addr

  (* Can we convert ip addr to an int? *)
  let test_inet4_addr_to_int32 str num =
    let inet = of_string str in
    [%test_result: Int32.t] (inet4_addr_to_int32_exn inet) ~expect:num
  ;;

  let%test_unit _ = test_inet4_addr_to_int32 "0.0.0.1" 1l
  let%test_unit _ = test_inet4_addr_to_int32 "1.0.0.0" 0x1000000l
  let%test_unit _ = test_inet4_addr_to_int32 "172.25.42.1" 0xac192a01l
  let%test_unit _ = test_inet4_addr_to_int32 "4.2.2.1" 0x4020201l
  let%test_unit _ = test_inet4_addr_to_int32 "8.8.8.8" 0x8080808l
  let%test_unit _ = test_inet4_addr_to_int32 "173.194.73.103" 0xadc24967l
  let%test_unit _ = test_inet4_addr_to_int32 "98.139.183.24" 0x628bb718l
  let%test_unit _ = test_inet4_addr_to_int32 "0.0.0.0" 0l
  let%test_unit _ = test_inet4_addr_to_int32 "127.0.0.1" 0x7F000001l
  let%test_unit _ = test_inet4_addr_to_int32 "239.0.0.0" 0xEF000000l
  let%test_unit _ = test_inet4_addr_to_int32 "255.255.255.255" 0xFFFFFFFFl

  let test_inet4_addr_to_int63 str num =
    let inet = of_string str in
    let expect = Int63.of_int64_exn num in
    [%test_result: Int63.t] (inet4_addr_to_int63_exn inet) ~expect
  ;;

  let%test_unit _ = test_inet4_addr_to_int63 "0.0.0.1" 1L
  let%test_unit _ = test_inet4_addr_to_int63 "1.0.0.0" 0x1000000L
  let%test_unit _ = test_inet4_addr_to_int63 "255.255.255.255" 0xffffffffL
  let%test_unit _ = test_inet4_addr_to_int63 "172.25.42.1" 0xac192a01L
  let%test_unit _ = test_inet4_addr_to_int63 "4.2.2.1" 0x4020201L
  let%test_unit _ = test_inet4_addr_to_int63 "8.8.8.8" 0x8080808L
  let%test_unit _ = test_inet4_addr_to_int63 "173.194.73.103" 0xadc24967L
  let%test_unit _ = test_inet4_addr_to_int63 "98.139.183.24" 0x628bb718L
  let%test_unit _ = test_inet4_addr_to_int63 "0.0.0.0" 0L
  let%test_unit _ = test_inet4_addr_to_int63 "127.0.0.1" 0x7F000001L
  let%test_unit _ = test_inet4_addr_to_int63 "239.0.0.0" 0xEF000000L
  let%test_unit _ = test_inet4_addr_to_int63 "255.255.255.255" 0xFFFFFFFFL

  (* And from an int to a string? *)
  let test_inet4_addr_of_int32 num str =
    let inet = of_string str in
    [%test_result: t] (inet4_addr_of_int32 num) ~expect:inet
  ;;

  let%test_unit _ = test_inet4_addr_of_int32 0xffffffffl "255.255.255.255"
  let%test_unit _ = test_inet4_addr_of_int32 0l "0.0.0.0"
  let%test_unit _ = test_inet4_addr_of_int32 0x628bb718l "98.139.183.24"
  let%test_unit _ = test_inet4_addr_of_int32 0xadc24967l "173.194.73.103"

  let test_inet4_addr_of_int63 num str =
    let inet = of_string str in
    [%test_result: t] (inet4_addr_of_int63 num) ~expect:inet
  ;;

  let%test_unit _ =
    test_inet4_addr_of_int63 (Int63.of_int64_exn 0xffffffffL) "255.255.255.255"
  ;;

  let%test_unit _ = test_inet4_addr_of_int63 (Int63.of_int64_exn 0L) "0.0.0.0"

  let%test_unit _ =
    test_inet4_addr_of_int63 (Int63.of_int64_exn 0x628bb718L) "98.139.183.24"
  ;;

  let%test_unit _ =
    test_inet4_addr_of_int63 (Int63.of_int64_exn 0xadc24967L) "173.194.73.103"
  ;;

  (* And round trip for kicks *)
  let%test_unit _ =
    let inet = of_string "4.2.2.1" in
    let inet' = inet4_addr_of_int32 (inet4_addr_to_int32_exn inet) in
    if inet <> inet'
    then failwithf "round-tripping %s produced %s" (to_string inet) (to_string inet') ()
  ;;
end

(* Test the Sexplib_unix exn converter was added correctly *)
let%test_unit "Sexplib_unix sexp converter" =
  let open Sexp.O in
  match sexp_of_exn (Unix.Unix_error (E2BIG, "loc", "arg")) with
  | List [ Atom "Unix.Unix_error"; Atom _human_readable_message; Atom "loc"; Atom "arg" ]
    -> ()
  | something_else ->
    failwithf "sexp_of_exn (Unix_error ...) gave %s" (Sexp.to_string something_else) ()
;;

let%test_module "" =
  (module struct
    open Ifaddr.Flag
    open Ifaddr.Flag.Private

    let int_of_set = Set.fold ~init:0 ~f:(fun acc t -> acc lor core_unix_iff_to_int t)
    let to_int = core_unix_iff_to_int

    let%test_unit _ = [%test_result: Set.t] (set_of_int 0) ~expect:Set.empty

    let%test_unit _ =
      List.iter all ~f:(fun t ->
        let x = to_int t in
        if Int.( <> ) (Int.ceil_pow2 x) x
        then failwiths ~here:[%here] "Flag is not a power of 2" t sexp_of_t)
    ;;

    let%test_unit _ =
      List.iter all ~f:(fun t ->
        [%test_result: Set.t]
          (set_of_int (int_of_set (Set.singleton t)))
          ~expect:(Set.singleton t))
    ;;

    let%test_unit _ =
      [%test_result: Set.t]
        (set_of_int (int_of_set (Set.of_list all)))
        ~expect:(Set.of_list all)
    ;;
  end)
;;

let%expect_test "[symlink] arguments are correct" =
  let dir = mkdtemp "foo" in
  let old_cwd = getcwd () in
  chdir dir;
  let target = "target" in
  let link_name = "link_name" in
  symlink ~target ~link_name;
  let target_of_link_name = readlink link_name in
  print_s [%message "readlink" (target_of_link_name : string)];
  show_raise (fun () -> readlink target);
  remove link_name;
  chdir old_cwd;
  rmdir dir;
  [%expect
    {|
    (readlink (target_of_link_name target))
    (raised (
      Unix.Unix_error "No such file or directory" readlink "((filename target))")) |}]
;;

let%test_module "the search path passed to [create_process_env] has an effect" =
  (module struct
    let call_ls = Unix.create_process_env ~prog:"ls" ~args:[] ~env:(`Extend [])

    let%expect_test "default search path" =
      require_does_not_raise [%here] (fun () -> ignore (Sys.opaque_identity (call_ls ())))
    ;;

    let%expect_test "empty search path" =
      require_does_raise [%here] (fun () -> call_ls ~prog_search_path:[] ());
      [%expect
        {| (Invalid_argument "Core_unix.create_process: empty prog_search_path") |}]
    ;;
  end)
;;

let%expect_test "unix socket path limit workaround" =
  if Sys_unix.file_exists_exn "/proc"
  then
    Exn.protectx
      (Filename_unix.temp_dir ("dir" ^ String.make 120 'r') "")
      ~finally:Unix.rmdir
      ~f:(fun dir ->
        let sock = Unix.socket ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0 () in
        Unix.bind sock ~addr:(ADDR_UNIX (dir ^/ "socket"));
        Unix.set_nonblock sock;
        Unix.listen sock ~backlog:10 (* would fail if bind didn't work *);
        Unix.close sock;
        Unix.unlink (dir ^/ "socket"))
;;

let%expect_test ("Clock.get_cpuclock_for" [@tags "64-bits-only"]) =
  let get_cpuclock_for = ok_exn Unix.Clock.get_cpuclock_for in
  let gettime = ok_exn Unix.Clock.gettime in
  (* This pid is too large to be real  *)
  let bad_pid = Pid.of_int 100_000_000 in
  require_does_raise [%here] (fun () -> get_cpuclock_for bad_pid);
  [%expect {|
    (Unix.Unix_error "No such process" clock_getcpuclockid "") |}];
  let clock = get_cpuclock_for (Unix.getpid ()) in
  let cputime = gettime (Custom clock) in
  (* Testing cpu clocks are hard, but this doesn't crash, and we've definitely accrued cpu
     time. *)
  require [%here] Int63.(cputime > zero);
  [%expect {| |}]
;;

let%test_unit "PGID set by setpgid is inherited by child" =
  protectx ~finally:remove (Filename_unix.temp_file "test" "setpgid") ~f:(fun temp_file ->
    let parent_pid = getpid () in
    setpgid ~of_:parent_pid ~to_:parent_pid;
    match fork () with
    | `In_the_child ->
      let child_pid = getpid () in
      let oc = Out_channel.create temp_file in
      Out_channel.output_string oc (Pid.to_string (getpgid child_pid));
      Out_channel.close oc;
      exit_immediately 0
    | `In_the_parent child_pid ->
      waitpid_exn child_pid;
      let ic = In_channel.create temp_file in
      let child_pgid = In_channel.input_all ic |> String.strip |> Pid.of_string in
      In_channel.close ic;
      [%test_result: Pid.t] ~expect:parent_pid child_pgid)
;;

let with_large_file f =
  let filename = Filename_unix.temp_file "large" "test" in
  protect
    ~f:(fun () ->
      ignore
        (system
           (sprintf
              "dd if=/dev/zero of=%s bs=1 count=1 seek=$(echo '2 ^ 32 + 1' | bc -l) > \
               /dev/null 2>/dev/null"
              filename));
      ignore (f filename))
    ~finally:(fun () -> Sys_unix.remove filename);
  true
;;

let%expect_test "stat-large" = assert (with_large_file (fun fn -> stat fn))
let%expect_test "lstat-large" = assert (with_large_file (fun fn -> lstat fn))

let%expect_test "mcast_sockopts" =
  let sock = Unix.socket ~domain:Unix.PF_INET ~kind:Unix.SOCK_DGRAM ~protocol:0 () in
  Unix.set_mcast_ttl sock 2;
  assert (Unix.get_mcast_ttl sock = 2);
  Unix.set_mcast_ttl sock 4;
  assert (Unix.get_mcast_ttl sock = 4);
  Unix.set_mcast_loop sock true;
  assert (Unix.get_mcast_loop sock);
  Unix.set_mcast_loop sock false;
  assert (not (Unix.get_mcast_loop sock));
  Unix.close sock
;;

let%test_unit "readdir_detailed" =
  let cwd = Sys_unix.getcwd () in
  let cwd_dir, cwd_base = Filename.split cwd in
  let l = Unix.ls_dir_detailed cwd_dir in
  match List.find l ~f:(fun d -> String.( = ) d.name cwd_base) with
  | None ->
    raise_s
      [%sexp
        "couldn't find file in directory"
      , `dir_contents (l : Readdir_detailed.t list)
      , `basename (cwd_base : string)]
  | Some x ->
    (match x.kind with
     | None | Some S_DIR -> ()
     | Some _ -> assert false)
;;
