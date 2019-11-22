open! Import

let threads_have_been_created = ref false

include Thread

let sexp_of_t t = [%message "thread" ~id:(id t : int)]

let create_should_raise = ref false

let create ~on_uncaught_exn f arg =
  if !create_should_raise
  then raise_s [%message "Thread.create requested to raise"];
  threads_have_been_created := true;
  let f arg : unit =
    let exit =
      match on_uncaught_exn with
      | `Print_to_stderr -> false
      | `Kill_whole_process -> true
    in
    Exn.handle_uncaught ~exit (fun () -> f arg)
  in
  create f arg
;;

let threads_have_been_created () = !threads_have_been_created

let wait_signal sigs = wait_signal (List.map ~f:Signal.to_caml_int sigs)

let sigmask cmd sigs =
  let cmd =
    match cmd with
    | `Set -> Unix.SIG_SETMASK
    | `Block -> Unix.SIG_BLOCK
    | `Unblock -> Unix.SIG_UNBLOCK
  in
  let sigs = List.map ~f:Signal.to_caml_int sigs in
  List.map ~f:Signal.of_caml_int (sigmask cmd sigs)
;;

let num_threads () =
  let rec find_thread_count = function
    | [] -> None
    | line :: xs ->
      if String.is_prefix line ~prefix:"Threads:" then
        begin
          try
            Some (int_of_string
                    (String.strip (snd (String.lsplit2_exn line ~on:':'))))
          with
          | _ -> None
        end
      else find_thread_count xs
  in
  try
    find_thread_count
      (In_channel.read_lines
         ("/proc/" ^ string_of_int (Unix.getpid ()) ^ "/status"))
  with _ -> None
;;

let block_forever () =
  Event.sync (Event.receive (Event.new_channel ()))
;;

[%%import "config.h"]

[%%ifdef JSC_PTHREAD_NP]
external setaffinity_self_exn : int Array.t -> unit        = "pthread_np_setaffinity_self"
external getaffinity_self_exn : unit        -> int Array.t = "pthread_np_getaffinity_self"

let setaffinity_self_exn =
  let setaffinity_self_exn cpuset =
    setaffinity_self_exn (Int.Set.to_array cpuset)
  in
  Ok setaffinity_self_exn
;;

let getaffinity_self_exn =
  let getaffinity_self_exn () =
    Int.Set.of_array (getaffinity_self_exn ())
  in
  Ok getaffinity_self_exn
;;
[%%else]

let not_supported name =
  Error.of_string
    (sprintf
       "%s: non-portable pthread extension is not supported on this platform"
       name)
;;

let setaffinity_self_exn = Error (not_supported "pthread_setaffinity_np")
let getaffinity_self_exn = Error (not_supported "pthread_getaffinity_np")
[%%endif]

module For_testing = struct
  let create_should_raise = create_should_raise
end
