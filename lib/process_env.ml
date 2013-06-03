open Core_kernel.Std

module Unix = Core_unix

let parse_ssh_client () =
  let var = "SSH_CLIENT" in
  match Core_sys.getenv var with
  | None -> Ok `Nowhere
  | Some s ->
    match String.split ~on:' ' s with
    | [address; _; _] ->
      Or_error.try_with (fun () -> `From (Unix.Inet_addr.of_string address))
      |! (fun e -> Or_error.tag_arg e "Could not parse IP address in SSH_CLIENT"
                     s <:sexp_of< string >>)
    | _ ->
      Or_error.error
        (var ^ " did not contain exactly three space-separated segments")
        s <:sexp_of< string >>

