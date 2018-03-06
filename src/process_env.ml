open! Import

module Unix = Core_unix

let ssh_client_var = "SSH_CLIENT"

let parse_ssh_client_var =
  function
  | None   -> Ok `Nowhere
  | Some s ->
    match String.split ~on:' ' s with
    | [] -> failwith "This should never happen, empty string splits as [\"\"]"
    (* Allow any SSH_CLIENT var containing an IP address as the first element.
       Normally, it should have three parts, but relaxing this constraint helps
       debugging/troubleshooting easier. *)
    | address :: _ ->
      Or_error.try_with (fun () -> `From (Unix.Inet_addr.of_string address))
      |> (fun e -> Or_error.tag_arg e "Could not parse IP address in SSH_CLIENT"
                     s [%sexp_of: string])

let parse_ssh_client () =
  parse_ssh_client_var (Core_sys.getenv ssh_client_var)

let%test_module "process ssh client env" = (module struct
  let ip = "127.0.0.1"
  let inet_addr = Unix.Inet_addr.of_string ip

  let%test _ = parse_ssh_client_var None = Ok `Nowhere

  (* IP-only SSH_CLIENT env var, which maybe used for debugging and troubleshooting *)
  let%test _ = parse_ssh_client_var (Some ip) = Ok (`From inet_addr)
  (* Correctly formatted SSH_CLIENT env var *)
  let%test _ = parse_ssh_client_var (Some (ip ^ " 12345 67890")) = Ok (`From inet_addr)

  (* malformed ip *)
  let%test _ = Result.is_error (parse_ssh_client_var (Some "12345.67890"))
  let%test _ = Result.is_error (parse_ssh_client_var (Some "random string"))
  let%test _ = Result.is_error (parse_ssh_client_var (Some ""))
end)

