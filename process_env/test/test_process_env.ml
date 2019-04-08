open! Core
open Poly
open! Process_env
open! Process_env.Private

let%test_module "process ssh client env" =
  (module struct
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
;;
