open! Core
open! Import

let ssh_client_var = "SSH_CLIENT"

let parse_ssh_client_var = function
  | None -> Ok `Nowhere
  | Some s ->
    (match String.split ~on:' ' s with
     | [] -> failwith "This should never happen, empty string splits as [\"\"]"
     (* Allow any SSH_CLIENT var containing an IP address as the first element.
        Normally, it should have three parts, but relaxing this constraint helps
        debugging/troubleshooting easier. *)
     | address :: _ ->
       Or_error.try_with (fun () -> `From (Unix.Inet_addr.of_string address))
       |> fun e ->
       Or_error.tag_arg e "Could not parse IP address in SSH_CLIENT" s [%sexp_of: string])
;;

let parse_ssh_client () = parse_ssh_client_var (Sys.getenv ssh_client_var)

module Private = struct
  let parse_ssh_client_var = parse_ssh_client_var
end
