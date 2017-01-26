open Core

let () =
  Unix.getifaddrs () |> List.iter ~f:(printf !"%{sexp:Unix.Ifaddr.t}\n")
;;
