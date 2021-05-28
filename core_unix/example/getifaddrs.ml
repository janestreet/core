open Core
module Unix = Core_unix

let () =
  Unix.getifaddrs () |> List.iter ~f:(printf !"%{sexp:Unix.Ifaddr.t}\n")
;;
