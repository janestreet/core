open! Core
open Core_bench
module Always = Type_immediacy.Always

module M = struct
  type t =
    | A
    | B
    | C
  [@@deriving typerep]

  let always = Always.of_typerep_exn [%here] typerep_of_t
end

let tests =
  [ Bench.Test.create ~name:"Always.value_as_int" (fun () ->
      ignore (Always.value_as_int M.always M.A : int))
  ]
;;

let () = Command_unix.run (Bench.make_command tests)
