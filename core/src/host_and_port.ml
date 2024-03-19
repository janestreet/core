module Stable = struct
  open Stable_internal

  module V1 = struct
    module Serializable = struct
      type t = string * int [@@deriving sexp, bin_io, stable_witness]
    end

    module T0 = struct
      type t =
        { host : String.t
        ; port : Int.t
        }
      [@@deriving compare, equal, hash, quickcheck]

      let to_serializable { host; port } = host, port
      let of_serializable (host, port) = { host; port }
    end

    module T1 = struct
      include T0

      include
        Binable.Stable.Of_binable.V1 [@alert "-legacy"]
          (Serializable)
          (struct
            include T0

            let to_binable = to_serializable
            let of_binable = of_serializable
          end)

      let stable_witness =
        Stable_witness.of_serializable
          Serializable.stable_witness
          of_serializable
          to_serializable
      ;;

      let%expect_test "stable" =
        print_endline [%bin_digest: t];
        print_endline [%bin_digest: Serializable.t];
        [%expect
          {|
          957990f0fc4161fb874e66872550fb40
          957990f0fc4161fb874e66872550fb40
          |}]
      ;;

      include
        Sexpable.Stable.Of_sexpable.V1
          (Serializable)
          (struct
            include T0

            let to_sexpable = to_serializable
            let of_sexpable = of_serializable
          end)

      open! Import
      open! Std_internal
      open! T0

      let to_string { host; port } = sprintf "%s:%d" host port

      let of_string s =
        match String.split s ~on:':' with
        | [ host; port ] ->
          let port =
            try Int.of_string port with
            | _exn -> failwithf "Host_and_port.of_string: bad port: %s" s ()
          in
          { host; port }
        | _ -> failwithf "Host_and_port.of_string: %s" s ()
      ;;

      let t_of_sexp = function
        | Sexp.Atom s as sexp ->
          (try of_string s with
           | Failure err -> of_sexp_error err sexp)
        | sexp -> t_of_sexp sexp
      ;;

      let t_sexp_grammar =
        Sexplib.Sexp_grammar.
          { untyped =
              Union
                [ (* handles the host:port string case *)
                  String
                ; (* handles the list (host port) case  *)
                  List (Cons (String, Cons (Integer, Empty)))
                ]
          }
      ;;

      include (val Comparator.Stable.V1.make ~compare ~sexp_of_t)
    end

    include T1
    include Comparable.Stable.V1.With_stable_witness.Make (T1)

    let%test_unit "t_of_sexp" =
      [%test_result: t]
        (t_of_sexp (Sexp.of_string {|(localhost 8080)|}))
        ~expect:{ host = "localhost"; port = 8080 };
      [%test_result: t]
        (t_of_sexp (Sexp.of_string {|localhost:8080|}))
        ~expect:{ host = "localhost"; port = 8080 }
    ;;

    let%test_unit "sexp roundtrip" =
      Quickcheck.test [%quickcheck.generator: t] ~f:(fun t ->
        [%test_result: t] (t_of_sexp (sexp_of_t t)) ~expect:t)
    ;;
  end
end

open! Import
open! Std_internal

module Latest = struct
  module T = Stable.V1
  include T

  include Pretty_printer.Register (struct
    type nonrec t = t

    let to_string = to_string
    let module_name = "Core.Host_and_port"
  end)

  include (Hashable.Make_binable (T) : Hashable.S_binable with type t := t)
  include Comparable.Make_binable_using_comparator (T)
end

include Latest

let create ~host ~port = { host; port }
let host t = t.host
let port t = t.port
let tuple t = to_serializable t
let type_id = Type_equal.Id.create ~name:"Host_and_port" sexp_of_t

module Hide_port_in_test = struct
  include Latest

  let sexp_of_t t =
    match am_running_test with
    | false -> sexp_of_t t
    | true -> List [ Atom t.host; Atom "PORT" ]
  ;;
end
