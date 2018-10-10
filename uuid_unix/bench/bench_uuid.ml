open! Core_kernel

let%bench "Uuid_unix.create" = Uuid_unix.create ()

let uuid_str = Uuid_unix.create () |> Uuid.to_string

let%bench "Uuid.of_string" = ignore (Uuid.of_string uuid_str : Uuid.t)
