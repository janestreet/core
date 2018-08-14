open! Core_kernel

let%bench "Uuid.create" = Uuid.create ()

let uuid_str = Uuid.create () |> Uuid.to_string

let%bench "Uuid.of_string" = ignore (Uuid.of_string uuid_str : Uuid.t)
