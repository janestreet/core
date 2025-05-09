open! Base

let name _t = "name"

let%bench_fun "list on big list success" =
  let l = List.create ~len:10_000 Validate.pass in
  fun () -> (Validate.list ~name Fn.id l |> Sys.opaque_identity : Validate.t) |> ignore
;;

let%bench_fun "list on big list mixed" =
  let l =
    List.init 10_000 ~f:(fun n ->
      if n % 2 = 0 then Validate.pass else Validate.fail (Printf.sprintf "fail%d" n))
  in
  fun () -> (Validate.list ~name Fn.id l |> Sys.opaque_identity : Validate.t) |> ignore
;;

let%bench_fun "list on big list failure" =
  let l = List.init 10_000 ~f:(fun n -> Validate.fail (Printf.sprintf "fail%d" n)) in
  fun () -> (Validate.list ~name Fn.id l |> Sys.opaque_identity : Validate.t) |> ignore
;;

let%bench_fun "list_indexed on big list success" =
  let l = List.create ~len:10_000 Validate.pass in
  fun () -> (Validate.list_indexed Fn.id l |> Sys.opaque_identity : Validate.t) |> ignore
;;

let%bench_fun "list_indexed on big list mixed" =
  let l =
    List.init 10_000 ~f:(fun n ->
      if n % 2 = 0 then Validate.pass else Validate.fail (Printf.sprintf "fail%d" n))
  in
  fun () -> (Validate.list_indexed Fn.id l |> Sys.opaque_identity : Validate.t) |> ignore
;;

let%bench_fun "list_indexed on big list failure" =
  let l = List.init 10_000 ~f:(fun n -> Validate.fail (Printf.sprintf "fail%d" n)) in
  fun () -> (Validate.list_indexed Fn.id l |> Sys.opaque_identity : Validate.t) |> ignore
;;
