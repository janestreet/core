open Core

let nobox name f =
  let message = "Float_test.nobox " ^ name in
  List.iter (List.range (-19) 19) ~f:(fun e ->
    let scale = 10. ** float e in
    for _ = 0 to 14_000 do
      let t = [| Random.float scale -. (scale /. 2.) |] in
      if Version_util.x_library_inlining
      then (
        match
          let minor_before = Gc.minor_words () in
          let major_before = Gc.major_words () in
          let result = f t in
          let minor_after = Gc.minor_words () in
          let major_after = Gc.major_words () in
          ( `minor (minor_after - minor_before)
          , `major (major_after - major_before)
          , `result result )
        with
        (* When the result doesn't fit, [f] will raise.  In that case, we allow
           allocation, so we don't check anything here. *)
        | exception _ -> ()
        | `minor minor, `major major, `result result ->
          let _message = message ^ " -> " ^ Int.to_string result in
          [%test_result: int] ~message minor ~expect:0;
          [%test_result: int] ~message major ~expect:0)
    done)
;;

(* We have to carefully allow the compiler to unbox t.(0) to test that each function
   doesn't force it to re-box the float.  We need the extra fun per function to allow the
   compiler to inline each one separately around t.(0). *)
let () =
  List.iter
    ~f:(fun (name, t) -> nobox name t)
    [ ("iround_down_exn", fun t -> Float.iround_down_exn t.(0))
    ; ("iround_nearest_exn", fun t -> Float.iround_nearest_exn t.(0))
    ; ("iround_up_exn", fun t -> Float.iround_up_exn t.(0))
    ; ("iround_towards_zero_exn", fun t -> Float.iround_towards_zero_exn t.(0))
    ; ("Caml.int_of_float", fun t -> Caml.int_of_float t.(0))
    ]
;;
