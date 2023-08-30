open Core
open Core_bench

let rec make_needle k =
  if k < 0
  then ""
  else (
    let b = make_needle (k - 1) in
    String.concat ~sep:(String.of_char (Char.of_int_exn (65 + k))) [ b; b ])
;;

let make_haystack k j =
  let c = make_needle (k - 1) in
  String.concat ~sep:"$" (List.init j ~f:(fun _ -> c) @ [ make_needle k ])
;;

let compile_kmp p () = ignore (String.Search_pattern.create p : String.Search_pattern.t)
let compile_re2 p () = ignore (Re2.create_exn p : Re2.t)
let compile_pcre p () = ignore (Pcre.regexp ~study:true p : Pcre.regexp)
let _ = compile_pcre

let search_kmp haystack needle =
  let expected = Some (String.length haystack - String.length needle) in
  let needle = String.Search_pattern.create needle in
  stage (fun () ->
    assert (
      [%equal: int option] (String.Search_pattern.index needle ~in_:haystack) expected))
;;

let search_re2 haystack needle =
  let needle = Re2.create_exn needle in
  stage (fun () -> assert (Re2.matches needle haystack))
;;

let search_pcre haystack needle =
  let needle = Pcre.regexp ~study:true needle in
  stage (fun () -> assert (Pcre.pmatch ~rex:needle haystack))
;;

let () =
  Command_unix.run
    (Bench.make_command
       (List.concat
          (List.map [ 3; 9; 12; 16 ] ~f:(fun k ->
             let needle = make_needle k in
             let needle_length = Int.to_string (String.length needle) in
             [ Bench.Test.create
                 ~name:("kmp_create__" ^ needle_length)
                 (compile_kmp needle)
             ; Bench.Test.create
                 ~name:("re2_compile_" ^ needle_length)
                 (compile_re2 needle)
               (* ; Bench.Test.create ~name:("pcre_compile_" ^ needle_length) (compile_pcre x) *)
             ]))
        @ List.concat
            (List.map [ 3; 9 (* 12 *) ] ~f:(fun k ->
               let needle = make_needle k in
               let needle_length = Int.to_string (String.length needle) in
               List.concat
                 (List.map [ 10; 100 (*300 *) ] ~f:(fun j ->
                    let haystack = make_haystack k j in
                    let haystack_length = Int.to_string (String.length haystack) in
                    List.map
                      [ search_kmp, "kmp_search_______"
                      ; search_re2, "re2_search_______"
                      ; search_pcre, "pcre_search_"
                      ]
                      ~f:(fun (f, n) ->
                        Bench.Test.create
                          ~name:(n ^ needle_length ^ "_" ^ haystack_length)
                          (unstage (f haystack needle)))))))))
;;
