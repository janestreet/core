open Core
open Core_bench.Std

let rec x k =
  if k < 0 then "" else
    let b = x (k - 1) in
    String.concat ~sep:(String.of_char (Char.of_int_exn (65 + k))) [b; b]
;;

let pat3  = x 3
let pat10 = x 10
let pat16 = x 16

let y k j =
  let c = x (k - 1) in
  String.concat ~sep:"$" ((List.init j ~f:(fun _ -> c)) @ [x k])
;;

let a p = fun () -> ignore (String.Search_pattern.create p)
let b p = fun () -> ignore (Re2.create_exn p)
let z p = fun () -> ignore (Pcre.regexp ~study:true p)

let c h n =
  let z = Some (String.length h - String.length n) in
  let n = String.Search_pattern.create n in
  fun () -> assert (String.Search_pattern.index n ~in_:h = z)
;;
let d h n =
  let n = Re2.create_exn n in
  fun () -> assert (Re2.matches n h)
;;
let e h n =
  fun () -> assert (Core_extended.Extended_string.is_substring_deprecated ~substring:n h)
;;
let f h n =
  let n = Pcre.regexp ~study:true n in
  fun () -> assert (Pcre.pmatch ~rex:n h)
;;

let prefix s n = String.sub s ~pos:0 ~len:n
let suffix s n = String.sub s ~pos:(String.length s - n) ~len:n

let slow_create needle =
  (* Compute the longest prefix-suffix array from definition, O(n^3) *)
  let n = String.length needle in
  let kmp_arr = Array.create ~len:n (-1) in
  for i = 0 to n - 1 do
    let x = prefix needle (i + 1) in
    for j = 0 to i do
      if prefix x j = suffix x j then
        kmp_arr.(i) <- j
    done
  done;
  (needle, kmp_arr)
;;

let () =

  Command.run (
    Bench.make_command (
      (*
         List.init 13 ~f:(fun k ->
         let x = x k in
         Bench.Test.create ~name:("slow_create_" ^ (Int.to_string (String.length x))) (fun () ->
         ignore (slow_create x)))

         @
      *)
      List.concat (
        List.map [3; 9; 12; 16] ~f:(fun k ->
          let x = x k in
          let kk = Int.to_string (String.length x) in
          [Bench.Test.create ~name:("kmp_create__" ^ kk)  (a x);
           Bench.Test.create ~name:("re2_compile_" ^ kk)    (b x);
           (* Bench.Test.create ~name:("pcre_compile_" ^ kk)   (z x); *)
          ]))
      @
      List.concat (
        List.map [3; 9; (* 12 *)] ~f:(fun k ->
          let x = x k in
          let kk = Int.to_string (String.length x) in
          List.concat (
            List.map [10; 100; (*300 *)] ~f:(fun j ->
              let y = y k j in
              let jj = Int.to_string (String.length y) in
              List.map [(c, "kmp_search_______");
                        (d, "re2_search_______");
                        (e, "mshinwell_search_");
                        (* (f, "pcre_search_"); *)]
                ~f:(fun (f, n) ->
                  Bench.Test.create ~name:(n ^ kk ^ "_" ^ jj) (f y x))))))
    ))
;;
