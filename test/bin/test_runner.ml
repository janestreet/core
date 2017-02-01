open Core;;
open OUnit;;

let rec count acc = function
  | TestCase _ -> acc + 1
  | TestLabel (_,test) -> count acc test
  | TestList l ->
    List.fold_left l
      ~f:count
      ~init:acc

let count_pa_ounit = function
  | TestList l ->
    List.fold_left l
      ~f:(fun (pa_ounit,total) test ->
        match test with
        | TestLabel (s,tst) when String.is_suffix ~suffix:".ml" s ->
          let cnt = count 0 tst in
          (pa_ounit+cnt,total+cnt)
        | test ->
          pa_ounit,count total test)
      ~init:(0,0)
  | v ->
    0,count 0 v

let main () =
  let cnt_ounit =
    "-cnt",
    Arg.Unit (fun () ->
      let pa_ounit,total = count_pa_ounit (Test.all ()) in
      Printf.printf "converted: %i total: %i\n%!" pa_ounit total;
      exit 0
    ),
    " Count how many of the tests were converted to pa_ounit (for \
     informational purposes only)"
  in
  ignore (run_test_tt_main
            ~arg_specs:[cnt_ounit]
            (Test.all ()):OUnit.test_result list)
let () = Exn.handle_uncaught ~exit:true main
