open Core.Std

let main () =
  match Sys.argv with
  | [| _; file1; file2 |]  ->
      let original = Sexp.load_sexp file1 in
      let updated = Sexp.load_sexp file2 in
      Printf.printf "--- %s\n" file1;
      Printf.printf "+++ %s\n" file2;
      Core_extended.Extended_sexp.print_diff ~original ~updated ()
  | _ -> print_endline "usage: diff_sexp <original file> <updated file>"

let () = main ()
