open Core.Std

let main () =
  match Sys.argv with
  | [| _; file1; file2 |]  ->
      let original = Sexp.load_sexps file1 in
      let updated  = Sexp.load_sexps file2 in
      Printf.printf "--- %s\n" file1;
      Printf.printf "+++ %s\n" file2;
      Core_extended.Extended_sexp.print_diff
        ~original:(Sexp.List original)
        ~updated:(Sexp.List updated) ()
  | _ -> print_endline "usage: diff_sexps <original file> <updated file>"

let () = main ()
