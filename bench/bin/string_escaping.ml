open Core

module Bench = Core_bench.Bench

let () =
  let module E = String.Escaping in
  let escape =
    Staged.unstage (E.escape ~escapeworthy:['x';'y';'z'] ~escape_char:'\\')
  in
  let unescape =
    Staged.unstage (E.unescape ~escape_char:'\\')
  in
  let rex = Pcre.regexp "[xyz\\\\]" in
  let unrex = Pcre.regexp "\\\\." in
  let escape_pcre s =
    Pcre.substitute ~rex ~subst:(fun s -> "\\"^s) s
  in
  let unescape_pcre s =
    Pcre.substitute ~rex:unrex ~subst:(fun s -> Char.to_string s.[1]) s
  in
  let strings =
    [ "aaa"
    ; "xyz"
    ; "aaaaaaaaa"
    ; "aaxaaxaax"
    ; "abcde\\abcde\\abcde\\abcde\\abcde\\abcde\\"
    ; "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    ; "aaaaaaaaaaaaaaaaaa\\aaaaaaaaaaaaaaaaaaaaaaa"
    ; "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
    ]
  in
  Bench.bench (List.concat_map strings ~f:(fun str ->
    let go name escape unescape =
      Bench.Test.create ~name:(sprintf "%s-%s" name str)
        (fun () -> assert (unescape (escape str) = str))
    in
    [ go "String.Escaping" escape unescape
    ; go "Pcre" escape_pcre unescape_pcre
    ])
  )
;;
