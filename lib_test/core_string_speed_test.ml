open Core.Std

(* let escape = String.Escaping.escape ~escapeworthy:['%';'^'] ~escape_char:'\\'
let escape_gen = String.Escaping.escape_gen ~escapeworthy_map:['%','%';'^','^'] ~escape_char:'\\'

let unescape = String.Escaping.unescape ~escape_char:'\\'
let unescape_gen = String.Escaping.unescape_gen ~map:[] ~escape_char:'\\'

let () =
  let now = Utime.now () in
  for i = 1 to 500_000 do
    ignore (escape_gen
    "foo%bar\\^baz^quux%%%%%%%%%%%%%%%aoeusnthoaeusntaohusoaeusnatohunsaoehusnaoehusnaotusnaoehuasoenuht^^^^^^^^^")
  done;
  let later = Utime.now () in
  prerr_endline (Utime.span_to_string (Utime.abs_diff later now));

  let now = Utime.now () in
  for i = 1 to 500_000 do
    ignore (escape
    "foo%bar\\^baz^quux%%%%%%%%%%%%%%%aoeusnthoaeusntaohusoaeusnatohunsaoehusnaoehusnaotusnaoehuasoenuht^^^^^^^^^")
  done;
  let later = Utime.now () in
  prerr_endline (Utime.span_to_string (Utime.abs_diff later now));



  let now = Utime.now () in
  for i = 1 to 500_000 do
    ignore (unescape_gen
    "foo%bar\\^baz^quux\\s\\s\\s\\s\\s\\s\\s\\s%%%%%%%%%%%%%%%aoeusnthoaeusntaohusoaeusnatohunsaoehusnaoehusnaotusnaoehuasoenuht^^^^^^^^^")
  done;
  let later = Utime.now () in
  prerr_endline (Utime.span_to_string (Utime.abs_diff later now));

  let now = Utime.now () in
  for i = 1 to 500_000 do
    ignore (unescape
    "foo%bar\\^baz^quux\\s\\s\\s\\s\\s\\s\\s\\s%%%%%%%%%%%%%%%aoeusnthoaeusntaohusoaeusnatohunsaoehusnaoehusnaotusnaoehuasoenuht^^^^^^^^^")
  done;
  let later = Utime.now () in
  prerr_endline (Utime.span_to_string (Utime.abs_diff later now)); *)
