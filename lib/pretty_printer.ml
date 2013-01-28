let r = ref [ "Sexplib.Sexp.pp_hum" ]

let all () = !r

let register p = r := p :: !r
