open Sexplib

type 'a z = 
| Forced
| Not_forced of (unit -> 'a)

type 'a t = 'a z ref

let create f = ref (Not_forced f)

let ignore () = create (fun () -> ())

let force t =
  match !t with
  | Forced -> failwith "Force_once.force"
  | Not_forced f -> t := Forced; f()
      
let sexp_of_t _ t =
  match !t with
  | Forced -> Sexp.Atom "<Forced>"
  | Not_forced _ -> Sexp.Atom "<Not_forced>"
