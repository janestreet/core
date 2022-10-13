include Caml

(* At Jane Street, the OCaml stdlib is patched to define [Pervasives.raise] as the
   ["%reraise"] primitive. We do this as the compiler is currently not good enough at
   automatically detecting reraise [1]. We patch the stdlib so that everything is
   affected, including libraries defined before base such as sexplib or non Jane Street
   libraries.

   We need this definition so that this implementation can match its interface with the
   patched stdlib and with the original one.

   [[1] http://caml.inria.fr/mantis/view.php?id=6556
*)
external raise : exn -> 'a = "%reraise"

[%%if ocaml_version < (4, 12, 0)]

let __FUNCTION__ = "<__FUNCTION__ not supported before OCaml 4.12>"

[%%endif]

[%%if ocaml_version >= (5, 0, 0)]

external ( & ) : bool -> bool -> bool = "%sequand"
external ( or ) : bool -> bool -> bool = "%sequor"

[%%endif]
