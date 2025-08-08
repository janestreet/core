include Stdlib

(* At Jane Street, the OCaml stdlib is patched to define [Pervasives.raise] as the
   ["%reraise"] primitive. We do this as the compiler is currently not good enough at
   automatically detecting reraise [1]. We patch the stdlib so that everything is
   affected, including libraries defined before base such as sexplib or non Jane Street
   libraries.

   We need this definition so that this implementation can match its interface with the
   patched stdlib and with the original one.

   [[1] http://caml.inria.fr/mantis/view.php?id=6556
*)
external raise : 'a. exn -> 'a = "%reraise"
external ignore : 'a. ('a[@local_opt]) -> unit = "%ignore" [@@layout_poly]

[%%if ocaml_version < (4, 12, 0)]

let __FUNCTION__ = "<__FUNCTION__ not supported before OCaml 4.12>"

[%%endif]

external __LOC_OF__ : ('a[@local_opt]) -> (string * 'a[@local_opt]) = "%loc_LOC"
external __LINE_OF__ : ('a[@local_opt]) -> (int * 'a[@local_opt]) = "%loc_LINE"

external __POS_OF__
  :  ('a[@local_opt])
  -> ((string * int * int * int) * 'a[@local_opt])
  = "%loc_POS"

external ( |> ) : 'a 'b. 'a -> (('a -> 'b)[@local_opt]) -> 'b = "%revapply"
[@@layout_poly]

external ( @@ ) : 'a 'b. (('a -> 'b)[@local_opt]) -> 'a -> 'b = "%apply" [@@layout_poly]
external int_of_char : (char[@local_opt]) -> int = "%identity"

external format_of_string
  :  (('a, 'b, 'c, 'd, 'e, 'f) format6[@local_opt])
  -> (('a, 'b, 'c, 'd, 'e, 'f) format6[@local_opt])
  = "%identity"

external ( ** )
  :  (float[@local_opt])
  -> (float[@local_opt])
  -> float
  = "caml_power_float" "pow"
[@@unboxed] [@@noalloc]

external sqrt : (float[@local_opt]) -> float = "caml_sqrt_float" "sqrt"
[@@unboxed] [@@noalloc]

external exp : (float[@local_opt]) -> float = "caml_exp_float" "exp"
[@@unboxed] [@@noalloc]

external log : (float[@local_opt]) -> float = "caml_log_float" "log"
[@@unboxed] [@@noalloc]
