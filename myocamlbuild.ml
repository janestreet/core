(* OASIS_START *)
(* OASIS_STOP *)

let dispatch = function
  | After_rules ->
    pflag ["compile"; "ocaml"] "I" (fun x -> S [A "-I"; A x]);

    dep  ["ocaml"; "ocamldep"; "mlh"] ["lib/version_defaults.mlh";
                                       "lib/config.mlh"];

    flag ["mlh"; "ocaml"; "ocamldep"] (S[A"-ppopt"; A"-Ilib/"]);
    flag ["mlh"; "ocaml"; "compile"]  (S[A"-ppopt"; A"-Ilib/"]);
    flag ["mlh"; "ocaml"; "doc"]      (S[A"-ppopt"; A"-Ilib/"]);

    let env = BaseEnvLight.load () in
    let rt_possible = BaseEnvLight.var_get "rt_possible" env = "true" in
    if rt_possible then begin
      flag ["ocamlmklib"; "c"]                      (S[A"-lrt"]);
      flag ["use_libcore_stubs"; "link"] (S[A"-cclib"; A"-lrt"])
    end
  | _ ->
    ()

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook)
