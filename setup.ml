(* OASIS_START *)
#use "topfind";;
#require "oasis.dynrun";;
open OASISDynRun;;
open OASISTypes;;
(* OASIS_STOP *)

let () =
  InternalInstallPlugin.lib_hook :=
    fun (cs, bs, lib) ->
      match lib.OASISTypes.lib_findlib_name with
        | Some "core" ->
            (cs, bs, lib, [
                           "src/ocaml_utils_macros.h";
                           "src/unix_utils.h";
                           "src/core_config.h"])
        | _ ->
            (cs, bs, lib, [])
;;

let () = setup ()
