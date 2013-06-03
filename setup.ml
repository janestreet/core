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
            (cs, bs, lib, ["lib/jane_common.h";
                           "lib/ocaml_utils.h";
                           "lib/ocaml_utils_macros.h";
                           "lib/unix_utils.h";
                           "lib/core_config.h"])
        | _ ->
            (cs, bs, lib, [])
;;

let () = setup ()
