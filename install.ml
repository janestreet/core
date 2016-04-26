#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"core"
  [ oasis_lib "core"
  ; oasis_lib "core_top"
  ; file "META" ~section:"lib"
  ; file "src/config.h" ~section:"lib"
  ; file "src/config.mlh" ~section:"lib"
  ; file "coretop" ~section:"bin"
  ; file "corebuild" ~section:"bin"
  ]
