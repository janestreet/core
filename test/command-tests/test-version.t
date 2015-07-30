Every group command gets a version subcommand.

  $ demo_help.exe version -help
  print version information
  
    demo_help.exe version 
  
  === flags ===
  
    [-build-info]  print build info for this build
    [-version]     print the version of this build
    [-help]        print this help text and exit
                   (alias: -?)
  
Build-info says interesting things about the context in which the
executable was built: Who built it?  Where and when did they build it?
Using what version of linux and ocaml?

  $ demo_help.exe version -build-info
  ((username *) (hostname *) (kernel *) (build_date *) (build_time *) (x_library_inlining *) (ocaml_version *) (executable_path *) (build_system *) (nodynlink *) (packing *)) (glob)

The version flag shows what hg versions of what clones were built.

  $ demo_help.exe version -version
  (.*?) (re)

If you fail to specify -version or -build-info, you get both.

  $ demo_help.exe version
  ((username *) (hostname *) (kernel *) (build_date *) (build_time *) (x_library_inlining *) (ocaml_version *) (executable_path *) (build_system *) (nodynlink *) (packing *)) (glob)
  (.*?) (re)

The -version and -build-info flags also work for the top-level command.

  $ demo_help.exe -build-info
  ((username *) (hostname *) (kernel *) (build_date *) (build_time *) (x_library_inlining *) (ocaml_version *) (executable_path *) (build_system *) (nodynlink *) (packing *)) (glob)

  $ demo_help.exe -version
  (.*?) (re)
