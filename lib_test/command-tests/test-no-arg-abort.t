Demonstrating the use of Command.Spec.no_arg_abort.

  $ demo_no_arg_abort.exe -help
  demonstrate the no_arg_abort flag type
  
    demo_no_arg_abort.exe [NUM]
  
  === flags ===
  
    [-foo NUM]     numeric flag
    [-grammar]     display GRAMMAR.txt and quit
    [-readme]      display README.txt and quit
    [-build-info]  print info about this build and exit
    [-version]     print the version of this build and exit
    [-help]        print this help text and exit
                   (alias: -?)
  
  $ demo_no_arg_abort.exe -foo 1 2
  entering main with args: ((foo (1)) (anon (2)))
  doing stuff ...

The -grammar and -readme flags are no_arg_abort flags, meaning that
they cause us to quit parsing the command line and exit.

  $ demo_no_arg_abort.exe -readme -foo non-int blah blah blah
  This is the contents of README.txt.

  $ demo_no_arg_abort.exe -grammar -foo non-int blah blah blah
  This is the contents of GRAMMAR.txt.

Whichever no_arg_abort flag comes first is the one that is run.

  $ demo_no_arg_abort.exe -readme -grammar
  This is the contents of README.txt.

Of course, the command line must be well-formed up to the no_arg_abort
flag.

  $ demo_no_arg_abort.exe -ok non-int -readme
  demonstrate the no_arg_abort flag type
  
    demo_no_arg_abort.exe [NUM]
  
  === flags ===
  
    [-foo NUM]     numeric flag
    [-grammar]     display GRAMMAR.txt and quit
    [-readme]      display README.txt and quit
    [-build-info]  print info about this build and exit
    [-version]     print the version of this build and exit
    [-help]        print this help text and exit
                   (alias: -?)
  
  unknown flag -ok
  [1]
