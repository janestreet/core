parent.exe and child.exe are separate executables, but the "child"
subcommand of the former is defined as a Command.exec of the latter.

  $ demo_parent.exe -help
  parent half of Command.exec demo
  
    demo_parent.exe SUBCOMMAND
  
  === subcommands ===
  
    child    this command is in another executable
    version  print version information
    help     explain a given subcommand (perhaps recursively)
  
  $ demo_child.exe -help
  child half of Command.exec demo
  
    demo_child.exe NUM [BOOL]
  
  readme text. readme text. readme text. readme text. readme text. readme
  text. readme text. readme text. readme text. readme text. readme text. readme
  text. readme text. readme text.
  
  === flags ===
  
    -date DATE     a required date argument
    [-file FILE]   an optional file argument
    [-build-info]  print info about this build and exit
    [-version]     print the version of this build and exit
    [-help]        print this help text and exit
                   (alias: -?)
  
Help works across Command.exec

  $ demo_parent.exe child -help
  child half of Command.exec demo
  
    demo_child.exe NUM [BOOL]
  
  readme text. readme text. readme text. readme text. readme text. readme
  text. readme text. readme text. readme text. readme text. readme text. readme
  text. readme text. readme text.
  
  === flags ===
  
    -date DATE     a required date argument
    [-file FILE]   an optional file argument
    [-build-info]  print info about this build and exit
    [-version]     print the version of this build and exit
    [-help]        print this help text and exit
                   (alias: -?)
  
So does tab completion

  $ tab_complete demo_parent.exe child -
  -build-info
  -date
  -file
  -help
  -version
