parent.exe and child.exe are separate executables, but the "child"
subcommand of the former is defined as a Command.exec of the latter.

  $ ENVVAR=test demo_parent.exe -help
  parent part of Command.exec demo
  
    demo_parent.exe SUBCOMMAND
  
  === subcommands ===
  
    child    this command is in another executable
    version  print version information
    help     explain a given subcommand (perhaps recursively)
  
  $ ENVVAR=test demo_child.exe -help
  child part of Command.exec demo
  
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

  $ ENVVAR=test demo_parent.exe child -help
  child part of Command.exec demo
  
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

  $ ENVVAR=test tab_complete demo_parent.exe child -
  -build-info
  -date
  -file
  -help
  -version

This works for multiple levels of nesting as well

  $ ENVVAR=test demo_grandparent.exe help -r -f
  grandparent part of Command.exec demo
  
    demo_grandparent.exe SUBCOMMAND
  
  === subcommands and flags ===
  
    parent             this command is in another executable
    . child            this command is in another executable
    . . -date DATE     a required date argument
    . . [-file FILE]   an optional file argument
    . . [-build-info]  print info about this build and exit
    . . [-version]     print the version of this build and exit
    . version          print version information
    . . [-build-info]  print build info for this build
    . . [-version]     print the version of this build
    version            print version information
    . [-build-info]    print build info for this build
    . [-version]       print the version of this build
    help               explain a given subcommand (perhaps recursively)
    . [-expand-dots]   expand subcommands in recursive help
    . [-flags]         show flags as well in recursive help
    . [-recursive]     show subcommands of subcommands, etc.
  
