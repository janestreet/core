Each leaf command in demo_internals.exe knows

 - its position in the command hierarchy via Command.Spec.path,
 - its list of unprocessed arguments via Command.Spec.args, and
 - its own help message via Command.Spec.help.

The leaf commands themselves just print out this information.

  $ demo_internals.exe foo 1 -foo 2
  path = (demo_internals.exe foo)
  args = (1 -foo 2)
  help = 
  summary
  
    demo_internals.exe foo [BAR]
  
  === flags ===
  
    [-foo NUM]  flag value
    [-help]     print this help text and exit
                (alias: -?)

Here is one deeper in the hierarchy:

  $ demo_internals.exe bar baz 1 -foo 2
  path = (demo_internals.exe bar baz)
  args = (1 -foo 2)
  help = 
  summary
  
    demo_internals.exe bar baz [BAR]
  
  === flags ===
  
    [-foo NUM]  flag value
    [-help]     print this help text and exit
                (alias: -?)


