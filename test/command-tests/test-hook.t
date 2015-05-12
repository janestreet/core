Every Command-built exe supports a help subcommand which shows all
other subcommands along with their descriptions

  $ demo_help.exe adverb
  this command does more stuff
  
    demo_help.exe adverb SUBCOMMAND
  
  Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque et
  ante a nibh scelerisque ultrices. Fusce consectetur dictum ante quis
  commodo.
  
  === subcommands ===
  
    drolly     this command does stuff here
    opposable  this command does stuff here
    help       explain a given subcommand (perhaps recursively)
  
  missing subcommand for command demo_help.exe adverb
  [1]

  $ demo_help.exe with-body
  BEGIN body code
    path = (demo_help.exe with-body)
    (running this instead of reporting a missing subcommand)
  END body code
