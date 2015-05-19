Here we demonstrate tab completion

  $ demo_help.exe help -recursive
  this command does stuff
  
    demo_help.exe SUBCOMMAND
  
  === subcommands ===
  
    jab          this command does stuff here
    jib          this command does stuff here
    adverb       this command does more stuff
    . drolly     this command does stuff here
    . opposable  this command does stuff here
    with-body    this command does more stuff
    . drolly     this command does stuff here
    . opposable  this command does stuff here
    ordered      group with unsorted subcommands
    . zzz        this command does stuff here
    . aaa        this command does stuff here
    version      print version information
    help         explain a given subcommand (perhaps recursively)
  
Tab completion works for subcommands ...

  $ tab_complete demo_help.exe ''
  adverb
  help
  jab
  jib
  ordered
  version
  with-body

  $ tab_complete demo_help.exe 'j'
  jab
  jib

  $ tab_complete demo_help.exe 'jab'
  jab

... at all positions in the command hierarchy.

  $ tab_complete demo_help.exe adverb ''
  drolly
  help
  opposable

  $ tab_complete demo_help.exe adverb 'dr'
  drolly

Tab completion works for flag names.

  $ tab_complete demo_help.exe jab '-'
  -help
  -ping
  -pong

  $ tab_complete demo_help.exe jab '-p'
  -ping
  -pong

  $ tab_complete demo_help.exe jab '-pi'
  -ping

  $ tab_complete demo_help.exe jab '-ping'
  -ping

Tab completion works for [Arg_type.t] values which have defined it:
both for flag arguments ...

  $ tab_complete demo_help.exe jab -pong ''
  baa\ baa
  bar
  baz
  foo

  $ tab_complete demo_help.exe jab -pong 'ba'
  baa\ baa
  bar
  baz

  $ tab_complete demo_help.exe jab -pong 'bar'
  bar

... and anonymous arguments (assumed not to be flags).

  $ tab_complete demo_help.exe jab ''
  baa\ baa
  bar
  baz
  foo

  $ tab_complete demo_help.exe jab 'ba'
  baa\ baa
  bar
  baz

  $ tab_complete demo_help.exe jab 'bar'
  bar

It continues to work even if one of the previous values failed to parse.

  $ tab_complete demo_help.exe jab FeO 'ba'
  baa\ baa
  bar
  baz

  $ demo_help.exe jab FeO bar
  this command does stuff here
  
    demo_help.exe jab OXIDE DIODE [PATH]
  
  Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque et
  ante a nibh scelerisque ultrices. Fusce consectetur dictum ante quis
  commodo.
  
  === flags ===
  
    [-ping]       make sure ping is doing the same stuff
    [-pong NAME]  which pong should do the stuff
    [-help]       print this help text and exit
                  (alias: -?)
  
  failed to parse OXIDE value "FeO"
  (Failure "valid arguments: {baa\\ baa,bar,baz,foo}")
  [1]
