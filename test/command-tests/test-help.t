Every Command-built exe supports a help subcommand which shows all
other subcommands along with their descriptions

  $ demo_help.exe help
  this command does stuff
  
    demo_help.exe SUBCOMMAND
  
  === subcommands ===
  
    jab        this command does stuff here
    jib        this command does stuff here
    adverb     this command does more stuff
    with-body  this command does more stuff
    ordered    group with unsorted subcommands
    version    print version information
    help       explain a given subcommand (perhaps recursively)
  
It can be used to show the entire command hierarchy

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
  
And the flags for leaf commands.

  $ demo_help.exe help -recursive -flags
  this command does stuff
  
    demo_help.exe SUBCOMMAND
  
  === subcommands and flags ===
  
    jab               this command does stuff here
    . [-ping]         make sure ping is doing the same stuff
    . [-pong NAME]    which pong should do the stuff
    jib               this command does stuff here
    . [-ping]         make sure ping is doing the same stuff
    . [-pong NAME]    which pong should do the stuff
    adverb            this command does more stuff
    . drolly          this command does stuff here
    . . [-ping]       make sure ping is doing the same stuff
    . . [-pong NAME]  which pong should do the stuff
    . opposable       this command does stuff here
    . . [-ping]       make sure ping is doing the same stuff
    . . [-pong NAME]  which pong should do the stuff
    with-body         this command does more stuff
    . drolly          this command does stuff here
    . . [-ping]       make sure ping is doing the same stuff
    . . [-pong NAME]  which pong should do the stuff
    . opposable       this command does stuff here
    . . [-ping]       make sure ping is doing the same stuff
    . . [-pong NAME]  which pong should do the stuff
    ordered           group with unsorted subcommands
    . zzz             this command does stuff here
    . . [-ping]       make sure ping is doing the same stuff
    . . [-pong NAME]  which pong should do the stuff
    . aaa             this command does stuff here
    . . [-ping]       make sure ping is doing the same stuff
    . . [-pong NAME]  which pong should do the stuff
    version           print version information
    . [-build-info]   print build info for this build
    . [-version]      print the version of this build
    help              explain a given subcommand (perhaps recursively)
    . [-expand-dots]  expand subcommands in recursive help
    . [-flags]        show flags as well in recursive help
    . [-recursive]    show subcommands of subcommands, etc.
  
Oh look, there is another flag for the help subcommand.  It fills in
all the contextual dots in the output, which is helpful when viewing
diffs of help output.

  $ demo_help.exe help -recursive -flags -expand-dots
  this command does stuff
  
    demo_help.exe SUBCOMMAND
  
  === subcommands and flags ===
  
    jab                               this command does stuff here
    jab [-ping]                       make sure ping is doing the same stuff
    jab [-pong NAME]                  which pong should do the stuff
    jib                               this command does stuff here
    jib [-ping]                       make sure ping is doing the same stuff
    jib [-pong NAME]                  which pong should do the stuff
    adverb                            this command does more stuff
    adverb drolly                     this command does stuff here
    adverb drolly [-ping]             make sure ping is doing the same stuff
    adverb drolly [-pong NAME]        which pong should do the stuff
    adverb opposable                  this command does stuff here
    adverb opposable [-ping]          make sure ping is doing the same stuff
    adverb opposable [-pong NAME]     which pong should do the stuff
    with-body                         this command does more stuff
    with-body drolly                  this command does stuff here
    with-body drolly [-ping]          make sure ping is doing the same stuff
    with-body drolly [-pong NAME]     which pong should do the stuff
    with-body opposable               this command does stuff here
    with-body opposable [-ping]       make sure ping is doing the same stuff
    with-body opposable [-pong NAME]  which pong should do the stuff
    ordered                           group with unsorted subcommands
    ordered zzz                       this command does stuff here
    ordered zzz [-ping]               make sure ping is doing the same stuff
    ordered zzz [-pong NAME]          which pong should do the stuff
    ordered aaa                       this command does stuff here
    ordered aaa [-ping]               make sure ping is doing the same stuff
    ordered aaa [-pong NAME]          which pong should do the stuff
    version                           print version information
    version [-build-info]             print build info for this build
    version [-version]                print the version of this build
    help                              explain a given subcommand (perhaps
                                      recursively)
    help [-expand-dots]               expand subcommands in recursive help
    help [-flags]                     show flags as well in recursive help
    help [-recursive]                 show subcommands of subcommands, etc.
  
Every subcommand which itself has subcommands has a help subcommand
among them.  There is a -help flag that works

  $ demo_help.exe adverb help
  this command does more stuff
  
    demo_help.exe adverb SUBCOMMAND
  
  Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque et
  ante a nibh scelerisque ultrices. Fusce consectetur dictum ante quis
  commodo.
  
  === subcommands ===
  
    drolly     this command does stuff here
    opposable  this command does stuff here
    help       explain a given subcommand (perhaps recursively)
  
Every command in the hierarchy also takes a -help flag.

For group commands, the -help flag behaves just like the help
subcommand

  $ demo_help.exe -help
  this command does stuff
  
    demo_help.exe SUBCOMMAND
  
  === subcommands ===
  
    jab        this command does stuff here
    jib        this command does stuff here
    adverb     this command does more stuff
    with-body  this command does more stuff
    ordered    group with unsorted subcommands
    version    print version information
    help       explain a given subcommand (perhaps recursively)
  
  $ demo_help.exe adverb -help
  this command does more stuff
  
    demo_help.exe adverb SUBCOMMAND
  
  Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque et
  ante a nibh scelerisque ultrices. Fusce consectetur dictum ante quis
  commodo.
  
  === subcommands ===
  
    drolly     this command does stuff here
    opposable  this command does stuff here
    help       explain a given subcommand (perhaps recursively)
  
For leaf commands, it shows all available flags, along with their documentation.

  $ demo_help.exe adverb drolly -help
  this command does stuff here
  
    demo_help.exe adverb drolly OXIDE DIODE [PATH]
  
  Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque et
  ante a nibh scelerisque ultrices. Fusce consectetur dictum ante quis
  commodo.
  
  === flags ===
  
    [-ping]       make sure ping is doing the same stuff
    [-pong NAME]  which pong should do the stuff
    [-help]       print this help text and exit
                  (alias: -?)
  
We can ask for help on the help subcommand itself 

  $ demo_help.exe help -help
  explain a given subcommand (perhaps recursively)
  
    demo_help.exe help [SUBCOMMAND]
  
  === flags ===
  
    [-expand-dots]  expand subcommands in recursive help
    [-flags]        show flags as well in recursive help
    [-recursive]    show subcommands of subcommands, etc.
    [-help]         print this help text and exit
                    (alias: -?)
  
We see that help subcommands can also be passed an argument: one of
their sibling subcommands.

  $ demo_help.exe help jab
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
  
This provides another way to ask for help on help.

  $ demo_help.exe help help
  explain a given subcommand (perhaps recursively)
  
    demo_help.exe help [SUBCOMMAND]
  
  === flags ===
  
    [-expand-dots]  expand subcommands in recursive help
    [-flags]        show flags as well in recursive help
    [-recursive]    show subcommands of subcommands, etc.
    [-help]         print this help text and exit
                    (alias: -?)
  
