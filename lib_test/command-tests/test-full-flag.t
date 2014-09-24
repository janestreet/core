Help still lists the aliases

  $ demo_full_flag.exe -help
  Test full flag validation
  
    demo_full_flag.exe 
  
  Simple readme
  
  === flags ===
  
    [-full-flag-not-required]  make sure that prefix matches still work
                               (alias: -full-alias-not-required)
    [-full-flag-required]      make sure if the full argument is not provided,
                               command bails
                               (alias: -full-alias-required)
    [-build-info]              print info about this build and exit
    [-version]                 print the version of this build and exit
    [-help]                    print this help text and exit
                               (alias: -?)
  
If you do not provide the full flag: error

  $ demo_full_flag.exe -full-flag-requi
  Test full flag validation
  
    demo_full_flag.exe 
  
  Simple readme
  
  === flags ===
  
    [-full-flag-not-required]  make sure that prefix matches still work
                               (alias: -full-alias-not-required)
    [-full-flag-required]      make sure if the full argument is not provided,
                               command bails
                               (alias: -full-alias-required)
    [-build-info]              print info about this build and exit
    [-version]                 print the version of this build and exit
    [-help]                    print this help text and exit
                               (alias: -?)
  
  unknown flag -full-flag-requi
  [1]

If you do not provide the full alias: error

  $ demo_full_flag.exe -full-alias-requi
  Test full flag validation
  
    demo_full_flag.exe 
  
  Simple readme
  
  === flags ===
  
    [-full-flag-not-required]  make sure that prefix matches still work
                               (alias: -full-alias-not-required)
    [-full-flag-required]      make sure if the full argument is not provided,
                               command bails
                               (alias: -full-alias-required)
    [-build-info]              print info about this build and exit
    [-version]                 print the version of this build and exit
    [-help]                    print this help text and exit
                               (alias: -?)
  
  unknown flag -full-alias-requi
  [1]

If you provide the full flag: success.

  $ demo_full_flag.exe -full-flag-required
  passed the un-abbreviatable flag
  doing stuff here!

Or the alias.

  $ demo_full_flag.exe -full-alias-required
  passed the un-abbreviatable flag
  doing stuff here!

But prefix matches still work

  $ demo_full_flag.exe -full-flag-no
  passed the abbreviatable flag
  doing stuff here!

Even on aliases
  $ demo_full_flag.exe -full-alias-no
  passed the abbreviatable flag
  doing stuff here!

