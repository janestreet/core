This command has both flags and anons that must be passed at least once

  $ demo_one_or_more.exe -help
  Command.Spec.{one_or_more,non_empty_sequence} demo
  
    demo_one_or_more.exe Y [Y ...]
  
  === flags ===
  
    -foo X         required listed flag
    [-build-info]  print info about this build and exit
    [-version]     print the version of this build and exit
    [-help]        print this help text and exit
                   (alias: -?)
  
  $ demo_one_or_more.exe 1 2 3 -foo a -foo b -foo c
  ((xs (a b c)) (ys (1 2 3)))

  $ demo_one_or_more.exe 1 -foo a
  ((xs (a)) (ys (1)))

  $ demo_one_or_more.exe -foo a
  Command.Spec.{one_or_more,non_empty_sequence} demo
  
    demo_one_or_more.exe Y [Y ...]
  
  === flags ===
  
    -foo X         required listed flag
    [-build-info]  print info about this build and exit
    [-version]     print the version of this build and exit
    [-help]        print this help text and exit
                   (alias: -?)
  
  missing anonymous argument: Y
  [1]

  $ demo_one_or_more.exe 1
  Command.Spec.{one_or_more,non_empty_sequence} demo
  
    demo_one_or_more.exe Y [Y ...]
  
  === flags ===
  
    -foo X         required listed flag
    [-build-info]  print info about this build and exit
    [-version]     print the version of this build and exit
    [-help]        print this help text and exit
                   (alias: -?)
  
  missing required flag: -foo
  [1]
