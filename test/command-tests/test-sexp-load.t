If we call Sexp.load_conv_exn inside command line parsing code, we get
a stack trace and a source file location pointing to where the problem
is.

  $ demo_sexp_load.exe $TEST_ETC/malformed.sexp
  demonstrate sexp loading during command line parsing
  
    demo_sexp_load.exe CONFIG-FILE
  
  === flags ===
  
    [-build-info]  print info about this build and exit
    [-version]     print the version of this build and exit
    [-help]        print this help text and exit
                   (alias: -?)
  
  Uncaught exception:
    
    (Sexplib.Conv.Of_sexp_error
     (Sexplib.Sexp.Annotated.Conv_exn
      */lib/core/test/command-tests/etc/malformed.sexp:1:0 (glob)
      (Failure
       "demo_sexp_load.ml.config_of_sexp: the following record elements were undefined: bar"))
     ((foo 3)))
  
  (Raised|Re-raised|Called) (at|from) file ".*", line \d+, characters .* (re)
  (Raised|Re-raised|Called) (at|from) file ".*", line \d+, characters .* (re)
  (Raised|Re-raised|Called) (at|from) file ".*", line \d+, characters .* (re)
  (Raised|Re-raised|Called) (at|from) file ".*", line \d+, characters .* (re)
  (Raised|Re-raised|Called) (at|from) file ".*", line \d+, characters .* (re)
  (Raised|Re-raised|Called) (at|from) file ".*", line \d+, characters .* (re)
  (Raised|Re-raised|Called) (at|from) file ".*", line \d+, characters .* (re)
  (Raised|Re-raised|Called) (at|from) file ".*", line \d+, characters .* (re)
  [1]
