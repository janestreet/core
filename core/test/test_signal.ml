open! Core
open! Import
open! Signal

let%test_unit _ =
  [%test_eq: string] (to_string bus) "sigbus";
  [%test_eq: string] (Sexp.to_string (sexp_of_t bus)) "sigbus";
  [%test_eq: string] (Sexp.to_string (Stable.V1.sexp_of_t bus)) {|"<unknown signal -22>"|};
  [%test_eq: string] (Sexp.to_string (Stable.V2.sexp_of_t bus)) "sigbus"
;;

let%expect_test "all_posix" =
  print_s [%sexp (all_posix : t list)];
  [%expect
    {|
    (sigabrt
     sigalrm
     sigbus
     sigchld
     sigcont
     sigfpe
     sighup
     sigill
     sigint
     sigkill
     sigpipe
     sigpoll
     sigprof
     sigquit
     sigsegv
     sigstop
     sigsys
     sigterm
     sigtrap
     sigtstp
     sigttin
     sigttou
     sigurg
     sigusr1
     sigusr2
     sigvtalrm
     sigxcpu
     sigxfsz
     sigzero)
    |}]
;;
