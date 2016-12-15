open! Import

let initialize_module () =
  (* This sets the environment variable needed for
     [Ppx_inline_test_lib.Runtime.am_running_inline_test] to be [true] in child processes
     created by inline tests.  We do this here in [Core] rather than in
     [Ppx_inline_test_lib] because it uses [Unix], and INRIA's [Sys] only has [getenv],
     not [putenv].  We don't do it in, e.g. [Unix.fork], because there may be other ways
     to create child processes. *)
  let module R = Ppx_inline_test_lib.Runtime in
  let module Unix = Core_unix in
  if R.am_running_inline_test
  then Unix.putenv ~key:R.am_running_inline_test_env_var ~data:"";
;;
