open Core.Std

let command =
  Command.basic' ~summary:"Test full flag validation" ~readme:(Fn.const "Simple readme")
    Command.Param.(
      flag "-full-flag-required"
           ~full_flag_required:()
           ~aliases:["full-alias-required"]
           ~doc:" make sure if the full argument is not provided, command bails"
           no_arg
      @> flag "-full-flag-not-required"
           ~aliases:["full-alias-not-required"]
           ~doc:" make sure that prefix matches still work"
           no_arg
      @> nil
    )
    (fun un_abbreviatable abbreviatable  () ->
       if un_abbreviatable then print_endline "passed the un-abbreviatable flag";
       if abbreviatable    then print_endline "passed the abbreviatable flag";
       print_endline "doing stuff here!"
    )

let () = Command.run command

