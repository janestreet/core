open Core.Std  let _ = _squelch_unused_module_warning_

module Stack = Core.Std.Stack

module Bench = Core_extended.Deprecated_bench

let () =
  Bench.bench
    [
      Bench.Test.create ~name:"Stack.fold"
        begin
          let s = Stack.of_list (List.init 100 ~f:Fn.id) in
          fun () ->
            ignore (Stack.fold s ~init:0 ~f:(+))
        end;
      Bench.Test.create ~name:"stack_push_pop"
        begin
          let s = Stack.create () in
          Stack.push s ();
          fun () ->
            for _i = 1 to 10 do
              Stack.push s ();
              Stack.pop_exn s;
            done
        end
    ]
;;

