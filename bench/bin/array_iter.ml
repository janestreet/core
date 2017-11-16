open Core

module Bench = Core_bench.Bench

let () =
  let a = Array.init 1000 ~f:ident in
  let test_array_iter = Bench.Test.create ~name:"array_iter" (fun () ->
    Array.iter a ~f:(fun i ->
      if i > 2000 then
        assert false))
  in
  let test_array_for = Bench.Test.create ~name:"array_for" (fun () ->
    let length = (Array.length a) - 1 in
    for i = 0 to length do
      if i > 2000 then
        assert false
    done)
  in
  Bench.bench [test_array_iter; test_array_for]
