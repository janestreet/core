open Core

let test_with_overhead number_of_knots number_of_lookups =
  let xmax = 10. in
  let kx = Array.init number_of_knots ~f:(fun _i -> Random.float xmax) in
  Array.sort kx ~compare:Float.compare;
  let ky = Array.init number_of_knots ~f:(fun _i -> Random.float 20.) in
  let knots = Array.map2_exn kx ky ~f:(fun x y -> (x,y)) |> Array.to_list in
  let t = Or_error.ok_exn (Piecewise_linear.Float.create knots) in
  let x = ref 0. in
  let dx = xmax /. Float.of_int number_of_lookups in
  let mark0 = Time.now () in
  for _ = 1 to number_of_lookups do
    x := !x +. dx;
    ignore (Piecewise_linear.Float.get t !x)
  done;
  let mark1 = Time.now () in
  x := 0.;
  for _ = 1 to number_of_lookups do
    x := !x +. dx
  done;
  let mark2 = Time.now () in
  (Time.diff mark1 mark0, Time.diff mark2 mark1)

let test number_of_knots number_of_lookups =
  let xmax = 10. in
  let kx = Array.init number_of_knots ~f:(fun _i -> Random.float xmax) in
  Array.sort kx ~compare:Float.compare;
  let ky = Array.init number_of_knots ~f:(fun _i -> Random.float 20.) in
  let knots = Array.map2_exn kx ky ~f:(fun x y -> (x,y)) |> Array.to_list in
  let t = Or_error.ok_exn (Piecewise_linear.Float.create knots) in
  let x = ref 0. in
  let dx = xmax /. Float.of_int number_of_lookups in
  (*  let x = ref kx.(0) in
      let dx = kx.(number_of_knots - 1) /. Float.of_int number_of_lookups in *)
  let mark0 = Time.now () in
  for _ = 1 to number_of_lookups do
    x := !x +. dx;
    ignore (Piecewise_linear.Float.get t !x)
  done;
  let mark1 = Time.now () in
  Time.diff mark1 mark0

let _do_trials_and_print knots lookups trials =
  printf "# knots = %i, lookups = %i\n\
          # Each row contains data for one trial.  The first value is the runtime with\n\
          # lookups, while the second value is the runtime of the same code but skipping\n\
          # the lookups.  Values are in seconds.\n" knots lookups;
  for _ = 1 to trials do
    let (runtime, estimated_overhead) = test_with_overhead knots lookups in
    printf "%.17g, %.17g\n" (Time.Span.to_sec runtime)
      (Time.Span.to_sec estimated_overhead)
  done

let do_trials knots lookups trials =
  printf "# knots = %i, lookups = %i, trials = %i\n\
          # Values in nanoseconds.\n" knots lookups trials;
  let tsum = ref 0. in
  let t2sum = ref 0. in
  let lookups_inverse = 1. /. Float.of_int lookups in
  for _ = 1 to trials do
    let runtime = Time.Span.to_ns (test knots lookups) *. lookups_inverse in
    tsum := !tsum +. runtime;
    t2sum := !t2sum +. runtime *. runtime
  done;
  let trials_ = Float.of_int trials in
  let mu = !tsum /. trials_ in
  let second_moment = !t2sum /. trials_ in
  let std = sqrt ((second_moment -. mu *. mu) *. trials_ /. (trials_ -. 1.)) in
  printf "mu = %g; std = %g\n" mu std

let () =
  if Array.length Sys.argv < 4
  then printf "Usage:\n\
               piecewise_linear_per_test.exe knots lookups trials\n\
               where [knots] is the number of knots, [lookups] is the number of lookups\n\
               per trial, and [trials] is the number of trials.\n"
  else
    let knots = Int.of_string Sys.argv.(1) in
    let lookups = Int.of_string Sys.argv.(2) in
    let trials = Int.of_string Sys.argv.(3) in
    do_trials knots lookups trials
