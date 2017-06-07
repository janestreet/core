open Core
open Expect_test_helpers_kernel

let time_ns_of_string s =
  let suffix = "-04:00" in
  assert (String.is_suffix s ~suffix);
  match String.split ~on:'.' (String.slice s 0 (-6)) with
  | [_] -> Time_ns.of_string s
  | [base; ns] ->
    let base = Time_ns.of_string (base ^ suffix) in
    let span = Time_ns.Span.of_int63_ns (Int63.of_string ns) in
    Time_ns.add base span
  | _ -> assert false

(* Workaround for [Time_ns.Ofday.of_string] truncating. *)
let ofday_of_string s =
  match String.split ~on:'.' s with
  | [s] -> Time_ns.Ofday.of_string s
  | [ofday; ns] ->
    let base = Time_ns.Ofday.of_string ofday in
    let ns = Time_ns.Span.of_int63_ns (Int63.of_string ns) in
    Time_ns.Ofday.add_exn base ns
  | _ -> assert false

let%expect_test "Piecewise_linear.Time_ns" =
  let module PL = Piecewise_linear.Time_ns in
  let knot_x =
    [ "2017-05-25 10:30:00-04:00"
    ; "2017-05-25 10:30:00-04:00"
    ; "2017-05-25 12:00:00-04:00"
    ; "2017-05-25 18:00:00-04:00"
    ]
    |> List.map ~f:time_ns_of_string
  in
  let knots = List.mapi knot_x ~f:(fun i x -> (x, Float.of_int i)) in
  let t = PL.create knots |> Or_error.ok_exn in
  let test_x =
    [ "2017-05-25 10:29:59.999999-04:00"
    ; "2017-05-25 10:30:00-04:00"
    ; "2017-05-25 10:44:03.228657455-04:00"
    ]
    |> List.map ~f:time_ns_of_string
  in
  let test_y = List.map test_x ~f:(PL.get t) in
  List.iter test_y ~f:(printf !"%{Float}\n");
  (* The ideal value for the last x would be more like 1.15615345508425926. *)
  [%expect {|
    0.
    1.
    1.1561534550755557
  |}]

let%expect_test "Piecewise_linear.Time_ns extremely fine" =
  let module PL = Piecewise_linear.Time_ns in
  let knot_x =
    (* These two knots are too close together for us to interpolate between. *)
    [ "2017-05-25 10:44:03.228657443-04:00"
    ; "2017-05-25 10:44:03.228657481-04:00"
    (* These two knots are far enough apart that we can interpolate, but it's
       pretty coarse. *)
    ; "2017-05-25 10:44:03.228658000-04:00"
    ; "2017-05-25 10:44:03.228658576-04:00"
    ; "2017-05-25 10:44:03.228659000-04:00"
    ; "2017-05-25 10:44:03.228660000-04:00"
    ]
    |> List.map ~f:time_ns_of_string
  in
  let knots = List.mapi knot_x ~f:(fun i x -> (x, Float.of_int i)) in
  let t = PL.create knots |> Or_error.ok_exn in
  let test_x =
    [ "2017-05-25 10:44:03.228657455-04:00"
    ; "2017-05-25 10:44:03.228658238-04:00"
    ; "2017-05-25 10:44:03.228659050-04:00"
    ; "2017-05-25 10:44:03.228659100-04:00"
    ; "2017-05-25 10:44:03.228659500-04:00"
    ]
    |> List.map ~f:time_ns_of_string
  in
  let test_y = List.map test_x ~f:(PL.get t) in
  List.iter test_y ~f:(printf !"%{Float}\n");
  [%expect {|
      1.
      2.3333333333333335
      4.
      4.25
      4.5
  |}]

let%expect_test "Piecewise_linear.Span_ns" =
  let module PL = Piecewise_linear.Span_ns in
  let knot_x =
    [ 0L
    ; 1L
    ; 15L
    ; 1_000L
    ; 1234567890L
    ; 1234567900L
    ]
    |> List.map ~f:(fun x -> Time_ns.Span.of_int63_ns (Int63.of_int64_exn x))
  in
  let knots = List.mapi knot_x ~f:(fun i x -> (x, Float.of_int i)) in
  let t = PL.create knots |> Or_error.ok_exn in
  let test_x =
    [ 0L
    ; 1L
    ; 15L
    ; 1_000L
    ; 1234567890L
    ; 1234567900L
    ; 2L
    ; 100L
    ; 1234567891L
    ]
    |> List.map ~f:(fun x -> Time_ns.Span.of_int63_ns (Int63.of_int64_exn x))
  in
  let test_y = List.map test_x ~f:(PL.get t) in
  List.iter test_y ~f:(printf !"%{Float}\n");
  (* Note that if we convert to/from seconds rather than ns, we get 4.1000000066613378
     below rather than 4.1.  That's off by 7_500_000 ulps.  The issue is that
     while the two spans themselves are only off by a fraction of a ulp when we convert to
     seconds, their difference is off by many ulps.  In absolute terms, it's not so bad.
  *)
  [%expect {|
    0.
    1.
    2.
    3.
    4.
    5.
    1.0714285714285714
    2.0862944162436547
    4.1
  |}]

let%expect_test "Piecewise_linear.Ofday_ns" =
  let module PL = Piecewise_linear.Ofday_ns in
  let knot_x =
    [ "10:30:00"
    ; "10:30:00"
    ; "12:00:00"
    ; "18:00:00"
    ]
    |> List.map ~f:ofday_of_string
  in
  let knots = List.mapi knot_x ~f:(fun i x -> (x, Float.of_int i)) in
  let t = PL.create knots |> Or_error.ok_exn in
  let test_x =
    [ "10:29:59.999999"
    ; "10:30:00"
    ; "10:44:03.228657455"
    ]
    |> List.map ~f:ofday_of_string
  in
  let test_y = List.map test_x ~f:(PL.get t) in
  List.iter test_y ~f:(printf !"%{Float}\n");
  (* For the last x here, we get much closer to the idealized value than in the Time_ns
     example above. *)
  [%expect {|
    0.
    1.
    1.1561534550842596
  |}]
