open Core_kernel.Std

module Step_test = struct
  let offset off = Time.add Time.epoch (Time.Span.of_sec off)

  type step =
    | Take of float * bool
    | Fill of float
  with sexp

  type timed_step = (float * step) with sexp

  let take time amount expect = (time,Take (amount,expect))
  let fill time amount = (time,Fill amount)

  let run t l =
    try
      List.iter l ~f:(fun (now_offset, step) ->
        Limiter.invariant t;
        let now = offset now_offset in
        begin match step with
        | Fill amount -> Limiter.Expert.return_to_hopper t ~now (Float.abs amount)
        | Take (amount,expect) ->
          match Limiter.Expert.try_take t ~now amount with
          | `Asked_for_more_than_bucket_size ->
            failwithf !"test asked to take more (%g) than bucket size (%{Sexp}) at time %f"
              amount (Limiter.sexp_of_t t) now_offset ()
          | `Taken  ->
            if not expect
            then failwithf !"incorrectly able to take %g from the bucket (%{Sexp}) at time %f"
                   amount (Limiter.sexp_of_t t) now_offset ();
          | `Unable ->
            if expect
            then failwithf !"unable to take %g from the bucket (%{Sexp}) at time %f"
                   amount (Limiter.sexp_of_t t) now_offset ()
        end;
        Limiter.invariant t)
    with e ->
      Error.raise
        (Error.tag_arg (Error.of_exn e)
           "Limiter step test failed" (t,l)
           <:sexp_of< Limiter.t * timed_step list>>)
  ;;

  TEST_UNIT "return_to_hopper invariants" =
    let t =
      Limiter.Expert.create_exn
        ~now:Time.epoch
        ~hopper_to_bucket_rate_per_sec:Infinite
        ~bucket_size:10.
        ~initial_bucket_level:10.
        ~initial_hopper_level:(Finite 0.)
    in
    <:test_result<bool>>
      ~expect:true
      (Exn.does_raise (fun () -> Limiter.Expert.return_to_hopper t ~now:Time.epoch 1.))

  TEST_UNIT "return_to_hopper after lowering target" =
    let t =
      Limiter.Expert.create_exn
        ~now:Time.epoch
        ~hopper_to_bucket_rate_per_sec:Infinite
        ~bucket_size:10.
        ~initial_bucket_level:10.
        ~initial_hopper_level:(Finite 0.)
    in
    let now = Time.epoch in
    begin match  Limiter.Expert.try_take t ~now 10. with
    | `Asked_for_more_than_bucket_size
    | `Unable -> assert false
    | `Taken -> ()
    end;
    Limiter.Expert.set_token_target_level_exn t ~now (Finite 5.);
    Limiter.Expert.return_to_hopper t ~now 6.;
    <:test_result<bool>>
      ~expect:true
      (Limiter.in_system t ~now = Finite 5.)

  TEST_UNIT "return_to_hopper after raising target" =
    let t =
      Limiter.Expert.create_exn
        ~now:Time.epoch
        ~hopper_to_bucket_rate_per_sec:Infinite
        ~bucket_size:10.
        ~initial_bucket_level:10.
        ~initial_hopper_level:(Finite 0.)
    in
    let now = Time.epoch in
    begin match  Limiter.Expert.try_take t ~now 10. with
    | `Asked_for_more_than_bucket_size
    | `Unable -> assert false
    | `Taken -> ()
    end;
    Limiter.Expert.set_token_target_level_exn t ~now (Finite 15.);
    Limiter.Expert.return_to_hopper t ~now 6.;
    <:test_result<bool>>
      ~expect:true
      (Limiter.in_system t ~now = Finite 15.)

  TEST_UNIT "Generic" =
    run (Limiter.Expert.create_exn
          ~now:Time.epoch
          ~hopper_to_bucket_rate_per_sec:(Finite (60. /. 60.))
          ~bucket_size:60.
          ~initial_bucket_level:0.
          ~initial_hopper_level:Infinite)
      [ take 0.0  1. false
      ; take 1.0  1. true
      ; take 1.0  1. false
      ; take 1.5  1. false
      ; take 60. 60. false
      ; take 60. 59. true ]

  TEST_UNIT "Generic" =
    run (Limiter.Expert.create_exn
           ~now:Time.epoch
           ~hopper_to_bucket_rate_per_sec:(Finite (60. /. 60.))
           ~bucket_size:120.
           ~initial_bucket_level:0.
           ~initial_hopper_level:Infinite)
      [ take 0.0  1.   false
      ; take 1.0  1.   true
      ; take 1.0  1.   false
      ; take 1.5  1.   false
      ; take 60.  60.  false
      ; take 360. 120. true
      ]

  TEST_UNIT "Generic" =
    run (Limiter.Expert.create_exn
           ~now:Time.epoch
           ~hopper_to_bucket_rate_per_sec:(Finite (60. /. 60.))
           ~bucket_size:60.
           ~initial_bucket_level:0.
           ~initial_hopper_level:(Finite 10.))
      [ take  1.  1. true
      ; fill  1.  1.
      ; take 10.  9. true
      ; fill 10.  9.
      ; take 11.  1. true
      ; fill 11.  1.
      ; take 15.  5. false
      ; take 15.  4. true
      ; fill 15.  4.
      ; take 30. 11. false
      ; take 30. 10. true
      ]
  ;;

  TEST_UNIT "Throttled_rate_limiter" =
    let limiter =
      Limiter.Throttled_rate_limiter.create_exn
        ~now:Time.epoch
        ~burst_size:3
        ~sustained_rate_per_sec:(2. /. 1.)
        ~max_concurrent_jobs:5
    in
    run
      (limiter :> Limiter.t)
      [ take  0.0  1. true
      ; take  0.1  1. true
      ; take  0.2  1. true   (* we can open these jobs because of the burst size *)
      ; take  0.3  1. false  (* and now that's done *)
      ; take  0.5 1. true   (* but after 1/2 second, we have another *)
      ; take  1.0 1. true   (* and now one more.  We need to wait a bit longer than
                                would be perfect to accomodate token drip granularity. *)
      ; take  2.0  2. false  (* but now there are too many concurrent jobs *)
      ; fill  2.0  3.        (* give some back *)
      ; take  2.0  1. false  (* and it takes time for them to get in the bucket *)
      ; take  3.0  2. true   (* and now we can do a burst of 2 *)
      ; take 10.0  1. true   (* and one more *)
      ; take 10.0  1. false  (* but now we're out of concurrent jobs *)
      ]
  ;;

  TEST_UNIT "Throttle" =
    let throttle =
      Limiter.Throttle.create_exn
        ~now:Time.epoch
        ~max_concurrent_jobs:3
    in
    run (throttle :> Limiter.t)
      [ take 0. 1. true
      ; take 0. 1. true
      ; take 0. 1. true
      ; take 0. 1. false
      ; fill 1. 1.
      ; take 1. 1. true ]

  TEST_UNIT "Rounding is reasonble for 1_000_000 elements" =
    let throttle =
      Limiter.Throttle.create_exn
        ~now:Time.epoch
        ~max_concurrent_jobs:1
    in
    let max_jobs_per_sec = 1_000_000 in
    let denominator = Float.of_int max_jobs_per_sec in
    for i = 0 to 2 * max_jobs_per_sec do
      run (throttle :> Limiter.t)
        [take 0. (1. /. denominator) (i < max_jobs_per_sec)]
    done

end
