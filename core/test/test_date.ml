open! Core
open! Import
open! Date
open! Date.Private

let%expect_test _ =
  print_and_check_container_sexps
    (module Date)
    [ Date.of_string "1955-11-12"
    ; Date.of_string "1985-10-26"
    ; Date.of_string "2015-10-21"
    ];
  [%expect
    {|
    (Set (1955-11-12 1985-10-26 2015-10-21))
    (Map (
      (1955-11-12 0)
      (1985-10-26 1)
      (2015-10-21 2)))
    (Hash_set (1955-11-12 1985-10-26 2015-10-21))
    (Table (
      (1955-11-12 0)
      (1985-10-26 1)
      (2015-10-21 2)))
    |}]
;;

let max_value = Date.create_exn ~y:9999 ~m:Dec ~d:31

let%expect_test "ensure this is the real [max_value]" =
  require_does_raise (fun () -> Date.add_days max_value 1);
  [%expect
    {|
    (Invalid_argument
     "Date.create_exn ~y:10000 ~m:Jan ~d:1 error: year outside of [0..9999]")
    |}]
;;

let min_value = Date.create_exn ~y:0 ~m:Jan ~d:1

let%expect_test "ensure this is the real [min_value]" =
  require_does_raise (fun () -> Date.add_days min_value (-1));
  [%expect
    {|
    (Invalid_argument
     "Date.create_exn ~y:-1 ~m:Dec ~d:31 error: year outside of [0..9999]")
    |}]
;;

let%expect_test "limits are representable in an [int] in 32 bit ocaml" =
  let min_value = Date.Stable.V1.to_int min_value in
  let max_value = Date.Stable.V1.to_int max_value in
  print_s [%message (min_value : int) (max_value : int)];
  [%expect
    {|
    ((min_value 257)
     (max_value 655_297_567))
    |}];
  let open Int.O in
  (* There would be no issue if min_value was much smaller, but this is true at the time
     of writing. Just make sure it's representable in 32-bit OCaml. *)
  assert (0 < min_value);
  assert (0 < max_value);
  (* These asserts are trivially true in 32-bit (because no value larger than that can
     exist), but they're relevant when running in 64-bit, and we want to make sure we
     don't overflow. *)
  assert (min_value < Int.max_value_30_bits);
  assert (max_value < Int.max_value_30_bits)
;;

let%expect_test "Date.V1" =
  let examples =
    [ Date.create_exn ~y:1066 ~m:Oct ~d:16
    ; Date.create_exn ~y:1955 ~m:Nov ~d:5
    ; Date.create_exn ~y:2012 ~m:Apr ~d:19
    ; min_value
    ; max_value
    ]
  in
  print_and_check_stable_type (module Date.Stable.V1) examples;
  [%expect
    {|
    (bin_shape_digest 47681bb034560d96024e1b2eca0d98ca)
    ((sexp   1066-10-16)
     (bin_io "\254*\004\t\016"))
    ((sexp   1955-11-05)
     (bin_io "\254\163\007\n\005"))
    ((sexp   2012-04-19)
     (bin_io "\254\220\007\003\019"))
    ((sexp   0000-01-01)
     (bin_io "\000\000\001"))
    ((sexp   9999-12-31)
     (bin_io "\254\015'\011\031"))
    |}];
  List.iter examples ~f:(fun date ->
    let int = Date.Stable.V1.to_int date in
    print_s [%sexp (date : Date.Stable.V1.t), (int : int)];
    let round_trip = Date.Stable.V1.of_int_exn int in
    require_compare_equal (module Date.Stable.V1) date round_trip);
  [%expect
    {|
    (1066-10-16 69_863_952)
    (1955-11-05 128_125_701)
    (2012-04-19 131_859_475)
    (0000-01-01 257)
    (9999-12-31 655_297_567)
    |}];
  require_does_raise (fun () -> Date.Stable.V1.of_int_exn 0);
  [%expect {| (Failure "Month.of_int_exn 0") |}]
;;

let%expect_test "Date.V1.Set" =
  print_and_check_stable_type
    (module Date.Stable.V1.Set)
    [ Date.Set.empty
    ; Date.Set.singleton (Date.create_exn ~y:1066 ~m:Oct ~d:16)
    ; Date.Set.of_list
        [ Date.create_exn ~y:1955 ~m:Nov ~d:5; Date.create_exn ~y:2012 ~m:Apr ~d:19 ]
    ; Date.Set.of_list
        [ Date.create_exn ~y:1066 ~m:Oct ~d:16
        ; Date.create_exn ~y:1955 ~m:Nov ~d:5
        ; Date.create_exn ~y:2012 ~m:Apr ~d:19
        ]
    ];
  [%expect
    {|
    (bin_shape_digest ccde15fc17afce11a067d80e40cb1e8d)
    ((sexp ()) (bin_io "\000"))
    ((sexp (1066-10-16)) (bin_io "\001\254*\004\t\016"))
    ((sexp (1955-11-05 2012-04-19))
     (bin_io "\002\254\163\007\n\005\254\220\007\003\019"))
    ((sexp (1066-10-16 1955-11-05 2012-04-19))
     (bin_io "\003\254*\004\t\016\254\163\007\n\005\254\220\007\003\019"))
    |}]
;;

let%expect_test "Date.V1.Map" =
  let module T = struct
    type t = string Date.Stable.V1.Map.t [@@deriving bin_io, compare, sexp]
  end
  in
  print_and_check_stable_type
    (module T)
    [ Date.Map.empty
    ; Date.Map.singleton
        (Date.create_exn ~y:1066 ~m:Oct ~d:16)
        "not the Battle of Hastings"
    ; Date.Map.of_alist_exn
        [ Date.create_exn ~y:1955 ~m:Nov ~d:5, "flux capacitor"
        ; Date.create_exn ~y:2012 ~m:Apr ~d:19, "a Thursday"
        ]
    ; Date.Map.of_alist_exn
        [ Date.create_exn ~y:1066 ~m:Oct ~d:16, "not the Battle of Hastings"
        ; Date.create_exn ~y:1955 ~m:Nov ~d:5, "flux capacitor"
        ; Date.create_exn ~y:2012 ~m:Apr ~d:19, "a Thursday"
        ]
    ];
  [%expect
    {|
    (bin_shape_digest a0aa3c6d1173d784fbd03980ac5d0be5)
    ((sexp ()) (bin_io "\000"))
    ((sexp ((1066-10-16 "not the Battle of Hastings")))
     (bin_io "\001\254*\004\t\016\026not the Battle of Hastings"))
    ((sexp (
       (1955-11-05 "flux capacitor")
       (2012-04-19 "a Thursday")))
     (bin_io
      "\002\254\163\007\n\005\014flux capacitor\254\220\007\003\019\na Thursday"))
    ((sexp (
       (1066-10-16 "not the Battle of Hastings")
       (1955-11-05 "flux capacitor")
       (2012-04-19 "a Thursday")))
     (bin_io
      "\003\254*\004\t\016\026not the Battle of Hastings\254\163\007\n\005\014flux capacitor\254\220\007\003\019\na Thursday"))
    |}]
;;

let%expect_test "Date.Option.V1" =
  let date_examples =
    [ Date.create_exn ~y:1066 ~m:Oct ~d:16
    ; Date.create_exn ~y:1955 ~m:Nov ~d:5
    ; Date.create_exn ~y:2012 ~m:Apr ~d:19
    ]
  in
  let date_opt_examples =
    Date.Option.none :: List.map date_examples ~f:Date.Option.some
  in
  print_and_check_stable_type (module Date.Stable.Option.V1) date_opt_examples;
  [%expect
    {|
    (bin_shape_digest aff59493f3c14f005635a016cd36c44b)
    ((sexp ()) (bin_io "\000"))
    ((sexp (1066-10-16)) (bin_io "\253\016\n*\004"))
    ((sexp (1955-11-05)) (bin_io "\253\005\011\163\007"))
    ((sexp (2012-04-19)) (bin_io "\253\019\004\220\007"))
    |}];
  List.iter date_opt_examples ~f:(fun date ->
    let int = Date.Stable.Option.V1.to_int date in
    print_s [%sexp (date : Date.Stable.Option.V1.t), (int : int)];
    let round_trip = Date.Stable.Option.V1.of_int_exn int in
    require_compare_equal (module Date.Stable.Option.V1) date round_trip);
  [%expect
    {|
    (() 0)
    ((1066-10-16) 69_863_952)
    ((1955-11-05) 128_125_701)
    ((2012-04-19) 131_859_475)
    |}]
;;

let%expect_test "create_exn doesn't allocate" =
  let y, m, d = Sys.opaque_identity (1999, Month.Dec, 31) in
  require_no_allocation (fun () ->
    ignore (Sys.opaque_identity (Date.create_exn ~y ~m ~d) : Date.t));
  [%expect {| |}]
;;

let%test_unit "creation and destruction" =
  let test y m d =
    let t = Date.create_exn ~y ~m ~d in
    [%test_result: int] ~expect:y (Date.year t);
    [%test_result: Month.t] ~expect:m (Date.month t);
    [%test_result: int] ~expect:d (Date.day t)
  in
  test 2014 Month.Sep 24;
  test 9999 Month.Dec 31
;;

let%expect_test "add_years" =
  let test string =
    let date = Date.of_string string in
    for years = -4 to 4 do
      let date_plus_years = Date.add_years date years in
      printf !"%{Date} + %2d years = %{Date}\n" date years date_plus_years
    done
  in
  (* non-leap day *)
  test "2013-10-07";
  [%expect
    {|
    2013-10-07 + -4 years = 2009-10-07
    2013-10-07 + -3 years = 2010-10-07
    2013-10-07 + -2 years = 2011-10-07
    2013-10-07 + -1 years = 2012-10-07
    2013-10-07 +  0 years = 2013-10-07
    2013-10-07 +  1 years = 2014-10-07
    2013-10-07 +  2 years = 2015-10-07
    2013-10-07 +  3 years = 2016-10-07
    2013-10-07 +  4 years = 2017-10-07
    |}];
  (* leap day maps to Feb 28 on non-leap years (and 400-year century behaves properly) *)
  test "2004-02-29";
  [%expect
    {|
    2004-02-29 + -4 years = 2000-02-29
    2004-02-29 + -3 years = 2001-02-28
    2004-02-29 + -2 years = 2002-02-28
    2004-02-29 + -1 years = 2003-02-28
    2004-02-29 +  0 years = 2004-02-29
    2004-02-29 +  1 years = 2005-02-28
    2004-02-29 +  2 years = 2006-02-28
    2004-02-29 +  3 years = 2007-02-28
    2004-02-29 +  4 years = 2008-02-29
    |}];
  (* non-leap year century behaves properly *)
  test "1904-02-29";
  [%expect
    {|
    1904-02-29 + -4 years = 1900-02-28
    1904-02-29 + -3 years = 1901-02-28
    1904-02-29 + -2 years = 1902-02-28
    1904-02-29 + -1 years = 1903-02-28
    1904-02-29 +  0 years = 1904-02-29
    1904-02-29 +  1 years = 1905-02-28
    1904-02-29 +  2 years = 1906-02-28
    1904-02-29 +  3 years = 1907-02-28
    1904-02-29 +  4 years = 1908-02-29
    |}]
;;

module%test [@name "week_number and week_number_and_year"] _ = struct
  let%test_unit _ =
    [%test_result: int] (ordinal_date (create_exn ~y:2014 ~m:Jan ~d:1)) ~expect:1
  ;;

  let%test_unit _ =
    [%test_result: int] (ordinal_date (create_exn ~y:2014 ~m:Dec ~d:31)) ~expect:365
  ;;

  let%test_unit _ =
    [%test_result: int] (ordinal_date (create_exn ~y:2014 ~m:Feb ~d:28)) ~expect:59
  ;;

  let test_week_number_and_year y m d ~expect =
    [%test_result: int] (week_number (create_exn ~y ~m ~d)) ~expect:(fst expect);
    [%test_result: int * int] (week_number_and_year (create_exn ~y ~m ~d)) ~expect
  ;;

  let%test_unit _ = test_week_number_and_year 2014 Jan 1 ~expect:(1, 2014)
  let%test_unit _ = test_week_number_and_year 2014 Dec 31 ~expect:(1, 2015)
  let%test_unit _ = test_week_number_and_year 2010 Jan 1 ~expect:(53, 2009)
  let%test_unit _ = test_week_number_and_year 2017 Jan 1 ~expect:(52, 2016)
  let%test_unit _ = test_week_number_and_year 2014 Jan 10 ~expect:(2, 2014)
  let%test_unit _ = test_week_number_and_year 2012 Jan 1 ~expect:(52, 2011)
  let%test_unit _ = test_week_number_and_year 2012 Dec 31 ~expect:(1, 2013)
end

module%test [@name "diff_weekdays"] _ = struct
  let c y m d = create_exn ~y ~m ~d

  let%test "2014 Jan 1 is a Wednesday" =
    Day_of_week.( = ) (day_of_week (c 2014 Jan 1)) Day_of_week.Wed
  ;;

  let ( = ) = Int.( = )

  (* future minus Wednesday *)
  let%test _ = diff_weekdays (c 2014 Jan 1) (c 2014 Jan 1) = 0
  let%test _ = diff_weekdays (c 2014 Jan 2) (c 2014 Jan 1) = 1
  let%test _ = diff_weekdays (c 2014 Jan 3) (c 2014 Jan 1) = 2
  let%test _ = diff_weekdays (c 2014 Jan 4) (c 2014 Jan 1) = 3
  let%test _ = diff_weekdays (c 2014 Jan 5) (c 2014 Jan 1) = 3
  let%test _ = diff_weekdays (c 2014 Jan 6) (c 2014 Jan 1) = 3
  let%test _ = diff_weekdays (c 2014 Jan 7) (c 2014 Jan 1) = 4
  let%test _ = diff_weekdays (c 2014 Jan 8) (c 2014 Jan 1) = 5
  let%test _ = diff_weekdays (c 2014 Jan 9) (c 2014 Jan 1) = 6
  let%test _ = diff_weekdays (c 2014 Jan 10) (c 2014 Jan 1) = 7
  let%test _ = diff_weekdays (c 2014 Jan 11) (c 2014 Jan 1) = 8
  let%test _ = diff_weekdays (c 2014 Jan 12) (c 2014 Jan 1) = 8
  let%test _ = diff_weekdays (c 2014 Jan 13) (c 2014 Jan 1) = 8
  let%test _ = diff_weekdays (c 2014 Jan 14) (c 2014 Jan 1) = 9

  (* Wednesday minus future *)
  let%test _ = diff_weekdays (c 2014 Jan 1) (c 2014 Jan 2) = -1
  let%test _ = diff_weekdays (c 2014 Jan 1) (c 2014 Jan 3) = -2
  let%test _ = diff_weekdays (c 2014 Jan 1) (c 2014 Jan 4) = -3
  let%test _ = diff_weekdays (c 2014 Jan 1) (c 2014 Jan 5) = -3
  let%test _ = diff_weekdays (c 2014 Jan 1) (c 2014 Jan 6) = -3
  let%test _ = diff_weekdays (c 2014 Jan 1) (c 2014 Jan 7) = -4
  let%test _ = diff_weekdays (c 2014 Jan 1) (c 2014 Jan 8) = -5
  let%test _ = diff_weekdays (c 2014 Jan 1) (c 2014 Jan 9) = -6

  (* diff_weekend_days *)
  let%test _ = diff_weekend_days (c 2014 Jan 1) (c 2014 Jan 1) = 0
  let%test _ = diff_weekend_days (c 2014 Jan 2) (c 2014 Jan 1) = 0
  let%test _ = diff_weekend_days (c 2014 Jan 3) (c 2014 Jan 1) = 0
  let%test _ = diff_weekend_days (c 2014 Jan 4) (c 2014 Jan 1) = 0
  let%test _ = diff_weekend_days (c 2014 Jan 5) (c 2014 Jan 1) = 1
  let%test _ = diff_weekend_days (c 2014 Jan 6) (c 2014 Jan 1) = 2
  let%test _ = diff_weekend_days (c 2014 Jan 7) (c 2014 Jan 1) = 2
  let%test _ = diff_weekend_days (c 2014 Jan 8) (c 2014 Jan 1) = 2
  let%test _ = diff_weekend_days (c 2014 Jan 9) (c 2014 Jan 1) = 2
  let%test _ = diff_weekend_days (c 2014 Jan 10) (c 2014 Jan 1) = 2
  let%test _ = diff_weekend_days (c 2014 Jan 11) (c 2014 Jan 1) = 2
  let%test _ = diff_weekend_days (c 2014 Jan 12) (c 2014 Jan 1) = 3
  let%test _ = diff_weekend_days (c 2014 Jan 13) (c 2014 Jan 1) = 4
  let%test _ = diff_weekend_days (c 2014 Jan 14) (c 2014 Jan 1) = 4
end

module%test [@name "dates in a month"] _ = struct
  let sexp_of_year year =
    Dynamic.with_temporarily Sexp.of_int_style `No_underscores ~f:(fun () ->
      sexp_of_int year)
  ;;

  (* 2024 is a leap year. 2023 and 2025 are not. *)
  let years = [ 2023; 2024; 2025 ]

  (* Leap day month, 31-day month, and 30-day month. *)
  let months : Month.t list = [ Feb; Mar; Apr ]

  let%expect_test "last_date_in_month" =
    List.iter years ~f:(fun year ->
      List.iter Month.all ~f:(fun month ->
        let date = Date.last_date_in_month ~year ~month in
        print_s [%sexp ((year, month, date) : year * Month.t * Date.t)]));
    [%expect
      {|
      (2023 Jan 2023-01-31)
      (2023 Feb 2023-02-28)
      (2023 Mar 2023-03-31)
      (2023 Apr 2023-04-30)
      (2023 May 2023-05-31)
      (2023 Jun 2023-06-30)
      (2023 Jul 2023-07-31)
      (2023 Aug 2023-08-31)
      (2023 Sep 2023-09-30)
      (2023 Oct 2023-10-31)
      (2023 Nov 2023-11-30)
      (2023 Dec 2023-12-31)
      (2024 Jan 2024-01-31)
      (2024 Feb 2024-02-29)
      (2024 Mar 2024-03-31)
      (2024 Apr 2024-04-30)
      (2024 May 2024-05-31)
      (2024 Jun 2024-06-30)
      (2024 Jul 2024-07-31)
      (2024 Aug 2024-08-31)
      (2024 Sep 2024-09-30)
      (2024 Oct 2024-10-31)
      (2024 Nov 2024-11-30)
      (2024 Dec 2024-12-31)
      (2025 Jan 2025-01-31)
      (2025 Feb 2025-02-28)
      (2025 Mar 2025-03-31)
      (2025 Apr 2025-04-30)
      (2025 May 2025-05-31)
      (2025 Jun 2025-06-30)
      (2025 Jul 2025-07-31)
      (2025 Aug 2025-08-31)
      (2025 Sep 2025-09-30)
      (2025 Oct 2025-10-31)
      (2025 Nov 2025-11-30)
      (2025 Dec 2025-12-31)
      |}]
  ;;

  let%expect_test "all_dates_in_month" =
    List.iter years ~f:(fun year ->
      List.iter months ~f:(fun month ->
        let dates = Date.all_dates_in_month ~year ~month in
        Core.print_s [%sexp ((year, month, dates) : year * Month.t * Date.t list)]));
    [%expect
      {|
      (2023 Feb
       (2023-02-01 2023-02-02 2023-02-03 2023-02-04 2023-02-05 2023-02-06
        2023-02-07 2023-02-08 2023-02-09 2023-02-10 2023-02-11 2023-02-12
        2023-02-13 2023-02-14 2023-02-15 2023-02-16 2023-02-17 2023-02-18
        2023-02-19 2023-02-20 2023-02-21 2023-02-22 2023-02-23 2023-02-24
        2023-02-25 2023-02-26 2023-02-27 2023-02-28))
      (2023 Mar
       (2023-03-01 2023-03-02 2023-03-03 2023-03-04 2023-03-05 2023-03-06
        2023-03-07 2023-03-08 2023-03-09 2023-03-10 2023-03-11 2023-03-12
        2023-03-13 2023-03-14 2023-03-15 2023-03-16 2023-03-17 2023-03-18
        2023-03-19 2023-03-20 2023-03-21 2023-03-22 2023-03-23 2023-03-24
        2023-03-25 2023-03-26 2023-03-27 2023-03-28 2023-03-29 2023-03-30
        2023-03-31))
      (2023 Apr
       (2023-04-01 2023-04-02 2023-04-03 2023-04-04 2023-04-05 2023-04-06
        2023-04-07 2023-04-08 2023-04-09 2023-04-10 2023-04-11 2023-04-12
        2023-04-13 2023-04-14 2023-04-15 2023-04-16 2023-04-17 2023-04-18
        2023-04-19 2023-04-20 2023-04-21 2023-04-22 2023-04-23 2023-04-24
        2023-04-25 2023-04-26 2023-04-27 2023-04-28 2023-04-29 2023-04-30))
      (2024 Feb
       (2024-02-01 2024-02-02 2024-02-03 2024-02-04 2024-02-05 2024-02-06
        2024-02-07 2024-02-08 2024-02-09 2024-02-10 2024-02-11 2024-02-12
        2024-02-13 2024-02-14 2024-02-15 2024-02-16 2024-02-17 2024-02-18
        2024-02-19 2024-02-20 2024-02-21 2024-02-22 2024-02-23 2024-02-24
        2024-02-25 2024-02-26 2024-02-27 2024-02-28 2024-02-29))
      (2024 Mar
       (2024-03-01 2024-03-02 2024-03-03 2024-03-04 2024-03-05 2024-03-06
        2024-03-07 2024-03-08 2024-03-09 2024-03-10 2024-03-11 2024-03-12
        2024-03-13 2024-03-14 2024-03-15 2024-03-16 2024-03-17 2024-03-18
        2024-03-19 2024-03-20 2024-03-21 2024-03-22 2024-03-23 2024-03-24
        2024-03-25 2024-03-26 2024-03-27 2024-03-28 2024-03-29 2024-03-30
        2024-03-31))
      (2024 Apr
       (2024-04-01 2024-04-02 2024-04-03 2024-04-04 2024-04-05 2024-04-06
        2024-04-07 2024-04-08 2024-04-09 2024-04-10 2024-04-11 2024-04-12
        2024-04-13 2024-04-14 2024-04-15 2024-04-16 2024-04-17 2024-04-18
        2024-04-19 2024-04-20 2024-04-21 2024-04-22 2024-04-23 2024-04-24
        2024-04-25 2024-04-26 2024-04-27 2024-04-28 2024-04-29 2024-04-30))
      (2025 Feb
       (2025-02-01 2025-02-02 2025-02-03 2025-02-04 2025-02-05 2025-02-06
        2025-02-07 2025-02-08 2025-02-09 2025-02-10 2025-02-11 2025-02-12
        2025-02-13 2025-02-14 2025-02-15 2025-02-16 2025-02-17 2025-02-18
        2025-02-19 2025-02-20 2025-02-21 2025-02-22 2025-02-23 2025-02-24
        2025-02-25 2025-02-26 2025-02-27 2025-02-28))
      (2025 Mar
       (2025-03-01 2025-03-02 2025-03-03 2025-03-04 2025-03-05 2025-03-06
        2025-03-07 2025-03-08 2025-03-09 2025-03-10 2025-03-11 2025-03-12
        2025-03-13 2025-03-14 2025-03-15 2025-03-16 2025-03-17 2025-03-18
        2025-03-19 2025-03-20 2025-03-21 2025-03-22 2025-03-23 2025-03-24
        2025-03-25 2025-03-26 2025-03-27 2025-03-28 2025-03-29 2025-03-30
        2025-03-31))
      (2025 Apr
       (2025-04-01 2025-04-02 2025-04-03 2025-04-04 2025-04-05 2025-04-06
        2025-04-07 2025-04-08 2025-04-09 2025-04-10 2025-04-11 2025-04-12
        2025-04-13 2025-04-14 2025-04-15 2025-04-16 2025-04-17 2025-04-18
        2025-04-19 2025-04-20 2025-04-21 2025-04-22 2025-04-23 2025-04-24
        2025-04-25 2025-04-26 2025-04-27 2025-04-28 2025-04-29 2025-04-30))
      |}]
  ;;
end

module%test [@name "adding weekdays and business days"] _ = struct
  let test alist day_of_week date_string =
    let date = Date.of_string date_string in
    require_equal (module Day_of_week) day_of_week (Date.day_of_week date);
    List.iter alist ~f:(fun (name, round_and_add) ->
      let list =
        List.map [ -2; -1; 0; 1; 2 ] ~f:(fun increment ->
          let date = round_and_add date increment in
          let day_of_week = Date.day_of_week date in
          increment, day_of_week, date)
      in
      print_s [%sexp (name : string), (list : (int * Day_of_week.t * Date.t) list)])
  ;;

  let%expect_test "weekdays" =
    let open Day_of_week in
    let test =
      test
        [ "add_weekdays_rounding_backward", add_weekdays_rounding_backward
        ; "add_weekdays_rounding_forward", add_weekdays_rounding_forward
        ]
    in
    (* Friday *)
    test Fri "2019-05-03";
    [%expect
      {|
      (add_weekdays_rounding_backward (
        (-2 WED 2019-05-01)
        (-1 THU 2019-05-02)
        (0  FRI 2019-05-03)
        (1  MON 2019-05-06)
        (2  TUE 2019-05-07)))
      (add_weekdays_rounding_forward (
        (-2 WED 2019-05-01)
        (-1 THU 2019-05-02)
        (0  FRI 2019-05-03)
        (1  MON 2019-05-06)
        (2  TUE 2019-05-07)))
      |}];
    (* Saturday, Sunday: both round back to Friday or forward to Monday *)
    List.iter
      [ Sat, "2019-05-04"; Sun, "2019-05-05" ]
      ~f:(fun (day_of_week, date_string) ->
        test day_of_week date_string;
        [%expect
          {|
          (add_weekdays_rounding_backward (
            (-2 WED 2019-05-01)
            (-1 THU 2019-05-02)
            (0  FRI 2019-05-03)
            (1  MON 2019-05-06)
            (2  TUE 2019-05-07)))
          (add_weekdays_rounding_forward (
            (-2 THU 2019-05-02)
            (-1 FRI 2019-05-03)
            (0  MON 2019-05-06)
            (1  TUE 2019-05-07)
            (2  WED 2019-05-08)))
          |}]);
    (* Monday *)
    test Mon "2019-05-06";
    [%expect
      {|
      (add_weekdays_rounding_backward (
        (-2 THU 2019-05-02)
        (-1 FRI 2019-05-03)
        (0  MON 2019-05-06)
        (1  TUE 2019-05-07)
        (2  WED 2019-05-08)))
      (add_weekdays_rounding_forward (
        (-2 THU 2019-05-02)
        (-1 FRI 2019-05-03)
        (0  MON 2019-05-06)
        (1  TUE 2019-05-07)
        (2  WED 2019-05-08)))
      |}]
  ;;

  let%expect_test "business days" =
    let open Day_of_week in
    let test ~is_weekday =
      let is_holiday = Date.equal (Date.of_string "2019-05-06") in
      test
        [ ( "add_business_days_rounding_backward"
          , add_business_days_rounding_backward ~is_holiday ?is_weekday )
        ; ( "add_business_days_rounding_forward"
          , add_business_days_rounding_forward ~is_holiday ?is_weekday )
        ]
    in
    (* Friday *)
    test ~is_weekday:None Fri "2019-05-03";
    [%expect
      {|
      (add_business_days_rounding_backward (
        (-2 WED 2019-05-01)
        (-1 THU 2019-05-02)
        (0  FRI 2019-05-03)
        (1  TUE 2019-05-07)
        (2  WED 2019-05-08)))
      (add_business_days_rounding_forward (
        (-2 WED 2019-05-01)
        (-1 THU 2019-05-02)
        (0  FRI 2019-05-03)
        (1  TUE 2019-05-07)
        (2  WED 2019-05-08)))
      |}];
    (* Saturday, Sunday, Monday: all round back to Friday or forward to Tuesday *)
    List.iter
      [ Sat, "2019-05-04"; Sun, "2019-05-05"; Mon, "2019-05-06" ]
      ~f:(fun (day_of_week, date_string) ->
        test ~is_weekday:None day_of_week date_string;
        [%expect
          {|
          (add_business_days_rounding_backward (
            (-2 WED 2019-05-01)
            (-1 THU 2019-05-02)
            (0  FRI 2019-05-03)
            (1  TUE 2019-05-07)
            (2  WED 2019-05-08)))
          (add_business_days_rounding_forward (
            (-2 THU 2019-05-02)
            (-1 FRI 2019-05-03)
            (0  TUE 2019-05-07)
            (1  WED 2019-05-08)
            (2  THU 2019-05-09)))
          |}]);
    (* Tuesday *)
    test ~is_weekday:None Tue "2019-05-07";
    [%expect
      {|
      (add_business_days_rounding_backward (
        (-2 THU 2019-05-02)
        (-1 FRI 2019-05-03)
        (0  TUE 2019-05-07)
        (1  WED 2019-05-08)
        (2  THU 2019-05-09)))
      (add_business_days_rounding_forward (
        (-2 THU 2019-05-02)
        (-1 FRI 2019-05-03)
        (0  TUE 2019-05-07)
        (1  WED 2019-05-08)
        (2  THU 2019-05-09)))
      |}];
    (* Weekday override tests - a Sun-Thu schedule *)
    let is_weekday_nonstandard day =
      match day with
      | Fri | Sat -> false
      | _ -> true
    in
    (* Friday and Saturday roll forward to Sunday and backward to Thursday. *)
    List.iter
      [ Fri, "2019-05-03"; Sat, "2019-05-04" ]
      ~f:(fun (day_of_week, date_string) ->
        test ~is_weekday:(Some is_weekday_nonstandard) day_of_week date_string;
        [%expect
          {|
          (add_business_days_rounding_backward (
            (-2 TUE 2019-04-30)
            (-1 WED 2019-05-01)
            (0  THU 2019-05-02)
            (1  SUN 2019-05-05)
            (2  TUE 2019-05-07)))
          (add_business_days_rounding_forward (
            (-2 WED 2019-05-01)
            (-1 THU 2019-05-02)
            (0  SUN 2019-05-05)
            (1  TUE 2019-05-07)
            (2  WED 2019-05-08)))
          |}]);
    (* weekdays override: Tuesday *)
    test ~is_weekday:(Some is_weekday_nonstandard) Tue "2019-05-07";
    [%expect
      {|
      (add_business_days_rounding_backward (
        (-2 THU 2019-05-02)
        (-1 SUN 2019-05-05)
        (0  TUE 2019-05-07)
        (1  WED 2019-05-08)
        (2  THU 2019-05-09)))
      (add_business_days_rounding_forward (
        (-2 THU 2019-05-02)
        (-1 SUN 2019-05-05)
        (0  TUE 2019-05-07)
        (1  WED 2019-05-08)
        (2  THU 2019-05-09)))
      |}]
  ;;
end

module%test [@name "ordinal_date"] _ = struct
  (* check the ordinal date tables we found on wikipedia... *)
  let check_table year ordinal_date_table =
    let days_of_year =
      dates_between
        ~min:(create_exn ~y:year ~m:Month.Jan ~d:01)
        ~max:(create_exn ~y:year ~m:Month.Dec ~d:31)
    in
    [%test_result: int]
      (List.length days_of_year)
      ~expect:(if is_leap_year ~year then 366 else 365);
    let months =
      List.group days_of_year ~break:(fun d d' -> Month.( <> ) (month d) (month d'))
    in
    let sum =
      List.foldi months ~init:0 ~f:(fun index sum month ->
        [%test_result: int] sum ~expect:ordinal_date_table.:(index);
        sum + List.length month)
    in
    [%test_result: int] sum ~expect:(List.length days_of_year)
  ;;

  let%test_unit _ = check_table 2015 non_leap_year_table
  let%test_unit _ = check_table 2000 leap_year_table
end

module%test [@name "weekdays_between"] _ = struct
  let c y m d = create_exn ~y ~m ~d

  (* systematic test of consistency between [weekdays_between] and [diff_weekdays] *)
  let dates =
    [ c 2014 Jan 1
    ; c 2014 Jan 2
    ; c 2014 Jan 3
    ; c 2014 Jan 4
    ; c 2014 Jan 5
    ; c 2014 Jan 6
    ; c 2014 Jan 7
    ; c 2014 Feb 15
    ; c 2014 Feb 16
    ; c 2014 Feb 17
    ; c 2014 Feb 18
    ; c 2014 Feb 19
    ; c 2014 Feb 20
    ; c 2014 Feb 21
    ]
  ;;

  let ( = ) = Int.( = )

  let%test_unit _ =
    List.iter dates ~f:(fun date1 ->
      List.iter dates ~f:(fun date2 ->
        if date1 <= date2
        then
          assert (
            List.length (weekdays_between ~min:date1 ~max:(add_days date2 (-1)))
            = diff_weekdays date2 date1)))
  ;;
end

module%test [@name "first_strictly_after"] _ = struct
  let mon1 = create_exn ~y:2013 ~m:Month.Apr ~d:1
  let tue1 = create_exn ~y:2013 ~m:Month.Apr ~d:2
  let wed1 = create_exn ~y:2013 ~m:Month.Apr ~d:3
  let thu1 = create_exn ~y:2013 ~m:Month.Apr ~d:4
  let fri1 = create_exn ~y:2013 ~m:Month.Apr ~d:5
  let sat1 = create_exn ~y:2013 ~m:Month.Apr ~d:6
  let sun1 = create_exn ~y:2013 ~m:Month.Apr ~d:7
  let mon2 = create_exn ~y:2013 ~m:Month.Apr ~d:8
  let tue2 = create_exn ~y:2013 ~m:Month.Apr ~d:9
  let%test _ = equal (first_strictly_after tue1 ~on:Day_of_week.Mon) mon2
  let%test _ = equal (first_strictly_after tue1 ~on:Day_of_week.Tue) tue2
  let%test _ = equal (first_strictly_after tue1 ~on:Day_of_week.Wed) wed1
  let%test _ = equal (first_strictly_after tue1 ~on:Day_of_week.Thu) thu1
  let%test _ = equal (first_strictly_after tue1 ~on:Day_of_week.Fri) fri1
  let%test _ = equal (first_strictly_after tue1 ~on:Day_of_week.Sat) sat1
  let%test _ = equal (first_strictly_after tue1 ~on:Day_of_week.Sun) sun1
  let%test _ = equal (first_strictly_after mon1 ~on:Day_of_week.Mon) mon2
  let%test _ = equal (first_strictly_after mon1 ~on:Day_of_week.Tue) tue1
  let%test _ = equal (first_strictly_after mon1 ~on:Day_of_week.Wed) wed1
  let%test _ = equal (first_strictly_after mon1 ~on:Day_of_week.Thu) thu1
  let%test _ = equal (first_strictly_after mon1 ~on:Day_of_week.Fri) fri1
  let%test _ = equal (first_strictly_after mon1 ~on:Day_of_week.Sat) sat1
  let%test _ = equal (first_strictly_after mon1 ~on:Day_of_week.Sun) sun1
end

let%test_unit _ =
  Quickcheck.test_can_generate quickcheck_generator ~sexp_of:sexp_of_t ~f:(fun t ->
    t = of_string "1900-01-01")
;;

let%test_unit _ =
  Quickcheck.test_can_generate quickcheck_generator ~sexp_of:sexp_of_t ~f:(fun t ->
    t = of_string "2100-01-01")
;;

let%test_unit _ =
  Quickcheck.test_can_generate quickcheck_generator ~sexp_of:sexp_of_t ~f:(fun t ->
    of_string "1900-01-01" < t && t < of_string "2100-01-01")
;;

let%test_unit _ =
  Quickcheck.test_distinct_values
    quickcheck_generator
    ~sexp_of:sexp_of_t
    ~compare
    ~trials:1_000
    ~distinct_values:500
;;

let%test_unit _ =
  Quickcheck.test_can_generate
    Date.Option.quickcheck_generator
    ~sexp_of:Date.Option.sexp_of_t
    ~f:(fun t -> Date.Option.equal t Date.Option.none)
;;

let%test_unit _ =
  Quickcheck.test_can_generate
    Date.Option.quickcheck_generator
    ~sexp_of:Date.Option.sexp_of_t
    ~f:(fun t -> Date.Option.equal t (Date.Option.some (Date.of_string "1900-01-01")))
;;

let%test_unit _ =
  Quickcheck.test_can_generate
    Date.Option.quickcheck_generator
    ~sexp_of:Date.Option.sexp_of_t
    ~f:(fun t -> Date.Option.equal t (Date.Option.some (Date.of_string "2100-01-01")))
;;

let%test_unit _ =
  Quickcheck.test_can_generate
    Date.Option.quickcheck_generator
    ~sexp_of:Date.Option.sexp_of_t
    ~f:(fun t ->
      Date.Option.between
        t
        ~low:(Date.Option.some (Date.of_string "1900-01-01"))
        ~high:(Date.Option.some (Date.of_string "2100-01-01")))
;;

let%test_unit _ =
  Quickcheck.test_distinct_values
    Date.Option.quickcheck_generator
    ~sexp_of:Date.Option.sexp_of_t
    ~compare:Date.Option.compare
    ~trials:1_000
    ~distinct_values:100
;;

let%test_unit "compare" =
  let slow_compare t1 t2 =
    let n = Int.compare (Date.year t1) (Date.year t2) in
    if Int.( <> ) n 0
    then n
    else (
      let n = Month.compare (Date.month t1) (Date.month t2) in
      if Int.( <> ) n 0 then n else Int.compare (Date.day t1) (Date.day t2))
  in
  let pairs =
    let%bind.Quickcheck.Generator d0 = Date.quickcheck_generator in
    let%map.Quickcheck.Generator d1 = Date.quickcheck_generator in
    d0, d1
  in
  Quickcheck.iter pairs ~f:(fun (d0, d1) ->
    let cmp0 = Date.compare d0 d1 in
    let cmp1 = slow_compare d0 d1 in
    if Int.( <> ) cmp0 cmp1
    then
      raise_s
        [%message
          "Date.compare gave unexpected result"
            (cmp0 : int)
            (cmp1 : int)
            (d0 : Date.t)
            (d1 : Date.t)])
;;
