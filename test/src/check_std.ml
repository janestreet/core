(* These checks are here rather than in their corresponding modules because we want to
   check a property of the module as it is exported in Core. *)

open Core
open Expect_test_helpers_kernel

let%test_module _ =
  (module struct

    let%expect_test _ =
      print_and_check_container_sexps [%here] (module Time) [
        Time.epoch;
        Time.of_string "1955-11-12 18:38:00-08:00";
        Time.of_string "1985-10-26 21:00:00-08:00";
        Time.of_string "2015-10-21 19:28:00-08:00";
      ];
      [%expect {|
        (Set (
          (1955-11-12 21:38:00.000000-05:00)
          (1969-12-31 19:00:00.000000-05:00)
          (1985-10-27 01:00:00.000000-04:00)
          (2015-10-21 23:28:00.000000-04:00)))
        (Map (
          ((1955-11-12 21:38:00.000000-05:00) 1)
          ((1969-12-31 19:00:00.000000-05:00) 0)
          ((1985-10-27 01:00:00.000000-04:00) 2)
          ((2015-10-21 23:28:00.000000-04:00) 3)))
        (Hash_set (
          (1955-11-12 21:38:00.000000-05:00)
          (1969-12-31 19:00:00.000000-05:00)
          (1985-10-27 01:00:00.000000-04:00)
          (2015-10-21 23:28:00.000000-04:00)))
        (Table (
          ((1955-11-12 21:38:00.000000-05:00) 1)
          ((1969-12-31 19:00:00.000000-05:00) 0)
          ((1985-10-27 01:00:00.000000-04:00) 2)
          ((2015-10-21 23:28:00.000000-04:00) 3))) |}];
    ;;

    let%expect_test _ =
      print_and_check_container_sexps [%here] (module Time.Ofday) [
        Time.Ofday.start_of_day;
        Time.Ofday.of_string "12:00:00";
        Time.Ofday.of_string "23:59:59.999999";
      ];
      [%expect {|
        (Set (00:00:00.000000 12:00:00.000000 23:59:59.999999))
        (Map (
          (00:00:00.000000 0)
          (12:00:00.000000 1)
          (23:59:59.999999 2)))
        (Hash_set (00:00:00.000000 12:00:00.000000 23:59:59.999999))
        (Table (
          (00:00:00.000000 0)
          (12:00:00.000000 1)
          (23:59:59.999999 2))) |}];
    ;;

    let%expect_test _ =
      print_and_check_container_sexps [%here] (module Time.Span) [
        Time.Span.zero;
        Time.Span.nanosecond;
        Time.Span.microsecond;
        Time.Span.millisecond;
        Time.Span.second;
        Time.Span.minute;
        Time.Span.hour;
        Time.Span.day;
      ];
      [%expect {|
        (Set (0s 1e-06ms 0.001ms 1ms 1s 1m 1h 1d))
        (Map (
          (0s      0)
          (1e-06ms 1)
          (0.001ms 2)
          (1ms     3)
          (1s      4)
          (1m      5)
          (1h      6)
          (1d      7)))
        (Hash_set (0s 1e-06ms 0.001ms 1ms 1s 1m 1h 1d))
        (Table (
          (0s      0)
          (1e-06ms 1)
          (0.001ms 2)
          (1ms     3)
          (1s      4)
          (1m      5)
          (1h      6)
          (1d      7))) |}];
    ;;

    let%expect_test _ =
      print_and_check_container_sexps [%here] (module Month) Month.all;
      [%expect {|
        (Set (Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec))
        (Map (
          (Jan 0)
          (Feb 1)
          (Mar 2)
          (Apr 3)
          (May 4)
          (Jun 5)
          (Jul 6)
          (Aug 7)
          (Sep 8)
          (Oct 9)
          (Nov 10)
          (Dec 11)))
        (Hash_set (Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec))
        (Table (
          (Jan 0)
          (Feb 1)
          (Mar 2)
          (Apr 3)
          (May 4)
          (Jun 5)
          (Jul 6)
          (Aug 7)
          (Sep 8)
          (Oct 9)
          (Nov 10)
          (Dec 11))) |}];
    ;;

  end)

(* This ensures that Time_intf.S fits inside the intersection of Time and Time_ns. *)
include ((Time    : Time_common.S) : sig end)
include ((Time_ns : Time_common.S) : sig end)
