open! Core
open! Import
open! Unit_of_time

let%test_unit "Span.Unit_of_time.t" =
  [%test_result: int] (compare Nanosecond Microsecond) ~expect:(-1);
  [%test_result: int] (compare Microsecond Millisecond) ~expect:(-1);
  [%test_result: int] (compare Millisecond Second) ~expect:(-1);
  [%test_result: int] (compare Second Minute) ~expect:(-1);
  [%test_result: int] (compare Minute Hour) ~expect:(-1);
  [%test_result: int] (compare Hour Day) ~expect:(-1)
;;
