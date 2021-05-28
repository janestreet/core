open! Core
module Time = Time_unix

let epoch = Time.epoch
let winter = Time.of_string "2000-01-01 06:00:00Z"
let summer = Time.of_string "2020-08-20 18:00:00Z"
let hkg = Time.Zone.find_exn "Asia/Hong_Kong"
let ldn = Time.Zone.find_exn "Europe/London"
let nyc = Time.Zone.find_exn "America/New_York"
let utc = Time.Zone.utc
let next_clock_shift zone time = Time.Zone.next_clock_shift zone ~strictly_after:time

let%bench "next_clock_shift hkg epoch" = next_clock_shift hkg epoch
let%bench "next_clock_shift hkg winter" = next_clock_shift hkg winter
let%bench "next_clock_shift hkg summer" = next_clock_shift hkg summer
let%bench "next_clock_shift ldn epoch" = next_clock_shift ldn epoch
let%bench "next_clock_shift ldn winter" = next_clock_shift ldn winter
let%bench "next_clock_shift ldn summer" = next_clock_shift ldn summer
let%bench "next_clock_shift nyc epoch" = next_clock_shift nyc epoch
let%bench "next_clock_shift nyc winter" = next_clock_shift nyc winter
let%bench "next_clock_shift nyc summer" = next_clock_shift nyc summer
let%bench "next_clock_shift utc epoch" = next_clock_shift utc epoch
let%bench "next_clock_shift utc winter" = next_clock_shift utc winter
let%bench "next_clock_shift utc summer" = next_clock_shift utc summer

let prev_clock_shift zone time = Time.Zone.prev_clock_shift zone ~at_or_before:time

let%bench "prev_clock_shift hkg epoch" = prev_clock_shift hkg epoch
let%bench "prev_clock_shift hkg winter" = prev_clock_shift hkg winter
let%bench "prev_clock_shift hkg summer" = prev_clock_shift hkg summer
let%bench "prev_clock_shift ldn epoch" = prev_clock_shift ldn epoch
let%bench "prev_clock_shift ldn winter" = prev_clock_shift ldn winter
let%bench "prev_clock_shift ldn summer" = prev_clock_shift ldn summer
let%bench "prev_clock_shift nyc epoch" = prev_clock_shift nyc epoch
let%bench "prev_clock_shift nyc winter" = prev_clock_shift nyc winter
let%bench "prev_clock_shift nyc summer" = prev_clock_shift nyc summer
let%bench "prev_clock_shift utc epoch" = prev_clock_shift utc epoch
let%bench "prev_clock_shift utc winter" = prev_clock_shift utc winter
let%bench "prev_clock_shift utc summer" = prev_clock_shift utc summer

let to_date_ofday zone time = Time.to_date_ofday ~zone time

let%bench "to_date_ofday hkg epoch" = to_date_ofday hkg epoch
let%bench "to_date_ofday hkg winter" = to_date_ofday hkg winter
let%bench "to_date_ofday hkg summer" = to_date_ofday hkg summer
let%bench "to_date_ofday ldn epoch" = to_date_ofday ldn epoch
let%bench "to_date_ofday ldn winter" = to_date_ofday ldn winter
let%bench "to_date_ofday ldn summer" = to_date_ofday ldn summer
let%bench "to_date_ofday nyc epoch" = to_date_ofday nyc epoch
let%bench "to_date_ofday nyc winter" = to_date_ofday nyc winter
let%bench "to_date_ofday nyc summer" = to_date_ofday nyc summer
let%bench "to_date_ofday utc epoch" = to_date_ofday utc epoch
let%bench "to_date_ofday utc winter" = to_date_ofday utc winter
let%bench "to_date_ofday utc summer" = to_date_ofday utc summer

let of_date_ofday zone time =
  let date = Time.to_date ~zone time in
  let ofday = Time.to_ofday ~zone time in
  fun () -> Time.of_date_ofday ~zone date ofday
;;

let%bench_fun "of_date_ofday hkg epoch" = of_date_ofday hkg epoch
let%bench_fun "of_date_ofday hkg winter" = of_date_ofday hkg winter
let%bench_fun "of_date_ofday hkg summer" = of_date_ofday hkg summer
let%bench_fun "of_date_ofday ldn epoch" = of_date_ofday ldn epoch
let%bench_fun "of_date_ofday ldn winter" = of_date_ofday ldn winter
let%bench_fun "of_date_ofday ldn summer" = of_date_ofday ldn summer
let%bench_fun "of_date_ofday nyc epoch" = of_date_ofday nyc epoch
let%bench_fun "of_date_ofday nyc winter" = of_date_ofday nyc winter
let%bench_fun "of_date_ofday nyc summer" = of_date_ofday nyc summer
let%bench_fun "of_date_ofday utc epoch" = of_date_ofday utc epoch
let%bench_fun "of_date_ofday utc winter" = of_date_ofday utc winter
let%bench_fun "of_date_ofday utc summer" = of_date_ofday utc summer

let reset_caches zone =
  Time.reset_date_cache ();
  Time.Zone.reset_transition_cache zone
;;

let reset_and_next_clock_shift zone time =
  reset_caches zone;
  Time.Zone.next_clock_shift zone ~strictly_after:time
;;

let%bench "reset + next_clock_shift hkg epoch" = reset_and_next_clock_shift hkg epoch
let%bench "reset + next_clock_shift hkg winter" = reset_and_next_clock_shift hkg winter
let%bench "reset + next_clock_shift hkg summer" = reset_and_next_clock_shift hkg summer
let%bench "reset + next_clock_shift ldn epoch" = reset_and_next_clock_shift ldn epoch
let%bench "reset + next_clock_shift ldn winter" = reset_and_next_clock_shift ldn winter
let%bench "reset + next_clock_shift ldn summer" = reset_and_next_clock_shift ldn summer
let%bench "reset + next_clock_shift nyc epoch" = reset_and_next_clock_shift nyc epoch
let%bench "reset + next_clock_shift nyc winter" = reset_and_next_clock_shift nyc winter
let%bench "reset + next_clock_shift nyc summer" = reset_and_next_clock_shift nyc summer
let%bench "reset + next_clock_shift utc epoch" = reset_and_next_clock_shift utc epoch
let%bench "reset + next_clock_shift utc winter" = reset_and_next_clock_shift utc winter
let%bench "reset + next_clock_shift utc summer" = reset_and_next_clock_shift utc summer

let reset_and_prev_clock_shift zone time =
  reset_caches zone;
  Time.Zone.prev_clock_shift zone ~at_or_before:time
;;

let%bench "reset + prev_clock_shift hkg epoch" = reset_and_prev_clock_shift hkg epoch
let%bench "reset + prev_clock_shift hkg winter" = reset_and_prev_clock_shift hkg winter
let%bench "reset + prev_clock_shift hkg summer" = reset_and_prev_clock_shift hkg summer
let%bench "reset + prev_clock_shift ldn epoch" = reset_and_prev_clock_shift ldn epoch
let%bench "reset + prev_clock_shift ldn winter" = reset_and_prev_clock_shift ldn winter
let%bench "reset + prev_clock_shift ldn summer" = reset_and_prev_clock_shift ldn summer
let%bench "reset + prev_clock_shift nyc epoch" = reset_and_prev_clock_shift nyc epoch
let%bench "reset + prev_clock_shift nyc winter" = reset_and_prev_clock_shift nyc winter
let%bench "reset + prev_clock_shift nyc summer" = reset_and_prev_clock_shift nyc summer
let%bench "reset + prev_clock_shift utc epoch" = reset_and_prev_clock_shift utc epoch
let%bench "reset + prev_clock_shift utc winter" = reset_and_prev_clock_shift utc winter
let%bench "reset + prev_clock_shift utc summer" = reset_and_prev_clock_shift utc summer

let reset_and_to_date_ofday zone time =
  reset_caches zone;
  Time.to_date_ofday ~zone time
;;

let%bench "reset + to_date_ofday hkg epoch" = reset_and_to_date_ofday hkg epoch
let%bench "reset + to_date_ofday hkg winter" = reset_and_to_date_ofday hkg winter
let%bench "reset + to_date_ofday hkg summer" = reset_and_to_date_ofday hkg summer
let%bench "reset + to_date_ofday ldn epoch" = reset_and_to_date_ofday ldn epoch
let%bench "reset + to_date_ofday ldn winter" = reset_and_to_date_ofday ldn winter
let%bench "reset + to_date_ofday ldn summer" = reset_and_to_date_ofday ldn summer
let%bench "reset + to_date_ofday nyc epoch" = reset_and_to_date_ofday nyc epoch
let%bench "reset + to_date_ofday nyc winter" = reset_and_to_date_ofday nyc winter
let%bench "reset + to_date_ofday nyc summer" = reset_and_to_date_ofday nyc summer
let%bench "reset + to_date_ofday utc epoch" = reset_and_to_date_ofday utc epoch
let%bench "reset + to_date_ofday utc winter" = reset_and_to_date_ofday utc winter
let%bench "reset + to_date_ofday utc summer" = reset_and_to_date_ofday utc summer

let reset_and_of_date_ofday zone time =
  let date = Time.to_date ~zone time in
  let ofday = Time.to_ofday ~zone time in
  fun () ->
    reset_caches zone;
    Time.of_date_ofday ~zone date ofday
;;

let%bench_fun "reset + of_date_ofday hkg epoch" = reset_and_of_date_ofday hkg epoch
let%bench_fun "reset + of_date_ofday hkg winter" = reset_and_of_date_ofday hkg winter
let%bench_fun "reset + of_date_ofday hkg summer" = reset_and_of_date_ofday hkg summer
let%bench_fun "reset + of_date_ofday ldn epoch" = reset_and_of_date_ofday ldn epoch
let%bench_fun "reset + of_date_ofday ldn winter" = reset_and_of_date_ofday ldn winter
let%bench_fun "reset + of_date_ofday ldn summer" = reset_and_of_date_ofday ldn summer
let%bench_fun "reset + of_date_ofday nyc epoch" = reset_and_of_date_ofday nyc epoch
let%bench_fun "reset + of_date_ofday nyc winter" = reset_and_of_date_ofday nyc winter
let%bench_fun "reset + of_date_ofday nyc summer" = reset_and_of_date_ofday nyc summer
let%bench_fun "reset + of_date_ofday utc epoch" = reset_and_of_date_ofday utc epoch
let%bench_fun "reset + of_date_ofday utc winter" = reset_and_of_date_ofday utc winter
let%bench_fun "reset + of_date_ofday utc summer" = reset_and_of_date_ofday utc summer
