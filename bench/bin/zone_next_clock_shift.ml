open Core
open Core_bench.Std

let zone = Time.Zone.find_exn "Europe/London" ;;

(* on every half-hour in the year *)
let times =
  let epoch = Time.of_string "2013-01-01 00:30:00+00:00" in
  List.map (List.range 0 (365 * 24))
    ~f:(fun hr -> Time.add epoch (Time.Span.create ~hr ()))
;;

Command.run (Bench.make_command [
  Bench.Test.create ~name:"Time.Zone.next_clock_shift" (fun () ->
    List.iter times ~f:(fun time ->
      (ignore (Time.Zone.next_clock_shift zone ~strictly_after:time))))
]);;
