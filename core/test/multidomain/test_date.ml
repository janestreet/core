open! Core
open! Date
open! Date.Private

module%test [@name "[Date_cache] does not race"] _ = struct
  module Barrier = Portable_test_helpers.Barrier

  let test
    : type (time : value mod contended portable) (result : value mod contended portable).
      (int -> time)
      -> (time -> zone:Timezone.t -> result) @ portable
      -> (module Expect_test_helpers_core.With_equal with type t = result iarray)
      -> unit
    =
    fun time_of_days f m ->
    let num_domains = 20 in
    let barrier = Barrier.create num_domains in
    let times = Iarray.init num_domains ~f:(fun n -> time_of_days (n * 5)) in
    let results = Iarray.init num_domains ~f:(fun _ -> Await_sync.Ivar.create ()) in
    for i = 0 to num_domains - 1 do
      match
        Multicore.spawn
          (fun () ->
            let time = Iarray.get times i in
            let ivar = Iarray.get results i in
            Barrier.await barrier;
            Await_sync.Ivar.fill_exn ivar (f time ~zone:Timezone.utc))
          ()
      with
      | Spawned -> ()
      | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt
    done;
    let results_parallel =
      Await_blocking.with_await Await.Terminator.never ~f:(fun block ->
        Iarray.map results ~f:(fun ivar -> Await_sync.Ivar.read block ivar) [@nontail])
    in
    let results_sequential =
      Iarray.map times ~f:(fun time -> f time ~zone:Timezone.utc)
    in
    Expect_test_helpers_base.require_equal m results_parallel results_sequential
  ;;

  let%expect_test "[Time_ns]" =
    let time_ns_of_days n = Time_ns.add Time_ns.epoch (Time_ns.Span.create ~day:n ()) in
    test
      time_ns_of_days
      Time_ns.to_date
      (module struct
        type t = Date.t Iarray.t [@@deriving equal, sexp_of]
      end);
    test
      time_ns_of_days
      Time_ns.to_ofday
      (module struct
        type t = Time_ns.Ofday.t Iarray.t [@@deriving equal, sexp_of]
      end)
  ;;

  let%expect_test "[Time_float]" =
    let time_float_of_days n =
      Time_float.add Time_float.epoch (Time_float.Span.create ~day:n ())
    in
    test
      time_float_of_days
      Time_float.to_date
      (module struct
        type t = Date.t Iarray.t [@@deriving equal, sexp_of]
      end);
    test
      time_float_of_days
      Time_float.to_ofday
      (module struct
        type t = Time_float.Ofday.t Iarray.t [@@deriving equal, sexp_of]
      end)
  ;;
end
