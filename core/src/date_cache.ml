open! Import
include Date_cache_intf.Definitions

module Make (Time : Time) () = struct
  open struct
    module Date_and_ofday = Time.Date_and_ofday
    module Ofday = Time.Ofday
    module Span = Time.Span
    module Zone = Time.Zone
  end

  module Date_cache = struct
    type t =
      { mutable zone : Zone.t
      ; mutable cache_start_incl : Time.t
      ; mutable cache_until_excl : Time.t
      ; mutable effective_day_start : Time.t
      ; mutable date : Date0.t
      }

    let create () =
      { zone = Zone.utc
      ; cache_start_incl = Time.epoch
      ; cache_until_excl = Time.epoch
      ; effective_day_start = Time.epoch
      ; date = Date0.unix_epoch
      }
    ;;

    let reset t =
      t.zone <- Zone.utc;
      t.cache_start_incl <- Time.epoch;
      t.cache_until_excl <- Time.epoch;
      t.effective_day_start <- Time.epoch;
      t.date <- Date0.unix_epoch
    ;;

    let is_in_cache t time ~zone =
      phys_equal zone t.zone
      && Time.( >= ) time t.cache_start_incl
      && Time.( < ) time t.cache_until_excl
    ;;

    let update t time ~zone =
      let index = Zone.index zone time in
      (* no exn because [Zone.index] always returns a valid index *)
      let offset_from_utc = Zone.index_offset_from_utc_exn zone index in
      let rel = Date_and_ofday.of_absolute time ~offset_from_utc in
      let date = Date_and_ofday.to_date rel in
      let span = Date_and_ofday.to_ofday rel |> Ofday.to_span_since_start_of_day in
      let effective_day_start =
        Time.sub (Date_and_ofday.to_absolute rel ~offset_from_utc) span
      in
      let effective_day_until = Time.add effective_day_start Span.day in
      let cache_start_incl =
        match Zone.index_has_prev_clock_shift zone index with
        | false -> effective_day_start
        | true ->
          effective_day_start
          |> Time.max (Zone.index_prev_clock_shift_time_exn zone index)
      in
      let cache_until_excl =
        match Zone.index_has_next_clock_shift zone index with
        | false -> effective_day_until
        | true ->
          effective_day_until
          |> Time.min (Zone.index_next_clock_shift_time_exn zone index)
      in
      t.zone <- zone;
      t.cache_start_incl <- cache_start_incl;
      t.cache_until_excl <- cache_until_excl;
      t.effective_day_start <- effective_day_start;
      t.date <- date
    ;;
  end

  let date_cache = Date_cache.create ()

  let populate time ~zone =
    match Date_cache.is_in_cache date_cache time ~zone with
    | true -> ()
    | false -> Date_cache.update date_cache time ~zone
  ;;

  let get_date time ~zone =
    populate time ~zone;
    date_cache.date
  ;;

  let get_day_start time ~zone =
    populate time ~zone;
    date_cache.effective_day_start
  ;;

  let reset () = Date_cache.reset date_cache
end
