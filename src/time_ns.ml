open! Import
open Import_time

module Zone = Time.Zone

(* To break the dependency in the public release *)
module Time_ns = Core_kernel.Time_ns

(* This signature constraint is semi-temporary and serves to make the implementation more
   type-safe (so the compiler can help us more).  It would go away if we broke the
   implementation into multiple files. *)
module Span : sig
  include Time_ns_intf.Span

  val check_range : t -> t
end = struct
  let half_microsecond = Int63.of_int 500

  let nearest_microsecond t =
    Int63.((Time_ns.Span.to_int63_ns t + half_microsecond) /% of_int 1000)
  ;;

  let check_range t =
    let open Time_ns.Span in
    if t < min_value || t > max_value then
      failwiths "Span.t exceeds limits" (t, min_value, max_value)
        [%sexp_of: Alternate_sexp.t *
                   Alternate_sexp.t *
                   Alternate_sexp.t]
    else t
  ;;

  let to_span t =
    Time.Span.of_us (Int63.to_float (nearest_microsecond (check_range t)))
  ;;

  let min_kspan_value = to_span Time_ns.Span.min_value
  let max_kspan_value = to_span Time_ns.Span.max_value

  let of_span s =
    if Time.Span.( > ) s max_kspan_value
    || Time.Span.( < ) s min_kspan_value
    then
      failwiths "Time_ns.Span does not support this span" s [%sexp_of: Time.Span.t];
    (* Using [Time.Span.to_sec] (being the identity) so that
       we make don't apply too many conversion
       - Too many : `[Span.t] -> [a] -> [Time_ns.Span.t]`
       - Only One : `[Span.t]==[a] -> [Time_ns.Span.t]`. *)
    Time_ns.Span.of_sec_with_microsecond_precision (Time.Span.to_sec s)
  ;;

  (* We don't just convert to [Time.Span.t] and use the conversion there because our
     [to_span] conversion is limited to microsecond precision. *)
  let to_string_hum ?(delimiter='_') ?(decimals=3) ?(align_decimal=false) ?unit_of_time t
    =
    let open Time_ns.Span in
    let float, suffix =
      match Option.value unit_of_time ~default:(to_unit_of_time t) with
      | Day         -> to_day t, "d"
      | Hour        -> to_hr  t, "h"
      | Minute      -> to_min t, "m"
      | Second      -> to_sec t, "s"
      | Millisecond -> to_ms  t, "ms"
      | Microsecond -> to_us  t, "us"
      | Nanosecond  -> to_ns  t, "ns"
    in
    let prefix =
      Float.to_string_hum float ~delimiter ~decimals ~strip_zero:(not align_decimal)
    in
    let suffix =
      if align_decimal && Int.(=) (String.length suffix) 1
      then suffix ^ " "
      else suffix
    in
    prefix ^ suffix
  ;;

  module Stable = struct
    module V1 = struct
      module T = struct
        type nonrec t = Time_ns.Span.t [@@deriving bin_io, compare]

        let sexp_of_t t = Time.Span.Stable.V1.sexp_of_t (to_span t)
        let t_of_sexp s = of_span (Time.Span.Stable.V1.t_of_sexp s)

        let of_int63_exn t = check_range (Time_ns.Span.of_int63_ns t)
        let to_int63     t = Time_ns.Span.to_int63_ns t
      end
      include T
      include Comparator.Stable.V1.Make (T)
    end
  end

  module T = struct
    include Time_ns.Span

    let sexp_of_t = Stable.V1.sexp_of_t
    let t_of_sexp = Stable.V1.t_of_sexp

    let module_name = "Core.Time_ns.Span"
    let to_string t = Time.Span.to_string (to_span t)
    let of_string s = of_span (Time.Span.of_string s)
    let hash t = Int63.hash (Time_ns.Span.to_int63_ns t)
    let compare = compare
  end
  include T
  include Comparable.Validate_with_zero (T)
  include Identifiable.Make (T)
  (* The inclusion of [Comparable.Validate_with_zero] replaces the infix compare operators
     with [caml_int_compare] versions. The difference is noticable, a benchmark of
     [of_span_since_start_of_day_exn] shows 9.53ns vs 2.45ns. *)
  include (T : Core_kernel.Polymorphic_compare_intf.Infix with type t := t)

  let to_short_string t = Time.Span.to_short_string (to_span t)
  let randomize t ~percent = of_span (Time.Span.randomize (to_span t) ~percent)

  module Option = struct
    type span = t [@@deriving sexp]
    type t = Int63.t [@@deriving bin_io, compare, hash, typerep] (* nanoseconds or none *)
    let none = Int63.min_value
    let some span = to_int63_ns (check_range span)
    let is_none t = Int63.(t = none)
    let is_some t = Int63.(t <> none)
    let value t ~default = if is_none t then default else of_int63_ns t
    let unchecked_value t = of_int63_ns t

    let value_exn t =
      if is_some t
      then unchecked_value t
      else raise_s [%message [%here] "Span.Option.value_exn none"]

    let of_option = function None -> none | Some t -> some t
    let to_option t = if is_none t then None else Some (of_int63_ns t)

    module Optional_syntax = struct
      module Optional_syntax = struct
        let is_none         = is_none
        let unchecked_value = unchecked_value
      end
    end

    module Stable = struct
      module V1 = struct
        module T = struct
          type nonrec t = t [@@deriving compare, bin_io]

          let sexp_of_t t = [%sexp_of: Stable.V1.t option] (to_option t)
          let t_of_sexp s = of_option ([%of_sexp: Stable.V1.t option] s)

          let of_int63_exn i = if is_none i then none else some (of_int63_ns i)
          let to_int63     t = t
        end
        include T
        include Comparator.Stable.V1.Make (T)
      end
    end

    let sexp_of_t = Stable.V1.sexp_of_t
    let t_of_sexp = Stable.V1.t_of_sexp

    include Identifiable.Make (struct
        type nonrec t = t [@@deriving sexp, compare, bin_io]
        let hash = Int63.hash
        let module_name = "Core.Time_ns.Span.Option"
        include Sexpable.To_stringable (struct type nonrec t = t [@@deriving sexp] end)
      end)
  end
end

include (Time_ns : module type of struct include Time_ns end
         with module Span   := Time_ns.Span
          and module Stable := Time_ns.Stable)

let nanosleep t = Span.of_sec (Core_unix.nanosleep (Span.to_sec t))

let pause_for t =
  let time_remaining =
    (* If too large a float is passed in (Span.max_value for instance) then nanosleep
       will return immediately, leading to an infinite and expensive select loop.  This
       is handled by pausing for no longer than 100 days. *)
    nanosleep (Span.min t (Span.scale Span.day 100.))
  in
  if Span.( > ) time_remaining Span.zero
  then `Remaining time_remaining
  else `Ok
;;

(** Pause and don't allow events to interrupt. *)
let rec pause span =
  match pause_for span with
  | `Remaining span -> pause span
  | `Ok -> ()
;;

(** Pause but allow events to interrupt. *)
let interruptible_pause = pause_for

let rec pause_forever () =
  pause Span.day;
  pause_forever ()
;;

let to_time t =
  Time.add Time.epoch (Span.to_span (to_span_since_epoch t))
;;

let min_time_value = to_time min_value
let max_time_value = to_time max_value

let of_time t =
  if Time.( < ) t min_time_value
  || Time.( > ) t max_time_value
  then failwiths "Time_ns does not support this time" t [%sexp_of: Time.t];
  of_span_since_epoch (Span.of_span (Time.diff t Time.epoch))
;;

module Stable0 = struct
  module V1 = struct
    module T = struct
      type nonrec t = t [@@deriving bin_io, compare]

      let sexp_of_t (t : t) : Sexp.t = Time.Stable.V1.sexp_of_t (to_time t)
      let t_of_sexp s : t = of_time (Time.Stable.V1.t_of_sexp s)

      let of_int63_exn t =
      of_span_since_epoch (Span.check_range (Span.of_int63_ns t))

      let to_int63 t = to_int63_ns_since_epoch t
    end
    include T
    include Comparator.Stable.V1.Make (T)
  end
end

let sexp_of_t = Stable0.V1.sexp_of_t
let t_of_sexp = Stable0.V1.t_of_sexp

let to_string t = Time.to_string (to_time t)
let of_string s = of_time (Time.of_string s)

let to_string_abs t ~zone = Time.to_string_abs ~zone (to_time t)
let of_string_abs s = of_time (Time.of_string_abs s)

module Option = struct
  type time = t [@@deriving sexp, compare]

  type t = Span.Option.t [@@deriving bin_io, compare, hash, typerep]

  let none = Span.Option.none
  let some time = Span.Option.some (to_span_since_epoch time)
  let is_none = Span.Option.is_none
  let is_some = Span.Option.is_some
  let value t ~default =
    of_span_since_epoch
      (Span.Option.value
         ~default:(to_span_since_epoch default) t)
  let value_exn t = of_span_since_epoch (Span.Option.value_exn t)
  let unchecked_value t = of_span_since_epoch (Span.Option.unchecked_value t)

  let of_option = function None -> none | Some t -> some t
  let to_option t = if is_none t then None else Some (value_exn t)

  module Optional_syntax = struct
    module Optional_syntax = struct
      let is_none         = is_none
      let unchecked_value = unchecked_value
    end
  end

  module Stable = struct
    module V1 = struct
      module T = struct
        type nonrec t = t [@@deriving compare, bin_io]

        let sexp_of_t t = [%sexp_of: Stable0.V1.t option] (to_option t)
        let t_of_sexp s = of_option ([%of_sexp: Stable0.V1.t option] s)

        let to_int63     t = Span.Option.Stable.V1.to_int63     t
        let of_int63_exn t = Span.Option.Stable.V1.of_int63_exn t
      end
      include T
      include Comparator.Stable.V1.Make (T)
    end
  end

  let sexp_of_t = Stable.V1.sexp_of_t
  let t_of_sexp = Stable.V1.t_of_sexp

  include Identifiable.Make (struct
      type nonrec t = t [@@deriving sexp, compare, bin_io]
      let module_name = "Core.Time_ns.Option"
      let hash = Span.Option.hash
      include Sexpable.To_stringable (struct type nonrec t = t [@@deriving sexp] end)
    end)
  (* bring back the efficient implementation of comparison operators *)
  include (Span.Option : Core_kernel.Polymorphic_compare_intf.Infix with type t := t)
end

(* Note: This is FIX standard millisecond precision. You should use
   [Zero.Time_ns_with_fast_accurate_to_of_string] if you need nanosecond precision. *)
let to_string_fix_proto zone t = Time.to_string_fix_proto zone (to_time t)
let of_string_fix_proto zone s = of_time (Time.of_string_fix_proto zone s)

include Identifiable.Make (struct
    type nonrec t = t [@@deriving sexp, bin_io, compare]
    let module_name = "Core.Time_ns"
    let hash t = Int63.hash (to_int63_ns_since_epoch t)
    let of_string, to_string = of_string, to_string
  end)
(* bring back the efficient implementation of comparison operators *)
include (Core_kernel.Time_ns : Core_kernel.Polymorphic_compare_intf.Infix with type t := t)

(* Helper function to avoid inaccuracies in [Time_ns.t] <-> [Date.t * Ofday.t] conversions
   below.  We do the conversions by round-tripping through [Time.t] and [Time.Ofday.t],
   which (if done naively) would be inaccurate.  However, multiples of 1s can be
   accurately represented as [Time.t] and [Time.Ofday.t], so we deal with it by splitting
   into the full second and whatever is left. *)
let split_into_sec_and_ns (x : Int63.t) =
  let billion = Int63.of_int 1_000_000_000 in
  let sec = Int63.(/) x billion in
  let sec_as_ns = Int63.( * ) sec billion in
  let rem_ns = Int63.(-) x sec_as_ns in
  (sec_as_ns, Span.of_int63_ns rem_ns)
;;

let to_date t ~zone =
  let t', _ = split_into_sec_and_ns (to_int63_ns_since_epoch t) in
  Time.to_date (to_time (of_int63_ns_since_epoch t')) ~zone
;;

let of_date_ofday ~zone date ofday =
  of_time (Time.of_date_ofday ~zone date ofday)
;;

(* Presently this is not zoned.

   Does not represent extra hours due to DST (daylight saving time) (because DST makes
   adjustments in terms of wall clock time) or leap seconds (which aren't represented in
   Unix linear time).  See {!Ofday}. *)
module Ofday = struct
  type t = Span.t (* since wall-clock midnight *)
  [@@deriving typerep, compare, bin_io]

  include Comparable.Validate_with_zero (Span)


  let start_of_day : t = Span.zero
  let end_of_day   : t = Span.day

  let to_span_since_start_of_day t = t

  let [@inline never] input_out_of_bounds s =
    raise_s [%message "Time_ns.Ofday.of_span_since_start_of_day_exn: input out of bounds"
                        (s : Span.t)]
  ;;

  let of_span_since_start_of_day_exn (s : Span.t) =
    (* Why we use [Span.(>)] rather than [.(>=)] below:

       We allow to represent the end-of-day sentinel value ([24.000000000h]), which is not
       itself a valid clock face time.  However, since valid clock face times readily
       round up to it, it's better to allow it to be represented. *)
    if Span.(<) s start_of_day || Span.(>) s end_of_day
    then input_out_of_bounds s
    else s

  let add_exn t span = of_span_since_start_of_day_exn (Span.(+) t span)
  let sub_exn t span = of_span_since_start_of_day_exn (Span.(-) t span)

  let diff t u = Span.(-) t u

  let midnight date ~zone = of_date_ofday ~zone date Time.Ofday.start_of_day

  let of_ofday core = Span.of_span (Time.Ofday.to_span_since_start_of_day core)

  let create ?hr ?min ?sec ?ms ?us () =
    of_ofday (Time.Ofday.create ?hr ?min ?sec ?ms ?us ())
  ;;

  let of_time =
    let module Cache = struct
      type t =
        { mutable zone          : Zone.t Lazy.t
        ; mutable midnight      : Time_ns.t
        ; mutable next_midnight : Time_ns.t
        }
    end in
    let cache : Cache.t =
      { zone = Zone.local; midnight = epoch; next_midnight = epoch }
    in
    fun time ~zone ->
      (* Zones are strings.  You have to cache-validate them physically. *)
      if phys_equal (Lazy.force cache.zone) zone
      && time >= cache.midnight
      && time < cache.next_midnight
      then Time_ns.diff time cache.midnight
      else
        let time', ns     = split_into_sec_and_ns (to_int63_ns_since_epoch time) in
        let time'         = of_int63_ns_since_epoch time' in
        let date, ofday   = Time.to_date_ofday (to_time time')        ~zone in
        let next_midnight = midnight           (Date.add_days date 1) ~zone in
        let midnight      = midnight           date                   ~zone in
        (* Use one code path uniformly on non-DST-transition days, and a different one on
           DST-transition days (of_ofday). *)
        if Span.(=) (Time_ns.diff next_midnight midnight) Span.day then
          begin
            cache.zone          <- lazy zone;
            cache.midnight      <- midnight;
            cache.next_midnight <- next_midnight;
            Time_ns.diff time cache.midnight
          end
        else
          begin
            Span.(+) (of_ofday ofday) ns
          end
  ;;
  let%bench_module "of_time" =
    (module struct
      let zones = !Zone.likely_machine_zones

      let%bench_fun "now" [@indexed i = List.range 0 (List.length zones)] =
        let zone = Zone.find_exn (List.nth_exn zones i) in
        let time = now () in
        fun () -> of_time time ~zone

      let%bench_fun "random" =
        let time = random () in
        let zone = Zone.find_exn List.(hd_exn (permute !Zone.likely_machine_zones)) in
        fun () -> of_time time ~zone
    end)
  ;;

  let of_local_time time = of_time time ~zone:(Lazy.force Zone.local)

  let now ~zone = of_time (now ()) ~zone

  let local_now () = now ~zone:(force Time.Zone.local)

  let to_string t =
    if Span.(<=) start_of_day t && Span.(<) t end_of_day then
      let ns = Span.to_int63_ns t in
      let s = Span.to_int_sec t in
      let m = s / 60 in
      let h = m / 60 in
      sprintf "%02d:%02d:%02d.%09d"
        h
        (m mod 60)
        (s mod 60)
        Int63.(to_int_exn (rem ns Span.(to_int63_ns second)))
    else "Incorrect day"
  ;;

  let to_millisecond_string t =
    if Span.(<=) start_of_day t && Span.(<) t end_of_day then
      let ms = Int63.(Span.to_int63_ns t / of_int 1_000_000) in
      let s = Int63.(ms / of_int 1000) in
      let m = Int63.(s / of_int 60) in
      let h = Int63.(m / of_int 60) in
      sprintf "%02d:%02d:%02d.%03d"
        Int63.(to_int_exn h)
        Int63.(to_int_exn (rem m (of_int 60)))
        Int63.(to_int_exn (rem s (of_int 60)))
        Int63.(to_int_exn (rem ms (of_int 1000)))
    else "Incorrect day"
  ;;

  let to_ofday t = Time.Ofday.of_span_since_start_of_day (Span.to_span t)

  let of_string s = of_ofday (Time.Ofday.of_string s)

  module Stable = struct
    module V1 = struct
      module T = struct
        type nonrec t = t [@@deriving compare, bin_io]

        let t_of_sexp s : t = of_ofday (Time.Ofday.Stable.V1.t_of_sexp s)
        let sexp_of_t (t : t) = Time.Ofday.Stable.V1.sexp_of_t (to_ofday t)

        let to_int63     t = Span.Stable.V1.to_int63     t
        let of_int63_exn t = Span.Stable.V1.of_int63_exn t
      end
      include T
      include Comparator.Stable.V1.Make (T)
    end
  end

  let sexp_of_t = Stable.V1.sexp_of_t
  let t_of_sexp = Stable.V1.t_of_sexp

  include Identifiable.Make (struct
      type nonrec t = t [@@deriving sexp, compare, bin_io]
      let module_name = "Core.Time_ns.Ofday"
      let hash = Span.hash
      let of_string, to_string = of_string, to_string
    end)

  module Option = struct
    type ofday = t [@@deriving sexp, compare]
    type t = Span.Option.t [@@deriving bin_io, compare, hash, typerep]

    let none            = Span.Option.none
    let some            = Span.Option.some
    let is_none         = Span.Option.is_none
    let is_some         = Span.Option.is_some
    let value           = Span.Option.value
    let value_exn       = Span.Option.value_exn
    let unchecked_value = Span.Option.unchecked_value

    let of_option = function None -> none | Some t -> some t
    let to_option t = if is_none t then None else Some (value_exn t)

    module Optional_syntax = struct
      module Optional_syntax = struct
        let is_none         = is_none
        let unchecked_value = unchecked_value
      end
    end

    module Stable = struct
      module V1 = struct
        module T = struct
          type nonrec t = t [@@deriving compare, bin_io]

          let sexp_of_t t = [%sexp_of: Stable.V1.t option] (to_option t)
          let t_of_sexp s = of_option ([%of_sexp: Stable.V1.t option] s)

          let to_int63     t = Span.Option.Stable.V1.to_int63     t
          let of_int63_exn t = Span.Option.Stable.V1.of_int63_exn t
        end
        include T
        include Comparator.Stable.V1.Make (T)
      end
    end

    let sexp_of_t = Stable.V1.sexp_of_t
    let t_of_sexp = Stable.V1.t_of_sexp

    include Identifiable.Make (struct
        type nonrec t = t [@@deriving sexp, compare, bin_io]
        let module_name = "Core.Time_ns.Ofday.Option"
        let hash = Span.Option.hash
        include Sexpable.To_stringable (struct type nonrec t = t [@@deriving sexp] end)
      end)
  end
end

let%bench_module "Ofday" =
  (module struct
    let%bench_fun "of_local_time random" =
      let time = random () in
      fun () -> Ofday.of_local_time time

    let%bench_fun "of_span_since_start_of_day_exn random" =
      let random_span = Random.float Span.(to_ns day) |> Span.of_ns in
      fun () -> Ofday.of_span_since_start_of_day_exn random_span
  end)

let to_ofday t ~zone = Ofday.of_time t ~zone

let to_date_ofday t ~zone = (to_date ~zone t, to_ofday t ~zone)

let split_into_sec_and_ns ~ofday =
  let ofday = Ofday.to_span_since_start_of_day ofday |> Span.to_int63_ns in
  let ofday, span = split_into_sec_and_ns ofday in
  let ofday = Span.of_int63_ns ofday |> Ofday.of_span_since_start_of_day_exn in
  ofday, span
;;

let of_date_ofday ~zone date ofday =
  let ofday, span = split_into_sec_and_ns ~ofday in
  Time_ns.add (of_date_ofday ~zone date (Ofday.to_ofday ofday)) span
;;

(* This may round rather than truncate, since [to_time] rounds. *)
let to_sec_string t ~zone = Time.to_sec_string (to_time t) ~zone

(* [{to,of}_filename_string] just call the functions from [Time]. It doesn't seem worth
   duplicating the code until people actually want sub-millisecond precision in filenames.
*)
let to_filename_string t ~zone =
  Time.to_filename_string (to_time t) ~zone
;;

let of_filename_string time_str ~zone =
  of_time (Time.of_filename_string time_str ~zone)
;;

let occurrence what t ~ofday ~zone =
  let ofday, span = split_into_sec_and_ns ~ofday in
  Time_ns.add
    (of_time (Time.occurrence what (to_time t) ~ofday:(Ofday.to_ofday ofday) ~zone))
    span
;;

module Stable = struct
  module Option = Option.Stable
  module Span = struct
    include Span.Stable
    module Option = Span.Option.Stable
  end
  module Ofday = struct
    include Ofday.Stable
    module Option = Ofday.Option.Stable
  end
  include Stable0
end

(*
   Dropping Time in favor of Time_ns is possible and has been discussed, but we have
   chosen not to do so at this time for a few reasons:

  - It's a lot of work.  All functions over Time, including the related
    modules Date, Ofday, Zone, Span, Schedule have to be converted to Time_ns
    space.  This is largely mechanical, but will create a lot of churn within
    the modules and possibly externally where the floatiness of the Time world
    leaks out.

  - It's of limited utility compared to other things we could be working on.
    Time math would be easier to understand and somewhat faster, but very few
    modules/programs would benefit from faster time math.  Those that do can
    use Time_ns already for the most part.

  - Having Time_ns and a conversion function already gives the bulk of the
    value to programs that want a fast, non-allocating version of [Time.now].
    Indeed, many remaining unconverted functions

  - We aren't certain about how the boundaries around Time_ns will affect the
    external viability of Core.  Internally we don't think being limited to
    a smaller time range is an issue, and really far off times are better
    represented as (Date.t * Ofday.t), but it is still a restriction.  This
    pushback is probably minimal and, if we could get over the work concerns,
    could be eliminated.

   - Converting between Time and Time_ns when you use libraries based on different ones
   isn't so bad. (?)
*)
