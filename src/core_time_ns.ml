open! Import
open! Int.Replace_polymorphic_compare
open  Import_time

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

  let [@inline never] invalid_range t =
    let open Time_ns.Span in
    raise_s [%message
      "Span.t exceeds limits"
        (t         : t)
        (min_value : t)
        (max_value : t)]
  ;;

  let check_range t =
    let open Time_ns.Span in
    if t < min_value || t > max_value
    then invalid_range t
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

  module Stable = struct
    module V1 = struct
      module T = struct
        type nonrec t = Time_ns.Span.t [@@deriving bin_io, compare]

        let sexp_of_t t = Time.Stable.Span.V1.sexp_of_t (to_span t)
        let t_of_sexp s = of_span (Time.Stable.Span.V1.t_of_sexp s)

        let of_int63_exn t = check_range (Time_ns.Span.of_int63_ns t)
        let to_int63     t = Time_ns.Span.to_int63_ns t
      end
      include T
      include Comparator.Stable.V1.Make (T)
    end

    module V2 = Time_ns.Stable.Span.V2
  end

  module T = struct
    include Time_ns.Span

    let hash t = Int63.hash (Time_ns.Span.to_int63_ns t)
    let compare = compare
  end
  include T

  module Option = struct
    type span = t [@@deriving sexp]
    type t = Int63.t [@@deriving bin_io, compare, hash, typerep] (* nanoseconds or none *)
    let none = Int63.min_value
    let is_none t = Int63.(t = none)
    let is_some t = Int63.(t <> none)
    let some_is_representable span = is_some (to_int63_ns span)

    let some span =
      if some_is_representable span
      then to_int63_ns span
      else raise_s [%message [%here] "Span.Option.some value not representable"]

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
        let unsafe_value = unchecked_value
      end
    end

    module Stable = struct
      module V1 = struct
        module T = struct
          type nonrec t = t [@@deriving compare, bin_io]

          let v1_some span = to_int63_ns (check_range span)

          let sexp_of_t t = [%sexp_of: Stable.V1.t option] (to_option t)
          let t_of_sexp s = of_option ([%of_sexp: Stable.V1.t option] s)

          let of_int63_exn i = if is_none i then none else v1_some (of_int63_ns i)
          let to_int63     t = t
        end
        include T
        include Comparator.Stable.V1.Make (T)
      end

      module V2 = struct
        module T = struct
          type nonrec t = t [@@deriving compare, bin_io]

          let sexp_of_t t = Sexp.List (if is_none t then []
                                       else [Stable.V2.sexp_of_t (unchecked_value t)])

          let t_of_sexp sexp =
            let fail () =
              of_sexp_error "Time_ns.Span.Option.Stable.V2.t_of_sexp: sexp must be a List of 0-1 Atom" sexp
            in
            match sexp with
            | Sexp.Atom _    -> fail ()
            | Sexp.List list ->
              match list with
              | []            -> none
              | [Sexp.Atom x] ->
                some (try of_string x
                      with exn -> of_sexp_error (Exn.to_string exn) sexp)
              | _             -> fail ()

          let of_int63_exn i = i
          let to_int63     t = t
        end
        include T
        include Comparator.Stable.V1.Make (T)
      end
    end

    let sexp_of_t = Stable.V2.sexp_of_t
    let t_of_sexp = Stable.V2.t_of_sexp

    include Identifiable.Make (struct
        type nonrec t = t [@@deriving sexp, compare, bin_io, hash]
        let module_name = "Core.Time_ns.Span.Option"
        include Sexpable.To_stringable (struct type nonrec t = t [@@deriving sexp] end)
      end)
    include (Int63 : Core_kernel.Comparisons.S with type t := t)
  end
end

include (Time_ns : module type of struct include Time_ns end
         with module Span   := Time_ns.Span
          and module Ofday  := Time_ns.Ofday
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
  let some_is_representable time =
    Span.Option.some_is_representable (to_span_since_epoch time)
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
      let unsafe_value = unchecked_value
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
      type nonrec t = t [@@deriving sexp, compare, bin_io, hash]
      let module_name = "Core.Time_ns.Option"
      include Sexpable.To_stringable (struct type nonrec t = t [@@deriving sexp] end)
    end)
  (* bring back the efficient implementation of comparison operators *)
  include (Span.Option : Core_kernel.Comparisons.S with type t := t)
end

(* Note: This is FIX standard millisecond precision. You should use
   [Zero.Time_ns_with_fast_accurate_to_of_string] if you need nanosecond precision. *)
let to_string_fix_proto zone t = Time.to_string_fix_proto zone (to_time t)
let of_string_fix_proto zone s = of_time (Time.of_string_fix_proto zone s)

include Identifiable.Make (struct
    type nonrec t = t [@@deriving bin_io, compare, hash, sexp]
    let module_name = "Core.Time_ns"
    let of_string, to_string = of_string, to_string
  end)
(* bring back the efficient implementation of comparison operators *)
include (Core_kernel.Time_ns : Core_kernel.Comparisons.S with type t := t)

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
  include Time_ns.Ofday

  let midnight date ~zone = of_date_ofday ~zone date Time.Ofday.start_of_day

  let of_ofday core =
    of_span_since_start_of_day_exn
      (Span.of_span (Time.Ofday.to_span_since_start_of_day core))

  let of_time =
    let module Cache = struct
      type t =
        { mutable zone          : Time.Zone.t Lazy.t
        ; mutable midnight      : Time_ns.t
        ; mutable next_midnight : Time_ns.t
        }
    end in
    let cache : Cache.t =
      { zone = Time.Zone.local; midnight = Time_ns.epoch; next_midnight = Time_ns.epoch }
    in
    fun time ~zone ->
      let span =
        (* Zones are strings.  You have to cache-validate them physically. *)
        if phys_equal (Lazy.force cache.zone) zone
        && Time_ns.( >= ) time cache.midnight
        && Time_ns.( <  ) time cache.next_midnight
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
              Span.(+) (to_span_since_start_of_day (of_ofday ofday)) ns
            end
      in
      of_span_since_start_of_day_exn span
  ;;

  let to_ofday t =
    Time.Ofday.of_span_since_start_of_day_exn (Span.to_span (to_span_since_start_of_day t))

  let now ~zone = of_time (now ()) ~zone

  module Option = struct
    type ofday = t [@@deriving sexp, compare]
    type t = Span.Option.t [@@deriving bin_io, compare, hash, typerep]

    let none = Span.Option.none

    let some t = Span.Option.some (to_span_since_start_of_day t)

    let is_none = Span.Option.is_none
    let is_some = Span.Option.is_some

    let some_is_representable t =
      Span.Option.some_is_representable (to_span_since_start_of_day t)

    let value t ~default =
      match is_some t with
      | true  -> of_span_since_start_of_day_exn (Span.Option.unchecked_value t)
      | false -> default

    let value_exn t = of_span_since_start_of_day_exn (Span.Option.value_exn t)

    let unchecked_value t = of_span_since_start_of_day_exn (Span.Option.unchecked_value t)

    let of_option = function None -> none | Some t -> some t
    let to_option t = if is_none t then None else Some (value_exn t)

    module Optional_syntax = struct
      module Optional_syntax = struct
        let is_none         = is_none
        let unsafe_value = unchecked_value
      end
    end

    module Stable = struct
      module V1 = struct
        module T = struct
          type nonrec t = t [@@deriving compare, bin_io]

          let sexp_of_t t = [%sexp_of: Time_ns.Stable.Ofday.V1.t option] (to_option t)

          let t_of_sexp s = of_option ([%of_sexp: Time_ns.Stable.Ofday.V1.t option] s)

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
        type nonrec t = t [@@deriving sexp, compare, bin_io, hash]
        let module_name = "Core.Time_ns.Ofday.Option"
        include Sexpable.To_stringable (struct type nonrec t = t [@@deriving sexp] end)
      end)

    include (Span.Option : Core_kernel.Comparisons.S with type t := t)
  end
end

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
    include Time_ns.Stable.Ofday
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
