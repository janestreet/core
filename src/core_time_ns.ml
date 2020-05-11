open! Import
open! Int.Replace_polymorphic_compare
open  Import_time

module Zone = Time.Zone

(* To break the dependency in the public release *)
module Time_ns = Core_kernel.Time_ns

module Span = struct
  open Time_ns.Span

  let arg_type = Core_kernel.Command.Arg_type.create of_string

  module Stable = struct
    module V1 = struct
      module T = struct
        type nonrec t = Time_ns.Span.t [@@deriving bin_io, compare, hash]

        let sexp_of_t t = Time.Stable.Span.V1.sexp_of_t (to_span_float_round_nearest t)
        let t_of_sexp s = of_span_float_round_nearest (Time.Stable.Span.V1.t_of_sexp s)

        let of_int63_exn t = Time_ns.Span.of_int63_ns t
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

    let[@cold] raise_some_error span =
      raise_s [%message [%here] "Span.Option.some value not representable" (span : span)]

    let some span =
      if some_is_representable span
      then to_int63_ns span
      else raise_some_error span

    let value t ~default = if is_none t then default else of_int63_ns t
    let unchecked_value t = of_int63_ns t

    let value_exn t =
      if is_some t
      then unchecked_value t
      else raise_s [%message [%here] "Span.Option.value_exn none"]

    let of_option = function None -> none | Some t -> some t
    let to_option t = if is_none t then None else Some (of_int63_ns t)

    module For_quickcheck = struct
      module Some = struct
        type t = span

        let quickcheck_generator =
          Quickcheck.Generator.filter quickcheck_generator ~f:some_is_representable

        let quickcheck_observer = quickcheck_observer

        let quickcheck_shrinker =
          Base_quickcheck.Shrinker.filter quickcheck_shrinker ~f:some_is_representable
      end

      type t = Some.t option [@@deriving quickcheck]
    end

    let quickcheck_generator =
      Quickcheck.Generator.map For_quickcheck.quickcheck_generator ~f:of_option

    let quickcheck_observer  =
      Quickcheck.Observer.unmap For_quickcheck.quickcheck_observer ~f:to_option

    let quickcheck_shrinker  =
      Quickcheck.Shrinker.map
        For_quickcheck.quickcheck_shrinker
        ~f:of_option
        ~f_inverse:to_option

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

          let v1_some span =
            assert (some_is_representable span);
            to_int63_ns span

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

let to_string t = to_string_abs t ~zone:(Lazy.force Zone.local)

exception Time_string_not_absolute of string [@@deriving sexp]

let of_string_gen ~if_no_timezone s =
  let default_zone () =
    match if_no_timezone with
    | `Fail              -> raise (Time_string_not_absolute s);
    | `Local             -> Lazy.force Zone.local
    | `Use_this_one zone -> zone
  in
  of_string_gen ~default_zone ~find_zone:Zone.find_exn s
;;

let of_string_abs s = of_string_gen ~if_no_timezone:`Fail s
let of_string s     = of_string_gen ~if_no_timezone:`Local s

let arg_type = Core_kernel.Command.Arg_type.create of_string_abs

(* Does not represent extra hours due to DST (daylight saving time) (because DST makes
   adjustments in terms of wall clock time) or leap seconds (which aren't represented in
   Unix linear time).  See {!Ofday}. *)
module Ofday = struct
  include Time_ns.Ofday

  let arg_type = Core_kernel.Command.Arg_type.create of_string

  let of_ofday_float_round_nearest_microsecond core =
    of_span_since_start_of_day_exn
      (Span.of_span_float_round_nearest_microsecond
         (Time.Ofday.to_span_since_start_of_day core))

  let of_ofday_float_round_nearest core =
    of_span_since_start_of_day_exn
      (Span.of_span_float_round_nearest (Time.Ofday.to_span_since_start_of_day core))

  let of_time time ~zone = to_ofday time ~zone

  let to_ofday_float_round_nearest_microsecond t =
    Time.Ofday.of_span_since_start_of_day_exn
      (Span.to_span_float_round_nearest_microsecond (to_span_since_start_of_day t))

  let to_ofday_float_round_nearest t =
    Time.Ofday.of_span_since_start_of_day_exn
      (Span.to_span_float_round_nearest (to_span_since_start_of_day t))

  let now ~zone = of_time (now ()) ~zone

  (* Legacy conversions that round to the nearest microsecond *)
  let to_ofday = to_ofday_float_round_nearest_microsecond
  let of_ofday = of_ofday_float_round_nearest_microsecond

  (* This module is in [Core] instead of [Core_kernel] because to sexp a [Zone.t], we need
     to read a time zone database to work out DST transitions. We do not have a portable
     way to do that, and currently only support the operation on Unix via Core. *)
  module Zoned = struct
    type t =
      { ofday : Time_ns.Ofday.t;
        zone  : Zone.t;
      }
    [@@deriving bin_io, fields, compare, equal, hash]

    type sexp_repr = Time_ns.Ofday.t * Zone.t
    [@@deriving sexp]

    let sexp_of_t t = [%sexp_of: sexp_repr] (t.ofday, t.zone)

    let t_of_sexp sexp =
      let (ofday, zone) = [%of_sexp: sexp_repr] sexp in
      { ofday; zone; }
    ;;

    let to_time_ns t date = of_date_ofday ~zone:(zone t) date (ofday t)

    let create ofday zone = { ofday; zone }

    let create_local ofday = create ofday (Lazy.force Zone.local)

    let of_string string : t =
      match String.split string ~on:' ' with
      | [ ofday; zone ] ->
        { ofday = Time_ns.Ofday.of_string ofday;
          zone  = Zone.of_string  zone;
        }
      | _ ->
        failwithf "Ofday.Zoned.of_string %s" string ()
    ;;

    let to_string (t : t) : string =
      String.concat [
        Time_ns.Ofday.to_string t.ofday;
        " ";
        Zone.to_string t.zone ]
    ;;

    let arg_type = Core_kernel.Command.Arg_type.create of_string

    module With_nonchronological_compare = struct
      type nonrec t = t
      [@@deriving bin_io, compare, equal, sexp, hash]
    end

    include Pretty_printer.Register (struct
        type nonrec t = t
        let to_string = to_string
        let module_name = "Core.Time_ns.Ofday.Zoned"
      end)

    module Stable = struct
      module V1 = struct
        type nonrec t = t [@@deriving hash]
        let compare = With_nonchronological_compare.compare

        module Bin_repr = struct
          type t =
            { ofday : Time_ns.Stable.Ofday.V1.t;
              zone  : Timezone.Stable.V1.t;
            } [@@deriving bin_io]
        end

        include (Binable.Of_binable_without_uuid [@alert "-legacy"]) (Bin_repr) (struct
            type nonrec t = t

            let to_binable t : Bin_repr.t =
              { ofday = ofday t; zone = zone t }

            let of_binable (repr : Bin_repr.t) =
              create repr.ofday repr.zone
          end)

        type sexp_repr = Time_ns.Stable.Ofday.V1.t * Timezone.Stable.V1.t
        [@@deriving sexp]

        let sexp_of_t t = [%sexp_of: sexp_repr] (ofday t, zone t)

        let t_of_sexp sexp =
          let (ofday, zone) = [%of_sexp: sexp_repr] sexp in
          create ofday zone
        ;;
      end
    end
  end

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
      | true  -> of_span_since_start_of_day_unchecked (Span.Option.unchecked_value t)
      | false -> default

    let of_span_since_start_of_day span =
      if span_since_start_of_day_is_valid span
      then Span.Option.some span
      else none

    let value_exn t = of_span_since_start_of_day_unchecked (Span.Option.value_exn t)

    let unchecked_value t =
      of_span_since_start_of_day_unchecked (Span.Option.unchecked_value t)

    let of_option = function None -> none | Some t -> some t
    let to_option t = if is_none t then None else Some (value_exn t)


    (* Can't use the quickcheck generator and shrinker inherited from [Span.Option]
       because they may produce spans whose representation is larger than
       [start_of_next_day] *)
    let quickcheck_generator : t Quickcheck.Generator.t =
      Quickcheck.Generator.map ~f:of_option
        (Option.quickcheck_generator
           (Quickcheck.Generator.filter
              ~f:some_is_representable
              Time_ns.Ofday.quickcheck_generator))
    ;;

    let quickcheck_shrinker : t Quickcheck.Shrinker.t =
      Quickcheck.Shrinker.map ~f:of_option ~f_inverse:to_option
        (Option.quickcheck_shrinker
           (Base_quickcheck.Shrinker.filter
              ~f:some_is_representable
              Time_ns.Ofday.quickcheck_shrinker))
    ;;

    let quickcheck_observer = Span.Option.quickcheck_observer

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

let get_sexp_zone = Time.get_sexp_zone
let set_sexp_zone = Time.set_sexp_zone

let t_of_sexp_gen ~if_no_timezone sexp =
  try
    match sexp with
    | Sexp.List [Sexp.Atom date; Sexp.Atom ofday; Sexp.Atom tz] ->
      of_date_ofday ~zone:(Zone.find_exn tz)
        (Date.of_string date) (Ofday.of_string ofday)
    (* This is actually where the output of [sexp_of_t] is handled, since that's e.g.
       (2015-07-06 09:09:44.787988+01:00). *)
    | Sexp.List [Sexp.Atom date; Sexp.Atom ofday_and_possibly_zone] ->
      of_string_gen ~if_no_timezone (date ^ " " ^ ofday_and_possibly_zone)
    | Sexp.Atom datetime ->
      of_string_gen ~if_no_timezone datetime
    | _ -> of_sexp_error "Time.t_of_sexp" sexp
  with
  | Of_sexp_error _ as e -> raise e
  | e -> of_sexp_error (sprintf "Time.t_of_sexp: %s" (Exn.to_string e)) sexp
;;

let t_of_sexp sexp =
  t_of_sexp_gen sexp ~if_no_timezone:(`Use_this_one (get_sexp_zone ()))
let t_of_sexp_abs sexp =
  t_of_sexp_gen sexp ~if_no_timezone:`Fail

let sexp_of_t_abs t ~zone =
  Sexp.List (List.map (Time_ns.to_string_abs_parts ~zone t) ~f:(fun s -> Sexp.Atom s))
;;

let sexp_of_t t = sexp_of_t_abs ~zone:(get_sexp_zone ()) t

let of_date_ofday_zoned date ofday_zoned =
  Ofday.Zoned.to_time_ns ofday_zoned date

let to_date_ofday_zoned t ~zone =
  let (date,ofday) = to_date_ofday t ~zone in
  (date, Ofday.Zoned.create ofday zone)

let to_ofday_zoned t ~zone =
  let ofday = to_ofday t ~zone in
  Ofday.Zoned.create ofday zone

module Stable0 = struct
  module V1 = struct
    module T0 = struct
      (* We use the unstable sexp here, and rely on comprehensive tests of the stable
         conversion to make sure we don't change it. *)
      type nonrec t = t [@@deriving bin_io, compare, sexp, hash]

      let of_int63_exn t = of_span_since_epoch (Span.of_int63_ns t)
      let to_int63 t = to_int63_ns_since_epoch t
    end
    module T = struct
      include T0
      module Comparator = Comparator.Stable.V1.Make (T0)
      include Comparator
    end
    include T
    include Comparable.Stable.V1.Make (T)
  end
end
include Stable0.V1.Comparator

module Option = struct
  type time = t [@@deriving sexp, compare]

  type t = Span.Option.t [@@deriving bin_io, compare, hash, typerep, quickcheck]

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
let to_string_fix_proto zone t =
  Time.to_string_fix_proto zone (to_time_float_round_nearest_microsecond t)
let of_string_fix_proto zone s =
  of_time_float_round_nearest_microsecond (Time.of_string_fix_proto zone s)

include Identifiable.Make_using_comparator (struct
    include Stable0.V1
    let module_name = "Core.Time_ns"
    let of_string, to_string = of_string, to_string
  end)
(* bring back the efficient implementation of comparison operators *)
include (Core_kernel.Time_ns : Core_kernel.Comparisons.S with type t := t)

module Stable = struct
  module Option = Option.Stable
  module Span = struct
    include Span.Stable
    module Option = Span.Option.Stable
  end
  module Ofday = struct
    include Time_ns.Stable.Ofday

    module Zoned = Ofday.Zoned.Stable
    module Option = Ofday.Option.Stable
  end
  include Stable0
  module Alternate_sexp = Core_kernel.Time_ns.Stable.Alternate_sexp

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
