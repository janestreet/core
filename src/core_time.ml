(**

   Outside of Core Time appears to be a single module with a number of submodules:

   - Time
   - Span
   - Ofday
   - Zone

   The reality under the covers isn't as simple for a three reasons:

   - We want as much Time functionality available to Core_kernel as possible, and
     Core_kernel modules shouldn't rely on Unix functions.  Some functions in Time
     require Unix, which creates one split.

   - We want some functionality to be functorized so that code can be shared
     between Time and Time_ns.

   - Time has internal circular dependencies.  For instance, Ofday.now relies on
     Time.now, but Time also wants to expose Time.to_date_ofday, which relies on Ofday.
     We use a stack of modules to break the cycle.

   This leads to the following modules within Core_kernel and Core:

   Core_kernel.Span  - the core type of span
   Core_kernel.Ofday - the core type of ofday, which is really a constrained span
   Core_kernel.Date  - the core type of date
   Core_kernel.Zone  - the base functor for creating a Zone type
   Core_kernel.Time_float0 - contains the base Time.t type and lays out the basic
   relationship between Time, Span, Ofday, and Zone
   Core_kernel.Time_float  - ties Time, Span, Ofday, Zone, and Date together and provides
   the higher level functions for them that don't rely on Unix
   Core_kernel.Time    - re-exposes Time_float

   Core.Zone_cache   - implements a caching layer between the Unix filesystem and Zones
   Core.Core_date    - adds the Unix dependent functions to Date
   Core.Core_time    - adds the Unix dependent functions to Time

   Core          - renames the Core_{base} modules to {base} for ease of access in
   modules outside of Core
*)

open! Import
open Core_kernel.Core_kernel_private
open! Int.Replace_polymorphic_compare

module Sys = Core_sys

include Core_time_intf

module Make (Time0 : Time0_intf.S) (Time : Time_intf.S with module Time := Time0) = struct

  module Zone = struct
    include Time.Zone

    include (Core_zone : Core_zone.Extend_zone with type t := t)
  end

  module Ofday = struct
    include Time.Ofday

    let now ~zone = Time.to_ofday ~zone (Time.now ())

    module Zoned = struct
      type t =
        { ofday : Time.Ofday.t;
          zone  : Zone.t;
        }
      [@@deriving bin_io, fields, compare, hash]

      type sexp_repr = Time.Ofday.t * Zone.t
      [@@deriving sexp]

      let sexp_of_t t = [%sexp_of: sexp_repr] (t.ofday, t.zone)

      let t_of_sexp sexp =
        let (ofday, zone) = [%of_sexp: sexp_repr] sexp in
        { ofday; zone; }
      ;;
      let to_time t date = Time.of_date_ofday ~zone:(zone t) date (ofday t)

      let create ofday zone = { ofday; zone }

      let create_local ofday = create ofday (Lazy.force Zone.local)

      let of_string string : t =
        match String.split string ~on:' ' with
        | [ ofday; zone ] ->
          { ofday = Time.Ofday.of_string ofday;
            zone  = Zone.of_string  zone;
          }
        | _ ->
          failwithf "Ofday.Zoned.of_string %s" string ()
      ;;

      let to_string (t : t) : string =
        String.concat [
          Time.Ofday.to_string t.ofday;
          " ";
          Zone.to_string t.zone ]
      ;;

      module With_nonchronological_compare = struct
        type nonrec t = t
        [@@deriving bin_io, compare, sexp, hash]
      end

      include Pretty_printer.Register (struct
          type nonrec t = t
          let to_string = to_string
          let module_name = "Core.Time.Ofday.Zoned"
        end)
    end
  end

  include (Time : module type of Time
           with module Zone  := Zone
            and module Ofday := Ofday)

  let of_tm tm ~zone =
    (* Explicitly ignoring isdst, wday, yday (they are redundant with the other fields
       and the [zone] argument) *)
    let
      { Core_unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec
      ; tm_isdst = _; tm_wday = _; tm_yday = _ } = tm
    in
    let date =
      Date.create_exn
        ~y:(tm_year + 1900)
        ~m:(Month.of_int_exn (tm_mon + 1))
        ~d:tm_mday
    in
    let ofday = Ofday.create ~hr:tm_hour ~min:tm_min ~sec:tm_sec () in
    of_date_ofday ~zone date ofday
  ;;

  let of_string_fix_proto utc str =
    try
      let expect_length = 21 in  (* = 8 + 1 + 12 *)
      let expect_dash = 8 in
      if Char.(<>) str.[expect_dash] '-' then
        failwithf "no dash in position %d" expect_dash ();
      let zone =
        match utc with
        | `Utc   -> Zone.utc
        | `Local -> Lazy.force Zone.local
      in
      if Int.(>) (String.length str) expect_length then
        failwithf "input too long" ();
      of_date_ofday ~zone
        (Date.of_string_iso8601_basic str ~pos:0)
        (Ofday.of_string_iso8601_extended str ~pos:(expect_dash + 1))
    with exn ->
      invalid_argf "Time.of_string_fix_proto %s: %s" str (Exn.to_string exn) ()
  ;;

  let to_string_fix_proto utc t =
    let zone =
      match utc with
      | `Utc   -> Zone.utc
      | `Local -> Lazy.force Zone.local
    in
    let date, sec = to_date_ofday t ~zone in
    (Date.to_string_iso8601_basic date) ^ "-" ^ (Ofday.to_millisec_string sec)
  ;;

  let format t s ~zone =
    let epoch_time =
      Zone.relative_time_of_absolute_time zone t
      |> Relative_to_unspecified_zone.to_span_since_epoch
      |> Span.to_sec
    in
    Core_unix.strftime (Unix.gmtime epoch_time) s
  ;;

  let parse s ~fmt ~zone =
    Core_unix.strptime ~fmt s
    |> of_tm ~zone
  ;;

  let pause_for span =
    let time_remaining =
      (* If too large a float is passed in (Span.max_value for instance) then
         nanosleep will return immediately, leading to an infinite and expensive
         select loop.  This is handled by pausing for no longer than 100 days.
      *)
      let span = Span.min span (Span.scale Span.day 100.) in
      Core_unix.nanosleep (Span.to_sec span)
    in
    if Float.(>) time_remaining 0.0
    then `Remaining (Span.of_sec time_remaining)
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
    pause (Span.of_day 1.0);
    pause_forever ()
  ;;

  let to_string t = to_string_abs t ~zone:(Lazy.force Zone.local)

  let ensure_colon_in_offset offset =
    if Char.(=) offset.[1] ':'
    || Char.(=) offset.[2] ':'
    then offset
    else begin
      let offset_length = String.length offset in
      if Int.(<) offset_length 3 || Int.(>) offset_length 4
      then failwithf "invalid offset %s" offset ()
      else String.concat
             [ String.slice offset 0 (offset_length - 2)
             ; ":"
             ; String.slice offset (offset_length - 2) offset_length ]
    end
  ;;

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

  include Pretty_printer.Register (struct
      type nonrec t = t
      let to_string = to_string
      let module_name = "Core.Time"
    end)

  let sexp_zone = ref Zone.local
  let get_sexp_zone () = (Lazy.force !sexp_zone)
  let set_sexp_zone zone = sexp_zone := lazy zone

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
    t_of_sexp_gen sexp ~if_no_timezone:(`Use_this_one (Lazy.force !sexp_zone))
  let t_of_sexp_abs sexp =
    t_of_sexp_gen sexp ~if_no_timezone:`Fail

  let sexp_of_t_abs t ~zone =
    Sexp.List (List.map (Time.to_string_abs_parts ~zone t) ~f:(fun s -> Sexp.Atom s))
  ;;

  let sexp_of_t t = sexp_of_t_abs ~zone:(Lazy.force !sexp_zone) t

  module type C = Comparable.Map_and_set_binable
    with type t := t
     and type comparator_witness := comparator_witness

  let make_comparable ?(sexp_of_t = sexp_of_t) ?(t_of_sexp = t_of_sexp) ()
    : (module C) =
    (module struct
      module C = struct
        type nonrec t = t [@@deriving bin_io]

        type nonrec comparator_witness = comparator_witness

        let comparator = comparator

        let sexp_of_t = sexp_of_t

        let t_of_sexp = t_of_sexp
      end

      include C

      module Map = Map.Make_binable_using_comparator (C)
      module Set = Set.Make_binable_using_comparator (C)
    end)

  (* In 108.06a and earlier, times in sexps of Maps and Sets were raw floats.  From
     108.07 through 109.13, the output format remained raw as before, but both the raw
     and pretty format were accepted as input.  From 109.14 on, the output format was
     changed from raw to pretty, while continuing to accept both formats.  Once we
     believe most programs are beyond 109.14, we will switch the input format to no
     longer accept raw. *)
  include (val make_comparable () ~t_of_sexp:(fun sexp ->
    match Option.try_with (fun () ->
      of_span_since_epoch (Span.of_sec (Float.t_of_sexp sexp))) with
    | Some t -> t
    | None -> t_of_sexp sexp
  ))

  let%test _ =
    Set.equal
      (Set.of_list [epoch])
      (Set.t_of_sexp
         (Sexp.List [Float.sexp_of_t (Span.to_sec (to_span_since_epoch epoch))]))
  ;;

  include Hashable.Make_binable (struct
      type nonrec t = t [@@deriving bin_io, compare, hash, sexp]
    end)

  module Exposed_for_tests = struct
    let ensure_colon_in_offset = ensure_colon_in_offset
  end
end
