(* Functions for parsing time zone database files (zic files).

   A time zone file consists (conceptually - the representation is more
   compact) of an ordered list of (Time.t * [local_time_type]) that mark
   the boundaries (marked from the epoch) at which various time adjustment
   regimes are in effect.  This can also be thought of as breaking down all
   time past the epoch into ranges with a [local_time_type] that describes the
   offset from GMT to apply to each range to get local time.
*)

open Import
open Std_internal
open! Int.Replace_polymorphic_compare
include Zone_intf

exception Invalid_file_format of string [@@deriving sexp]

module Stable = struct
  module Full_data = struct
    module V1 = struct
      module Index = struct
        type t = int

        let next = Int.succ
        let prev = Int.pred
        let before_first_transition = -1

        (* Some existing clients expect [index >= 0], so we never serialize a negative
           index. This conversion can be removed if new stable versions are minted. *)
        let to_external t = max 0 t

        (* When the index of a time zone with no transitions is converted via to_external,
           its value becomes 0 even though its transition array is empty (and it should
           have been -1). When the converted value is changed back to a Zone.t through
           of_external, returning this value for its index could result in unsafe array
           accesses to the transition array of the zone (since there is no transition at
           index 0). Also, it does not make sense to keep the converted index because it
           is intended to be a mutable value used for caching. So of_external always sets
           the index to -1, which is a safe value. *)
        let of_external (_ : t) = -1

        include
          Binable.Of_binable_without_uuid [@alert "-legacy"]
            (Int.Stable.V1)
            (struct
              type t = int

              let to_binable = to_external
              let of_binable = of_external
            end)

        let stable_witness =
          Stable_witness.of_serializable
            Int.Stable.V1.stable_witness
            of_external
            to_external
        ;;

        include
          Sexpable.Of_sexpable
            (Int)
            (struct
              type t = int

              let to_sexpable = to_external
              let of_sexpable = of_external
            end)
      end

      module Regime = struct
        type t = Timezone_types.Regime.t =
          { utc_offset_in_seconds : Int63.Stable.V1.t
          ; is_dst : bool
          ; abbrv : string
          }
        [@@deriving bin_io, sexp, stable_witness]
      end

      (* holds information about when leap seconds should be applied - unused
         because we are translating based on a epoch system clock (see the Core_zone
         documentation). *)
      module Leap_second = struct
        type t =
          { time_in_seconds_since_epoch : Int63.Stable.V1.t
          ; seconds : int
          }
        [@@deriving bin_io, sexp, stable_witness]
      end

      module Transition = struct
        type t = Timezone_types.Transition.t =
          { start_time_in_seconds_since_epoch : Int63.Stable.V1.t
          ; new_regime : Regime.t
          }
        [@@deriving bin_io, sexp, stable_witness]
      end

      type t =
        { name : string
        ; original_filename : string option
        ; digest : Md5.As_binary_string.Stable.V1.t option
        ; transitions : Transition.t array
        ; (* caches the index of the last transition we used to make lookups faster *)
          mutable last_regime_index : Index.t
        ; default_local_time_type : Regime.t
        ; leap_seconds : Leap_second.t list
        }
      [@@deriving bin_io, sexp, stable_witness]

      (* this relies on zones with the same name having the same transitions *)
      let compare t1 t2 = String.compare t1.name t2.name
      let original_filename zone = zone.original_filename
      let digest zone = zone.digest

      module Zone_file : sig
        val input_tz_file : zonename:string -> filename:string -> t
      end = struct
        let bool_of_int i = i <> 0

        let input_long_as_int32 =
          let long = Bytes.create 4 in
          let int32_of_char chr = Int32.of_int_exn (int_of_char chr) in
          fun ic ->
            In_channel.really_input_exn ic ~buf:long ~pos:0 ~len:4;
            let sb1 = Int32.shift_left (int32_of_char (Bytes.get long 0)) 24 in
            let sb2 = Int32.shift_left (int32_of_char (Bytes.get long 1)) 16 in
            let sb3 = Int32.shift_left (int32_of_char (Bytes.get long 2)) 8 in
            let sb4 = int32_of_char (Bytes.get long 3) in
            Int32.bit_or (Int32.bit_or sb1 sb2) (Int32.bit_or sb3 sb4)
        ;;

        (* Note that this is only safe to use on numbers that will fit into a 31-bit
           int. UNIX timestamps won't, for example.  In our case this is only used
           to hold small numbers that are never interpreted as timestamps. *)
        let input_long_as_int ic = Int32.to_int_exn (input_long_as_int32 ic)
        let input_long_as_int63 ic = Int63.of_int32 (input_long_as_int32 ic)

        let input_long_long_as_int63 ic =
          let int63_of_char chr = Int63.of_int_exn (int_of_char chr) in
          let shift c bits = Int63.shift_left (int63_of_char c) bits in
          let long_long = Bytes.create 8 in
          In_channel.really_input_exn ic ~buf:long_long ~pos:0 ~len:8;
          let result = shift (Bytes.get long_long 0) 56 in
          let result = Int63.bit_or result (shift (Bytes.get long_long 1) 48) in
          let result = Int63.bit_or result (shift (Bytes.get long_long 2) 40) in
          let result = Int63.bit_or result (shift (Bytes.get long_long 3) 32) in
          let result = Int63.bit_or result (shift (Bytes.get long_long 4) 24) in
          let result = Int63.bit_or result (shift (Bytes.get long_long 5) 16) in
          let result = Int63.bit_or result (shift (Bytes.get long_long 6) 8) in
          let result = Int63.bit_or result (int63_of_char (Bytes.get long_long 7)) in
          result
        ;;

        let input_list ic ~len ~f =
          let rec loop c lst =
            if c > 0 then loop (c - 1) (f ic :: lst) else List.rev lst
          in
          loop len []
        ;;

        let input_array ic ~len ~f = Array.of_list (input_list ic ~len ~f)

        let input_regime ic =
          let utc_offset_in_seconds = input_long_as_int63 ic in
          let is_dst = bool_of_int (Option.value_exn (In_channel.input_byte ic)) in
          let abbrv_index = Option.value_exn (In_channel.input_byte ic) in
          let lt abbrv = { Regime.utc_offset_in_seconds; is_dst; abbrv } in
          lt, abbrv_index
        ;;

        let input_abbreviations ic ~len =
          let raw_abbrvs =
            input_list ic ~len ~f:(fun ic -> Option.value_exn (In_channel.input_char ic))
          in
          let buf = Buffer.create len in
          let _, indexed_abbrvs =
            List.fold raw_abbrvs ~init:(0, Map.Poly.empty) ~f:(fun (index, abbrvs) c ->
              match c with
              | '\000' ->
                let data = Buffer.contents buf in
                let next_index = index + String.length data + 1 in
                let abbrvs = Map.set abbrvs ~key:index ~data in
                Buffer.clear buf;
                next_index, abbrvs
              | c ->
                Buffer.add_char buf c;
                index, abbrvs)
          in
          if Buffer.length buf <> 0
          then
            raise
              (Invalid_file_format
                 "missing \000 terminating character in input_abbreviations");
          indexed_abbrvs
        ;;

        let input_tz_file_gen ~input_transition ~input_leap_second ic =
          let utc_local_count = input_long_as_int ic in
          let std_wall_count = input_long_as_int ic in
          let leap_count = input_long_as_int ic in
          let transition_count = input_long_as_int ic in
          let type_count = input_long_as_int ic in
          let abbrv_char_count = input_long_as_int ic in
          let transition_times =
            input_list ic ~f:input_transition ~len:transition_count
          in
          let transition_indices =
            input_list
              ic
              ~f:(fun ic -> Option.value_exn (In_channel.input_byte ic))
              ~len:transition_count
          in
          let regimes = input_list ic ~f:input_regime ~len:type_count in
          let abbreviations = input_abbreviations ic ~len:abbrv_char_count in
          let leap_seconds = input_list ic ~f:input_leap_second ~len:leap_count in
          (* The following two arrays indicate two boolean values per regime that
             represent a three-value type that would translate to:

             type transition_type = UTC | Standard | Wall_clock

             However, these are only used by the system library when handling the case where the
             TZ variable is set, not to a time zone name, but instead is of the form:

             TZ = "std offset dst offset, rule"

             Which is deeply obscure, and almost certainly a mistake to use.  This library makes
             no pretense about handling this case.  We continue to read them in for
             completeness, and because it's possible that we will later discover a case where
             they are used. *)
          let _std_wall_indicators =
            input_array ic ~len:std_wall_count ~f:(fun ic ->
              bool_of_int (Option.value_exn (In_channel.input_byte ic)))
          in
          let _utc_local_indicators =
            input_array ic ~len:utc_local_count ~f:(fun ic ->
              bool_of_int (Option.value_exn (In_channel.input_byte ic)))
          in
          let regimes =
            Array.of_list
              (List.map regimes ~f:(fun (lt, abbrv_index) ->
                 let abbrv = Map.find_exn abbreviations abbrv_index in
                 lt abbrv))
          in
          let raw_transitions =
            List.map2_exn transition_times transition_indices ~f:(fun time index ->
              let regime = regimes.(index) in
              time, regime)
          in
          let transitions =
            let rec make_transitions acc l =
              match l with
              | [] -> Array.of_list (List.rev acc)
              | (start_time_in_seconds_since_epoch, new_regime) :: rest ->
                make_transitions
                  ({ Transition.start_time_in_seconds_since_epoch; new_regime } :: acc)
                  rest
            in
            make_transitions [] raw_transitions
          in
          let default_local_time_type =
            match Array.find regimes ~f:(fun r -> not r.Regime.is_dst) with
            | None -> regimes.(0)
            | Some ltt -> ltt
          in
          fun name ~original_filename ~digest ->
            { name
            ; original_filename = Some original_filename
            ; digest = Some digest
            ; transitions
            ; last_regime_index = Index.before_first_transition
            ; default_local_time_type
            ; leap_seconds
            }
        ;;

        let input_leap_second_gen ~input_leap_second ic =
          let time_in_seconds_since_epoch = input_leap_second ic in
          let seconds = input_long_as_int ic in
          { Leap_second.time_in_seconds_since_epoch; seconds }
        ;;

        let read_header ic =
          let magic =
            let buf = Bytes.create 4 in
            In_channel.really_input_exn ic ~buf ~pos:0 ~len:4;
            Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
          in
          if not (String.equal magic "TZif")
          then raise (Invalid_file_format "magic characters TZif not present");
          let version =
            match In_channel.input_char ic with
            | Some '\000' -> `V1
            | Some '2' -> `V2
            | Some '3' -> `V3
            | None -> raise (Invalid_file_format "expected version, found nothing")
            | Some bad_version ->
              raise (Invalid_file_format (sprintf "version (%c) is invalid" bad_version))
          in
          (* space reserved for future use in the format *)
          In_channel.really_input_exn ic ~buf:(Bytes.create 15) ~pos:0 ~len:15;
          version
        ;;

        let input_tz_file_v1 ic =
          let input_leap_second =
            input_leap_second_gen ~input_leap_second:input_long_as_int63
          in
          input_tz_file_gen ~input_transition:input_long_as_int63 ~input_leap_second ic
        ;;

        (*
           version 2 timezone files have the format:

           part 1 - exactly the same as v1

           part 2 - same format as v1, except that 8 bytes are used to store
           transition times and leap seconds

           part 3 - a newline-encloded, POSIX-TZ-environment-variable-style
           string for use in handling instants after the last transition time
           stored in the file (with nothing between the newlines if there is no
           POSIX representation for such instants)

           We handle files in this format by parsing the first part exactly as a v1
           timezone file and then continuing to parse with 64bit reading functions in the
           right places.

           Version 3 timezone files are the same as version 2, except the
           POSIX-TZ-environment-variable-style string in part 3 may use two minor
           extensions to the POSIX TZ format (the hours part of its transition
           times may be signed and range from -167 through 167 instead of the
           POSIX-required unsigned values from 0 through 24; and DST is in effect all
           year if it starts January 1 at 00:00 and ends December 31 at 24:00 plus the
           difference between daylight saving and standard time).

           As we don't actually do anything with part 3 anyway, we can just read v3
           files as v2.
        *)
        let input_tz_file_v2_or_v3 ~version ic =
          let (_ : string -> original_filename:string -> digest:Md5_lib.t -> t) =
            input_tz_file_v1 ic
          in
          (* the header is fully repeated *)
          assert ([%compare.equal: [ `V1 | `V2 | `V3 ]] (read_header ic) version);
          let input_leap_second =
            input_leap_second_gen ~input_leap_second:input_long_long_as_int63
          in
          input_tz_file_gen
            ~input_transition:input_long_long_as_int63
            ~input_leap_second
            ic
        ;;

        let input_tz_file ~zonename ~filename =
          try
            protectx (In_channel.create filename) ~finally:In_channel.close ~f:(fun ic ->
              let make_zone =
                match read_header ic with
                | `V1 -> input_tz_file_v1 ic
                | (`V2 | `V3) as version -> input_tz_file_v2_or_v3 ~version ic
              in
              let digest = Md5.digest_file_blocking filename in
              let r = make_zone zonename ~original_filename:filename ~digest in
              r)
          with
          | Invalid_file_format reason ->
            raise (Invalid_file_format (sprintf "%s - %s" filename reason))
        ;;
      end

      let of_utc_offset_explicit_name ~name ~hours:offset =
        assert (offset >= -24 && offset <= 24);
        let utc_offset_in_seconds = Int63.of_int (offset * 60 * 60) in
        { name
        ; original_filename = None
        ; digest = None
        ; transitions = [||]
        ; last_regime_index = Index.before_first_transition
        ; default_local_time_type =
            { Regime.utc_offset_in_seconds; is_dst = false; abbrv = name }
        ; leap_seconds = []
        }
      ;;

      let of_utc_offset ~hours:offset =
        let name =
          if offset = 0
          then "UTC"
          else sprintf "UTC%s%d" (if offset < 0 then "-" else "+") (abs offset)
        in
        of_utc_offset_explicit_name ~name ~hours:offset
      ;;
    end
  end
end

include Stable.Full_data.V1

let sexp_of_t t = Sexp.Atom t.name

let likely_machine_zones =
  ref [ "America/New_York"; "Europe/London"; "Asia/Hong_Kong"; "America/Chicago" ]
;;

let finalize_js_loaded ~zonename ~filename ~first_transition ~remaining_transitions =
  let transitions = Array.of_list (first_transition :: remaining_transitions) in
  { name = zonename
  ; original_filename = Some filename
  ; digest = None
  ; transitions
  ; last_regime_index = Index.before_first_transition
  ; default_local_time_type = first_transition.Transition.new_regime
  ; leap_seconds = []
  }
;;

let input_tz_file ~zonename ~filename =
  match Timezone_js_loader.load zonename with
  | Error (Disabled | Platform_not_supported) ->
    Zone_file.input_tz_file ~zonename ~filename
  | Error (Failed exn_from_js_loader) ->
    (try Zone_file.input_tz_file ~zonename ~filename with
     | exn_from_input_tz_file ->
       raise (Exn.Finally (exn_from_js_loader, exn_from_input_tz_file)))
  | Ok { first_transition; remaining_transitions } ->
    finalize_js_loaded ~zonename ~filename ~first_transition ~remaining_transitions
;;

let utc = of_utc_offset ~hours:0
let name zone = zone.name
let reset_transition_cache t = t.last_regime_index <- Index.before_first_transition

(* Raises if [index >= Array.length t.transitions] *)
let get_regime_exn t index =
  if index < 0 then t.default_local_time_type else t.transitions.(index).new_regime
;;

(* In "absolute mode", a number of seconds is interpreted as an offset of that many
   seconds from the UNIX epoch, ignoring leap seconds.

   In "date and ofday mode", you interpret the number of seconds as a number of days in
   combination with a number of seconds since midnight, which gives you a calendar day and
   a clock face time. Then you take the time that those represent in some relevant
   timezone.

   Of course, if the timezone in question has DST shifts, the date and ofday might
   represent two or zero times. These times will be interpreted according to either the
   previous UTC offset or the next one, in a way whose precise details you probably
   shouldn't depend on.

   (For the curious, what we do is: compute the "relative time" of the shift according to
   the new regime, and assign relative times to the old regime or new regime depending on
   which side of the shift time they occur. Since this amounts to using the old regime
   when the clocks move forward and the new regime when the clocks move back, it's
   equivalent to calculating the corresponding Time.t's relative to both the old and the
   new regime and picking the one that occurs later. Yes, later. I had to draw a diagram
   to persuade myself that it's that way round, but it is.)
*)
module Mode = struct
  type t =
    | Absolute
    | Date_and_ofday
end

let effective_start_time ~mode (x : Transition.t) =
  let open Int63.O in
  match (mode : Mode.t) with
  | Absolute -> x.start_time_in_seconds_since_epoch
  | Date_and_ofday ->
    x.start_time_in_seconds_since_epoch + x.new_regime.utc_offset_in_seconds
;;

let index_lower_bound_contains_seconds_since_epoch t index ~mode seconds =
  index < 0 || Int63.( >= ) seconds (effective_start_time ~mode t.transitions.(index))
;;

let index_upper_bound_contains_seconds_since_epoch t index ~mode seconds =
  index + 1 >= Array.length t.transitions
  || Int63.( < ) seconds (effective_start_time ~mode t.transitions.(index + 1))
;;

let binary_search_index_of_seconds_since_epoch t ~mode seconds : Index.t =
  Array.binary_search_segmented t.transitions `Last_on_left ~segment_of:(fun transition ->
    if Int63.( <= ) (effective_start_time transition ~mode) seconds then `Left else `Right)
  |> Option.value ~default:Index.before_first_transition
;;

let index_of_seconds_since_epoch t ~mode seconds =
  let index =
    let index = t.last_regime_index in
    if not (index_lower_bound_contains_seconds_since_epoch t index ~mode seconds)
       (* time is before cached index; try previous index *)
    then (
      let index = index - 1 in
      if not (index_lower_bound_contains_seconds_since_epoch t index ~mode seconds)
         (* time is before previous index; fall back on binary search *)
      then
        binary_search_index_of_seconds_since_epoch t ~mode seconds
        (* time is before cached index and not before previous, so within previous *)
      else index)
    else if not (index_upper_bound_contains_seconds_since_epoch t index ~mode seconds)
            (* time is after cached index; try next index *)
    then (
      let index = index + 1 in
      if not (index_upper_bound_contains_seconds_since_epoch t index ~mode seconds)
         (* time is after next index; fall back on binary search *)
      then
        binary_search_index_of_seconds_since_epoch t ~mode seconds
        (* time is after cached index and not after next, so within next *)
      else index (* time is within cached index *))
    else index
  in
  t.last_regime_index <- index;
  index
;;

module Time_in_seconds : sig
  include Zone_intf.Time_in_seconds
end = struct
  module Span = struct
    type t = Int63.t

    let of_int63_seconds = Fn.id
    let to_int63_seconds_round_down_exn = Fn.id
  end

  module Absolute = struct
    type t = Int63.t

    let of_span_since_epoch = Fn.id
    let to_span_since_epoch = Fn.id
  end

  module Date_and_ofday = struct
    type t = Int63.t

    let of_synthetic_span_since_epoch = Fn.id
    let to_synthetic_span_since_epoch = Fn.id
  end

  include Absolute
end

let index t time =
  Time_in_seconds.to_span_since_epoch time
  |> Time_in_seconds.Span.to_int63_seconds_round_down_exn
  |> index_of_seconds_since_epoch t ~mode:Absolute
;;

let index_of_date_and_ofday t time =
  Time_in_seconds.Date_and_ofday.to_synthetic_span_since_epoch time
  |> Time_in_seconds.Span.to_int63_seconds_round_down_exn
  |> index_of_seconds_since_epoch t ~mode:Date_and_ofday
;;

let index_has_prev_clock_shift t index = index >= 0 && index < Array.length t.transitions
let index_has_next_clock_shift t index = index_has_prev_clock_shift t (index + 1)

let index_prev_clock_shift_time_exn t index =
  let transition = t.transitions.(index) in
  transition.start_time_in_seconds_since_epoch
  |> Time_in_seconds.Span.of_int63_seconds
  |> Time_in_seconds.of_span_since_epoch
;;

let index_next_clock_shift_time_exn t index = index_prev_clock_shift_time_exn t (index + 1)

let index_prev_clock_shift_amount_exn t index =
  let transition = t.transitions.(index) in
  let after = transition.new_regime in
  let before =
    if index = 0 then t.default_local_time_type else t.transitions.(index - 1).new_regime
  in
  Int63.( - ) after.utc_offset_in_seconds before.utc_offset_in_seconds
  |> Time_in_seconds.Span.of_int63_seconds
;;

let index_next_clock_shift_amount_exn t index =
  index_prev_clock_shift_amount_exn t (index + 1)
;;

let index_abbreviation_exn t index =
  let regime = get_regime_exn t index in
  regime.abbrv
;;

let index_offset_from_utc_exn t index =
  let regime = get_regime_exn t index in
  Time_in_seconds.Span.of_int63_seconds regime.utc_offset_in_seconds
;;
