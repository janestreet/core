(* Functions for parsing time zone database files (zic files).

   A time zone file consists (conceptually - the representation is more compact) of an
   ordered list of (float * [local_time_type]) that mark the boundaries (marked from the
   epoch) at which various time adjustment regimes are in effect.  This can also be
   thought of as breaking down all time past the epoch into ranges with a
   [local_time_type] that describes the offset from GMT to apply to each range to get
   local time.
*)

open Std_internal
module Hashtbl = Core_hashtbl
module Unix = Core_unix

exception Unknown_zone of string with sexp
exception Invalid_file_format of string with sexp

module Digest = struct
  include Digest

  include Bin_prot.Utils.Make_binable (struct
    module Binable = struct
      type t = string with bin_io
    end

    let to_binable str = str
    let of_binable str = str

    type t = string
  end)

  let sexp_of_t t = Sexp.Atom (Digest.to_hex t)
end

module Regime = struct
  type t = {
    utc_off : float;
    is_dst  : bool;
    abbrv   : string;
  } with sexp_of, bin_io
end

(* holds information about when leap seconds should be applied - unused
  because we are translating based on a epoch system clock (see the .mli). *)
module Leap_second = struct
  type t = {
    time    : float;
    seconds : float;
  } with sexp_of, bin_io
end

module Transition = struct
  type t = {
    start_time : float;
    end_time   : float; (* non-inclusive *)
    regime     : Regime.t
  } with sexp_of, bin_io
end

type t = {
  name                    : string;
  file_digest             : Digest.t option;
  transitions             : Transition.t array;
  (* transitions close to the current time *)
  likely_transitions      : Transition.t array;
  default_local_time_type : Regime.t;
  leap_seconds            : Leap_second.t list;
} with sexp_of

module Zone_file : sig
  val input_tz_file : zonename:string -> filename:string -> t
end = struct
  let bool_of_int i = i <> 0

  let input_long_as_float ic =
    let int32_of_char chr = Int32.of_int_exn (int_of_char chr) in
    let long = String.create 4 in
    really_input ic long 0 4;
    let sb1 = Int32.shift_left (int32_of_char long.[0]) 24 in
    let sb2 = Int32.shift_left (int32_of_char long.[1]) 16 in
    let sb3 = Int32.shift_left (int32_of_char long.[2]) 8 in
    let sb4 = int32_of_char long.[3] in
    let result = (Int32.bit_or (Int32.bit_or sb1 sb2) (Int32.bit_or sb3 sb4)) in
    Int32.to_float result
  ;;

  let input_long_long_as_float ic =
    let int63_of_char chr = Core_int63.of_int_exn (int_of_char chr) in
    let shift c bits = Core_int63.shift_left (int63_of_char c) bits in
    let long_long = String.create 8 in
    really_input ic long_long 0 8;
    let result =                           shift long_long.[0] 56 in
    let result = Core_int63.bit_or result (shift long_long.[1] 48) in
    let result = Core_int63.bit_or result (shift long_long.[2] 40) in
    let result = Core_int63.bit_or result (shift long_long.[3] 32) in
    let result = Core_int63.bit_or result (shift long_long.[4] 24) in
    let result = Core_int63.bit_or result (shift long_long.[5] 16) in
    let result = Core_int63.bit_or result (shift long_long.[6] 8) in
    let result = Core_int63.bit_or result (int63_of_char long_long.[7]) in
    Core_int63.to_float result
  ;;

  let input_long_as_int ic =
    let f = input_long_as_float ic in
    if f > float_of_int max_int then
      raise (Invalid_file_format "read int that cannot be represented as an \
        OCaml native int");
    int_of_float f
  ;;

  let input_list ic ~len ~f =
    let rec loop c lst =
      if c > 0 then loop (c - 1) ((f ic) :: lst)
      else List.rev lst
    in
    loop len []
  ;;

  let input_array ic ~len ~f = Array.of_list (input_list ic ~len ~f)

  let input_regime ic =
    let utc_off = input_long_as_float ic in
    let is_dst = bool_of_int (input_byte ic) in
    let abbrv_index = input_byte ic in
    let lt abbrv =
      { Regime.
        utc_off = utc_off;
        is_dst  = is_dst;
        abbrv   = abbrv;
      }
    in
    (lt,abbrv_index)
  ;;

  let input_abbreviations ic ~len =
    let raw_abbrvs = input_list ic ~len ~f:(input_char) in
    let buf = Buffer.create len in
    let _,indexed_abbrvs = List.fold raw_abbrvs ~init:(0, Map.Poly.empty)
      ~f:(fun (index,abbrvs) c ->
        match c with
        | '\000' ->
            let data = Buffer.contents buf in
            let next_index = index + (String.length data) + 1 in
            let abbrvs = Map.add abbrvs ~key:index ~data in
            Buffer.clear buf;
            (next_index,abbrvs)
        | c -> Buffer.add_char buf c; (index,abbrvs)
      )
    in
    if Buffer.length buf <> 0 then
      raise
        (Invalid_file_format "missing \000 terminating character in input_abbreviations");
    indexed_abbrvs
  ;;

  let input_tz_file_gen ~input_transition ~input_leap_second ic =
    let utc_local_count    = input_long_as_int ic in
    let std_wall_count     = input_long_as_int ic in
    let leap_count         = input_long_as_int ic in
    let transition_count   = input_long_as_int ic in
    let type_count         = input_long_as_int ic in
    let abbrv_char_count   = input_long_as_int ic in
    let transition_times   = input_list ic ~f:input_transition ~len:transition_count in
    let transition_indices = input_list ic ~f:input_byte ~len:transition_count in
    let regimes            = input_list ic ~f:input_regime ~len:type_count in
    let abbreviations      = input_abbreviations ic ~len:abbrv_char_count in
    let leap_seconds       = input_list ic ~f:input_leap_second ~len:leap_count in
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
      input_array ic ~len:std_wall_count ~f:(fun ic -> bool_of_int (input_byte ic))
    in
    let _utc_local_indicators =
      input_array ic ~len:utc_local_count ~f:(fun ic -> bool_of_int (input_byte ic))
    in
    let regimes =
      Array.of_list (List.map regimes
        ~f:(fun (lt,abbrv_index) ->
            let abbrv = Map.find_exn abbreviations abbrv_index in
            lt abbrv
          ))
    in
    let raw_transitions =
      List.map2_exn transition_times transition_indices
        ~f:(fun time index ->
          let regime = regimes.(index) in
          (time, regime))
    in
    let transitions =
      let rec make_transitions acc l =
        match l with
        | [] -> Array.of_list (List.rev acc)
        | [(time, regime)] ->
          make_transitions
            ({Transition.
              start_time = time;
              end_time   = Float.max_value;
              regime     = regime;
            } :: acc) []
        | (start_time,regime) :: (((end_time,_) :: _) as rest) ->
          make_transitions
            ({Transition.
              start_time = start_time;
              end_time   = end_time;
              regime     = regime
            } :: acc) rest
      in
      make_transitions [] raw_transitions
    in
    let now = Time_internal.T.to_float (Time_internal.T.now ()) in
    let likely_transitions =
      match
        Array.findi transitions ~f:(fun _i t ->
          t.Transition.start_time <= now && t.Transition.end_time > now)
      with
      | None -> [||]
      | Some (i, _x) ->
        let last_pos = Array.length transitions - 1 in
        if i > 0 && i < last_pos then
          [| transitions.(i); transitions.(i + 1); transitions.(i - 1) |]
        else if i > 0 then
          [| transitions.(i); transitions.(i - 1) |]
        else if i = 0 && i < last_pos then
          [| transitions.(i); transitions.(i + 1) |]
        else
          [| transitions.(i) |]
    in
    let default_local_time_type =
      match
        Array.find regimes ~f:(fun r -> not r.Regime.is_dst)
      with
      | None -> regimes.(0)
      | Some ltt -> ltt
    in
    (fun name digest ->
        {
          name                    = name;
          file_digest             = Some digest;
          transitions             = transitions;
          likely_transitions      = likely_transitions;
          default_local_time_type = default_local_time_type;
          leap_seconds            = leap_seconds;
        }
      )
  ;;

  let input_leap_second_gen ~input_leap_second ic =
    let leap_time = input_leap_second ic in
    let seconds   = input_long_as_float ic in
    { Leap_second.
      time    = leap_time;
      seconds = seconds;
    }
  ;;

  let read_header ic =
    let buf = String.create 4 in
    really_input ic buf 0 4;
    if buf <> "TZif" then
      raise (Invalid_file_format "magic characters TZif not present");
    let version =
      match input_char ic with
      | '\000' -> `V1
      | '2'    -> `V2
      | bad_version ->
          raise (Invalid_file_format (sprintf "version (%c) is invalid" bad_version))
    in
    (* space reserved for future use in the format *)
    really_input ic (String.create 15) 0 15;
    version
  ;;

  let input_tz_file_v1 ic =
    let input_leap_second =
      input_leap_second_gen ~input_leap_second:input_long_as_float
    in
    input_tz_file_gen ~input_transition:input_long_as_float ~input_leap_second ic
  ;;

  (*
    version 2 timezone files have the format:
      part 1 - exactly the same as v1
      part 2 - same format as v1, except that 8 bytes are used to store transition times
        and leap seconds
      part 3 - a newline-encloded, POSIX-TZ-environment-variable-style string for use in
        handling instants after the last transition time stored in the file
        (with nothing between the newlines if there is no POSIX representation for such
        instants)

    We handle files in this format by parsing the first part exactly as a v1 timezone
    file and then continuing to parse with 64bit reading functions in the right places.
  *)
  let input_tz_file_v2 ic =
    let _ = input_tz_file_v1 ic in
    (* the header is fully repeated *)
    assert (read_header ic = `V2);
    let input_leap_second =
      input_leap_second_gen ~input_leap_second:input_long_long_as_float
    in
    input_tz_file_gen ~input_transition:input_long_long_as_float ~input_leap_second ic
  ;;

  let input_tz_file ~zonename ~filename =
    try
      protectx (In_channel.create filename) ~finally:In_channel.close ~f:(fun ic ->
        let make_zone =
          match read_header ic with
          | `V1 ->
            input_tz_file_v1 ic
          | `V2 ->
            input_tz_file_v2 ic
        in
        let r = make_zone zonename (Digest.file filename) in
        r)
    with
    | Invalid_file_format reason ->
      raise (Invalid_file_format (sprintf "%s - %s" filename reason))
  ;;
end

module Zone_cache : sig
  type z

  val initialized_zones : unit -> (string * t) list
  val fill              : unit -> unit
  val find_or_load      : string -> t option
  val to_alist          : unit -> (string * t) list
end = struct
  type z = {
    mutable full : bool;
    basedir      : string;
    table        : t String.Table.t
  }
  type t = z

  let the_one_and_only =
    {
      full    = false;
      basedir = "/usr/share/zoneinfo/";
      table   = String.Table.create ();
    }
  ;;

  let fill () =
    (* GMT based timezones in Etc in the posix timezone system are very deceptive
       and basically shouldn't be used, so we never load them.  The other two are
       duplicates of other zone files. *)
    let skip_prefixes =
      [
        "Etc/GMT";
        "right/";
        "posix/";
      ]
    in
    if not the_one_and_only.full then begin
      the_one_and_only.full <- true;
      let maxdepth = 10 in
      let traverse ~maxdepth dir =
        let rec dfs dir depth =
          if depth < 1 then []
          else
            begin
              let entries = Array.to_list (Sys.readdir dir) in
              List.fold entries ~init:[] ~f:(fun acc fn ->
                  let fn = dir ^ "/" ^ fn in
                  if Sys.is_directory fn = `Yes then
                    List.rev_append (dfs fn (depth - 1)) acc
                  else (fn :: acc)
                )
            end
        in
        dfs dir maxdepth
      in
      let zonefiles = traverse ~maxdepth the_one_and_only.basedir in
      let pos = (String.length the_one_and_only.basedir) + 1 in
      List.iter zonefiles ~f:(fun filename ->
        try
          let len = (String.length filename) - pos in
          let zonename = String.sub filename ~pos ~len in
          if
            not (List.exists skip_prefixes
              ~f:(fun prefix -> String.is_prefix ~prefix zonename))
          then
            Hashtbl.replace
              the_one_and_only.table ~key:zonename ~data:(Zone_file.input_tz_file
                ~zonename ~filename);
        with
        | _e -> ());
    end
  ;;

  let to_alist () = Hashtbl.to_alist the_one_and_only.table

  let initialized_zones t =
    List.sort ~cmp:(fun a b -> ascending (fst a) (fst b)) (to_alist t)
  ;;

  let find zone = Hashtbl.find the_one_and_only.table zone

  let find_exn zone =
    match find zone with
    | None      -> raise (Unknown_zone zone)
    | Some data -> data
  ;;

  let find_or_load zonename =
    match find zonename with
    | Some z -> Some z
    | None   ->
        if the_one_and_only.full then None
        else begin
          try
            let filename = the_one_and_only.basedir ^ "/" ^ zonename in
            let zone     = Zone_file.input_tz_file ~zonename ~filename in
            Hashtbl.replace the_one_and_only.table ~key:zonename ~data:zone;
            Some zone
          with
          | _ -> None
        end
  ;;
end

let init () = Zone_cache.fill ()

let initialized_zones () = Zone_cache.initialized_zones ()

let of_utc_offset offset =
  assert (offset >= -24 && offset <= 24);
  let name =
    if offset = 0 then "UTC"
    else sprintf "UTC%s%d" (if offset < 0 then "-" else "+") (abs offset) in
  {
    name = name;
    file_digest = None;
    transitions = [||];
    likely_transitions = [||];
    default_local_time_type = {Regime.
      utc_off = Float.of_int (offset * 60 * 60);
      is_dst  = false;
      abbrv   = name;
    };
    leap_seconds = []
  }
;;

let to_utc_offset t =
  let ltt = t.default_local_time_type in
  Int.of_float ltt.Regime.utc_off
;;

let map_office_to_zone office =
  match office with
  | `chi -> "America/Chicago"
  | `hkg -> "Asia/Hong_Kong"
  | `ldn -> "Europe/London"
  | `nyc -> "America/New_York"
  | `tot -> "America/New_York"
;;

let find zone =
  let zone =
    match zone with
    | "chi"         -> map_office_to_zone `chi
    | "nyc"         -> map_office_to_zone `nyc
    | "hkg"         -> map_office_to_zone `hkg
    | "lon" | "ldn" -> map_office_to_zone `ldn
    | "tot"         -> map_office_to_zone `tot
    (* legacy/deprecated *)
    | "tyo"         -> "Asia/Tokyo"
    | _             -> zone
  in
  Zone_cache.find_or_load zone
;;

let find_exn zone =
  match find zone with
  | None -> raise (Unknown_zone zone)
  | Some z -> z
;;

let find_office office = find_exn (map_office_to_zone office)

let t_of_sexp sexp =
  match sexp with
  | Sexp.Atom name ->
    begin
      try
        if
          String.is_prefix name ~prefix:"GMT-"
          || String.is_prefix name ~prefix:"GMT+"
          || String.is_prefix name ~prefix:"UTC-"
          || String.is_prefix name ~prefix:"UTC+"
          || name = "GMT"
          || name = "UTC"
        then begin
          let offset =
            if name = "GMT" || name = "UTC" then 0
            else
              let base =
                Int.of_string (String.sub name ~pos:4 ~len:(String.length name - 4))
              in
              match name.[3] with
              | '-' -> (-1) * base
              | '+' -> base
              | _   -> assert false
          in
          of_utc_offset offset
        end else find_exn name
      with exc ->
        of_sexp_error
          (sprintf "Time.Zone.t_of_sexp: %s" (Exn.to_string exc)) sexp
    end
  | _ -> of_sexp_error "Time.Zone.t_of_sexp: expected atom" sexp
;;

let sexp_of_t t = Sexp.Atom t.name

module T = struct
  type t' = t with sexp
  type t = t' with sexp

  module Binable = struct
    type t = string with bin_io
  end

  let to_binable t = t.name
  let of_binable s = t_of_sexp (Sexp.Atom s)
end

include Sexpable.To_stringable (T)

include (Bin_prot.Utils.Make_binable (T) : Binable.S with type t := t)

let utc = of_utc_offset 0

(* This is intended to be thread safe at the expense of possibly doing more work than
   strictly necessary *)
let machine_zone =
  let zone = ref None in
  (fun ?(refresh=false) () ->
    if refresh then zone := None;
    match !zone with
    | Some t -> t
    | None ->
      let t =
        match Core_sys.getenv "TZ" with
        | Some zone_name ->
          find_exn zone_name
        | None ->
          Zone_file.input_tz_file ~zonename:"/etc/localtime" ~filename:"/etc/localtime"
      in
      zone := Some t;
      t)
;;

let transition_as_utc t = (t.Transition.start_time, t.Transition.end_time)

let transition_as_localtime t =
  let utc_off = t.Transition.regime.Regime.utc_off in
  (t.Transition.start_time +. utc_off, t.Transition.end_time +. utc_off)
;;

let linear_search ~min_bound ~max_bound transitions convert_transition time =
  let rec loop i =
    if i = min_bound then
      transitions.(i).Transition.regime
    else begin
      let transition = transitions.(i) in
      let (s,_e)      = convert_transition transition in
      if time > s then begin
        transition.Transition.regime
      end else loop (i - 1)
    end
  in
  loop max_bound
;;

(* [find_local_regime zone `UTC time] finds the local time regime in force
   in [zone] at [seconds], from 1970/01/01:00:00:00 UTC.

   [find_local_regime zone `Local seconds] finds the local time regime in force in
   [zone] at [seconds], from 1970/01/01:00:00:00 of [zone].
*)
let find_local_regime zone transtype time =
  let module T = Transition in
  let transitions         = zone.transitions in
  let convert_transition =
    match transtype with
    | `Local -> transition_as_localtime
    | `UTC   -> transition_as_utc
  in
  let num_transitions = Array.length transitions in
  if num_transitions = 0 then
    zone.default_local_time_type
  else if transitions.(0).T.start_time > time then
    zone.default_local_time_type
  else begin
    match
      Array.find zone.likely_transitions ~f:(fun t ->
        let (s,e) = convert_transition t in
        s <= time && time < e)
    with
    | Some t -> t.T.regime
    | None   ->
      let rec bin_search i min_bound max_bound =
        (* when we reach a small slice of the array drop to linear search *)
        if (max_bound - min_bound) <= 3 then
          linear_search ~min_bound ~max_bound transitions convert_transition time
        else begin
          let transition = transitions.(i) in
          let (s,e)      = convert_transition transition in
          if time < s then
            bin_search (i - (Int.max ((i - min_bound) / 2) 1)) min_bound i
          else if time >= e then
            bin_search (i + (Int.max ((max_bound - i) / 2) 1)) i max_bound
          else
            transition.T.regime
        end
      in
      let last_pos = num_transitions - 1 in
      bin_search (last_pos / 2) 0 last_pos
  end
;;

let shift_epoch_time zone repr_type epoch =
  let r = find_local_regime zone repr_type epoch in
  match repr_type with
  | `Local -> epoch -. r.Regime.utc_off
  | `UTC -> epoch +. r.Regime.utc_off
;;

let abbreviation zone time =
  (find_local_regime zone `UTC time).Regime.abbrv
;;

let digest zone = zone.file_digest

let name zone = zone.name

let pp ppf t = Format.fprintf ppf "%s" (name t)
let () = Pretty_printer.register "Core.Zone.pp"

