open! Import
open Std_internal

let arch_sixtyfour = Sys.word_size_in_bits = 64

module Span = Span_ns
module Ofday = Ofday_ns

type t = Span.t (* since the Unix epoch (1970-01-01 00:00:00 UTC) *)
[@@deriving bin_io, compare, hash, typerep]

module Replace_polymorphic_compare_efficient = Span.Replace_polymorphic_compare
module Replace_polymorphic_compare = Replace_polymorphic_compare_efficient
include Replace_polymorphic_compare_efficient
include (Span : Quickcheck.S_range with type t := t)

let now = Span.since_unix_epoch
let equal = Span.equal
let min_value_for_1us_rounding = Span.min_value_for_1us_rounding
let max_value_for_1us_rounding = Span.max_value_for_1us_rounding
let epoch = Span.zero
let add = Span.( + )
let sub = Span.( - )
let diff = Span.( - )
let abs_diff t u = Span.abs (diff t u)
let max = Span.max
let min = Span.min
let next = Span.next
let prev = Span.prev
let to_span_since_epoch t = t
let of_span_since_epoch s = s
let to_int63_ns_since_epoch t : Int63.t = Span.to_int63_ns (to_span_since_epoch t)
let[@inline] of_int63_ns_since_epoch i = of_span_since_epoch (Span.of_int63_ns i)
let[@cold] overflow () = raise_s [%message "Time_ns: overflow"]
let is_earlier t1 ~than:t2 = t1 < t2
let is_later t1 ~than:t2 = t1 > t2

let add_overflowed x y ~sum =
  if Span.( > ) y Span.zero then Span.( < ) sum x else Span.( > ) sum x
;;

let sub_overflowed x y ~diff =
  if Span.( > ) y Span.zero then Span.( > ) diff x else Span.( < ) diff x
;;

let add_exn x y =
  let sum = add x y in
  if add_overflowed x y ~sum then overflow () else sum
;;

let sub_exn x y =
  let diff = sub x y in
  if sub_overflowed x y ~diff then overflow () else diff
;;

let add_saturating x y =
  let sum = add x y in
  if add_overflowed x y ~sum
  then
    if Span.(y > zero) then Span.max_value_representable else Span.min_value_representable
  else sum
;;

let sub_saturating x y =
  let diff = sub x y in
  if sub_overflowed x y ~diff
  then
    if Span.(y > zero) then Span.min_value_representable else Span.max_value_representable
  else diff
;;

let to_int_ns_since_epoch =
  if arch_sixtyfour
  then fun t -> Int63.to_int_exn (to_int63_ns_since_epoch t)
  else fun _ -> failwith "Time_ns.to_int_ns_since_epoch: unsupported on 32bit machines"
;;

let of_int_ns_since_epoch i = of_int63_ns_since_epoch (Int63.of_int i)

let to_time_float_round_nearest t =
  Time_float.of_span_since_epoch
    (Span.to_span_float_round_nearest (to_span_since_epoch t))
;;

let to_time_float_round_nearest_microsecond t =
  Time_float.of_span_since_epoch
    (Span.to_span_float_round_nearest_microsecond (to_span_since_epoch t))
;;

let min_time_value_for_1us_rounding =
  to_time_float_round_nearest min_value_for_1us_rounding
;;

let max_time_value_for_1us_rounding =
  to_time_float_round_nearest max_value_for_1us_rounding
;;

let check_before_conversion_for_1us_rounding time =
  if Time_float.( < ) time min_time_value_for_1us_rounding
     || Time_float.( > ) time max_time_value_for_1us_rounding
  then
    failwiths
      ~here:[%here]
      "Time_ns does not support this time"
      time
      [%sexp_of: Time_float.Stable.With_utc_sexp.V2.t]
;;

let[@inline] of_time_float_round_nearest time =
  of_span_since_epoch
    (Span.of_span_float_round_nearest (Time_float.to_span_since_epoch time))
;;

let of_time_float_round_nearest_microsecond time =
  check_before_conversion_for_1us_rounding time;
  of_span_since_epoch
    (Span.of_span_float_round_nearest_microsecond (Time_float.to_span_since_epoch time))
;;

let[@cold] raise_next_multiple_got_nonpositive_interval ~calling_function_name interval =
  failwiths
    ~here:[%here]
    ("Time_ns." ^ calling_function_name ^ " got nonpositive interval")
    interval
    [%sexp_of: Span.t]
;;

let next_multiple_internal ~calling_function_name ~can_equal_after ~base ~after ~interval =
  if Span.( <= ) interval Span.zero
  then raise_next_multiple_got_nonpositive_interval ~calling_function_name interval;
  let base_to_after = diff after base in
  if Span.( < ) base_to_after Span.zero
  then base (* [after < base], choose [k = 0]. *)
  else (
    let next = add base (Span.scale_int63 interval (Span.div base_to_after interval)) in
    if next > after || (can_equal_after && next = after) then next else add next interval)
;;

let prev_multiple_internal
  ~calling_function_name
  ~can_equal_before
  ~base
  ~before
  ~interval
  =
  next_multiple_internal
    ~calling_function_name
    ~can_equal_after:(not can_equal_before)
    ~base
    ~after:(sub before interval)
    ~interval
;;

let next_multiple ?(can_equal_after = false) ~base ~after ~interval () =
  next_multiple_internal
    ~calling_function_name:"next_multiple"
    ~can_equal_after
    ~base
    ~after
    ~interval
;;

let prev_multiple ?(can_equal_before = false) ~base ~before ~interval () =
  prev_multiple_internal
    ~calling_function_name:"prev_multiple"
    ~can_equal_before
    ~base
    ~before
    ~interval
;;

let round_up t ~interval ~calling_function_name =
  next_multiple_internal
    ~calling_function_name
    ~can_equal_after:true
    ~base:epoch
    ~after:t
    ~interval
;;

let round_down t ~interval ~calling_function_name =
  prev_multiple_internal
    ~calling_function_name
    ~can_equal_before:true
    ~base:epoch
    ~before:t
    ~interval
;;

let round_up_to_us t =
  round_up t ~interval:Span.microsecond ~calling_function_name:"round_up_to_us"
;;

let round_up_to_ms t =
  round_up t ~interval:Span.millisecond ~calling_function_name:"round_up_to_ms"
;;

let round_up_to_sec t =
  round_up t ~interval:Span.second ~calling_function_name:"round_up_to_sec"
;;

let round_down_to_us t =
  round_down t ~interval:Span.microsecond ~calling_function_name:"round_down_to_us"
;;

let round_down_to_ms t =
  round_down t ~interval:Span.millisecond ~calling_function_name:"round_down_to_ms"
;;

let round_down_to_sec t =
  round_down t ~interval:Span.second ~calling_function_name:"round_down_to_sec"
;;

let random ?state () = Span.random ?state ()

module Utc : sig
  val to_date_and_span_since_start_of_day : t -> Date0.t * Span.t
  val of_date_and_span_since_start_of_day : Date0.t -> Span.t -> t
end = struct
  (* a recreation of the system call gmtime specialized to the fields we need that also
     doesn't rely on Unix. *)
  let to_date_and_span_since_start_of_day t =
    let open Int63.O in
    let ( !< ) i = Int63.of_int_exn i in
    let ( !> ) t = Int63.to_int_exn t in
    let ns_since_epoch = to_int63_ns_since_epoch t in
    let ns_per_day = !<86_400 * !<1_000_000_000 in
    let approx_days_from_epoch = ns_since_epoch / ns_per_day in
    let days_from_epoch =
      if ns_since_epoch < !<0 && approx_days_from_epoch * ns_per_day <> ns_since_epoch
      then approx_days_from_epoch - !<1
      else approx_days_from_epoch
    in
    let ns_since_start_of_day = ns_since_epoch - (ns_per_day * days_from_epoch) in
    let date =
      Date0.Days.add_days Date0.Days.unix_epoch !>days_from_epoch |> Date0.Days.to_date
    in
    let span_since_start_of_day = Span.of_int63_ns ns_since_start_of_day in
    date, span_since_start_of_day
  ;;

  let of_date_and_span_since_start_of_day date span_since_start_of_day =
    assert (
      Span.( >= ) span_since_start_of_day Span.zero
      && Span.( < ) span_since_start_of_day Span.day);
    let days_from_epoch =
      Date0.Days.diff (Date0.Days.of_date date) Date0.Days.unix_epoch
    in
    let span_in_days_since_epoch = Span.scale_int Span.day days_from_epoch in
    let span_since_epoch = Span.( + ) span_in_days_since_epoch span_since_start_of_day in
    of_span_since_epoch span_since_epoch
  ;;
end

module Alternate_sexp = struct
  module T = struct
    type nonrec t = t [@@deriving bin_io, compare, hash]

    module Ofday_as_span = struct
      open Int.O

      let seconds_to_string seconds_span =
        let seconds = Span.to_int_sec seconds_span in
        let h = seconds / 3600 in
        let m = seconds / 60 % 60 in
        let s = seconds % 60 in
        sprintf "%02d:%02d:%02d" h m s
      ;;

      let two_digit_of_string string =
        assert (String.length string = 2 && String.for_all string ~f:Char.is_digit);
        Int.of_string string
      ;;

      let seconds_of_string seconds_string =
        match String.split seconds_string ~on:':' with
        | [ h_string; m_string; s_string ] ->
          let h = two_digit_of_string h_string in
          let m = two_digit_of_string m_string in
          let s = two_digit_of_string s_string in
          Span.of_int_sec ((((h * 60) + m) * 60) + s)
        | _ -> assert false
      ;;

      let ns_of_100_ms = 100_000_000
      let ns_of_10_ms = 10_000_000
      let ns_of_1_ms = 1_000_000
      let ns_of_100_us = 100_000
      let ns_of_10_us = 10_000
      let ns_of_1_us = 1_000
      let ns_of_100_ns = 100
      let ns_of_10_ns = 10
      let ns_of_1_ns = 1

      let sub_second_to_string sub_second_span =
        let open Int.O in
        let ns = Span.to_int63_ns sub_second_span |> Int63.to_int_exn in
        if ns = 0
        then ""
        else if ns % ns_of_100_ms = 0
        then sprintf ".%01d" (ns / ns_of_100_ms)
        else if ns % ns_of_10_ms = 0
        then sprintf ".%02d" (ns / ns_of_10_ms)
        else if ns % ns_of_1_ms = 0
        then sprintf ".%03d" (ns / ns_of_1_ms)
        else if ns % ns_of_100_us = 0
        then sprintf ".%04d" (ns / ns_of_100_us)
        else if ns % ns_of_10_us = 0
        then sprintf ".%05d" (ns / ns_of_10_us)
        else if ns % ns_of_1_us = 0
        then sprintf ".%06d" (ns / ns_of_1_us)
        else if ns % ns_of_100_ns = 0
        then sprintf ".%07d" (ns / ns_of_100_ns)
        else if ns % ns_of_10_ns = 0
        then sprintf ".%08d" (ns / ns_of_10_ns)
        else sprintf ".%09d" ns
      ;;

      let sub_second_of_string string =
        if String.is_empty string
        then Span.zero
        else (
          let digits = String.chop_prefix_exn string ~prefix:"." in
          assert (String.for_all digits ~f:Char.is_digit);
          let multiplier =
            match String.length digits with
            | 1 -> ns_of_100_ms
            | 2 -> ns_of_10_ms
            | 3 -> ns_of_1_ms
            | 4 -> ns_of_100_us
            | 5 -> ns_of_10_us
            | 6 -> ns_of_1_us
            | 7 -> ns_of_100_ns
            | 8 -> ns_of_10_ns
            | 9 -> ns_of_1_ns
            | _ -> assert false
          in
          Span.of_int63_ns (Int63.of_int (Int.of_string digits * multiplier)))
      ;;

      let to_string span =
        assert (Span.( >= ) span Span.zero && Span.( < ) span Span.day);
        let seconds_span = span |> Span.to_int_sec |> Span.of_int_sec in
        let sub_second_span = Span.( - ) span seconds_span in
        seconds_to_string seconds_span ^ sub_second_to_string sub_second_span
      ;;

      let of_string string =
        let len = String.length string in
        let prefix_len = 8 in
        (* "HH:MM:DD" *)
        let suffix_len = len - prefix_len in
        let seconds_string = String.sub string ~pos:0 ~len:prefix_len in
        let sub_second_string = String.sub string ~pos:prefix_len ~len:suffix_len in
        let seconds_span = seconds_of_string seconds_string in
        let sub_second_span = sub_second_of_string sub_second_string in
        Span.( + ) seconds_span sub_second_span
      ;;
    end

    let to_string t =
      let date, span_since_start_of_day = Utc.to_date_and_span_since_start_of_day t in
      Date0.to_string date ^ " " ^ Ofday_as_span.to_string span_since_start_of_day ^ "Z"
    ;;

    let of_string string =
      let date_string, ofday_string_with_zone = String.lsplit2_exn string ~on:' ' in
      let ofday_string = String.chop_suffix_exn ofday_string_with_zone ~suffix:"Z" in
      let date = Date0.of_string date_string in
      let ofday = Ofday_as_span.of_string ofday_string in
      Utc.of_date_and_span_since_start_of_day date ofday
    ;;

    include Sexpable.Of_stringable (struct
      type nonrec t = t

      let to_string = to_string
      let of_string = of_string
    end)

    let t_sexp_grammar =
      let open Sexplib in
      Sexp_grammar.tag
        t_sexp_grammar
        ~key:Sexp_grammar.type_name_tag
        ~value:(Atom "Core.Time_ns.Alternate_sexp.t")
    ;;
  end

  include T
  include Comparable.Make (T)
  include Replace_polymorphic_compare_efficient

  include Diffable.Atomic.Make (struct
    type nonrec t = t [@@deriving bin_io, equal, sexp]
  end)

  module Stable = struct
    module V1 = struct
      module T = struct
        (* see tests in lib/core/test/src/test_time_ns that ensure stability of this
           representation *)
        type nonrec t = t [@@deriving bin_io, compare, equal, hash, sexp, sexp_grammar]

        let stable_witness : t Stable_witness.t = Stable_witness.assert_stable

        type nonrec comparator_witness = comparator_witness

        let comparator = comparator
      end

      include T
      include Comparable.Stable.V1.With_stable_witness.Make (T)
      include Diffable.Atomic.Make (T)
    end
  end
end

module Option0 = struct
  type time = t [@@deriving compare]
  type t = Span.Option.t [@@deriving bin_io, compare, hash, typerep, quickcheck]

  let none = Span.Option.none
  let some time = Span.Option.some (to_span_since_epoch time)
  let is_none = Span.Option.is_none
  let is_some = Span.Option.is_some

  let some_is_representable time =
    Span.Option.some_is_representable (to_span_since_epoch time)
  ;;

  let value t ~default =
    of_span_since_epoch (Span.Option.value ~default:(to_span_since_epoch default) t)
  ;;

  let value_exn t =
    if is_some t
    then of_span_since_epoch (Span.Option.unchecked_value t)
    else raise_s [%message [%here] "Time_ns.Option.value_exn none"]
  ;;

  let unchecked_value t = of_span_since_epoch (Span.Option.unchecked_value t)

  let of_option = function
    | None -> none
    | Some t -> some t
  ;;

  let to_option t = if is_none t then None else Some (value_exn t)

  module Optional_syntax = struct
    module Optional_syntax = struct
      let is_none = is_none
      let unsafe_value = unchecked_value
    end
  end

  module Alternate_sexp = struct
    module T = struct
      type nonrec t = t [@@deriving bin_io, compare, hash]

      let sexp_of_t t = [%sexp_of: Alternate_sexp.t option] (to_option t)
      let t_of_sexp s = of_option ([%of_sexp: Alternate_sexp.t option] s)

      let t_sexp_grammar =
        Sexplib.Sexp_grammar.coerce [%sexp_grammar: Alternate_sexp.t option]
      ;;
    end

    include T
    include Comparable.Make (T)

    include Diffable.Atomic.Make (struct
      include T

      let equal = [%compare.equal: t]
    end)

    module Stable = struct
      module V1 = struct
        module T = struct
          type nonrec t = t [@@deriving bin_io, compare, hash, sexp, sexp_grammar]

          let stable_witness : t Stable_witness.t =
            Stable_witness.of_serializable
              [%stable_witness: Alternate_sexp.Stable.V1.t option]
              of_option
              to_option
          ;;

          type nonrec comparator_witness = comparator_witness

          let comparator = comparator
        end

        include T
        include Comparable.Stable.V1.With_stable_witness.Make (T)

        include Diffable.Atomic.Make (struct
          include T

          let equal = [%compare.equal: t]
        end)
      end
    end
  end

  module Stable = struct
    module V1 = struct
      type nonrec t = t [@@deriving compare, bin_io]

      let stable_witness : t Stable_witness.t = Stable_witness.assert_stable
      let to_int63 t = Span.Option.Stable.V1.to_int63 t
      let of_int63_exn t = Span.Option.Stable.V1.of_int63_exn t
    end

    module Alternate_sexp = Alternate_sexp.Stable
  end

  let sexp_of_t = `Use_Time_ns_unix

  (* bring back the efficient implementation of comparison operators *)
  include (Span.Option : Comparisons.S with type t := t)
end

module Stable = struct
  module V1 = struct end
  module Option = Option0.Stable
  module Alternate_sexp = Alternate_sexp.Stable
  module Span = Span.Stable
  module Ofday = Ofday.Stable
end

(* this code is directly duplicated from Time.ml functor, converted enough to get Time_ns
   to/of_string working *)
module To_and_of_string : sig
  val of_date_ofday : zone:Zone.t -> Date.t -> Ofday.t -> t

  val of_date_ofday_precise
    :  Date.t
    -> Ofday.t
    -> zone:Zone.t
    -> [ `Once of t | `Twice of t * t | `Never of t ]

  val to_date_ofday : t -> zone:Zone.t -> Date.t * Ofday.t

  val to_date_ofday_precise
    :  t
    -> zone:Zone.t
    -> Date.t * Ofday.t * [ `Only | `Also_at of t | `Also_skipped of Date.t * Ofday.t ]

  val to_date : t -> zone:Zone.t -> Date.t
  val to_ofday : t -> zone:Zone.t -> Ofday.t
  val convert : from_tz:Zone.t -> to_tz:Zone.t -> Date.t -> Ofday.t -> Date.t * Ofday.t
  val reset_date_cache : unit -> unit
  val utc_offset : t -> zone:Zone.t -> Span.t

  val of_string : string -> t
    [@@deprecated "[since 2021-04] Use [of_string_with_utc_offset]"]

  val of_string_with_utc_offset : string -> t
  val to_string : t -> string [@@deprecated "[since 2021-04] Use [to_string_utc]"]
  val to_string_utc : t -> string
  val to_filename_string : t -> zone:Zone.t -> string
  val of_filename_string : string -> zone:Zone.t -> t
  val to_string_trimmed : t -> zone:Zone.t -> string
  val to_sec_string : t -> zone:Zone.t -> string
  val to_sec_string_with_zone : t -> zone:Zone.t -> string
  val of_localized_string : zone:Zone.t -> string -> t

  val of_string_gen
    :  default_zone:(unit -> Zone.t)
    -> find_zone:(string -> Zone.t)
    -> string
    -> t

  val to_string_abs : t -> zone:Zone.t -> string
  val to_string_abs_trimmed : t -> zone:Zone.t -> string
  val to_string_abs_parts : t -> zone:Zone.t -> string list
  val to_string_iso8601_basic : t -> zone:Zone.t -> string

  val occurrence
    :  [ `First_after_or_at | `Last_before_or_at ]
    -> t
    -> ofday:Ofday.t
    -> zone:Zone.t
    -> t
end = struct
  (* this code is directly duplicated from Time_float0.ml, converted enough to get
     Time_ns to/of_string working *)
  module Date_and_ofday = struct
    type t = Int63.t

    let to_synthetic_span_since_epoch t = Span.of_int63_ns t

    let of_date_ofday date ofday =
      let days =
        Date0.Days.diff (Date0.Days.of_date date) Date0.Days.unix_epoch |> Int63.of_int
      in
      let open Int63.O in
      (days * Span.to_int63_ns Span.day)
      + Span.to_int63_ns (Ofday.to_span_since_start_of_day ofday)
    ;;

    let to_absolute relative ~offset_from_utc =
      sub_exn (Span.of_int63_ns relative) offset_from_utc
    ;;

    let of_absolute absolute ~offset_from_utc =
      Span.to_int63_ns (add_exn absolute offset_from_utc)
    ;;

    let ns_per_day = Span.to_int63_ns Span.day

    let to_days_from_epoch t =
      (* note Time_ns represents about 146 years, not enough for [Date.create_exn] to ever
         raise *)
      let open Int63.O in
      let days_from_epoch_approx = t / ns_per_day in
      (* when [t] is negative the integer division that calculated days_from_epoch_approx
         will leave us one day short because it truncates (e.g. -100 / 86_400 = 0 and we
         want -1) -- adjust for that here. *)
      if t < days_from_epoch_approx * ns_per_day
      then Int63.pred days_from_epoch_approx
      else days_from_epoch_approx
    ;;

    let ofday_of_days_from_epoch t ~days_from_epoch =
      let open Int63.O in
      let days_from_epoch_in_ns = days_from_epoch * ns_per_day in
      let remainder = t - days_from_epoch_in_ns in
      Span.of_int63_ns remainder |> Ofday.of_span_since_start_of_day_exn
    ;;

    let date_of_days_from_epoch ~days_from_epoch =
      Int63.to_int_exn days_from_epoch
      |> Date0.Days.add_days Date0.Days.unix_epoch
      |> Date0.Days.to_date
    ;;

    let to_date t =
      let days_from_epoch = to_days_from_epoch t in
      date_of_days_from_epoch ~days_from_epoch
    ;;

    let to_ofday t =
      let days_from_epoch = to_days_from_epoch t in
      ofday_of_days_from_epoch t ~days_from_epoch
    ;;
  end

  module Zone : sig
    (* This interface is directly duplicated from Time_intf.Zone, converted enough to get
       this to work.

       The problem is has references to Time0_intf.S, which is the functor input interface
       that Time_ns currently does not satisfy. *)

    type time = t
    type t = Zone.t [@@deriving sexp_of]

    module Index = Zone.Index

    (* copied functions reexported from Zone *)

    val utc : t
    val index_has_prev_clock_shift : t -> Index.t -> bool
    val index_has_next_clock_shift : t -> Index.t -> bool

    (* new functions defined below *)

    val index : t -> time -> Index.t
    val index_offset_from_utc_exn : t -> Index.t -> time
    val index_prev_clock_shift_time_exn : t -> Index.t -> time
    val index_next_clock_shift_time_exn : t -> Index.t -> time
    val absolute_time_of_date_and_ofday : t -> Date_and_ofday.t -> time
    val date_and_ofday_of_absolute_time : t -> time -> Date_and_ofday.t
    val next_clock_shift : t -> strictly_after:time -> (time * Span.t) option
    val prev_clock_shift : t -> at_or_before:time -> (time * Span.t) option
  end = struct
    type time = t

    include Zone

    let of_span_in_seconds span_in_seconds =
      (* NB. no actual rounding or exns can occur here *)
      Time_in_seconds.Span.to_int63_seconds_round_down_exn span_in_seconds
      |> Span.of_int63_seconds
    ;;

    let of_time_in_seconds time_in_seconds =
      Time_in_seconds.to_span_since_epoch time_in_seconds
      (* NB. no actual rounding or exns can occur here *)
      |> Time_in_seconds.Span.to_int63_seconds_round_down_exn
      |> Span.of_int63_seconds
      |> of_span_since_epoch
    ;;

    let to_time_in_seconds_round_down_exn time =
      to_span_since_epoch time
      |> Span.to_int63_seconds_round_down_exn
      |> Time_in_seconds.Span.of_int63_seconds
      |> Time_in_seconds.of_span_since_epoch
    ;;

    let to_date_and_ofday_in_seconds_round_down_exn relative =
      Date_and_ofday.to_synthetic_span_since_epoch relative
      |> Span.to_int63_seconds_round_down_exn
      |> Time_in_seconds.Span.of_int63_seconds
      |> Time_in_seconds.Date_and_ofday.of_synthetic_span_since_epoch
    ;;

    let index t time = index t (to_time_in_seconds_round_down_exn time)

    let index_of_date_and_ofday t relative =
      index_of_date_and_ofday t (to_date_and_ofday_in_seconds_round_down_exn relative)
    ;;

    let index_offset_from_utc_exn t index =
      of_span_in_seconds (index_offset_from_utc_exn t index)
    ;;

    let index_prev_clock_shift_time_exn t index =
      of_time_in_seconds (index_prev_clock_shift_time_exn t index)
    ;;

    let index_next_clock_shift_time_exn t index =
      of_time_in_seconds (index_next_clock_shift_time_exn t index)
    ;;

    let index_prev_clock_shift_amount_exn t index =
      of_span_in_seconds (index_prev_clock_shift_amount_exn t index)
    ;;

    let index_prev_clock_shift t index =
      match index_has_prev_clock_shift t index with
      | false -> None
      | true ->
        Some
          ( index_prev_clock_shift_time_exn t index
          , index_prev_clock_shift_amount_exn t index )
    ;;

    let index_next_clock_shift t index = index_prev_clock_shift t (Index.next index)
    let prev_clock_shift t ~at_or_before:time = index_prev_clock_shift t (index t time)
    let next_clock_shift t ~strictly_after:time = index_next_clock_shift t (index t time)

    let date_and_ofday_of_absolute_time t time =
      let index = index t time in
      (* no exn because [index] always returns a valid index *)
      let offset_from_utc = index_offset_from_utc_exn t index in
      Date_and_ofday.of_absolute time ~offset_from_utc
    ;;

    let absolute_time_of_date_and_ofday t relative =
      let index = index_of_date_and_ofday t relative in
      (* no exn because [index_of_date_and_ofday] always returns a valid index *)
      let offset_from_utc = index_offset_from_utc_exn t index in
      Date_and_ofday.to_absolute relative ~offset_from_utc
    ;;
  end

  let of_date_ofday ~zone date ofday =
    let relative = Date_and_ofday.of_date_ofday date ofday in
    Zone.absolute_time_of_date_and_ofday zone relative
  ;;

  let of_date_ofday_precise date ofday ~zone =
    (* We assume that there will be only one zone shift within a given local day.  *)
    let start_of_day = of_date_ofday ~zone date Ofday.start_of_day in
    let proposed_time = add start_of_day (Ofday.to_span_since_start_of_day ofday) in
    match Zone.next_clock_shift zone ~strictly_after:start_of_day with
    | None -> `Once proposed_time
    | Some (shift_start, shift_amount) ->
      let shift_backwards = Span.(shift_amount < zero) in
      (* start and end of the "problematic region" *)
      let s, e =
        if shift_backwards
        then add shift_start shift_amount, shift_start
        else shift_start, add shift_start shift_amount
      in
      if proposed_time < s
      then `Once proposed_time
      else if s <= proposed_time && proposed_time < e
      then
        if shift_backwards
        then `Twice (proposed_time, sub proposed_time shift_amount)
        else `Never shift_start
      else `Once (sub proposed_time shift_amount)
  ;;

  module Date_cache = struct
    type nonrec t =
      { mutable zone : Zone.t
      ; mutable cache_start_incl : t
      ; mutable cache_until_excl : t
      ; mutable effective_day_start : t
      ; mutable date : Date0.t
      }
  end

  let date_cache : Date_cache.t =
    { zone = Zone.utc
    ; cache_start_incl = epoch
    ; cache_until_excl = epoch
    ; effective_day_start = epoch
    ; date = Date0.unix_epoch
    }
  ;;

  let reset_date_cache () =
    date_cache.zone <- Zone.utc;
    date_cache.cache_start_incl <- epoch;
    date_cache.cache_until_excl <- epoch;
    date_cache.effective_day_start <- epoch;
    date_cache.date <- Date0.unix_epoch
  ;;

  let is_in_cache time ~zone =
    phys_equal zone date_cache.zone
    && time >= date_cache.cache_start_incl
    && time < date_cache.cache_until_excl
  ;;

  let set_date_cache time ~zone =
    match is_in_cache time ~zone with
    | true -> ()
    | false ->
      let index = Zone.index zone time in
      (* no exn because [Zone.index] always returns a valid index *)
      let offset_from_utc = Zone.index_offset_from_utc_exn zone index in
      let rel = Date_and_ofday.of_absolute time ~offset_from_utc in
      let date = Date_and_ofday.to_date rel in
      let span = Date_and_ofday.to_ofday rel |> Ofday.to_span_since_start_of_day in
      let effective_day_start =
        sub (Date_and_ofday.to_absolute rel ~offset_from_utc) span
      in
      let effective_day_until = add effective_day_start Span.day in
      let cache_start_incl =
        match Zone.index_has_prev_clock_shift zone index with
        | false -> effective_day_start
        | true ->
          effective_day_start |> max (Zone.index_prev_clock_shift_time_exn zone index)
      in
      let cache_until_excl =
        match Zone.index_has_next_clock_shift zone index with
        | false -> effective_day_until
        | true ->
          effective_day_until |> min (Zone.index_next_clock_shift_time_exn zone index)
      in
      date_cache.zone <- zone;
      date_cache.cache_start_incl <- cache_start_incl;
      date_cache.cache_until_excl <- cache_until_excl;
      date_cache.effective_day_start <- effective_day_start;
      date_cache.date <- date
  ;;

  let to_date time ~zone =
    set_date_cache time ~zone;
    date_cache.date
  ;;

  let to_ofday time ~zone =
    set_date_cache time ~zone;
    diff time date_cache.effective_day_start |> Ofday.of_span_since_start_of_day_exn
  ;;

  let to_date_ofday time ~zone = to_date time ~zone, to_ofday time ~zone

  (* The correctness of this algorithm (interface, even) depends on the fact that
     timezone shifts aren't too close together (as in, it can't simultaneously be the
     case that a timezone shift of X hours occurred less than X hours ago, *and*
     a timezone shift of Y hours will occur in less than Y hours' time) *)
  let to_date_ofday_precise time ~zone =
    let date, ofday = to_date_ofday time ~zone in
    let clock_shift_after = Zone.next_clock_shift zone ~strictly_after:time in
    let clock_shift_before_or_at = Zone.prev_clock_shift zone ~at_or_before:time in
    let also_skipped_earlier amount =
      (* Using [date] and raising on [None] here is OK on the assumption that clock
         shifts can't cross date boundaries. This is true in all cases I've ever heard
         of (and [of_date_ofday_precise] would need revisiting if it turned out to be
         false) *)
      match Ofday.sub ofday amount with
      | Some ofday -> `Also_skipped (date, ofday)
      | None ->
        raise_s
          [%message
            "Time.to_date_ofday_precise"
              ~span_since_epoch:(to_span_since_epoch time : Span.t)
              (zone : Zone.t)]
    in
    let ambiguity =
      (* Edge cases: the instant of transition belongs to the new zone regime. So if the
         clock moved by an hour exactly one hour ago, there's no ambiguity, because the
         hour-ago time belongs to the same regime as you, and conversely, if the clock
         will move by an hour in an hours' time, there *is* ambiguity. Hence [>.] for
         the first case and [<=.] for the second. *)
      match clock_shift_before_or_at, clock_shift_after with
      | Some (start, amount), _ when add start (Span.abs amount) > time ->
        (* clock shifted recently *)
        if Span.(amount > zero)
        then
          (* clock shifted forward recently: we skipped a time *)
          also_skipped_earlier amount
        else (
          (* clock shifted back recently: this date/ofday already happened *)
          assert (Span.(amount < zero));
          `Also_at (sub time (Span.abs amount)))
      | _, Some (start, amount) when sub start (Span.abs amount) <= time ->
        (* clock is about to shift *)
        if Span.(amount > zero)
        then (* clock about to shift forward: no effect *)
          `Only
        else (
          (* clock about to shift back: this date/ofday will be repeated *)
          assert (Span.(amount < zero));
          `Also_at (add time (Span.abs amount)))
      | _ -> `Only
    in
    date, ofday, ambiguity
  ;;

  let convert ~from_tz ~to_tz date ofday =
    let start_time = of_date_ofday ~zone:from_tz date ofday in
    to_date_ofday ~zone:to_tz start_time
  ;;

  let utc_offset t ~zone =
    let utc_epoch = Zone.date_and_ofday_of_absolute_time zone t in
    Span.( - )
      (Date_and_ofday.to_synthetic_span_since_epoch utc_epoch)
      (to_span_since_epoch t)
  ;;

  let offset_string time ~zone =
    let utc_offset = utc_offset time ~zone in
    let is_utc = Span.( = ) utc_offset Span.zero in
    if is_utc
    then "Z"
    else
      String.concat
        [ (if Span.( < ) utc_offset Span.zero then "-" else "+")
        ; Ofday.to_string_trimmed
            (Ofday.of_span_since_start_of_day_exn (Span.abs utc_offset))
        ]
  ;;

  let to_string_abs_parts =
    let attempt time ~zone =
      let date, ofday = to_date_ofday time ~zone in
      let offset_string = offset_string time ~zone in
      [ Date0.to_string date
      ; String.concat ~sep:"" [ Ofday.to_string ofday; offset_string ]
      ]
    in
    fun time ~zone ->
      try attempt time ~zone with
      | (_ : exn) ->
        (* If we overflow applying the UTC offset, try again with UTC time. *)
        attempt time ~zone:Zone.utc
  ;;

  let to_string_abs_trimmed time ~zone =
    let date, ofday = to_date_ofday time ~zone in
    let offset_string = offset_string time ~zone in
    String.concat
      ~sep:" "
      [ Date0.to_string date; Ofday.to_string_trimmed ofday ^ offset_string ]
  ;;

  let to_string_abs time ~zone = String.concat ~sep:" " (to_string_abs_parts ~zone time)
  let to_string_utc t = to_string_abs t ~zone:Zone.utc
  let to_string = to_string_utc

  let to_string_iso8601_basic time ~zone =
    String.concat ~sep:"T" (to_string_abs_parts ~zone time)
  ;;

  let to_string_trimmed t ~zone =
    let date, sec = to_date_ofday ~zone t in
    Date0.to_string date ^ " " ^ Ofday.to_string_trimmed sec
  ;;

  let to_sec_string t ~zone =
    let date, sec = to_date_ofday ~zone t in
    Date0.to_string date ^ " " ^ Ofday.to_sec_string sec
  ;;

  let to_sec_string_with_zone t ~zone = to_sec_string t ~zone ^ offset_string t ~zone

  let to_filename_string t ~zone =
    let date, ofday = to_date_ofday ~zone t in
    Date0.to_string date
    ^ "_"
    ^ String.tr
        ~target:':'
        ~replacement:'-'
        (String.drop_suffix (Ofday.to_string ofday) 3)
  ;;

  let of_filename_string s ~zone =
    try
      match String.lsplit2 s ~on:'_' with
      | None -> failwith "no space in filename string"
      | Some (date, ofday) ->
        let date = Date0.of_string date in
        let ofday = String.tr ~target:'-' ~replacement:':' ofday in
        let ofday = Ofday.of_string ofday in
        of_date_ofday date ofday ~zone
    with
    | exn -> invalid_argf "Time.of_filename_string (%s): %s" s (Exn.to_string exn) ()
  ;;

  let of_localized_string ~zone str =
    try
      match String.lsplit2 str ~on:' ' with
      | None -> invalid_arg (sprintf "no space in date_ofday string: %s" str)
      | Some (date, time) ->
        let date = Date0.of_string date in
        let ofday = Ofday.of_string time in
        of_date_ofday ~zone date ofday
    with
    | e -> Exn.reraise e "Time.of_localized_string"
  ;;

  let occurrence before_or_after t ~ofday ~zone =
    let first_guess_date = to_date t ~zone in
    let first_guess = of_date_ofday ~zone first_guess_date ofday in
    let cmp, increment =
      match before_or_after with
      | `Last_before_or_at -> ( <= ), -1
      | `First_after_or_at -> ( >= ), 1
    in
    if cmp first_guess t
    then first_guess
    else of_date_ofday ~zone (Date0.add_days first_guess_date increment) ofday
  ;;

  let ensure_colon_in_offset offset =
    let offset_length = String.length offset in
    if Int.( <= ) offset_length 2
       && Char.is_digit offset.[0]
       && Char.is_digit offset.[offset_length - 1]
    then offset ^ ":00"
    else if Char.( = ) offset.[1] ':' || Char.( = ) offset.[2] ':'
    then offset
    else if Int.( < ) offset_length 3 || Int.( > ) offset_length 4
    then failwithf "invalid offset %s" offset ()
    else
      String.concat
        [ String.slice offset 0 (offset_length - 2)
        ; ":"
        ; String.slice offset (offset_length - 2) offset_length
        ]
  ;;

  exception Time_ns_of_string of string * Exn.t [@@deriving sexp]

  let of_string_gen ~default_zone ~find_zone s =
    try
      let date, ofday, tz =
        match String.split s ~on:' ' with
        | [ day; month; year; ofday ] ->
          String.concat [ day; " "; month; " "; year ], ofday, None
        | [ date; ofday; tz ] -> date, ofday, Some tz
        | [ date; ofday ] -> date, ofday, None
        | [ s ] ->
          (match String.rsplit2 ~on:'T' s with
           | Some (date, ofday) -> date, ofday, None
           | None -> failwith "no spaces or T found")
        | _ -> failwith "too many spaces"
      in
      let ofday_to_sec od = Span.to_sec (Ofday.to_span_since_start_of_day od) in
      let ofday, utc_offset =
        match tz with
        | Some _ -> ofday, None
        | None ->
          if Char.( = ) ofday.[String.length ofday - 1] 'Z'
          then String.sub ofday ~pos:0 ~len:(String.length ofday - 1), Some 0.
          else (
            match String.lsplit2 ~on:'+' ofday with
            | Some (l, r) ->
              l, Some (ofday_to_sec (Ofday.of_string (ensure_colon_in_offset r)))
            | None ->
              (match String.lsplit2 ~on:'-' ofday with
               | Some (l, r) ->
                 l, Some (-1. *. ofday_to_sec (Ofday.of_string (ensure_colon_in_offset r)))
               | None -> ofday, None))
      in
      let date = Date0.of_string date in
      let ofday = Ofday.of_string ofday in
      match tz with
      | Some tz -> of_date_ofday ~zone:(find_zone tz) date ofday
      | None ->
        (match utc_offset with
         | None ->
           let zone = default_zone () in
           of_date_ofday ~zone date ofday
         | Some utc_offset ->
           let utc_t = of_date_ofday ~zone:Zone.utc date ofday in
           sub utc_t (Span.of_sec utc_offset))
    with
    | e -> raise (Time_ns_of_string (s, e))
  ;;

  let of_string_with_utc_offset s =
    let default_zone () = raise_s [%message "time has no time zone or UTC offset" s] in
    let find_zone zone_name =
      failwithf "unable to lookup Zone %s.  Try using Core.Time.of_string" zone_name ()
    in
    of_string_gen ~default_zone ~find_zone s
  ;;

  let of_string = of_string_with_utc_offset
end

include To_and_of_string

let min_value_representable = of_span_since_epoch Span.min_value_representable
let max_value_representable = of_span_since_epoch Span.max_value_representable

(* Legacy definitions based on rounding to the nearest microsecond. *)
let min_value = min_value_for_1us_rounding
let max_value = max_value_for_1us_rounding
let to_time = to_time_float_round_nearest_microsecond
let of_time = of_time_float_round_nearest_microsecond

module _ = struct
  open Ppx_module_timer_runtime

  let () =
    Duration.format
      := (module struct
           let duration_of_span s = s |> Span.to_int63_ns |> Duration.of_nanoseconds
           let span_of_duration d = d |> Duration.to_nanoseconds |> Span.of_int63_ns
           let of_string string = string |> Span.of_string |> duration_of_span

           let to_string_with_same_unit durations =
             let spans = durations |> List.map ~f:span_of_duration in
             let unit_of_time =
               spans
               |> List.max_elt ~compare:Span.compare
               |> Option.value_map
                    ~f:Span.to_unit_of_time
                    ~default:Unit_of_time.Nanosecond
             in
             spans |> List.map ~f:(Span.to_string_hum ~unit_of_time ~align_decimal:true)
           ;;
         end)
  ;;
end

module Option = Option0
module Hash_queue = struct end
module Hash_set = struct end
module Map = struct end
module Set = struct end
module Table = struct end
module Zone = struct end

let arg_type = `Use_Time_ns_unix
let comparator = `Use_Time_ns_unix
let get_sexp_zone = `Use_Time_ns_unix
let interruptible_pause = `Use_Time_ns_unix
let of_date_ofday_zoned = `Use_Time_ns_unix
let of_string_abs = `Use_Time_ns_unix
let of_string_fix_proto = `Use_Time_ns_unix
let pause = `Use_Time_ns_unix
let pause_forever = `Use_Time_ns_unix
let pp = `Use_Time_ns_unix
let set_sexp_zone = `Use_Time_ns_unix
let sexp_of_t = `Use_Time_ns_unix_or_Time_ns_alternate_sexp
let sexp_of_t_abs = `Use_Time_ns_unix
let t_of_sexp = `Use_Time_ns_unix_or_Time_ns_alternate_sexp
let t_of_sexp_abs = `Use_Time_ns_unix
let to_date_ofday_zoned = `Use_Time_ns_unix
let to_ofday_zoned = `Use_Time_ns_unix
let to_string_fix_proto = `Use_Time_ns_unix
let validate_bound = `Use_Time_ns_unix
let validate_lbound = `Use_Time_ns_unix
let validate_ubound = `Use_Time_ns_unix

module O = struct
  let ( >= ) = ( >= )
  let ( <= ) = ( <= )
  let ( = ) = ( = )
  let ( > ) = ( > )
  let ( < ) = ( < )
  let ( <> ) = ( <> )
  let ( + ) = add
  let ( - ) = diff
end
