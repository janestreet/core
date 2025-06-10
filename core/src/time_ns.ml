open! Import
open Std_internal

let arch_sixtyfour = Sys.word_size_in_bits = 64

module Span = Span_ns

type t = Span.t (* since the Unix epoch (1970-01-01 00:00:00 UTC) *)
[@@deriving
  bin_io ~localize, compare ~localize, equal ~localize, globalize, hash, typerep]

module Replace_polymorphic_compare_efficient = Span.Replace_polymorphic_compare
include Replace_polymorphic_compare_efficient

include (
  Span :
  sig
  @@ portable
    include Quickcheck.S_int with type t := t
  end)

let[@zero_alloc] now t = Span.since_unix_epoch t
let equal = Span.equal
let min_value_for_1us_rounding = Span.min_value_for_1us_rounding
let max_value_for_1us_rounding = Span.max_value_for_1us_rounding
let epoch = Span.zero
let[@zero_alloc strict] add t1 t2 = Span.(t1 + t2)
let[@zero_alloc strict] sub t1 t2 = Span.(t1 - t2)
let[@zero_alloc strict] diff t1 t2 = Span.(t1 - t2)
let[@zero_alloc strict] abs_diff t u = Span.abs (diff t u)
let[@zero_alloc strict] next t = Span.next t
let[@zero_alloc strict] prev t = Span.prev t
let[@zero_alloc strict] to_span_since_epoch t = t
let[@zero_alloc strict] of_span_since_epoch s = s
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

let[@zero_alloc strict] add_saturating x y =
  let sum = add x y in
  if add_overflowed x y ~sum
  then
    if Span.(y > zero) then Span.max_value_representable else Span.min_value_representable
  else sum
;;

let[@zero_alloc strict] sub_saturating x y =
  let diff = sub x y in
  if sub_overflowed x y ~diff
  then
    if Span.(y > zero) then Span.min_value_representable else Span.max_value_representable
  else diff
;;

let to_int_ns_since_epoch =
  if arch_sixtyfour
  then fun [@zero_alloc] t -> Int63.to_int_exn (to_int63_ns_since_epoch t)
  else fun _ -> failwith "Time_ns.to_int_ns_since_epoch: unsupported on 32bit machines"
;;

let[@zero_alloc] to_int_ns_since_epoch t = to_int_ns_since_epoch t
let[@zero_alloc] of_int_ns_since_epoch i = of_int63_ns_since_epoch (Int63.of_int i)

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
      "Time_ns does not support this time"
      time
      [%sexp_of: Time_float.Stable.With_utc_sexp.V2.t]
;;

let[@inline] [@zero_alloc] of_time_float_round_nearest time =
  of_span_since_epoch
    (Span.of_span_float_round_nearest (Time_float.to_span_since_epoch time))
;;

let[@zero_alloc] of_time_float_round_nearest_microsecond time =
  check_before_conversion_for_1us_rounding time;
  of_span_since_epoch
    (Span.of_span_float_round_nearest_microsecond (Time_float.to_span_since_epoch time))
;;

let[@cold] raise_next_multiple_got_nonpositive_interval ~calling_function_name interval =
  failwiths
    ("Time_ns." ^ calling_function_name ^ " got nonpositive interval")
    interval
    [%sexp_of: Span.t]
;;

let[@zero_alloc] next_multiple_internal
  ~calling_function_name
  ~can_equal_after
  ~base
  ~after
  ~interval
  =
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

module Utc : sig @@ portable
  val to_date_and_span_since_start_of_day : t -> Date0.t * Span.t
  val of_date_and_span_since_start_of_day : Date0.t -> Span.t -> t [@@zero_alloc]
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

  let[@zero_alloc] of_date_and_span_since_start_of_day date span_since_start_of_day =
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
    type nonrec t = t [@@deriving bin_io, compare ~localize, hash]

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

    include%template Sexpable.Of_stringable [@modality portable] (struct
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

  include%template Comparable.Make [@mode local] [@modality portable] (T)

  include Replace_polymorphic_compare_efficient

  include%template Diffable.Atomic.Make [@modality portable] (struct
      type nonrec t = t [@@deriving bin_io, equal ~localize, sexp]
    end)

  module Stable = struct
    module V1 = struct
      module T = struct
        (* see tests in lib/core/test/src/test_time_ns that ensure stability of this
           representation *)
        type nonrec t = t
        [@@deriving
          bin_io ~localize
          , compare ~localize
          , equal ~localize
          , globalize
          , hash
          , sexp
          , sexp_grammar]

        let stable_witness : t Stable_witness.t = Stable_witness.assert_stable

        type nonrec comparator_witness = comparator_witness

        let comparator = comparator
      end

      include T

      include%template
        Comparable.Stable.V1.With_stable_witness.Make [@modality portable] (T)

      include%template Diffable.Atomic.Make [@modality portable] (T)
    end
  end
end

(* this code is directly duplicated from Time.ml functor, converted enough to get Time_ns
   to/of_string working *)
module To_and_of_string : sig @@ portable
  val of_date_ofday
    :  ?prefer:Zone.Earlier_or_later.t
    -> zone:Zone.t
    -> Date.t
    -> Ofday_ns.t
    -> t

  val of_date_ofday_precise
    :  Date.t
    -> Ofday_ns.t
    -> zone:Zone.t
    -> [ `Once of t | `Twice of t * t | `Never of t ]

  val to_date_ofday : t -> zone:Zone.t -> Date.t * Ofday_ns.t

  val to_date_ofday_precise
    :  t
    -> zone:Zone.t
    -> Date.t
       * Ofday_ns.t
       * [ `Only | `Also_at of t | `Also_skipped of Date.t * Ofday_ns.t ]

  val to_date : t -> zone:Zone.t -> Date.t
  val to_ofday : t -> zone:Zone.t -> Ofday_ns.t

  val convert
    :  from_tz:Zone.t
    -> to_tz:Zone.t
    -> Date.t
    -> Ofday_ns.t
    -> Date.t * Ofday_ns.t

  val reset_date_cache : unit -> unit
  val utc_offset : t -> zone:Zone.t -> Span.t
  val of_string_with_utc_offset : string -> t
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

  val to_string_iso8601_extended
    :  ?precision:[ `sec | `ms | `us | `ns ]
    -> zone:Zone.t
    -> t
    -> string

  val occurrence
    :  [ `First_after_or_at | `Last_before_or_at ]
    -> t
    -> ofday:Ofday_ns.t
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
      + Span.to_int63_ns (Ofday_ns.to_span_since_start_of_day ofday)
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
      Span.of_int63_ns remainder |> Ofday_ns.of_span_since_start_of_day_exn
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
    @@ portable
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

    val absolute_time_of_date_and_ofday
      :  ?prefer:Zone.Earlier_or_later.t
      -> t
      -> Date_and_ofday.t
      -> time

    val date_and_ofday_of_absolute_time : t -> time -> Date_and_ofday.t
    val next_clock_shift : t -> strictly_after:time -> (time * Span.t) option
    val next_clock_shift_incl : t -> at_or_after:time -> (time * Span.t) option
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

    let index_of_date_and_ofday ?prefer t relative =
      index_of_date_and_ofday
        ?prefer
        t
        (to_date_and_ofday_in_seconds_round_down_exn relative)
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

    let next_clock_shift_incl t ~at_or_after:time =
      let index = index t time in
      let shift_at_time =
        if index_has_prev_clock_shift t index
        then (
          let shift_time = index_prev_clock_shift_time_exn t index in
          if equal time shift_time
          then Some (shift_time, index_prev_clock_shift_amount_exn t index)
          else None)
        else None
      in
      match shift_at_time with
      | Some _ -> shift_at_time
      | None -> index_next_clock_shift t index
    ;;

    let date_and_ofday_of_absolute_time t time =
      let index = index t time in
      (* no exn because [index] always returns a valid index *)
      let offset_from_utc = index_offset_from_utc_exn t index in
      Date_and_ofday.of_absolute time ~offset_from_utc
    ;;

    let absolute_time_of_date_and_ofday ?prefer t relative =
      let index = index_of_date_and_ofday ?prefer t relative in
      (* no exn because [index_of_date_and_ofday] always returns a valid index *)
      let offset_from_utc = index_offset_from_utc_exn t index in
      Date_and_ofday.to_absolute relative ~offset_from_utc
    ;;
  end

  let of_date_ofday ?prefer ~zone date ofday =
    let relative = Date_and_ofday.of_date_ofday date ofday in
    Zone.absolute_time_of_date_and_ofday ?prefer zone relative
  ;;

  let of_date_ofday_precise date ofday ~zone =
    (* We assume that there will be only one zone shift within a given local day.  *)
    let start_of_day = of_date_ofday ~prefer:Earlier ~zone date Ofday_ns.start_of_day in
    let proposed_time = add start_of_day (Ofday_ns.to_span_since_start_of_day ofday) in
    match Zone.next_clock_shift_incl zone ~at_or_after:start_of_day with
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

  module Shared_date_cache =
    Date_cache.Make
      (struct
        include Span_ns
        module Span = Span_ns
        module Zone = Zone
        module Date_and_ofday = Date_and_ofday
        module Ofday = Ofday_ns

        let add = add
        let sub = sub
        let epoch = epoch
      end)
      ()

  let reset_date_cache = Shared_date_cache.reset
  let to_date time ~zone = Shared_date_cache.get_date time ~zone

  let to_ofday time ~zone =
    let effective_day_start = Shared_date_cache.get_day_start time ~zone in
    diff time effective_day_start |> Ofday_ns.of_span_since_start_of_day_exn
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
      match Ofday_ns.sub ofday amount with
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
        ; Ofday_ns.to_string_trimmed
            (Ofday_ns.of_span_since_start_of_day_exn (Span.abs utc_offset))
        ]
  ;;

  let to_string_abs_parts_with_precision ~precision =
    let attempt time ~zone =
      let date, ofday = to_date_ofday time ~zone in
      let offset_string = offset_string time ~zone in
      let ofday_string =
        match precision with
        | `sec -> Ofday_ns.to_sec_string ofday
        | `ms -> Ofday_ns.to_millisecond_string ofday
        | `us -> Ofday_ns.to_microsecond_string ofday
        | `ns -> Ofday_ns.to_nanosecond_string ofday
      in
      [ Date0.to_string date; String.concat ~sep:"" [ ofday_string; offset_string ] ]
    in
    fun time ~zone ->
      try attempt time ~zone with
      | (_ : exn) ->
        (* If we overflow applying the UTC offset, try again with UTC time. *)
        attempt time ~zone:Zone.utc
  ;;

  let to_string_abs_parts = to_string_abs_parts_with_precision ~precision:`ns

  let to_string_abs_trimmed time ~zone =
    let date, ofday = to_date_ofday time ~zone in
    let offset_string = offset_string time ~zone in
    String.concat
      ~sep:" "
      [ Date0.to_string date; Ofday_ns.to_string_trimmed ofday ^ offset_string ]
  ;;

  let to_string_abs time ~zone = String.concat ~sep:" " (to_string_abs_parts ~zone time)
  let to_string_utc t = to_string_abs t ~zone:Zone.utc

  let to_string_iso8601_extended ?(precision = `ns) ~zone time =
    String.concat ~sep:"T" (to_string_abs_parts_with_precision ~zone ~precision time)
  ;;

  let to_string_iso8601_basic t ~zone = to_string_iso8601_extended t ~precision:`ns ~zone

  let to_string_trimmed t ~zone =
    let date, sec = to_date_ofday ~zone t in
    Date0.to_string date ^ " " ^ Ofday_ns.to_string_trimmed sec
  ;;

  let to_sec_string t ~zone =
    let date, sec = to_date_ofday ~zone t in
    Date0.to_string date ^ " " ^ Ofday_ns.to_sec_string sec
  ;;

  let to_sec_string_with_zone t ~zone = to_sec_string t ~zone ^ offset_string t ~zone

  let to_filename_string t ~zone =
    let date, ofday = to_date_ofday ~zone t in
    Date0.to_string date
    ^ "_"
    ^ String.tr
        ~target:':'
        ~replacement:'-'
        (String.drop_suffix (Ofday_ns.to_string ofday) 3)
  ;;

  let of_filename_string s ~zone =
    try
      match String.lsplit2 s ~on:'_' with
      | None -> failwith "no space in filename string"
      | Some (date, ofday) ->
        let date = Date0.of_string date in
        let ofday = String.tr ~target:'-' ~replacement:':' ofday in
        let ofday = Ofday_ns.of_string ofday in
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
        let ofday = Ofday_ns.of_string time in
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
      let ofday_to_sec od = Span.to_sec (Ofday_ns.to_span_since_start_of_day od) in
      let ofday, utc_offset =
        match tz with
        | Some _ -> ofday, None
        | None ->
          if Char.( = ) ofday.[String.length ofday - 1] 'Z'
          then String.sub ofday ~pos:0 ~len:(String.length ofday - 1), Some 0.
          else (
            match String.lsplit2 ~on:'+' ofday with
            | Some (l, r) ->
              l, Some (ofday_to_sec (Ofday_ns.of_string (ensure_colon_in_offset r)))
            | None ->
              (match String.lsplit2 ~on:'-' ofday with
               | Some (l, r) ->
                 ( l
                 , Some
                     (-1. *. ofday_to_sec (Ofday_ns.of_string (ensure_colon_in_offset r)))
                 )
               | None -> ofday, None))
      in
      let date = Date0.of_string date in
      let ofday = Ofday_ns.of_string ofday in
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
      failwithf "unable to lookup Zone %s. Try using Core.Time_ns.of_string" zone_name ()
    in
    of_string_gen ~default_zone ~find_zone s
  ;;
end

include To_and_of_string

let to_string t = to_string_abs t ~zone:(Portable_lazy.force Timezone.local_portable)

exception Time_string_not_absolute of string [@@deriving sexp]

let of_string_gen ~if_no_timezone ?(find_zone = Timezone.find_exn) s =
  let default_zone () : Zone.t =
    match if_no_timezone with
    | `Fail -> raise (Time_string_not_absolute s)
    | `Local -> Portable_lazy.force Timezone.local_portable
    | `Use_this_one zone -> zone
    | `Use_this_one_lazy zone -> Lazy.force zone
  in
  of_string_gen ~default_zone ~find_zone s
;;

let of_string_abs s = of_string_gen ~if_no_timezone:`Fail s
let of_string s = of_string_gen ~if_no_timezone:`Local s
let%template arg_type = (Command.Arg_type.create [@mode portable]) of_string_abs

module Ofday = struct
  include Ofday_ns

  let%template arg_type = (Command.Arg_type.create [@mode portable]) of_string

  let[@zero_alloc] of_ofday_float_round_nearest_microsecond core =
    of_span_since_start_of_day_exn
      (Span.of_span_float_round_nearest_microsecond
         (Time_float.Ofday.to_span_since_start_of_day core))
  ;;

  let[@zero_alloc] of_ofday_float_round_nearest core =
    of_span_since_start_of_day_exn
      (Span.of_span_float_round_nearest
         (Time_float.Ofday.to_span_since_start_of_day core))
  ;;

  let of_time time ~zone = to_ofday time ~zone

  let to_ofday_float_round_nearest_microsecond t =
    Time_float.Ofday.of_span_since_start_of_day_exn
      (Span.to_span_float_round_nearest_microsecond (to_span_since_start_of_day t))
  ;;

  let to_ofday_float_round_nearest t =
    Time_float.Ofday.of_span_since_start_of_day_exn
      (Span.to_span_float_round_nearest (to_span_since_start_of_day t))
  ;;

  let now ~zone = of_time (now ()) ~zone

  (* Legacy conversions that round to the nearest microsecond *)
  let to_ofday = to_ofday_float_round_nearest_microsecond
  let of_ofday = of_ofday_float_round_nearest_microsecond

  module Zoned = struct
    type t =
      { ofday : Ofday_ns.t
      ; zone : Timezone.t
      }
    [@@deriving
      bin_io, fields ~getters ~local_getters, compare ~localize, equal ~localize, hash]

    type sexp_repr = Ofday_ns.t * Timezone.t [@@deriving sexp, sexp_grammar]

    let sexp_of_t t = [%sexp_of: sexp_repr] (t.ofday, t.zone)

    let t_of_sexp sexp =
      let ofday, zone = [%of_sexp: sexp_repr] sexp in
      { ofday; zone }
    ;;

    let t_sexp_grammar = Sexplib.Sexp_grammar.coerce [%sexp_grammar: sexp_repr]
    let to_time_ns t date = of_date_ofday ~zone:(zone t) date (ofday t)
    let create ofday zone = { ofday; zone }
    let create_local ofday = create ofday (Portable_lazy.force Timezone.local_portable)

    let of_string string : t =
      match String.rsplit2 string ~on:' ' with
      | Some (ofday, zone) ->
        { ofday = Ofday_ns.of_string ofday; zone = Timezone.of_string zone }
      | None -> failwithf "Ofday.Zoned.of_string %s" string ()
    ;;

    let to_string (t : t) : string =
      String.concat [ Ofday_ns.to_string t.ofday; " "; Timezone.to_string t.zone ]
    ;;

    let%template arg_type = (Command.Arg_type.create [@mode portable]) of_string

    module With_nonchronological_compare = struct
      type nonrec t = t
      [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp, sexp_grammar]
    end

    include%template Pretty_printer.Register [@modality portable] (struct
        type nonrec t = t

        let to_string = to_string
        let module_name = "Core.Time_ns.Ofday.Zoned"
      end)

    module Stable = struct
      module V1 = struct
        [%%rederive
          type t = With_nonchronological_compare.t
          [@@deriving compare ~localize ~portable, equal ~localize ~portable]]

        module Bin_repr = struct
          type nonrec t = t =
            { ofday : Ofday_ns.Stable.V1.t
            ; zone : Timezone.Stable.V1.t
            }
          [@@deriving bin_io ~localize, stable_witness]
        end

        include%template
          Binable.Of_binable_without_uuid
            [@mode local]
            [@modality portable]
            [@alert "-legacy"]
            (Bin_repr)
            (struct
              type nonrec t = t

              let%template[@alloc a @ m = (heap_global, stack_local)] to_binable t
                : Bin_repr.t
                =
                { ofday = (ofday [@mode m]) t; zone = (zone [@mode m]) t }
                [@exclave_if_stack a]
              ;;

              let%template[@mode local] to_binable = (to_binable [@alloc stack])
              let of_binable (repr : Bin_repr.t) = create repr.ofday repr.zone
            end)

        type nonrec t = t [@@deriving hash]

        let stable_witness : t Stable_witness.t = Bin_repr.stable_witness

        type sexp_repr = Ofday_ns.Stable.V1.t * Timezone.Stable.V1.t
        [@@deriving sexp, sexp_grammar]

        let sexp_of_t t = [%sexp_of: sexp_repr] (ofday t, zone t)

        let t_of_sexp sexp =
          let ofday, zone = [%of_sexp: sexp_repr] sexp in
          create ofday zone
        ;;

        let t_sexp_grammar = Sexplib.Sexp_grammar.coerce [%sexp_grammar: sexp_repr]
      end
    end
  end

  module Option = struct
    type ofday = t [@@deriving sexp, compare ~localize]

    type t = Span.Option.t
    [@@deriving
      bin_io ~localize, compare ~localize, equal ~localize, globalize, hash, typerep]

    let none = Span.Option.none
    let[@zero_alloc] some t = Span.Option.some (to_span_since_start_of_day t)
    let[@zero_alloc] is_none t = Span.Option.is_none t
    let[@zero_alloc] is_some t = Span.Option.is_some t

    let[@zero_alloc] some_is_representable t =
      Span.Option.some_is_representable (to_span_since_start_of_day t)
    ;;

    let[@zero_alloc] value t ~default =
      Bool.select
        (is_none t)
        default
        (of_span_since_start_of_day_unchecked (Span.Option.unchecked_value t))
    ;;

    let[@zero_alloc] of_span_since_start_of_day span =
      if span_since_start_of_day_is_valid span then Span.Option.some span else none
    ;;

    let[@zero_alloc] value_exn t =
      if is_some t
      then of_span_since_start_of_day_unchecked (Span.Option.unchecked_value t)
      else raise_s [%message [%here] "Time_ns.Ofday.Option.value_exn none"]
    ;;

    let[@zero_alloc] unchecked_value t =
      of_span_since_start_of_day_unchecked (Span.Option.unchecked_value t)
    ;;

    let[@zero_alloc] of_option = function
      | None -> none
      | Some t -> some t
    ;;

    let to_option t = if is_none t then None else Some (value_exn t)

    (* Can't use the quickcheck generator and shrinker inherited from [Span.Option]
       because they may produce spans whose representation is larger than
       [start_of_next_day] *)
    let%template quickcheck_generator : t Quickcheck.Generator.t =
      (Base_quickcheck.Generator.map [@mode portable])
        ~f:of_option
        ((quickcheck_generator_option [@mode portable])
           ((Base_quickcheck.Generator.filter [@mode portable])
              ~f:some_is_representable
              Ofday_ns.quickcheck_generator))
    ;;

    let%template quickcheck_shrinker : t Quickcheck.Shrinker.t =
      (Quickcheck.Shrinker.map [@mode portable])
        ~f:of_option
        ~f_inverse:to_option
        ((quickcheck_shrinker_option [@mode portable])
           ((Base_quickcheck.Shrinker.filter [@mode portable])
              ~f:some_is_representable
              Ofday_ns.quickcheck_shrinker))
    ;;

    let quickcheck_observer = Span.Option.quickcheck_observer

    module Optional_syntax = struct
      module Optional_syntax = struct
        let[@zero_alloc] is_none t = is_none t
        let[@zero_alloc] unsafe_value t = unchecked_value t
      end
    end

    module Stable = struct
      module V1 = struct
        module T = struct
          type nonrec t = t
          [@@deriving bin_io ~localize, compare ~localize, equal ~localize, globalize]

          let stable_witness : t Stable_witness.t = Stable_witness.assert_stable
          let sexp_of_t t = [%sexp_of: Ofday_ns.Stable.V1.t option] (to_option t)
          let t_of_sexp s = of_option ([%of_sexp: Ofday_ns.Stable.V1.t option] s)
          let to_int63 t = Span.Option.Stable.V1.to_int63 t
          let of_int63_exn t = Span.Option.Stable.V1.of_int63_exn t
        end

        include T

        include%template Comparator.Stable.V1.Make [@modality portable] (T)

        include%template Diffable.Atomic.Make [@modality portable] (struct
            type nonrec t = t [@@deriving sexp, bin_io, equal ~localize]
          end)
      end
    end

    let sexp_of_t = Stable.V1.sexp_of_t
    let t_of_sexp = Stable.V1.t_of_sexp

    include%template Identifiable.Make [@mode local] [@modality portable] (struct
        type nonrec t = t [@@deriving sexp, compare ~localize, bin_io ~localize, hash]

        let module_name = "Core.Time_ns.Ofday.Option"

        include%template Sexpable.To_stringable [@modality portable] (struct
            type nonrec t = t [@@deriving sexp]
          end)
      end)

    include (
      Span.Option :
      sig
      @@ portable
        include Comparisons.S with type t := t
      end)

    include%template Diffable.Atomic.Make [@modality portable] (struct
        type nonrec t = t [@@deriving sexp, bin_io, equal ~localize]
      end)
  end
end

let get_sexp_zone = Time_float.get_sexp_zone
let set_sexp_zone = Time_float.set_sexp_zone

let t_of_sexp_gen ~if_no_timezone sexp =
  try
    match sexp with
    | Sexp.List [ Sexp.Atom date; Sexp.Atom ofday; Sexp.Atom tz ] ->
      of_date_ofday
        ~zone:(Timezone.find_exn tz)
        (Date.of_string date)
        (Ofday.of_string ofday)
    (* This is actually where the output of [sexp_of_t] is handled, since that's e.g.
       (2015-07-06 09:09:44.787988+01:00). *)
    | Sexp.List [ Sexp.Atom date; Sexp.Atom ofday_and_possibly_zone ] ->
      of_string_gen ~if_no_timezone (date ^ " " ^ ofday_and_possibly_zone)
    | Sexp.Atom datetime -> of_string_gen ~if_no_timezone datetime
    | _ -> of_sexp_error "Time.t_of_sexp" sexp
  with
  | Of_sexp_error _ as e -> raise e
  | e -> of_sexp_error (sprintf "Time.t_of_sexp: %s" (Exn.to_string e)) sexp
;;

let t_of_sexp sexp = t_of_sexp_gen sexp ~if_no_timezone:(`Use_this_one (get_sexp_zone ()))
let t_of_sexp_abs sexp = t_of_sexp_gen sexp ~if_no_timezone:`Fail

let t_sexp_grammar : t Sexplib.Sexp_grammar.t =
  { untyped =
      Union
        [ String
        ; List (Cons (String, Cons (String, Empty)))
        ; List (Cons (String, Cons (String, Cons (String, Empty))))
        ]
  }
;;

let sexp_of_t_abs t ~zone =
  Sexp.List (List.map (to_string_abs_parts ~zone t) ~f:(fun s -> Sexp.Atom s))
;;

let sexp_of_t t = sexp_of_t_abs ~zone:(get_sexp_zone ()) t
let of_date_ofday_zoned date ofday_zoned = Ofday.Zoned.to_time_ns ofday_zoned date

let to_date_ofday_zoned t ~zone =
  let date, ofday = to_date_ofday t ~zone in
  date, Ofday.Zoned.create ofday zone
;;

let to_ofday_zoned t ~zone =
  let ofday = to_ofday t ~zone in
  Ofday.Zoned.create ofday zone
;;

include%template Diffable.Atomic.Make [@modality portable] (struct
    type nonrec t = t [@@deriving bin_io, equal ~localize, sexp]
  end)

(* Note: This is FIX standard millisecond precision. You should use
   [Zero.Time_ns_with_fast_accurate_to_of_string] if you need nanosecond precision. *)
let to_string_fix_proto zone t =
  Time_float.to_string_fix_proto zone (to_time_float_round_nearest_microsecond t)
;;

let of_string_fix_proto zone s =
  of_time_float_round_nearest_microsecond (Time_float.of_string_fix_proto zone s)
;;

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
             |> Option.value_map ~f:Span.to_unit_of_time ~default:Unit_of_time.Nanosecond
           in
           spans |> List.map ~f:(Span.to_string_hum ~unit_of_time ~align_decimal:true)
         ;;
       end)
  ;;
end

module Stable0 = struct
  module V1 = struct
    module T0 = struct
      (* We use the unstable serialization here, and rely on comprehensive tests of the
         stable conversion to make sure we don't change it. *)

      type nonrec t = t
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , quickcheck
        , sexp
        , sexp_grammar]

      let stable_witness : t Stable_witness.t = Stable_witness.assert_stable
      let of_int63_exn t = of_span_since_epoch (Span.of_int63_ns t)
      let to_int63 t = to_int63_ns_since_epoch t
    end

    module T = struct
      include T0
      module%template Comparator = Comparator.Stable.V1.Make [@modality portable] (T0)
      include Comparator
    end

    include T

    include%template Comparable.Stable.V1.With_stable_witness.Make [@modality portable] (T)
    include%template Diffable.Atomic.Make [@modality portable] (T)
  end
end

include Stable0.V1.Comparator

module Option = struct
  type time = t [@@deriving compare ~localize]

  type t = Span.Option.t
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , hash
    , typerep
    , quickcheck]

  let none = Span.Option.none
  let[@zero_alloc] some time = Span.Option.some (to_span_since_epoch time)
  let[@zero_alloc] is_none t = Span.Option.is_none t
  let[@zero_alloc] is_some t = Span.Option.is_some t

  let[@zero_alloc] some_is_representable time =
    Span.Option.some_is_representable (to_span_since_epoch time)
  ;;

  let[@zero_alloc] value t ~default =
    of_span_since_epoch (Span.Option.value ~default:(to_span_since_epoch default) t)
  ;;

  let[@zero_alloc] value_exn t =
    if is_some t
    then of_span_since_epoch (Span.Option.unchecked_value t)
    else raise_s [%message [%here] "Time_ns.Option.value_exn none"]
  ;;

  let[@zero_alloc] unchecked_value t = of_span_since_epoch (Span.Option.unchecked_value t)

  let[@zero_alloc] of_option = function
    | None -> none
    | Some t -> some t
  ;;

  let to_option t = if is_none t then None else Some (value_exn t)

  module Optional_syntax = struct
    module Optional_syntax = struct
      let[@zero_alloc] is_none t = is_none t
      let[@zero_alloc] unsafe_value t = unchecked_value t
    end
  end

  module Alternate_sexp = struct
    module T = struct
      type nonrec t = t [@@deriving bin_io, compare ~localize, hash]

      let sexp_of_t t = [%sexp_of: Alternate_sexp.t option] (to_option t)
      let t_of_sexp s = of_option ([%of_sexp: Alternate_sexp.t option] s)

      let t_sexp_grammar =
        Sexplib.Sexp_grammar.coerce [%sexp_grammar: Alternate_sexp.t option]
      ;;
    end

    include T

    include%template Comparable.Make [@mode local] [@modality portable] (T)

    include%template Diffable.Atomic.Make [@modality portable] (struct
        include T

        let equal = [%compare.equal: t]
      end)

    module Stable = struct
      module V1 = struct
        module T = struct
          type nonrec t = t
          [@@deriving
            bin_io ~localize
            , compare ~localize
            , equal ~localize
            , globalize
            , hash
            , sexp
            , sexp_grammar]

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

        include%template
          Comparable.Stable.V1.With_stable_witness.Make [@modality portable] (T)

        include%template Diffable.Atomic.Make [@modality portable] (struct
            include T

            let equal = [%compare.equal: t]
          end)
      end
    end
  end

  module Stable = struct
    module V1 = struct
      module T = struct
        type nonrec t = t
        [@@deriving
          bin_io ~localize, compare ~localize, equal ~localize, globalize, typerep]

        let sexp_of_t t = [%sexp_of: Stable0.V1.t option] (to_option t)
        let t_of_sexp s = of_option ([%of_sexp: Stable0.V1.t option] s)
      end

      include T

      include%template Comparator.Stable.V1.Make [@modality portable] (T)

      let stable_witness : t Stable_witness.t = Stable_witness.assert_stable
      let to_int63 t = Span.Option.Stable.V1.to_int63 t
      let of_int63_exn t = Span.Option.Stable.V1.of_int63_exn t

      include%template Diffable.Atomic.Make [@modality portable] (struct
          include T

          let equal = [%compare.equal: t]
        end)
    end

    module Alternate_sexp = Alternate_sexp.Stable
  end

  let sexp_of_t = Stable.V1.sexp_of_t
  let t_of_sexp = Stable.V1.t_of_sexp

  include%template Identifiable.Make [@mode local] [@modality portable] (struct
      type nonrec t = t [@@deriving sexp, compare ~localize, bin_io ~localize, hash]

      let module_name = "Core.Time_ns.Option"

      include%template Sexpable.To_stringable [@modality portable] (struct
          type nonrec t = t [@@deriving sexp]
        end)
    end)

  (* bring back the efficient implementation of comparison operators *)
  include (
  struct
    include Span.Option

    let[@zero_alloc] ( >= ) = [%eta2 ( >= )]
    let[@zero_alloc] ( <= ) = [%eta2 ( <= )]
    let[@zero_alloc] ( = ) = [%eta2 ( = )]
    let[@zero_alloc] ( > ) = [%eta2 ( > )]
    let[@zero_alloc] ( < ) = [%eta2 ( < )]
    let[@zero_alloc] ( <> ) = [%eta2 ( <> )]

    [%%template
    [@@@mode.default m = (local, global)]

    let[@zero_alloc] equal = [%eta2 equal [@mode m]]
    let[@zero_alloc] compare = [%eta2 compare [@mode m]]]

    let[@zero_alloc] min = [%eta2 min]
    let[@zero_alloc] max = [%eta2 max]
  end :
  sig
  @@ portable
    include%template Comparisons.S_with_zero_alloc [@mode local] with type t := t
  end)

  include%template Diffable.Atomic.Make [@modality portable] (struct
      type nonrec t = t [@@deriving bin_io, equal ~localize, sexp]
    end)
end

include%template
  Identifiable.Make_using_comparator [@mode local] [@modality portable] (struct
    include Stable0.V1

    let module_name = "Core.Time_ns"
    let of_string, to_string = of_string, to_string
  end)

(* bring back the efficient implementation of comparison operators *)
include (
struct
  include Replace_polymorphic_compare_efficient

  let[@zero_alloc strict] ( >= ) = [%eta2 ( >= )]
  let[@zero_alloc strict] ( <= ) = [%eta2 ( <= )]
  let[@zero_alloc strict] ( = ) = [%eta2 ( = )]
  let[@zero_alloc strict] ( > ) = [%eta2 ( > )]
  let[@zero_alloc strict] ( < ) = [%eta2 ( < )]
  let[@zero_alloc strict] ( <> ) = [%eta2 ( <> )]

  [%%template
  [@@@mode.default m = (local, global)]

  let[@zero_alloc strict] equal = [%eta2 equal [@mode m]]
  let[@zero_alloc strict] compare = [%eta2 compare [@mode m]]]

  let[@zero_alloc strict] min = [%eta2 min]
  let[@zero_alloc strict] max = [%eta2 max]
end :
sig
@@ portable
  include%template Comparisons.S_with_zero_alloc_strict [@mode local] with type t := t
end)

module Zone = Time_float.Zone

module Stable = struct
  include Stable0
  module Option = Option.Stable
  module Alternate_sexp = Alternate_sexp.Stable

  module Span = struct
    include Span.Stable
    module Option = Span.Option.Stable
  end

  module Ofday = struct
    include Ofday_ns.Stable
    module Zoned = Ofday.Zoned.Stable
    module Option = Ofday.Option.Stable
  end

  module Zone = Timezone.Stable
end

let interruptible_pause = `Use_Time_ns_unix
let pause = `Use_Time_ns_unix
let pause_forever = `Use_Time_ns_unix

module O = struct
  let[@zero_alloc strict] ( >= ) = [%eta2 ( >= )]
  let[@zero_alloc strict] ( <= ) = [%eta2 ( <= )]
  let[@zero_alloc strict] ( = ) = [%eta2 ( = )]
  let[@zero_alloc strict] ( > ) = [%eta2 ( > )]
  let[@zero_alloc strict] ( < ) = [%eta2 ( < )]
  let[@zero_alloc strict] ( <> ) = [%eta2 ( <> )]
  let[@zero_alloc strict] ( + ) = [%eta2 add]
  let[@zero_alloc strict] ( - ) = [%eta2 diff]
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
