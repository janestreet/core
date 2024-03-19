open! Import
module Span = Span_ns

type underlying = Int63.t
type t = Span.t (* since wall-clock midnight *) [@@deriving typerep]

include (Span : Robustly_comparable.S with type t := t)

let to_parts t = Span.to_parts t
let start_of_day : t = Span.zero
let start_of_next_day : t = Span.day
let approximate_end_of_day = Span.( - ) start_of_next_day Span.nanosecond
let to_span_since_start_of_day t = t

let[@cold] input_out_of_bounds span =
  raise_s
    [%message
      "Time_ns.Ofday.of_span_since_start_of_day_exn: input out of bounds"
        ~_:(span : Span.t)]
;;

let[@inline always] is_invalid span =
  (* Why we use [Span.( > )] rather than [( >= )] below:

     We allow to represent the end-of-day sentinel value ([24.000000000h]), which is not
     itself a valid clock face time.  However, since valid clock face times readily
     round up to it, it's better to allow it to be represented. *)
  Span.( < ) span start_of_day || Span.( > ) span start_of_next_day
;;

let span_since_start_of_day_is_valid span = not (is_invalid span)
let of_span_since_start_of_day_unchecked span = span

let of_span_since_start_of_day_exn span =
  if is_invalid span then input_out_of_bounds span else span
;;

let of_span_since_start_of_day_opt span = if is_invalid span then None else Some span
let add_exn t span = of_span_since_start_of_day_exn (Span.( + ) t span)
let sub_exn t span = of_span_since_start_of_day_exn (Span.( - ) t span)
let add t span = of_span_since_start_of_day_opt (Span.( + ) t span)
let sub t span = of_span_since_start_of_day_opt (Span.( - ) t span)
let next t = of_span_since_start_of_day_opt (Span.next t)
let prev t = of_span_since_start_of_day_opt (Span.prev t)
let diff t u = Span.( - ) t u

let create ?hr ?min ?sec ?ms ?us ?ns () =
  (* Similar to [Time.Ofday.create], if we detect a leap second we strip off all
     sub-second elements so that HH:MM:60.XXXXXXXXX is all mapped to HH:MM:60. *)
  let ms, us, ns =
    match sec with
    | Some 60 -> Some 0, Some 0, Some 0
    | _ -> ms, us, ns
  in
  of_span_since_start_of_day_exn (Span.create ?hr ?min ?sec ?ms ?us ?ns ())
;;

module Stable = struct
  module Option = struct end
  module Zoned = struct end

  module V1 = struct
    type t = Span.Stable.V2.t [@@deriving bin_io, compare, equal, hash, stable_witness]

    include (
      Span.Stable.V2 :
        Comparator.S
          with type t := t
           and type comparator_witness = Span.Stable.V2.comparator_witness)

    let to_string_with_unit =
      let ( / ) = Int63.( / ) in
      let ( mod ) = Int63.rem in
      let ( ! ) = Int63.of_int in
      let i = Int63.to_int_exn in
      fun t ~unit ->
        if Span.( < ) t start_of_day || Span.( < ) start_of_next_day t
        then "Incorrect day"
        else (
          let sixty = !60 in
          let thousand = !1000 in
          let ns = Span.to_int63_ns t in
          let us = ns / thousand in
          let ns = ns mod thousand |> i in
          let ms = us / thousand in
          let us = us mod thousand |> i in
          let s = ms / thousand in
          let ms = ms mod thousand |> i in
          let m = s / sixty in
          let s = s mod sixty |> i in
          let h = m / sixty |> i in
          let m = m mod sixty |> i in
          let unit =
            match unit with
            | (`Nanosecond | `Microsecond | `Millisecond | `Second) as unit -> unit
            | `Minute_or_less ->
              if ns <> 0
              then `Nanosecond
              else if us <> 0
              then `Microsecond
              else if ms <> 0
              then `Millisecond
              else if s <> 0
              then `Second
              else `Minute
          in
          let len =
            match unit with
            | `Minute -> 5
            | `Second -> 8
            | `Millisecond -> 12
            | `Microsecond -> 15
            | `Nanosecond -> 18
          in
          let str = Bytes.create len in
          Digit_string_helpers.write_2_digit_int str ~pos:0 h;
          Bytes.set str 2 ':';
          Digit_string_helpers.write_2_digit_int str ~pos:3 m;
          (match unit with
           | `Minute -> ()
           | (`Second | `Millisecond | `Microsecond | `Nanosecond) as unit ->
             Bytes.set str 5 ':';
             Digit_string_helpers.write_2_digit_int str ~pos:6 s;
             (match unit with
              | `Second -> ()
              | (`Millisecond | `Microsecond | `Nanosecond) as unit ->
                Bytes.set str 8 '.';
                Digit_string_helpers.write_3_digit_int str ~pos:9 ms;
                (match unit with
                 | `Millisecond -> ()
                 | (`Microsecond | `Nanosecond) as unit ->
                   Digit_string_helpers.write_3_digit_int str ~pos:12 us;
                   (match unit with
                    | `Microsecond -> ()
                    | `Nanosecond -> Digit_string_helpers.write_3_digit_int str ~pos:15 ns))));
          Bytes.unsafe_to_string ~no_mutation_while_string_reachable:str)
    ;;

    let parse_nanoseconds string ~pos ~until =
      let open Int.O in
      let digits = ref 0 in
      let num_digits = ref 0 in
      let pos = ref pos in
      (* read up to 10 digits; store the first 9, use the 10th to round *)
      while !pos < until && !num_digits < 10 do
        let c = string.[!pos] in
        if Char.is_digit c
        then (
          incr num_digits;
          if !num_digits < 10
          then digits := (!digits * 10) + Char.get_digit_exn c
          else if Char.get_digit_exn c >= 5
          then incr digits
          else ());
        incr pos
      done;
      (* if there are missing digits, add zeroes *)
      if !num_digits < 9 then digits := !digits * Int.pow 10 (9 - !num_digits);
      !digits
    ;;

    let create_from_parsed string ~hr ~min ~sec ~subsec_pos ~subsec_len =
      let nanoseconds =
        if Int.equal subsec_len 0
        then 0
        else
          parse_nanoseconds string ~pos:(subsec_pos + 1) ~until:(subsec_pos + subsec_len)
      in
      Span.of_int63_ns (Int63.of_int nanoseconds)
      |> Span.( + ) (Span.scale_int Span.second sec)
      |> Span.( + ) (Span.scale_int Span.minute min)
      |> Span.( + ) (Span.scale_int Span.hour hr)
      |> of_span_since_start_of_day_exn
    ;;

    let of_string string = Ofday_helpers.parse string ~f:create_from_parsed

    let t_of_sexp sexp : t =
      match sexp with
      | Sexp.List _ -> of_sexp_error "expected an atom" sexp
      | Sexp.Atom s ->
        (try of_string s with
         | exn -> of_sexp_error_exn exn sexp)
    ;;

    let t_sexp_grammar =
      let open Sexplib in
      Sexp_grammar.tag
        (Sexp_grammar.coerce String.t_sexp_grammar : t Sexp_grammar.t)
        ~key:Sexp_grammar.type_name_tag
        ~value:(Atom "Core.Time_ns.Ofday.t")
    ;;

    let to_string (t : t) = to_string_with_unit t ~unit:`Nanosecond
    let sexp_of_t (t : t) = Sexp.Atom (to_string t)
    let to_int63 t = Span_ns.Stable.V2.to_int63 t
    let of_int63_exn t = of_span_since_start_of_day_exn (Span_ns.Stable.V2.of_int63_exn t)

    include Diffable.Atomic.Make (struct
      type nonrec t = t [@@deriving bin_io, equal, sexp]
    end)
  end
end

include (
  Stable.V1 :
    Comparator.S
      with type t := t
       and type comparator_witness = Stable.V1.comparator_witness)

include Identifiable.Make_using_comparator (struct
  type t = Stable.V1.t [@@deriving bin_io, compare, hash, sexp]

  include (
    Stable.V1 :
      Comparator.S
        with type t := t
         and type comparator_witness = Stable.V1.comparator_witness)

  include (Stable.V1 : Stringable.S with type t := t)

  let module_name = "Core.Time_ns.Ofday"
end)

include Diffable.Atomic.Make (Stable.V1)

let t_sexp_grammar = Sexplib.Sexp_grammar.coerce Stable.V1.t_sexp_grammar
let to_microsecond_string t = Stable.V1.to_string_with_unit t ~unit:`Microsecond
let to_millisecond_string t = Stable.V1.to_string_with_unit t ~unit:`Millisecond
let to_sec_string t = Stable.V1.to_string_with_unit t ~unit:`Second
let to_string_trimmed t = Stable.V1.to_string_with_unit t ~unit:`Minute_or_less

let of_string_iso8601_extended ?pos ?len str =
  try
    Ofday_helpers.parse_iso8601_extended ?pos ?len str ~f:Stable.V1.create_from_parsed
  with
  | exn ->
    raise_s
      [%message
        "Time_ns.Ofday.of_string_iso8601_extended: cannot parse string"
          ~_:(String.subo str ?pos ?len : string)
          ~_:(exn : exn)]
;;

let every =
  let rec every_valid_ofday_span span ~start ~stop ~acc =
    (* Assumes [span], [start], and [stop] are valid ofdays. Assumes [start < stop].
       Assumes [span > 0]. *)
    let acc = start :: acc in
    let start = Span.( + ) start span in
    if Span.( > ) start stop (* cannot overflow *)
    then List.rev acc
    else every_valid_ofday_span span ~start ~stop ~acc
  in
  (* internal [every] named to show up in stack traces *)
  let every span ~start ~stop =
    if Span.( > ) start stop
    then
      Or_error.error_s
        [%message
          "[Time_ns.Ofday.every] called with [start] > [stop]" (start : t) (stop : t)]
    else if Span.( <= ) span Span.zero
    then
      Or_error.error_s
        [%message "[Time_ns.Ofday.every] called with negative span" ~_:(span : Span.t)]
    else if is_invalid span
    then Ok [ start ]
    else Ok (every_valid_ofday_span span ~start ~stop ~acc:[])
  in
  every
;;

let small_diff =
  let hour = Span.to_int63_ns Span.hour in
  fun ofday1 ofday2 ->
    let open Int63.O in
    let ofday1 = Span.to_int63_ns (to_span_since_start_of_day ofday1) in
    let ofday2 = Span.to_int63_ns (to_span_since_start_of_day ofday2) in
    let diff = ofday1 - ofday2 in
    (*  d1 is in (-hour; hour) *)
    let d1 = Int63.rem diff hour in
    (*  d2 is in (0;hour) *)
    let d2 = Int63.rem (d1 + hour) hour in
    let d = if d2 > hour / Int63.of_int 2 then d2 - hour else d2 in
    Span.of_int63_ns d
;;

let%expect_test "small_diff" =
  let test x y =
    let diff = small_diff x y in
    printf !"small_diff %s %s = %s\n" (to_string x) (to_string y) (Span.to_string diff)
  in
  let examples =
    List.map
      ~f:(fun (x, y) -> of_string x, of_string y)
      [ "12:00", "12:05"; "12:58", "13:02"; "00:52", "23:19"; "00:00", "24:00" ]
  in
  List.iter examples ~f:(fun (x, y) ->
    test x y;
    test y x);
  [%expect
    {|
    small_diff 12:00:00.000000000 12:05:00.000000000 = -5m
    small_diff 12:05:00.000000000 12:00:00.000000000 = 5m
    small_diff 12:58:00.000000000 13:02:00.000000000 = -4m
    small_diff 13:02:00.000000000 12:58:00.000000000 = 4m
    small_diff 00:52:00.000000000 23:19:00.000000000 = -27m
    small_diff 23:19:00.000000000 00:52:00.000000000 = 27m
    small_diff 00:00:00.000000000 24:00:00.000000000 = 0s
    small_diff 24:00:00.000000000 00:00:00.000000000 = 0s
    |}]
;;

let gen_incl = Span.gen_incl
let gen_uniform_incl = Span.gen_uniform_incl
let quickcheck_generator = gen_incl start_of_day start_of_next_day
let quickcheck_observer = Span.quickcheck_observer
let quickcheck_shrinker = Quickcheck.Shrinker.empty ()

include (Span : Comparisons.S with type t := t)

(* deprecated bindings *)
let of_span_since_start_of_day = of_span_since_start_of_day_exn
let to_millisec_string = to_millisecond_string
let arg_type = `Use_Time_ns_unix
let now = `Use_Time_ns_unix
let of_ofday_float_round_nearest = `Use_Time_ns_unix
let of_ofday_float_round_nearest_microsecond = `Use_Time_ns_unix
let to_ofday_float_round_nearest = `Use_Time_ns_unix
let to_ofday_float_round_nearest_microsecond = `Use_Time_ns_unix

module Option = struct end
module Zoned = struct end
