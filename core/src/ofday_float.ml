open! Import
open Std_internal
open Digit_string_helpers
open! Int.Replace_polymorphic_compare
module Span = Span_float

(* Create an abstract type for Ofday to prevent us from confusing it with
   other floats.
*)
module Stable = struct
  module V1 = struct
    module T : sig
      type underlying = float
      type t = private underlying [@@deriving bin_io, hash, typerep, stable_witness]

      include Comparable.S_common with type t := t
      include Robustly_comparable with type t := t
      include Floatable with type t := t

      val add : t -> Span.t -> t option
      val sub : t -> Span.t -> t option
      val next : t -> t option
      val prev : t -> t option
      val diff : t -> t -> Span.t
      val of_span_since_start_of_day_exn : Span.t -> t
      val of_span_since_start_of_day_unchecked : Span.t -> t
      val span_since_start_of_day_is_valid : Span.t -> bool
      val to_span_since_start_of_day : t -> Span.t
      val start_of_day : t
      val start_of_next_day : t
    end = struct
      (* Number of seconds since midnight. *)
      type underlying = Float.t

      include (
        struct
          include Float

          let sign = sign_exn

          let stable_witness : t Stable_witness.t =
            Stable_witness.Export.stable_witness_float
          ;;
        end :
          sig
            type t = underlying [@@deriving bin_io, hash, typerep, stable_witness]

            include Comparable.S_common with type t := t
            include Comparable.With_zero with type t := t
            include Robustly_comparable with type t := t
            include Floatable with type t := t
          end)

      (* IF THIS REPRESENTATION EVER CHANGES, ENSURE THAT EITHER
         (1) all values serialize the same way in both representations, or
         (2) you add a new Time.Ofday version to stable.ml *)

      (* due to precision limitations in float we can't expect better than microsecond
         precision *)
      include Float.Robust_compare.Make (struct
        let robust_comparison_tolerance = 1E-6
      end)

      let to_span_since_start_of_day t = Span.of_sec t

      (* Another reasonable choice would be only allowing Ofday.t to be < 24hr, but this
         choice was made early on and people became used to being able to easily call 24hr
         the end of the day.  It's a bit sad because it shares that moment with the
         beginning of the next day, and round trips oddly if passed through
         Time.to_date_ofday/Time.of_date_ofday.

         Note: [Schedule.t] requires that the end of day be representable, as it's the
         only way to write a schedule in terms of [Ofday.t]s that spans two weekdays. *)
      (* ofday must be >= 0 and <= 24h *)
      let is_valid (t : t) =
        let t = to_span_since_start_of_day t in
        Span.( <= ) Span.zero t && Span.( <= ) t Span.day
      ;;

      let of_span_since_start_of_day_unchecked span = Span.to_sec span

      let span_since_start_of_day_is_valid span =
        is_valid (of_span_since_start_of_day_unchecked span)
      ;;

      let of_span_since_start_of_day_exn span =
        let module C = Float.Class in
        let s = Span.to_sec span in
        match Float.classify s with
        | C.Infinite -> invalid_arg "Ofday.of_span_since_start_of_day_exn: infinite value"
        | C.Nan -> invalid_arg "Ofday.of_span_since_start_of_day_exn: NaN value"
        | C.Normal | C.Subnormal | C.Zero ->
          if not (is_valid s)
          then invalid_argf !"Ofday out of range: %{Span}" span ()
          else s
      ;;

      let start_of_day = 0.
      let start_of_next_day = of_span_since_start_of_day_exn Span.day

      let add (t : t) (span : Span.t) =
        let t = t +. Span.to_sec span in
        if is_valid t then Some t else None
      ;;

      let sub (t : t) (span : Span.t) =
        let t = t -. Span.to_sec span in
        if is_valid t then Some t else None
      ;;

      let next t =
        let candidate = Float.one_ulp `Up t in
        if is_valid candidate then Some candidate else None
      ;;

      let prev t =
        let candidate = Float.one_ulp `Down t in
        if is_valid candidate then Some candidate else None
      ;;

      let diff t1 t2 =
        Span.( - ) (to_span_since_start_of_day t1) (to_span_since_start_of_day t2)
      ;;
    end

    let approximate_end_of_day =
      Option.value_exn (T.sub T.start_of_next_day Span.microsecond)
    ;;

    (* [create] chops off any subsecond part when [sec = 60] to handle leap seconds. In
       particular it's trying to be generous about reading in times on things like fix
       messages that might include an extra unlikely second.

       Other ways of writing a time, like 1000ms, while mathematically valid, don't match
       ways that people actually write times down, so we didn't see the need to support
       them. That is, a clock might legitimately read 23:59:60 (or, with 60 seconds at
       times of day other than 23:59, depending on the time zone), but it doesn't seem
       reasonable for a clock to read "23:59:59 and 1000ms". *)
    let create ?hr ?min ?sec ?ms ?us ?ns () =
      let ms, us, ns =
        match sec with
        | Some 60 -> Some 0, Some 0, Some 0
        | _ -> ms, us, ns
      in
      T.of_span_since_start_of_day_exn (Span.create ?hr ?min ?sec ?ms ?us ?ns ())
    ;;

    let to_parts t = Span.to_parts (T.to_span_since_start_of_day t)

    let to_string_gen ~drop_ms ~drop_us ~trim t =
      let ( / ) = Int63.( / ) in
      let ( ! ) = Int63.of_int in
      let ( mod ) = Int63.rem in
      let i = Int63.to_int_exn in
      assert (if drop_ms then drop_us else true);
      let float_sec = Span.to_sec (T.to_span_since_start_of_day t) in
      let us = Float.int63_round_nearest_exn (float_sec *. 1e6) in
      let ms, us = us / !1000, us mod !1000 |> i in
      let sec, ms = ms / !1000, ms mod !1000 |> i in
      let min, sec = sec / !60, sec mod !60 |> i in
      let hr, min = min / !60, min mod !60 |> i in
      let hr = i hr in
      let dont_print_us = drop_us || (trim && us = 0) in
      let dont_print_ms = drop_ms || (trim && ms = 0 && dont_print_us) in
      let dont_print_s = trim && sec = 0 && dont_print_ms in
      let len =
        if dont_print_s
        then 5
        else if dont_print_ms
        then 8
        else if dont_print_us
        then 12
        else 15
      in
      let buf = Bytes.create len in
      write_2_digit_int buf ~pos:0 hr;
      Bytes.set buf 2 ':';
      write_2_digit_int buf ~pos:3 min;
      if dont_print_s
      then ()
      else (
        Bytes.set buf 5 ':';
        write_2_digit_int buf ~pos:6 sec;
        if dont_print_ms
        then ()
        else (
          Bytes.set buf 8 '.';
          write_3_digit_int buf ~pos:9 ms;
          if dont_print_us then () else write_3_digit_int buf ~pos:12 us));
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
    ;;

    let to_string_trimmed t = to_string_gen ~drop_ms:false ~drop_us:false ~trim:true t
    let to_sec_string t = to_string_gen ~drop_ms:true ~drop_us:true ~trim:false t
    let to_millisecond_string t = to_string_gen ~drop_ms:false ~drop_us:true ~trim:false t

    let small_diff =
      let hour = 3600. in
      fun ofday1 ofday2 ->
        let ofday1 = Span.to_sec (T.to_span_since_start_of_day ofday1) in
        let ofday2 = Span.to_sec (T.to_span_since_start_of_day ofday2) in
        let diff = ofday1 -. ofday2 in
        (*  d1 is in (-hour; hour) *)
        let d1 = Float.mod_float diff hour in
        (*  d2 is in (0;hour) *)
        let d2 = Float.mod_float (d1 +. hour) hour in
        let d = if Float.( > ) d2 (hour /. 2.) then d2 -. hour else d2 in
        Span.of_sec d
    ;;

    include T

    let to_string t = to_string_gen ~drop_ms:false ~drop_us:false ~trim:false t

    include Pretty_printer.Register (struct
      type nonrec t = t

      let to_string = to_string
      let module_name = "Core.Time.Ofday"
    end)

    let create_from_parsed string ~hr ~min ~sec ~subsec_pos ~subsec_len =
      let subsec =
        if Int.equal subsec_len 0
        then 0.
        else Float.of_string (String.sub string ~pos:subsec_pos ~len:subsec_len)
      in
      Float.of_int ((hr * 3600) + (min * 60) + sec) +. subsec
      |> Span.of_sec
      |> T.of_span_since_start_of_day_exn
    ;;

    let of_string s = Ofday_helpers.parse s ~f:create_from_parsed

    let t_of_sexp sexp =
      match sexp with
      | Sexp.Atom s ->
        (try of_string s with
         | Invalid_argument s -> of_sexp_error ("Ofday.t_of_sexp: " ^ s) sexp)
      | _ -> of_sexp_error "Ofday.t_of_sexp" sexp
    ;;

    let t_sexp_grammar = Sexplib.Sexp_grammar.coerce String.t_sexp_grammar
    let sexp_of_t span = Sexp.Atom (to_string span)

    let of_string_iso8601_extended ?pos ?len str =
      try Ofday_helpers.parse_iso8601_extended ?pos ?len str ~f:create_from_parsed with
      | exn ->
        invalid_argf
          "Ofday.of_string_iso8601_extended(%s): %s"
          (String.subo str ?pos ?len)
          (Exn.to_string exn)
          ()
    ;;

    include Diffable.Atomic.Make (struct
      type nonrec t = t [@@deriving bin_io, equal, sexp]
    end)
  end
end

include Stable.V1

let gen_incl lo hi =
  Span.gen_incl (to_span_since_start_of_day lo) (to_span_since_start_of_day hi)
  |> Quickcheck.Generator.map ~f:of_span_since_start_of_day_exn
;;

let gen_uniform_incl lo hi =
  Span.gen_uniform_incl (to_span_since_start_of_day lo) (to_span_since_start_of_day hi)
  |> Quickcheck.Generator.map ~f:of_span_since_start_of_day_exn
;;

let quickcheck_generator = gen_incl start_of_day start_of_next_day

let quickcheck_observer =
  Quickcheck.Observer.unmap Span.quickcheck_observer ~f:to_span_since_start_of_day
;;

let quickcheck_shrinker = Quickcheck.Shrinker.empty ()

include Hashable.Make_binable (struct
  type nonrec t = t [@@deriving bin_io, compare, hash, sexp_of]

  (* Previous versions rendered hash-based containers using float serialization rather
       than time serialization, so when reading hash-based containers in we accept either
       serialization. *)
  let t_of_sexp sexp =
    match Float.t_of_sexp sexp with
    | float -> of_float float
    | exception _ -> t_of_sexp sexp
  ;;
end)

module C = struct
  type t = T.t [@@deriving bin_io]
  type comparator_witness = T.comparator_witness

  let comparator = T.comparator
  let compare = T.comparator.compare

  (* In 108.06a and earlier, ofdays in sexps of Maps and Sets were raw floats.  From
     108.07 through 109.13, the output format remained raw as before, but both the raw and
     pretty format were accepted as input.  From 109.14 on, the output format was changed
     from raw to pretty, while continuing to accept both formats.  Once we believe most
     programs are beyond 109.14, we will switch the input format to no longer accept
     raw. *)
  let sexp_of_t = sexp_of_t

  let t_of_sexp sexp =
    match Option.try_with (fun () -> T.of_float (Float.t_of_sexp sexp)) with
    | Some t -> t
    | None -> t_of_sexp sexp
  ;;
end

module Map = Map.Make_binable_using_comparator (C)
module Set = Set.Make_binable_using_comparator (C)
include Comparable.Validate (C)

let of_span_since_start_of_day = of_span_since_start_of_day_exn
let to_millisec_string = to_millisecond_string
