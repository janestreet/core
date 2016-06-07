open Core_kernel.Std

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
  let%test "to_span +/-140y raises" =
    List.for_all [ 1.; -1. ]
      ~f:(fun sign ->
        does_raise (fun () ->
          to_span (Time_ns.Span.of_day (140. *. 366. *. sign))))
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
  let%test "of_span +/-140y raises" =
    List.for_all [ 1.; -1. ]
      ~f:(fun sign ->
        does_raise (fun () -> of_span (Time.Span.of_day (140. *. 366. *. sign))))
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

  let%test_unit "Span.to_string_hum" =
    let open Time_ns.Span in
    [%test_result: string] (to_string_hum nanosecond) ~expect:"1ns";
    [%test_result: string] (to_string_hum day) ~expect:"1d";
    [%test_result: string]
      (to_string_hum ~decimals:6                      day)
      ~expect:"1d";
    [%test_result: string]
      (to_string_hum ~decimals:6 ~align_decimal:false day)
      ~expect:"1d";
    [%test_result: string]
      (to_string_hum ~decimals:6 ~align_decimal:true  day)
      ~expect:"1.000000d ";
    [%test_result: string]
      (to_string_hum ~decimals:6 ~align_decimal:true ~unit_of_time:Day
         (hour + minute))
      ~expect:"0.042361d "

  let%test_unit "Time.Span.t -> Time_ns.Span.t round trip with microsecond precision" =
    let open Time.Span in
    let time_spans =                                (* touchstones *)
      min_kspan_value :: max_kspan_value :: Time.(diff (now ()) epoch)
      :: Time.Span.([ zero; microsecond; millisecond; second; minute; hour; day;
                      scale day 365.
                    ])
    in
    let time_spans =                    (* a few randoms *)
      time_spans
      @ List.init 9 ~f:(fun _ -> Time.Span.(of_us (Random.float (to_us max_kspan_value))))
    in
    let time_spans =                    (* a few multiples *)
      List.concat_map time_spans
        ~f:(fun time_span ->
          List.map (List.range (-3) 4)
            ~f:(fun s -> Time.Span.scale time_span (float s)))
    in
    let time_spans =                    (* a few microseconds around *)
      List.concat_map time_spans
        ~f:(fun time_span ->
          List.map (List.range (-3) 4)
            ~f:(fun s -> Time.Span.(time_span + scale microsecond (float s))))
    in
    let time_spans =                    (* nearest microsecond *)
      List.map time_spans
        ~f:(fun s -> Time.Span.(of_us (Float.round_nearest (to_us s))))
    in
    let time_spans =                    (* in range *)
      List.filter time_spans
        ~f:(fun s -> Time.Span.(s >= min_kspan_value && s <= max_kspan_value))
    in
    List.iter time_spans
      ~f:(fun expect ->
        let kspan = to_span (of_span expect) in
        [%test_pred: Span.t * Span.t] (fun (a,b) -> abs (a - b) <= microsecond)
          (expect, kspan))
  ;;

  let%test_unit "Time_ns.Span.t -> Time.Span.t round trip" =
    let open Time_ns.Span in
    (* The default sexp is not precise enough. *)
    let sexp_of_t kspan = Sexp.Atom (Int63.to_string (to_int63_ns kspan) ^ "ns") in
    let kspans =                        (* touchstones *)
      min_value :: max_value :: Time_ns.(diff (now ()) epoch)
      :: [ zero; microsecond; millisecond; second; minute; hour; day;
           scale day 365.
         ]
    in
    let kspans =                        (* a few randoms *)
      kspans @ List.init 9 ~f:(fun _ -> of_us (Random.float (to_us max_value)))
    in
    (* Some tweaks will be out of range, which will raise exceptions. *)
    let filter_map list ~f =
      List.filter_map list ~f:(fun x -> Option.try_with (fun () -> f x))
    in
    let kspans =                        (* a few multiples *)
      List.concat_map kspans
        ~f:(fun kspan ->
          filter_map (List.range (-3) 4) ~f:(fun s -> scale kspan (float s)))
    in
    let kspans =                        (* a few microseconds around *)
      List.concat_map kspans
        ~f:(fun kspan ->
          filter_map (List.range (-3) 4)
            ~f:(fun s -> kspan + scale microsecond (float s)))
    in
    let kspans =                        (* nearest microsecond *)
      List.map kspans
        ~f:(fun s -> of_int63_ns Int63.(nearest_microsecond s * of_int 1000))
    in
    let kspans =                        (* in range *)
      List.filter kspans ~f:(fun s -> s >= min_value && s <= max_value)
    in
    List.iter kspans
      ~f:(fun expect ->
        let kspan = of_span (to_span expect) in
        [%test_pred: t * t] (fun (a, b) -> abs (a - b) <= microsecond)
          (expect, kspan))
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

    let%test_module "Time_ns.Span.Stable.V1" =
      (module struct
        open V1

        include Core_kernel.Stable_unit_test.Make (struct
            type t = V1.t [@@deriving bin_io, sexp]

            let equal = Time_ns.Span.equal

            let tests =
              let t i = Time_ns.Span.of_int63_ns (Int63.of_int64_exn i) in
              [ t      1_234_560_000_000L,  "20.576m", "\252\000\160\130q\031\001\000\000"
              ; t                  1_000L,  "0.001ms", "\254\232\003"
              ; t 80_000_006_400_000_000L, "925.926d", "\252\000@\128\251\1487\028\001"
              ]
          end)

        let test str int64 =
          let t = t_of_sexp (Sexp.of_string str) in
          let int63 = Int63.of_int64_exn int64 in
          [%test_result: Int63.t] (V1.to_int63     t)     ~expect:int63;
          [%test_result: t]       (V1.of_int63_exn int63) ~expect:t

        let%test_unit _ = test "0s"        0L
        let%test_unit _ = test "1s"        1_000_000_000L
        let%test_unit _ = test "0.187924m" 11_275_440_000L
        let%test_unit _ = test "828.97d"   71_623_008_000_000_000L

        let%test_unit "of_int63_exn checks range" =
          assert (does_raise (fun () ->
            V1.of_int63_exn (Int63.succ Int63.min_value)))
      end)
  end

  module T = struct
    include Time_ns.Span

    let sexp_of_t = Stable.V1.sexp_of_t
    let t_of_sexp = Stable.V1.t_of_sexp

    let module_name = "Core.Std.Time_ns.Span"
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

  let%test _ = Time.Span.is_positive (to_span max_value)  (* make sure no overflow *)

  let to_short_string t = Time.Span.to_short_string (to_span t)
  let randomize t ~percent = of_span (Time.Span.randomize (to_span t) ~percent)

  module Option = struct
    type span = t [@@deriving sexp]
    type t = Int63.t [@@deriving bin_io, typerep, compare] (* nanoseconds or none *)
    let none = Int63.min_value
    let some span = to_int63_ns (check_range span)
    let%test "none is not a valid span" = does_raise (fun () -> some (of_int63_ns none))
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

      let%test_module "Time_ns.Span.Stable.V1" =
        (module struct
          open V1

          include
            Core_kernel.Stable_unit_test.Make (struct
              type t = V1.t [@@deriving bin_io, sexp]

              let equal = Int63.equal

              let tests =
                let mk_some i = some (of_int63_ns (Int63.of_int64_exn i)) in
                [ none, "()", "\252\000\000\000\000\000\000\000\192"
                ; mk_some 1_234_560_000_000L, "(20.576m)",
                  "\252\000\160\130q\031\001\000\000"
                ; mk_some 1_000L,  "(0.001ms)", "\254\232\003"
                ; mk_some 80_000_006_400_000_000L,
                  "(925.926d)",
                  "\252\000@\128\251\1487\028\001"
                ]
            end)

          let test str int64 =
            let t = t_of_sexp (Sexp.of_string str) in
            let int63 = Int63.of_int64_exn int64 in
            [%test_result: Int63.t] (V1.to_int63     t)     ~expect:int63;
            [%test_result: t]       (V1.of_int63_exn int63) ~expect:t

          let%test_unit _ = test "()"          (-4_611_686_018_427_387_904L)
          let%test_unit _ = test "(0s)"        0L
          let%test_unit _ = test "(1s)"        1_000_000_000L
          let%test_unit _ = test "(0.187924m)" 11_275_440_000L
          let%test_unit _ = test "(828.97d)"   71_623_008_000_000_000L

          let%test_unit _ = assert (does_raise (fun () ->
            V1.of_int63_exn (Int63.succ Int63.min_value)))
        end)
    end

    let sexp_of_t = Stable.V1.sexp_of_t
    let t_of_sexp = Stable.V1.t_of_sexp

    include Identifiable.Make (struct
        type nonrec t = t [@@deriving sexp, compare, bin_io]
        let hash = Int63.hash
        let module_name = "Core.Std.Time_ns.Span.Option"
        include Sexpable.To_stringable (struct type nonrec t = t [@@deriving sexp] end)
      end)
  end
end

include (Time_ns : module type of struct include Time_ns end
         with module Span := Time_ns.Span)

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

let%test_unit "Time.t -> Time_ns.t round trip" =
  let open Time in
  let sexp_of_t t = [%sexp_of: t * float] (t, to_float t) in (* more precise *)

  let us_since_epoch time = Time.(Span.to_us (diff time epoch)) in
  let min_us_since_epoch = us_since_epoch min_time_value in
  let max_us_since_epoch = us_since_epoch max_time_value in

  let time_of_us_since_epoch us_since_epoch =
    Time.(add epoch (Span.of_us (Float.round_nearest us_since_epoch)))
  in

  let times =                           (* touchstones *)
    [ min_time_value; Time.epoch; Time.now (); max_time_value ]
  in
  let times =                           (* a few units around *)
    List.concat_map times
      ~f:(fun time ->
        List.concat_map
          Time.Span.([ microsecond; millisecond; second; minute; hour; day;
                       scale day 365.
                     ])
          ~f:(fun unit ->
            List.map (List.map ~f:float (List.range (-3) 4))
              ~f:(fun s -> Time.add time (Time.Span.scale unit s))))
  in
  let times =                           (* a few randoms *)
    times @
    List.init 9
      ~f:(fun _ ->
        Time.add Time.epoch
          (Time.Span.of_us
             (min_us_since_epoch
              +. Random.float (max_us_since_epoch -. min_us_since_epoch))))
  in
  let times =                           (* nearest microsecond *)
    List.map times
      ~f:(fun time ->
        time_of_us_since_epoch
          (Float.round_nearest Time.(Span.to_us (diff time epoch))))
  in
  let times =                           (* in range *)
    List.filter times
      ~f:(fun time -> Time.(time >= min_time_value && time <= max_time_value))
  in
  let is_64bit = match Word_size.word_size with
    | W64 -> true
    | W32 -> false
  in
  List.iter times
    ~f:(fun expect ->
      let time = to_time (of_time expect) in
      (* We don't have full microsecond precision at the far end of the range. *)
      if is_64bit && expect < Time.of_string "2107-01-01 00:00:00" then
        [%test_result: t] ~expect time
      else
        [%test_pred: t * t]
          (fun (a, b) -> Span.(abs (diff a b) <= microsecond))
          (expect, time))
;;

let%test_unit "Time_ns.t -> Time.t round trip" =
  let open Alternate_sexp in
  let ts =                              (* touchstones *)
    [ min_value; epoch; now (); max_value ]
  in
  (* Some tweaks will be out of range, which will raise exceptions. *)
  let filter_map list ~f =
    List.filter_map list ~f:(fun x -> Option.try_with (fun () -> f x))
  in
  let ts =                              (* a few units around *)
    List.concat_map ts
      ~f:(fun time ->
        List.concat_map
          Span.([ microsecond; millisecond; second; minute; hour; day;
                  scale day 365.
                ])
          ~f:(fun unit ->
            filter_map (List.map ~f:float (List.range (-3) 4))
              ~f:(fun s -> add time (Span.scale unit s))))
  in
  let ts =                              (* a few randoms *)
    ts @ List.init 9 ~f:(fun _ -> random ())
  in
  let ts =                              (* nearest microsecond since epoch *)
    List.map ts
      ~f:(fun time ->
        Time_ns.of_int63_ns_since_epoch
          (let open Int63 in
           (Time_ns.to_int63_ns_since_epoch time + of_int 500)
           /% of_int 1000
           * of_int 1000))
  in
  let ts =                              (* in range *)
    List.filter ts ~f:(fun t -> t >= min_value && t <= max_value)
  in
  List.iter ts ~f:(fun expect -> [%test_result: t] ~expect (of_time (to_time expect)))
;;

module Stable = struct
  module V1 = struct
    module T = struct
      type nonrec t = t [@@deriving bin_io, compare]

      let sexp_of_t (t : t) : Sexp.t = Time.Stable.V1.sexp_of_t (to_time t)
      let sexp_of_t_abs ~zone t =
        Sexp.List
          (List.map (String.split ~on:' ' (Time.to_string_abs ~zone (to_time t)))
             ~f:(fun s -> Sexp.Atom s)
          )
      let t_of_sexp s : t = of_time (Time.Stable.V1.t_of_sexp s)

      let of_int63_exn t =
      of_span_since_epoch (Span.check_range (Span.of_int63_ns t))

      let to_int63 t = to_int63_ns_since_epoch t
    end
    include T
    include Comparator.Stable.V1.Make (T)
  end

  let%test_module "Time_ns.Stable.V1" =
    (module struct
      open V1

      include Core_kernel.Stable_unit_test.Make (struct
          type t = V1.t [@@deriving bin_io, of_sexp]

          let sexp_of_t = sexp_of_t_abs ~zone:Zone.utc

          let equal = equal

          let tests =
            let t i = of_span_since_epoch (Span.of_int63_ns (Int63.of_int64_exn i)) in
            [ t 1_234_560_000_000L,
              "(1970-01-01 00:20:34.560000Z)",
              "\252\000\160\130q\031\001\000\000"
            ; t 1_000L, "(1970-01-01 00:00:00.000001Z)", "\254\232\003"
            ; t 80_000_006_400_000_000L,
              "(1972-07-14 22:13:26.400000Z)",
              "\252\000@\128\251\1487\028\001"
            ]
        end)

      let test str int64 =
        let t = t_of_sexp (Sexp.of_string str) in
        let int63 = Int63.of_int64_exn int64 in
        [%test_result: Int63.t] (V1.to_int63     t)     ~expect:int63;
        [%test_result: t]       (V1.of_int63_exn int63) ~expect:t

      let%test_unit _ = test "(1970-01-01 00:00:00Z)"        0L
      let%test_unit _ = test "(2013-10-07 14:30:00.010101Z)" 1_381_156_200_010_101_000L
      let%test_unit _ = test "(2100-04-01 23:59:59.999999Z)" 4_110_307_199_999_999_000L

      let%test_unit "of_int63_exn checks range" =
        assert (does_raise (fun () ->
          V1.of_int63_exn (Int63.succ Int63.min_value)))
    end)
end

let sexp_of_t = Stable.V1.sexp_of_t
let t_of_sexp = Stable.V1.t_of_sexp

let to_string t = Time.to_string (to_time t)
let of_string s = of_time (Time.of_string s)

let to_string_abs t ~zone = Time.to_string_abs ~zone (to_time t)
let of_string_abs s = of_time (Time.of_string_abs s)

module Option = struct
  type time = t [@@deriving sexp, compare]

  type t = Span.Option.t [@@deriving bin_io, compare, typerep]

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
  let%test_module "round trip" = (module struct
    let roundtrip t = (value_exn (some t))
    let%test_unit "epoch" = [%test_result: time] (roundtrip epoch) ~expect:epoch
    let%test_unit "now" = let t = now () in [%test_result: time] (roundtrip t) ~expect:t
  end)
  let%test _ = is_error (Result.try_with (fun () -> value_exn none))

  let of_option = function None -> none | Some t -> some t
  let to_option t = if is_none t then None else Some (value_exn t)

  module Stable = struct
    module V1 = struct
      module T = struct
        type nonrec t = t [@@deriving compare, bin_io]

        let sexp_of_t t = [%sexp_of: Stable.V1.t option] (to_option t)
        let sexp_of_t_abs ~zone t =
          [%sexp_of: Sexp.t option]
            (Option.map (to_option t)
               ~f:(Stable.V1.sexp_of_t_abs ~zone)
            )
        let t_of_sexp s = of_option ([%of_sexp: Stable.V1.t option] s)

        let to_int63     t = Span.Option.Stable.V1.to_int63     t
        let of_int63_exn t = Span.Option.Stable.V1.of_int63_exn t
      end
      include T
      include Comparator.Stable.V1.Make (T)
    end

    let%test_module "Time_ns.Option.Stable.V1" =
      (module struct
        open V1

        include Core_kernel.Stable_unit_test.Make (struct
            type t = V1.t [@@deriving bin_io, of_sexp]

            let sexp_of_t = sexp_of_t_abs ~zone:Zone.utc

            let equal = Span.Option.equal

            let tests =
              let t i = of_span_since_epoch (Span.of_int63_ns (Int63.of_int64_exn i)) in
              [ none, "()", "\252\000\000\000\000\000\000\000\192"
              ; some (t 1_234_560_000_000L),
                "((1970-01-01 00:20:34.560000Z))",
                "\252\000\160\130q\031\001\000\000"
              ; some (t 1_000L), "((1970-01-01 00:00:00.000001Z))", "\254\232\003"
              ; some (t 80_000_006_400_000_000L),
                "((1972-07-14 22:13:26.400000Z))",
                "\252\000@\128\251\1487\028\001"
              ]
          end)

        let test str int64 =
          let t = t_of_sexp (Sexp.of_string str) in
          let int63 = Int63.of_int64_exn int64 in
          [%test_result: Int63.t] (V1.to_int63     t)     ~expect:int63;
          [%test_result: t]       (V1.of_int63_exn int63) ~expect:t

        let%test_unit _ = test "()"                       (-4_611_686_018_427_387_904L)
        let%test_unit _ = test "((1970-01-01 00:00:00Z))" 0L
        let%test_unit _ =
          test "((2013-10-07 14:30:00.010101Z))" 1_381_156_200_010_101_000L
        let%test_unit _ =
          test "((2100-04-01 23:59:59.999999Z))" 4_110_307_199_999_999_000L

        let%test_unit "of_int63_exn checks range" =
          assert (does_raise (fun () ->
            V1.of_int63_exn (Int63.succ Int63.min_value)))
      end)
  end

  let sexp_of_t = Stable.V1.sexp_of_t
  let t_of_sexp = Stable.V1.t_of_sexp

  include Identifiable.Make (struct
      type nonrec t = t [@@deriving sexp, compare, bin_io]
      let module_name = "Core.Std.Time_ns.Option"
      let hash = Span.Option.hash
      include Sexpable.To_stringable (struct type nonrec t = t [@@deriving sexp] end)
    end)
end

let to_string_fix_proto zone t = Time.to_string_fix_proto zone (to_time t)
let of_string_fix_proto zone s = of_time (Time.of_string_fix_proto zone s)

include Identifiable.Make (struct
    type nonrec t = t [@@deriving sexp, bin_io, compare]
    let module_name = "Core.Std.Time_ns"
    let hash t = Int63.hash (to_int63_ns_since_epoch t)
    let of_string, to_string = of_string, to_string
  end)

let%test_module _ = (module struct
  let%test _ = epoch = of_span_since_epoch Span.zero

  let%test_unit "round trip from [Time.t] to [t] and back" =
    let times = List.map ~f:Time.of_float [ 0.0; 1.0; 1.123456789 ] in
    List.iter times ~f:(fun time ->
      let res = to_time (of_time time) in
      [%test_result: Time.t] ~equal:Time.(=.) ~expect:time res
    )

  let%test_unit "round trip from [t] to [Time.t] and back" =
    List.iter Span.([ zero; second; scale day 365. ]) ~f:(fun since_epoch ->
      let t = of_span_since_epoch since_epoch in
      let res = of_time (to_time t) in
      (* Allow up to 100ns discrepancy in a year due to float precision issues. *)
      let discrepancy = diff res t in
      if Span.(abs discrepancy > of_ns 100.) then
        failwiths "Failed on span since epoch"
          (`since_epoch since_epoch, t, `res res, `discrepancy discrepancy)
          [%sexp_of: [ `since_epoch of Span.t ]
                     * t * [ `res of t ]
                     * [ `discrepancy of Span.t ]])
end)

let to_date t ~zone = Time.to_date (to_time t) ~zone

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


  let start_of_day : t = Span.zero
  let end_of_day   : t = Span.day

  let to_span_since_start_of_day t = t
  let of_span_since_start_of_day_exn (s : Span.t) =
    (* Why we use [Span.(>)] rather than [.(>=)] below:

       We allow to represent the end-of-day sentinel value ([24.000000000h]), which is not
       itself a valid clock face time.  However, since valid clock face times readily
       round up to it, it's better to allow it to be represented. *)
    if Span.(<) s start_of_day || Span.(>) s end_of_day
    then failwith "Time_ns.Ofday.of_span_since_start_of_day_exn: input out of bounds"
    else s

  let add_exn t span = of_span_since_start_of_day_exn (Span.(+) t span)
  let sub_exn t span = of_span_since_start_of_day_exn (Span.(-) t span)

  let diff t u = Span.(-) t u

  let midnight date ~zone = of_date_ofday ~zone date Time.Ofday.start_of_day

  let of_ofday core = Span.of_span (Time.Ofday.to_span_since_start_of_day core)

  let of_time =
    let module Cache = struct
      type t =
        { mutable zone          : Zone.t
        ; mutable midnight      : Time_ns.t
        ; mutable next_midnight : Time_ns.t
        }
    end in
    let cache : Cache.t =
      { zone = Zone.local; midnight = epoch; next_midnight = epoch }
    in
    fun time ~zone ->
      (* Zones are strings.  You have to cache-validate them physically. *)
      if phys_equal cache.zone zone
      && time >= cache.midnight
      && time < cache.next_midnight
      then Time_ns.diff time cache.midnight
      else
        let date, ofday   = Time.to_date_ofday (to_time time)         ~zone in
        let next_midnight = midnight           (Date.add_days date 1) ~zone in
        let midnight      = midnight           date                   ~zone in
        (* Use one code path uniformly on non-DST-transition days, and a different one on
           DST-transition days (of_ofday). *)
        if Span.(=) (Time_ns.diff next_midnight midnight) Span.day then
          begin
            cache.zone          <- zone;
            cache.midnight      <- midnight;
            cache.next_midnight <- next_midnight;
            Time_ns.diff time cache.midnight
          end
        else of_ofday ofday
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

  let of_local_time time = of_time time ~zone:Zone.local

  let local_now () = of_local_time (now ())

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

    let%test_module "Time_ns.Ofday.Stable.V1" =
      (module struct
        open V1

        include Core_kernel.Stable_unit_test.Make (struct
            type t = V1.t [@@deriving bin_io, sexp]

            let equal = Span.equal

            let tests =
              let t i = Span.of_int63_ns (Int63.of_int64_exn i) in
              [ t                 0L, "00:00:00.000000", "\000"
              ; t             1_000L, "00:00:00.000001", "\254\232\003"
              ; t 1_234_560_000_000L, "00:20:34.560000",
                "\252\000\160\130q\031\001\000\000"
              ]
          end)

        let test str int64 =
          let t = t_of_sexp (Sexp.of_string str) in
          let int63 = Int63.of_int64_exn int64 in
          [%test_result: Int63.t] (V1.to_int63     t)     ~expect:int63;
          [%test_result: t]       (V1.of_int63_exn int63) ~expect:t

        let%test_unit _ = test "00:00:00"        0L
        let%test_unit _ = test "14:30:00.010101" 52_200_010_101_000L
        let%test_unit _ = test "23:59:59.999999" 86_399_999_999_000L

        let%test_unit "of_int63_exn checks range" =
          assert (does_raise (fun () ->
            V1.of_int63_exn (Int63.succ Int63.min_value)))
      end)
  end

  let sexp_of_t = Stable.V1.sexp_of_t
  let t_of_sexp = Stable.V1.t_of_sexp

  include Identifiable.Make (struct
      type nonrec t = t [@@deriving sexp, compare, bin_io]
      let module_name = "Core.Std.Time_ns.Ofday"
      let hash = Span.hash
      let of_string, to_string = of_string, to_string
    end)

  module Option = struct
    type ofday = t [@@deriving sexp, compare]
    type t = Span.Option.t [@@deriving compare, bin_io, typerep]

    let none            = Span.Option.none
    let some            = Span.Option.some
    let is_none         = Span.Option.is_none
    let is_some         = Span.Option.is_some
    let value           = Span.Option.value
    let value_exn       = Span.Option.value_exn
    let unchecked_value = Span.Option.unchecked_value

    let of_option = function None -> none | Some t -> some t
    let to_option t = if is_none t then None else Some (value_exn t)

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

      let%test_module "Time_ns.Ofday.Option.Stable.V1" =
        (module struct
          open V1

          include Core_kernel.Stable_unit_test.Make (struct
              type t = V1.t [@@deriving bin_io, sexp]

              let equal = Span.Option.equal

              let tests =
                let t i = of_int63_exn (Int63.of_int64_exn i) in
                [ t (-4_611_686_018_427_387_904L),
                  "()",
                  "\252\000\000\000\000\000\000\000\192"
                ; t 0L, "(00:00:00.000000)", "\000"
                ; t 1_000L, "(00:00:00.000001)", "\254\232\003"
                ; t 987_654_321_000L,
                  "(00:16:27.654321)",
                  "\252h\243\200\244\229\000\000\000"
                ; t 86_399_999_999_000L,
                  "(23:59:59.999999)",
                  "\252\024\252N\145\148N\000\000"
                ]
            end)

          let test str int64 =
            let t = t_of_sexp (Sexp.of_string str) in
            let int63 = Int63.of_int64_exn int64 in
            [%test_result: Int63.t] (V1.to_int63     t)     ~expect:int63;
            [%test_result: t]       (V1.of_int63_exn int63) ~expect:t

          let%test_unit _ = test "()"                (-4_611_686_018_427_387_904L)
          let%test_unit _ = test "(00:00:00)"        0L
          let%test_unit _ = test "(14:30:00.010101)" 52_200_010_101_000L
          let%test_unit _ = test "(23:59:59.999999)" 86_399_999_999_000L

          let%test_unit "of_int63_exn checks range" =
            assert (does_raise (fun () ->
              V1.of_int63_exn (Int63.succ Int63.min_value)))
        end)
    end

    let sexp_of_t = Stable.V1.sexp_of_t
    let t_of_sexp = Stable.V1.t_of_sexp

    include Identifiable.Make (struct
        type nonrec t = t [@@deriving sexp, compare, bin_io]
        let module_name = "Core.Std.Time_ns.Ofday.Option"
        let hash = Span.Option.hash
        include Sexpable.To_stringable (struct type nonrec t = t [@@deriving sexp] end)
      end)
  end
end

let%test_module _ =
  (module struct
    let%test_unit _ =
      let span = Span.create ~hr:8 ~min:27 ~sec:14 ~ms:359 () in
      let ofday = Ofday.of_span_since_start_of_day_exn span in
      let expected = "08:27:14.359" in
      let ms_str = Ofday.to_millisecond_string ofday    in
      if String.(<>) ms_str expected then
        failwithf "Failed on Ofday.to_millisecond_string Got (%s) expected (%s)"
          ms_str expected ()

    let check ofday =
      try
        assert Ofday.(ofday >= start_of_day && ofday < end_of_day);
        (* The sexp is more similar than the string.  The string includes different
           numbers of trailing zeros, which are trimmed in the sexp. *)
        [%test_result: Sexp.t] (Ofday.sexp_of_t ofday)
          ~expect:(Time.Ofday.sexp_of_t (Ofday.to_ofday ofday))
      with raised ->
        failwiths "check ofday"
          (Or_error.try_with (fun () -> [%sexp_of: Ofday.t] ofday),
           Span.to_int63_ns ofday,
           raised)
          [%sexp_of: Sexp.t Or_error.t * Int63.t * exn]

    let%test_unit _ =
      (* Ensure that midnight_cache doesn't interfere with converting times that are much
         earlier or later than each other. *)
      check (Ofday.of_local_time epoch);
      check (Ofday.of_local_time (now ()));
      check (Ofday.of_local_time epoch)

    (* Reproduce a failure of the prior test before taking DST into account. *)
    let%test_unit "Ofday.of_local_time around fall 2015 DST transition" =
      List.iter
        ~f:(fun (time_ns, expect) ->
          let zone = Zone.find_exn "US/Eastern" in
          (* First make sure Time.Ofday.of_time behaves as expected with these inputs. *)
          let time_ofday = Time.to_ofday (to_time time_ns) ~zone in
          if Time.Ofday.(<>) time_ofday (Time.Ofday.of_string expect) then
            failwiths "Time.Ofday.of_time"
              [%sexp (time_ns    : t),
                     (time_ofday : Time.Ofday.t),
                     (expect     : string)]
              Fn.id;
          (* Then make sure we do the same, correct thing. *)
          let ofday = Ofday.of_time time_ns ~zone in
          check ofday;
          if Ofday.(<>) ofday (Ofday.of_string expect) then
            failwiths "Ofday.of_time"
              [%sexp (time_ns : t),
                     (ofday   : Ofday.t),
                     (expect  : string)]
              Fn.id)
        ([ epoch, "19:00:00"
         ; of_string_abs "2015-11-02 23:59:59 US/Eastern", "23:59:59"
         ; epoch, "19:00:00"
         (* [of_string] chooses the second occurrence of a repeated wall clock time in a
            DST (Daylight Saving Time) transition. *)
         ; add (of_string "2015-11-01 01:59:59 US/Eastern") Span.second, "02:00:00"
         ]
         (* We can denote specific linear times during the repeated wall-clock hour
            relative to a time before the ambiguity. *)
         @ List.map
             ~f:(fun (span, ofday) ->
               add (of_string "2015-11-01 00:59:59 US/Eastern") span, ofday)
             [ Span.second,          "01:00:00"
             ; Span.(second + hour), "01:00:00"
             ]
         @ [ add (of_string "2015-03-08 01:59:59 US/Eastern") Span.second, "03:00:00"
           ; epoch, "19:00:00"
           ])

    let random_nativeint_range =
      match Word_size.word_size with
      | W64 -> fun () -> random ()
      | W32 ->
        (* In 32 bits, some functions in [Time] don't work on all the float values, but
           only the part that fits in a native int. *)
        let in_ns = Int63.of_float 1e9 in
        let max_time_ns = Int63.(of_nativeint_exn Nativeint.max_value * in_ns) in
        let min_time_ns = Int63.(of_nativeint_exn Nativeint.min_value * in_ns) in
        let range = Int63.(one + max_time_ns - min_time_ns) in
        fun () ->
          let r = Time_ns.to_int63_ns_since_epoch (random ()) in
          Time_ns.of_int63_ns_since_epoch
            Int63.(((r - min_time_ns) % range) + min_time_ns)
    ;;

    let%test_unit "Ofday.of_time random" =
      List.iter !Zone.likely_machine_zones ~f:(fun zone ->
        let zone = Zone.find_exn zone in
        for _ = 0 to 1_000 do check (Ofday.of_time (random_nativeint_range ()) ~zone) done)

    let%test_unit "Ofday.of_local_time random" =
      for _ = 0 to 1_000 do check (Ofday.of_local_time (random_nativeint_range ())) done
  end)

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

let of_date_ofday ~zone date ofday =
  of_date_ofday ~zone date (Ofday.to_ofday ofday)
;;

let occurrence what t ~ofday ~zone =
  of_time (Time.occurrence what (to_time t) ~ofday:(Ofday.to_ofday ofday) ~zone)
;;

let%expect_test "in tests, [to_string] uses NYC's time zone" =
  printf "%s" (to_string epoch);
  [%expect {| 1969-12-31 19:00:00.000000-05:00 |}];
;;

let%expect_test "in tests, [sexp_of_t] uses NYC's time zone" =
  printf !"%{Sexp}" [%sexp (epoch : t)];
  [%expect {| (1969-12-31 19:00:00.000000-05:00) |}];
;;

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
