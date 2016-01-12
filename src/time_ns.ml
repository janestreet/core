open Core_kernel.Std

module Core_time = Time

module KSpan = Core_kernel.Time_ns.Span
module KTime_ns = Core_kernel.Time_ns

(* This signature constraint is semi-temporary and serves to make the implementation more
   type-safe (so the compiler can help us more).  It would go away if we broke the
   implementation into multiple files. *)
module Span : sig
  include module type of
  (struct
    include KSpan
  end)

  include Identifiable with type t := t

  val to_short_string : t -> string
  val randomize       : t -> percent : float -> t
  val to_span         : t -> Time.Span.t
  val of_span         : Time.Span.t -> t

  module Unit_of_time = Time.Span.Unit_of_time

  val to_unit_of_time : t -> Unit_of_time.t
  val of_unit_of_time : Unit_of_time.t -> t

  val to_string_hum
    :  ?delimiter:char
    -> ?decimals:int
    -> ?align_decimal:bool
    -> ?unit_of_time:Unit_of_time.t
    -> t
    -> string

  module Option : sig
    type span

    type t = private Int63.t [@@deriving typerep]
    include Identifiable with type t := t

    val none : t
    val some : span -> t
    val is_none : t -> bool
    val is_some : t -> bool
    val value : t -> default : span -> span
    val value_exn : t -> span

    module Stable : sig
      module V1 : sig
        type nonrec t = t
        include Stable with type t := t
        val to_int63     : t -> Int63.t
        val of_int63_exn : Int63.t -> t
      end
    end
  end with type span := t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      val to_int63     : t -> Int63.t
      val of_int63_exn : Int63.t -> t
    end
  end

  val random : unit -> t
end = struct
  let half_microsecond = Int63.of_int 500

  let nearest_microsecond t =
    Int63.((KSpan.to_int63_ns t + half_microsecond) /% of_int 1000)
  ;;

  let to_span t = Time.Span.of_us (Int63.to_float (nearest_microsecond t))
  let%test "to_span +/-140y raises" =
    List.for_all [ 1.; -1. ]
      ~f:(fun sign ->
        does_raise (fun () -> to_span (KSpan.of_day (140. *. 366. *. sign))))
  ;;

  let min_kspan_value = to_span KSpan.min_value
  let max_kspan_value = to_span KSpan.max_value

  let of_span s =
    if Time.Span.( > ) s max_kspan_value
    || Time.Span.( < ) s min_kspan_value
    then failwiths "Time_ns.Span does not support this span" s [%sexp_of: Time.Span.t];
    (* Using [Time.Span.to_sec] (being the identity) so that
       we make don't apply too many conversion
       - Too many : `[Span.t] -> [a] -> [KSpan.t]`
       - Only One : `[Span.t]==[a] -> [KSpan.t]`. *)
    KSpan.of_sec_with_microsecond_precision (Time.Span.to_sec s)
  ;;
  let%test "of_span +/-140y raises" =
    List.for_all [ 1.; -1. ]
      ~f:(fun sign ->
        does_raise (fun () -> of_span (Time.Span.of_day (140. *. 366. *. sign))))
  ;;

  module Unit_of_time = Core_time.Span.Unit_of_time

  let of_unit_of_time u =
    let open KSpan in
    match (u : Unit_of_time.t) with
    | Nanosecond  -> nanosecond
    | Microsecond -> microsecond
    | Millisecond -> millisecond
    | Second      -> second
    | Minute      -> minute
    | Hour        -> hour
    | Day         -> day
  ;;

  let to_unit_of_time t : Unit_of_time.t =
    let open KSpan in
    let abs_t = abs t in
    if abs_t >= day         then Day         else
    if abs_t >= hour        then Hour        else
    if abs_t >= minute      then Minute      else
    if abs_t >= second      then Second      else
    if abs_t >= millisecond then Millisecond else
    if abs_t >= microsecond then Microsecond else
      Nanosecond
  ;;

  (* We don't just convert to [Time.Span.t] and use the conversion there because our
     [to_span] conversion is limited to microsecond precision. *)
  let to_string_hum ?(delimiter='_') ?(decimals=3) ?(align_decimal=false) ?unit_of_time t
    =
    let open KSpan in
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
    let open KSpan in
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

  let random () =
    KSpan.of_int63_ns
      (let open Int63 in
       random KSpan.(to_int63_ns max_value)
       + random (of_int 2)
       - random KSpan.(to_int63_ns max_value)
       - random KSpan.(to_int63_ns (neg (max_value + min_value)))
       - random (of_int 2))
  ;;

  let%test_unit "Time.Span.t -> Time_ns.Span.t round trip with microsecond precision" =
    let open Time.Span in
    let sexp_of_t t = sexp_of_float (to_float t) in (* more precise *)
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
        [%test_result: t] ~expect (to_span (of_span expect)))
  ;;

  let%test_unit "Time_ns.Span.t -> Time.Span.t round trip" =
    let open KSpan in
    (* The default sexp is not precise enough. *)
    let sexp_of_t kspan = Sexp.Atom (Int63.to_string (to_int63_ns kspan) ^ "ns") in
    let kspans =                        (* touchstones *)
      min_value :: max_value :: KTime_ns.(diff (now ()) epoch)
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
        ~f:(fun kspan -> filter_map (List.range (-3) 4) ~f:(fun s -> scale kspan (float s)))
    in
    let kspans =                        (* a few microseconds around *)
      List.concat_map kspans
        ~f:(fun kspan ->
          filter_map (List.range (-3) 4) ~f:(fun s -> kspan + scale microsecond (float s)))
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
        [%test_pred: t * t] (fun (a, b) -> abs (a - b) <= microsecond) (expect, kspan))
  ;;

  module T = struct
    include KSpan

    let sexp_of_t t = Time.Span.Stable.V1.sexp_of_t (to_span t)
    let t_of_sexp s = of_span (Time.Span.Stable.V1.t_of_sexp s)

    let module_name = "Core.Std.Time_ns.Span"
    let to_string t = Time.Span.to_string (to_span t)
    let of_string s = of_span (Time.Span.of_string s)
    let hash t = Int63.hash (KSpan.to_int63_ns t)
    let compare = compare
  end
  include T
  include Comparable.Validate_with_zero (T)
  include Identifiable.Make (T)

  let%test _ = Time.Span.is_positive (to_span max_value)  (* make sure no overflow *)

  let to_short_string t = Time.Span.to_short_string (to_span t)
  let randomize t ~percent = of_span (Time.Span.randomize (to_span t) ~percent)

  module Option = struct
    type span = t [@@deriving sexp]
    type t = Int63.t [@@deriving bin_io, typerep, compare] (* nanoseconds or none *)
    let none = Int63.min_value
    let%test "none is not a valid span" = does_raise (fun () -> of_int63_ns none)
    let some t = to_int63_ns t
    let is_none t = Int63.(t = none)
    let is_some t = Int63.(t <> none)
    let value t ~default = if is_none t then default else of_int63_ns t
    let value_exn t = assert (is_some t); of_int63_ns t
    let of_option = function None -> none | Some t -> some t
    let to_option t = if is_none t then None else Some (of_int63_ns t)
    let sexp_of_t t = [%sexp_of: span option] (to_option t)
    let t_of_sexp s = of_option ([%of_sexp: span option] s)

    include Identifiable.Make (struct
      type nonrec t = t [@@deriving sexp, compare, bin_io]
      let hash = Int63.hash
      let module_name = "Core.Std.Time_ns.Span.Option"
      include Sexpable.To_stringable (struct type nonrec t = t [@@deriving sexp] end)
    end)

    module Stable = struct
      module V1 = struct
        type nonrec t = t [@@deriving sexp, compare, bin_io]
        let of_int63_exn n = if n = none then none else some (of_int63_ns n)
        let to_int63     t = t
      end

      let%test_module "Time_ns.Span.Stable.V1" =
        (module struct
          include Core_kernel.Stable_unit_test.Make (struct
              include V1

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
  end

  module Stable = struct
    module V1 = struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let of_int63_exn t = of_int63_ns t
      let to_int63     t = to_int63_ns t
    end

    let%test_module "Time_ns.Span.Stable.V1" =
      (module struct
        include Core_kernel.Stable_unit_test.Make (struct
            include V1

            let equal = KSpan.equal

            let tests =
              let t i = of_int63_ns (Int63.of_int64_exn i) in
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

        let%test_unit _ = assert (does_raise (fun () ->
          V1.of_int63_exn (Int63.succ Int63.min_value)))
      end)
  end
end

include (KTime_ns
         : module type of struct
           include KTime_ns
         end with module Span := Span)

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

let random () =
  KTime_ns.of_int63_ns_since_epoch
    (let max_ns = to_int63_ns_since_epoch max_value in
     let open Int63 in
     random max_ns
     + random (of_int 2)
     - random max_ns
     - (let extra = neg (max_ns + to_int63_ns_since_epoch KTime_ns.min_value) in
        (* Now that Core_kernel.Std.Time_ns.Span.min/max_value are symmetric around
           zero, watch out for that case. *)
        if extra = zero then zero else random extra)
     - random (of_int 2))
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
  List.iter times
    ~f:(fun expect ->
      let time = to_time (of_time expect) in
      (* We don't have full microsecond precision at the far end of the range. *)
      if expect < Time.of_string "2107-01-01 00:00:00" then
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
        KTime_ns.of_int63_ns_since_epoch
          (let open Int63 in
           (KTime_ns.to_int63_ns_since_epoch time + of_int 500)
           /% of_int 1000
           * of_int 1000))
  in
  let ts =                              (* in range *)
    List.filter ts ~f:(fun t -> t >= min_value && t <= max_value)
  in
  List.iter ts ~f:(fun expect -> [%test_result: t] ~expect (of_time (to_time expect)))
;;

let sexp_of_t (t : t) : Sexp.t = Time.sexp_of_t (to_time t)
let sexp_of_t_abs ~zone t =
  Sexp.List
    (List.map (String.split ~on:' ' (Time.to_string_abs ~zone (to_time t)))
       ~f:(fun s -> Sexp.Atom s)
    )
let t_of_sexp s : t = of_time (Time.t_of_sexp s)

let to_string t = Time.to_string (to_time t)
let of_string s = of_time (Time.of_string s)

let to_string_abs t ~zone = Time.to_string_abs ~zone (to_time t)
let of_string_abs s = of_time (Time.of_string_abs s)

module Stable0 = struct
  module V1 = struct
    type nonrec t = t [@@deriving bin_io, compare, sexp]
    let of_int63_exn t = of_int63_ns_since_epoch t
    let to_int63     t = to_int63_ns_since_epoch t
  end

  let%test_module "Time_ns.Stable.V1" =
    (module struct
      include Core_kernel.Stable_unit_test.Make (struct
          include V1
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

      let%test_unit _ = assert (does_raise (fun () ->
        V1.of_int63_exn (Int63.succ Int63.min_value)))
    end)
end

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
  let%test_module "round trip" = (module struct
    let roundtrip t = (value_exn (some t))
    let%test_unit "epoch" = [%test_result: time] (roundtrip epoch) ~expect:epoch
    let%test_unit "now" = let t = now () in [%test_result: time] (roundtrip t) ~expect:t
  end)
  let%test _ = is_error (Result.try_with (fun () -> value_exn none))

  let of_option = function None -> none | Some t -> some t
  let to_option t = if is_none t then None else Some (value_exn t)

  let sexp_of_t t = [%sexp_of: time option] (to_option t)
  let sexp_of_t_abs ~zone t =
    [%sexp_of: Sexp.t option]
      (Option.map (to_option t)
         ~f:(sexp_of_t_abs ~zone)
      )
  let t_of_sexp s = of_option ([%of_sexp: time option] s)

  include Identifiable.Make (struct
    type nonrec t = t [@@deriving sexp, compare, bin_io]
    let module_name = "Core.Std.Time_ns.Option"
    let hash = Span.Option.hash
    include Sexpable.To_stringable (struct type nonrec t = t [@@deriving sexp] end)
  end)

  module Stable = struct
    module V1 = struct
      type nonrec t = t [@@deriving compare, sexp, bin_io]
      let to_int63     t = Span.Option.Stable.V1.to_int63     t
      let of_int63_exn t = Span.Option.Stable.V1.of_int63_exn t
    end

    let%test_module "Time_ns.Option.Stable.V1" =
      (module struct
        include Core_kernel.Stable_unit_test.Make (struct
            include V1
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

        let%test_unit _ = assert (does_raise (fun () ->
          V1.of_int63_exn (Int63.succ Int63.min_value)))
      end)
  end
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

(** Presently this is not zoned. *)
module Ofday = struct
  type t = Span.t (* since midnight, i.e., since start of day *)
  [@@deriving typerep, compare, bin_io]


  let start_of_day : t = Span.zero
  let end_of_day : t = Span.day
  let end_of_day_with_dst_and_leap_second_allowance : t =
    Span.(end_of_day + hour + minute)

  let to_span_since_start_of_day t = t
  let of_span_since_start_of_day_exn (s : Span.t) =
    if Span.(<) s start_of_day || Span.(>) s end_of_day_with_dst_and_leap_second_allowance
    then failwith "Time_ns.Ofday.of_span_since_start_of_day_exn: input out of bounds"
    else s

  let add_exn t span = of_span_since_start_of_day_exn (Span.(+) t span)
  let sub_exn t span = of_span_since_start_of_day_exn (Span.(-) t span)

  let diff t u = Span.(-) t u

  let local_midnight time =
    let zone = Time.Zone.local in
    let date = Time.to_date (to_time time) ~zone in
    let midnight = Time.of_date_ofday date Time.Ofday.start_of_day ~zone in
    of_time midnight
  ;;

  let local_midnight_cache = ref (local_midnight (now ()))

  let of_local_time time =
    let t = Core_kernel.Time_ns.diff time !local_midnight_cache in
    if Span.(>=) t start_of_day && Span.(<) t end_of_day then t
    else begin
      local_midnight_cache := local_midnight time;
      Core_kernel.Time_ns.diff time !local_midnight_cache
    end
  ;;

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

  let of_ofday core = Span.of_span (Time.Ofday.to_span_since_start_of_day core)
  let to_ofday t = Time.Ofday.of_span_since_start_of_day (Span.to_span t)

  let of_string s = of_ofday (Time.Ofday.of_string s)

  let t_of_sexp s : t = of_ofday (Time.Ofday.t_of_sexp s)
  let sexp_of_t (t : t) = Time.Ofday.sexp_of_t (to_ofday t)

  include Identifiable.Make (struct
    type nonrec t = t [@@deriving sexp, compare, bin_io]
    let module_name = "Core.Std.Time_ns.Ofday"
    let hash = Span.hash
    let of_string, to_string = of_string, to_string
  end)

  module Stable = struct
    module V1 = struct
      type nonrec t = t [@@deriving compare, sexp, bin_io]
      let to_int63     t = Span.Stable.V1.to_int63     t
      let of_int63_exn t = Span.Stable.V1.of_int63_exn t
    end

    let%test_module "Time_ns.Ofday.Stable.V1" =
      (module struct
        include Core_kernel.Stable_unit_test.Make (struct
            include V1

            let equal = Span.equal

            let tests =
              let t i = Span.of_int63_ns (Int63.of_int64_exn i) in
              [ t                 0L, "00:00:00.000000", "\000"
              ; t             1_000L, "00:00:00.000001", "\254\232\003"
              ; t 1_234_560_000_000L, "00:20:34.560000", "\252\000\160\130q\031\001\000\000"
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

        let%test_unit _ = assert (does_raise (fun () ->
          V1.of_int63_exn (Int63.succ Int63.min_value)))
      end)
  end
end

let%test_module _ = (module struct

  let%test_unit _ =
    let span = Span.create ~hr:8 ~min:27 ~sec:14 ~ms:359 () in
    let ofday = Ofday.of_span_since_start_of_day_exn span in
    let expected = "08:27:14.359" in
    let ms_str = Ofday.to_millisecond_string ofday    in
    if String.(<>) ms_str expected then
      failwithf "Failed on Ofday.to_millisecond_string Got (%s) expected (%s)"
        ms_str expected ()

  let%test_unit _ =
    (* Ensure that local_midnight_cache doesn't interfere with converting times that are
       much earlier or later than each other. *)
    let check ofday =
      let to_string t = Span.to_int63_ns t |> Int63.to_string in
      if Ofday.(<) ofday Ofday.start_of_day
      then failwithf "too small: %s" (to_string ofday) ()
      else if Ofday.(>=) ofday Ofday.end_of_day
      then failwithf "too large: %s" (to_string ofday) ()
      else ()
    in
    check (Ofday.of_local_time epoch);
    check (Ofday.of_local_time (now ()));
    check (Ofday.of_local_time epoch)

end)

let of_date_ofday ~zone date ofday =
  of_time (Core_time.of_date_ofday ~zone date (Ofday.to_ofday ofday))
;;

let to_date t ~zone = Core_time.to_date (to_time t) ~zone

let occurrence what t ~ofday ~zone =
  of_time (Core_time.occurrence what (to_time t) ~ofday:(Ofday.to_ofday ofday) ~zone)
;;

module Stable = struct
  include Stable0
  module Span   = Span   .Stable
  module Option = Option .Stable
  module Ofday  = Ofday  .Stable
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
