open! Import
open Std_internal
open Digit_string_helpers

let is_leap_year ~year = (year mod 4 = 0 && not (year mod 100 = 0)) || year mod 400 = 0

(* Create a local private date type to ensure that all dates are created via
   Date.create_exn.
*)
module Stable = struct
  module V1 = struct
    module Without_comparable = struct
      module T : sig
        type t
        [@@immediate]
        [@@deriving bin_io ~localize, compare, equal, hash, typerep, stable_witness]

        val create_exn : y:int -> m:Month.Stable.V1.t -> d:int -> t
        val year : t -> int
        val month : t -> Month.Stable.V1.t
        val day : t -> int
        val days_in_month : year:int -> month:Month.t -> int
        val to_int : t -> int
        val of_int_exn : int -> t
        val of_int_unchecked : int -> t
        val invalid_value__for_internal_use_only : t
      end = struct
        (* We used to store dates like this:
           type t = { y: int; m: Month.Stable.V1.t; d: int; }
           In the below we make sure that the bin_io representation is
           identical (and the stable unit tests check this)

           In memory we use the following much more compact representation:
           2 bytes year
           1 byte month
           1 byte day

           all packed into a single immediate int (so from 4 words down to 1).
        *)
        type t = int
        [@@deriving
          compare
          , equal
          , hash
          , typerep
          , bin_shape ~basetype:"899ee3e0-490a-11e6-a10a-a3734f733566"
          , stable_witness]

        let create0 ~year ~month ~day =
          (* create_exn's validation make sure that each value fits *)
          (year lsl 16) lor (Month.to_int month lsl 8) lor day
        ;;

        let of_int_unchecked t = t
        let year t = t lsr 16
        let month t = Month.of_int_exn ((t lsr 8) land 0xff)
        let day t = t land 0xff

        let days_in_month ~year ~month =
          match (month : Month.t) with
          | Jan | Mar | May | Jul | Aug | Oct | Dec -> 31
          | Apr | Jun | Sep | Nov -> 30
          | Feb -> if is_leap_year ~year then 29 else 28
        ;;

        let create_exn ~y:year ~m:month ~d:day =
          (* year, month, and day need to be passed as parameters to avoid allocating
             a closure (see unit test below) *)
          let invalid ~year ~month ~day msg =
            invalid_argf
              !"Date.create_exn ~y:%d ~m:%{Month} ~d:%d error: %s"
              year
              month
              day
              msg
              ()
          in
          if year < 0 || year > 9999
          then invalid ~year ~month ~day "year outside of [0..9999]";
          if day <= 0 then invalid ~year ~month ~day "day <= 0";
          let days_in_month = days_in_month ~year ~month in
          if day > days_in_month
          then invalid ~year ~month ~day (sprintf "%d day month violation" days_in_month);
          create0 ~year ~month ~day
        ;;

        (* We don't use Make_binable here, because that would go via an immediate
           tuple or record.  That is exactly the 32 bytes we worked so hard above to
           get rid of.  We also don't want to just bin_io the integer directly
           because that would mean a new bin_io format.  *)

        let bin_read_t buf ~pos_ref =
          let year = Int.bin_read_t buf ~pos_ref in
          let month = Month.Stable.V1.bin_read_t buf ~pos_ref in
          let day = Int.bin_read_t buf ~pos_ref in
          create0 ~year ~month ~day
        ;;

        let __bin_read_t__ _buf ~pos_ref =
          (* __bin_read_t is only needed for variants *)
          Bin_prot.Common.raise_variant_wrong_type "Date.t" !pos_ref
        ;;

        let bin_reader_t =
          { Bin_prot.Type_class.read = bin_read_t; vtag_read = __bin_read_t__ }
        ;;

        let bin_size_t__local t =
          Int.bin_size_t (year t) + Month.bin_size_t (month t) + Int.bin_size_t (day t)
        ;;

        let bin_size_t t = bin_size_t__local t

        let bin_write_t__local buf ~pos t =
          let pos = Int.bin_write_t buf ~pos (year t) in
          let pos = Month.bin_write_t buf ~pos (month t) in
          Int.bin_write_t buf ~pos (day t)
        ;;

        let bin_write_t buf ~pos t = bin_write_t__local buf ~pos t
        let bin_writer_t = { Bin_prot.Type_class.size = bin_size_t; write = bin_write_t }

        let bin_t =
          { Bin_prot.Type_class.reader = bin_reader_t
          ; writer = bin_writer_t
          ; shape = bin_shape_t
          }
        ;;

        let to_int t = t
        let of_int_exn n = create_exn ~y:(year n) ~m:(month n) ~d:(day n)
        let invalid_value__for_internal_use_only = 0

        let%test "invalid value" =
          Exn.does_raise (fun () : t -> of_int_exn invalid_value__for_internal_use_only)
        ;;
      end

      include T

      (** YYYY-MM-DD *)
      let to_string_iso8601_extended t =
        let buf = Bytes.create 10 in
        write_4_digit_int buf ~pos:0 (year t);
        Bytes.set buf 4 '-';
        write_2_digit_int buf ~pos:5 (Month.to_int (month t));
        Bytes.set buf 7 '-';
        write_2_digit_int buf ~pos:8 (day t);
        Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
      ;;

      let to_string = to_string_iso8601_extended

      (** YYYYMMDD *)
      let to_string_iso8601_basic t =
        let buf = Bytes.create 8 in
        write_4_digit_int buf ~pos:0 (year t);
        write_2_digit_int buf ~pos:4 (Month.to_int (month t));
        write_2_digit_int buf ~pos:6 (day t);
        Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
      ;;

      (** MM/DD/YYYY *)
      let to_string_american t =
        let buf = Bytes.create 10 in
        write_2_digit_int buf ~pos:0 (Month.to_int (month t));
        Bytes.set buf 2 '/';
        write_2_digit_int buf ~pos:3 (day t);
        Bytes.set buf 5 '/';
        write_4_digit_int buf ~pos:6 (year t);
        Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
      ;;

      let parse_year4 str pos = read_4_digit_int str ~pos
      let parse_month str pos = Month.of_int_exn (read_2_digit_int str ~pos)
      let parse_day str pos = read_2_digit_int str ~pos

      (** YYYYMMDD *)
      let of_string_iso8601_basic str ~pos =
        if pos + 8 > String.length str
        then invalid_arg "Date.of_string_iso8601_basic: pos + 8 > string length";
        create_exn
          ~y:(parse_year4 str pos)
          ~m:(parse_month str (pos + 4))
          ~d:(parse_day str (pos + 6))
      ;;

      (* WARNING: if you are going to change this function in a material way, be sure you
         understand the implications of working in Stable *)
      let of_string s =
        let invalid () = failwith ("invalid date: " ^ s) in
        let ensure b = if not b then invalid () in
        let month_num ~year ~month ~day =
          create_exn ~y:(parse_year4 s year) ~m:(parse_month s month) ~d:(parse_day s day)
        in
        let month_abrv ~year ~month ~day =
          create_exn
            ~y:(parse_year4 s year)
            ~m:(Month.of_string (String.sub s ~pos:month ~len:3))
            ~d:(parse_day s day)
        in
        if String.contains s '/'
        then (
          let y, m, d =
            match String.split s ~on:'/' with
            | [ a; b; c ] ->
              if String.length a = 4 then a, b, c (* y/m/d *) else c, a, b (* m/d/y *)
            | _ -> invalid ()
          in
          let year = Int.of_string y in
          let year =
            if year >= 100 then year else if year < 75 then 2000 + year else 1900 + year
          in
          let month = Month.of_int_exn (Int.of_string m) in
          let day = Int.of_string d in
          create_exn ~y:year ~m:month ~d:day)
        else if String.contains s '-'
        then (
          (* yyyy-mm-dd *)
          ensure (String.length s = 10 && Char.( = ) s.[4] '-' && Char.( = ) s.[7] '-');
          month_num ~year:0 ~month:5 ~day:8)
        else if String.contains s ' '
        then
          if String.length s = 11 && Char.( = ) s.[2] ' ' && Char.( = ) s.[6] ' '
          then (* DD MMM YYYY *)
            month_abrv ~day:0 ~month:3 ~year:7
          else (
            (* YYYY MMM DD *)
            ensure (String.length s = 11 && Char.( = ) s.[4] ' ' && Char.( = ) s.[8] ' ');
            month_abrv ~day:9 ~month:5 ~year:0)
        else if String.length s = 9
        then (* DDMMMYYYY *)
          month_abrv ~day:0 ~month:2 ~year:5
        else if String.length s = 8
        then (* assume YYYYMMDD *)
          month_num ~year:0 ~month:4 ~day:6
        else invalid ()
      ;;

      let of_string s =
        try of_string s with
        | exn -> invalid_argf "Date.of_string (%s): %s" s (Exn.to_string exn) ()
      ;;

      module Sexpable = struct
        module Old_date = struct
          type t =
            { y : int
            ; m : int
            ; d : int
            }
          [@@deriving sexp]

          let to_date t = T.create_exn ~y:t.y ~m:(Month.of_int_exn t.m) ~d:t.d
        end

        let t_of_sexp = function
          | Sexp.Atom s -> of_string s
          | Sexp.List _ as sexp -> Old_date.to_date (Old_date.t_of_sexp sexp)
        ;;

        let t_of_sexp s =
          try t_of_sexp s with
          | Of_sexp_error _ as exn -> raise exn
          | Invalid_argument a -> of_sexp_error a s
        ;;

        let sexp_of_t t = Sexp.Atom (to_string t)

        let t_sexp_grammar =
          let open Sexplib in
          Sexp_grammar.tag
            (Sexp_grammar.coerce String.t_sexp_grammar : t Sexp_grammar.t)
            ~key:Sexp_grammar.type_name_tag
            ~value:(Atom "Core.Date.t")
        ;;
      end

      include Sexpable
      include (val Comparator.Stable.V1.make ~compare ~sexp_of_t)
    end

    include Without_comparable
    include Comparable.Stable.V1.With_stable_witness.Make (Without_comparable)
    include Hashable.Stable.V1.With_stable_witness.Make (Without_comparable)
    include Diffable.Atomic.Make (Without_comparable)
  end

  module Option = struct
    module V1 = struct
      type t = int
      [@@deriving
        bin_io ~localize
        , bin_shape ~basetype:"826a3e79-3321-451a-9707-ed6c03b84e2f"
        , compare
        , equal
        , hash
        , typerep
        , stable_witness]

      let none = V1.(to_int invalid_value__for_internal_use_only)
      let is_none t = t = none
      let is_some t = not (is_none t)
      let some_is_representable _ = true
      let some t = V1.to_int t
      let unchecked_value = V1.of_int_unchecked
      let to_option t = if is_some t then Some (unchecked_value t) else None

      let of_option opt =
        match opt with
        | None -> none
        | Some v -> some v
      ;;

      let value_exn t =
        if is_some t
        then unchecked_value t
        else raise_s [%message [%here] "Date.Option.value_exn none"]
      ;;

      let value t ~default = Bool.select (is_none t) default (unchecked_value t)
      let sexp_of_t t = to_option t |> Option.sexp_of_t V1.sexp_of_t
      let t_of_sexp sexp = (Option.t_of_sexp V1.t_of_sexp) sexp |> of_option
      let t_sexp_grammar = Sexplib.Sexp_grammar.coerce [%sexp_grammar: V1.t Option.t]

      let of_int_exn t =
        (* Don't just blindly convert from an integer. Use [V1.of_int_exn] to validate the
           date. *)
        if t = none then none else some (V1.of_int_exn t)
      ;;

      let to_int t = t
    end
  end
end

module Without_comparable = Stable.V1.Without_comparable
include Without_comparable
module C = Comparable.Make_binable_using_comparator (Without_comparable)
include C

include Diffable.Atomic.Make (struct
  include Without_comparable
  include C
end)

module O = struct
  include (C : Comparable.Infix with type t := t)
end

include (
  Hashable.Make_binable (struct
    include T
    include Sexpable
    include Binable

    let compare (a : t) (b : t) = compare a b
  end) :
    Hashable.S_binable with type t := t)

include Pretty_printer.Register (struct
  type nonrec t = t

  let module_name = "Core.Date"
  let to_string = to_string
end)

let unix_epoch = create_exn ~y:1970 ~m:Jan ~d:1

(* The Days module is used for calculations that involve adding or removing a known number
   of days from a date.  Internally the date is translated to a day number, the days are
   added, and the new date is returned.  Those interested in the math can read:

   http://alcor.concordia.ca/~gpkatch/gdate-method.html

   note: unit tests are in lib_test/time_test.ml
*)
module Days : sig
  type date = t
  type t [@@immediate]

  val of_date : date -> t
  val to_date : t -> date
  val diff : t -> t -> int
  val add_days : t -> int -> t
  val unix_epoch : t
end
with type date := t = struct
  open Int

  type t = int

  let of_year y = (365 * y) + (y / 4) - (y / 100) + (y / 400)

  let of_date date =
    let m = (Month.to_int (month date) + 9) % 12 in
    let y = year date - (m / 10) in
    of_year y + (((m * 306) + 5) / 10) + (day date - 1)
  ;;

  let c_10_000 = Int63.of_int 10_000
  let c_14_780 = Int63.of_int 14_780
  let c_3_652_425 = Int63.of_int 3_652_425

  let to_date days =
    let y =
      let open Int63 in
      to_int_exn (((c_10_000 * of_int days) + c_14_780) / c_3_652_425)
    in
    let ddd = days - of_year y in
    let y, ddd =
      if ddd < 0
      then (
        let y = y - 1 in
        y, days - of_year y)
      else y, ddd
    in
    let mi = ((100 * ddd) + 52) / 3_060 in
    let y = y + ((mi + 2) / 12) in
    let m = ((mi + 2) % 12) + 1 in
    let d = ddd - (((mi * 306) + 5) / 10) + 1 in
    create_exn ~y ~m:(Month.of_int_exn m) ~d
  ;;

  let unix_epoch = of_date unix_epoch
  let add_days t days = t + days
  let diff t1 t2 = t1 - t2
end

let add_days t days = Days.to_date (Days.add_days (Days.of_date t) days)
let diff t1 t2 = Days.diff (Days.of_date t1) (Days.of_date t2)

let add_months t n =
  let total_months = Month.to_int (month t) + n in
  let y = year t + (total_months /% 12) in
  let m = total_months % 12 in
  (* correct for december *)
  let y, m = if Int.( = ) m 0 then y - 1, m + 12 else y, m in
  let m = Month.of_int_exn m in
  (* handle invalid dates for months with fewer number of days *)
  let rec try_create d =
    try create_exn ~y ~m ~d with
    | _exn ->
      assert (Int.( >= ) d 1);
      try_create (d - 1)
  in
  try_create (day t)
;;

let add_years t n = add_months t (n * 12)

(* http://en.wikipedia.org/wiki/Determination_of_the_day_of_the_week#Purely_mathematical_methods

   note: unit tests in lib_test/time_test.ml
*)
let day_of_week =
  let table = [| 0; 3; 2; 5; 0; 3; 5; 1; 4; 6; 2; 4 |] in
  fun t ->
    let m = Month.to_int (month t) in
    let y = if Int.( < ) m 3 then year t - 1 else year t in
    Day_of_week.of_int_exn
      ((y + (y / 4) - (y / 100) + (y / 400) + table.(m - 1) + day t) % 7)
;;

(* http://en.wikipedia.org/wiki/Ordinal_date *)
let non_leap_year_table = [| 0; 31; 59; 90; 120; 151; 181; 212; 243; 273; 304; 334 |]
let leap_year_table = [| 0; 31; 60; 91; 121; 152; 182; 213; 244; 274; 305; 335 |]

let ordinal_date t =
  let table =
    if is_leap_year ~year:(year t) then leap_year_table else non_leap_year_table
  in
  let offset = table.(Month.to_int (month t) - 1) in
  day t + offset
;;

let last_week_of_year y =
  let first_of_year = create_exn ~y ~m:Jan ~d:1 in
  let is t day = Day_of_week.equal (day_of_week t) day in
  if is first_of_year Thu || (is_leap_year ~year:y && is first_of_year Wed)
  then 53
  else 52
;;

(* See http://en.wikipedia.org/wiki/ISO_week_date or ISO 8601 for the details of this
   algorithm.

   Uses a [~f] argument to avoid allocating a tuple when called by [week_number].
*)
let call_with_week_and_year t ~f =
  let ordinal = ordinal_date t in
  let weekday = Day_of_week.iso_8601_weekday_number (day_of_week t) in
  (* [ordinal - weekday + 4] is the ordinal of this week's Thursday, then (n + 6) / 7 is
     division by 7 rounding up *)
  let week = (ordinal - weekday + 10) / 7 in
  let year = year t in
  if Int.( < ) week 1
  then f ~week:(last_week_of_year (year - 1)) ~year:(year - 1)
  else if Int.( > ) week (last_week_of_year year)
  then f ~week:1 ~year:(year + 1)
  else f ~week ~year
;;

let week_number_and_year t = call_with_week_and_year t ~f:(fun ~week ~year -> week, year)
let week_number t = call_with_week_and_year t ~f:(fun ~week ~year:_ -> week)
let is_weekend t = Day_of_week.is_sun_or_sat (day_of_week t)
let is_weekday t = not (is_weekend t)
let is_business_day t ~is_holiday = is_weekday t && not (is_holiday t)

let rec diff_weekend_days t1 t2 =
  if t1 < t2
  then -diff_weekend_days t2 t1
  else (
    (* Basic date diff *)
    let diff = diff t1 t2 in
    (* Compute the number of Saturday -> Sunday crossings *)
    let d1 = day_of_week t1 in
    let d2 = day_of_week t2 in
    let num_satsun_crossings =
      if Int.( < ) (Day_of_week.to_int d1) (Day_of_week.to_int d2)
      then 1 + (diff / 7)
      else diff / 7
    in
    (num_satsun_crossings * 2)
    + (if Day_of_week.( = ) d2 Day_of_week.Sun then 1 else 0)
    + if Day_of_week.( = ) d1 Day_of_week.Sun then -1 else 0)
;;

let diff_weekdays t1 t2 = diff t1 t2 - diff_weekend_days t1 t2

let add_days_skipping t ~skip n =
  let step = if Int.( >= ) n 0 then 1 else -1 in
  let rec loop t k =
    let t_next = add_days t step in
    if skip t then loop t_next k else if Int.( = ) k 0 then t else loop t_next (k - 1)
  in
  loop t (abs n)
;;

let rec first_day_satisfying t ~step ~condition =
  if condition t then t else first_day_satisfying (add_days t step) ~step ~condition
;;

let next_day_satisfying t ~step ~condition =
  let next_day = add_days t step in
  first_day_satisfying next_day ~step ~condition
;;

let following_weekday t = next_day_satisfying t ~step:1 ~condition:is_weekday
let previous_weekday t = next_day_satisfying t ~step:(-1) ~condition:is_weekday
let round_forward_to_weekday t = first_day_satisfying t ~step:1 ~condition:is_weekday
let round_backward_to_weekday t = first_day_satisfying t ~step:(-1) ~condition:is_weekday

let round_forward_to_business_day t ~is_holiday =
  first_day_satisfying t ~step:1 ~condition:(is_business_day ~is_holiday)
;;

let round_backward_to_business_day t ~is_holiday =
  first_day_satisfying t ~step:(-1) ~condition:(is_business_day ~is_holiday)
;;

let add_weekdays t n = add_days_skipping t ~skip:is_weekend n
let add_weekdays_rounding_in_direction_of_step = add_weekdays

let add_weekdays_rounding_forward t n =
  add_days_skipping (round_forward_to_weekday t) ~skip:is_weekend n
;;

let add_weekdays_rounding_backward t n =
  add_days_skipping (round_backward_to_weekday t) ~skip:is_weekend n
;;

let add_business_days t ~is_holiday n =
  add_days_skipping t n ~skip:(fun d -> is_weekend d || is_holiday d)
;;

let add_business_days_rounding_in_direction_of_step = add_business_days

let add_business_days_rounding_forward t ~is_holiday n =
  add_days_skipping (round_forward_to_business_day ~is_holiday t) n ~skip:(fun d ->
    not (is_business_day ~is_holiday d))
;;

let add_business_days_rounding_backward t ~is_holiday n =
  add_days_skipping (round_backward_to_business_day ~is_holiday t) n ~skip:(fun d ->
    not (is_business_day ~is_holiday d))
;;

let dates_between ~min:t1 ~max:t2 =
  let rec loop t l = if t < t1 then l else loop (add_days t (-1)) (t :: l) in
  loop t2 []
;;

let weekdays_between ~min ~max =
  let all_dates = dates_between ~min ~max in
  Option.value_map (List.hd all_dates) ~default:[] ~f:(fun first_date ->
    (* to avoid a system call on every date, we just get the weekday for the first
       date and use it to get all the other weekdays *)
    let first_weekday = day_of_week first_date in
    let date_and_weekdays =
      List.mapi all_dates ~f:(fun i date -> date, Day_of_week.shift first_weekday i)
    in
    List.filter_map date_and_weekdays ~f:(fun (date, weekday) ->
      if Day_of_week.is_sun_or_sat weekday then None else Some date))
;;

let business_dates_between ~min ~max ~is_holiday =
  weekdays_between ~min ~max |> List.filter ~f:(fun d -> not (is_holiday d))
;;

let first_strictly_after t ~on:dow =
  let dow = Day_of_week.to_int dow in
  let tplus1 = add_days t 1 in
  let cur = Day_of_week.to_int (day_of_week tplus1) in
  let diff = (dow + 7 - cur) mod 7 in
  add_days tplus1 diff
;;

module For_quickcheck = struct
  open Quickcheck

  let gen_uniform_incl d1 d2 =
    if d1 > d2
    then
      raise_s
        [%message
          "Date.gen_uniform_incl: bounds are crossed"
            ~lower_bound:(d1 : t)
            ~upper_bound:(d2 : t)];
    Generator.map (Int.gen_uniform_incl 0 (diff d2 d1)) ~f:(fun days -> add_days d1 days)
  ;;

  let gen_incl d1 d2 =
    Generator.weighted_union
      [ 1., Generator.return d1; 1., Generator.return d2; 18., gen_uniform_incl d1 d2 ]
  ;;

  let quickcheck_generator = gen_incl (of_string "1900-01-01") (of_string "2100-01-01")
  let quickcheck_observer = Observer.create (fun t ~size:_ ~hash -> hash_fold_t hash t)
  let quickcheck_shrinker = Shrinker.empty ()
end

let quickcheck_generator = For_quickcheck.quickcheck_generator
let gen_incl = For_quickcheck.gen_incl
let gen_uniform_incl = For_quickcheck.gen_uniform_incl
let quickcheck_observer = For_quickcheck.quickcheck_observer
let quickcheck_shrinker = For_quickcheck.quickcheck_shrinker

module Private = struct
  let leap_year_table = leap_year_table
  let non_leap_year_table = non_leap_year_table
  let ordinal_date = ordinal_date
end

module Option = struct
  module Stable = Stable.Option
  include Stable.V1

  module Optional_syntax = struct
    module Optional_syntax = struct
      let is_none = is_none
      let unsafe_value = unchecked_value
    end
  end

  let quickcheck_generator =
    Quickcheck.Generator.map
      (Option.quickcheck_generator quickcheck_generator)
      ~f:of_option
  ;;

  let quickcheck_shrinker =
    Quickcheck.Shrinker.map
      (Option.quickcheck_shrinker quickcheck_shrinker)
      ~f:of_option
      ~f_inverse:to_option
  ;;

  let quickcheck_observer =
    Quickcheck.Observer.of_hash
      (module struct
        type nonrec t = t [@@deriving hash]
      end)
  ;;

  include Comparable.Make_plain (struct
    type nonrec t = t [@@deriving compare, sexp_of]
  end)
end
