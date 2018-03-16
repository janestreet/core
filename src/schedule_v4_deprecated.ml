open! Import
open Import_time

type zoned   = Zoned [@@deriving compare]
type unzoned = Unzoned [@@deriving compare]

module Inclusive_exclusive = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Inclusive
        | Exclusive
      [@@deriving sexp, bin_io, compare]
    end
  end

  include Stable.V1
end

module type S = sig
  type int_set         [@@deriving compare]
  type ofday_set       [@@deriving compare]
  type day_of_week_set [@@deriving compare]
  type month_set       [@@deriving compare]
  type date_set        [@@deriving compare]
  type boundary_type   [@@deriving compare]
end

module Make (T : S) = struct
  open T

  type ('a, 'b) gadt =
    | In_zone       : Time.Zone.t * (unzoned, 'b) gadt                -> (zoned, 'b) gadt
    | Tag           : 'b * ('a, 'b) gadt                              -> ('a, 'b) gadt
    | And           : ('a, 'b) gadt list                              -> ('a, 'b) gadt
    | Or            : ('a, 'b) gadt list                              -> ('a, 'b) gadt
    | Not           : ('a, 'b) gadt                                   -> ('a, 'b) gadt
    | If_then_else  : (('a, 'b) gadt * ('a, 'b) gadt * ('a, 'b) gadt) -> ('a, 'b) gadt
    | Shift         : Time.Span.t * ('a, 'b) gadt                     -> ('a, 'b) gadt
    | Between       : (Inclusive_exclusive.t * Time.Ofday.t)
                      * (Inclusive_exclusive.t * Time.Ofday.t)      -> (unzoned, 'b) gadt
    | At            : ofday_set                                       -> (unzoned, 'b) gadt
    | Secs          : int_set                                         -> (unzoned, 'b) gadt
    | Mins          : int_set                                         -> (unzoned, 'b) gadt
    | Hours         : int_set                                         -> (unzoned, 'b) gadt
    | Weekdays      : day_of_week_set                                 -> (unzoned, 'b) gadt
    | Days          : int_set                                         -> (unzoned, 'b) gadt
    | Weeks         : int_set                                         -> (unzoned, 'b) gadt
    | Months        : month_set                                       -> (unzoned, 'b) gadt
    | On            : date_set                                        -> (unzoned, 'b) gadt
    | Before        : (Inclusive_exclusive.t * boundary_type)         -> (unzoned, 'b) gadt
    | After         : (Inclusive_exclusive.t * boundary_type)         -> (unzoned, 'b) gadt
    | Always        : ('a, 'b) gadt
    | Never         : ('a, 'b) gadt

  let rec map_tags : type a. (a, 'b) gadt -> f:('b -> 'c) -> (a, 'c) gadt =
    fun t ~f ->
      match t with
      | In_zone (zone, t)         -> In_zone (zone, map_tags t ~f)
      | Tag (tag, t)              -> Tag (f tag, map_tags t ~f)
      | And l                     -> And (List.map l ~f:(fun t     -> map_tags t ~f))
      | Or l                      -> Or (List.map l ~f:(fun t      -> map_tags t ~f))
      | Not t                     -> Not (map_tags t ~f)
      | If_then_else (t1, t2, t3) -> If_then_else (map_tags t1 ~f, map_tags t2 ~f, map_tags t3 ~f)
      | Shift (span, t)           -> Shift (span, map_tags t ~f)
      | Between (s, e)            -> Between (s, e)
      | At ofdays                 -> At ofdays
      | Secs secs                 -> Secs secs
      | Mins mins                 -> Mins mins
      | Hours hrs                 -> Hours hrs
      | Days days                 -> Days days
      | Weeks weeks               -> Weeks weeks
      | Weekdays weekdays         -> Weekdays weekdays
      | Months months             -> Months months
      | On dates                  -> On dates
      | Before date               -> Before date
      | After date                -> After date
      | Always                    -> Always
      | Never                     -> Never
  ;;

  let compare_lists compare_zoning compare_a compare l1 l2 =
    let rec loop l1 l2 =
      match l1, l2 with
      | [], [] -> 0
      | [], _  -> -1
      | _, []  -> 1
      | t1 :: t1_rest, t2 :: t2_rest ->
        let c = compare compare_zoning compare_a t1 t2 in
        if c = 0
        then loop t1_rest t2_rest
        else c
    in
    loop l1 l2
  ;;

  let rec compare_gadt :
    type zoning.
    (zoning -> zoning -> int)
    -> ('b -> 'b -> int)
    -> (zoning, 'b) gadt
    -> (zoning, 'b) gadt
    -> int =
    fun compare_zoning compare_a t1 t2 ->
      match t1, t2 with
      | In_zone (z1, t1), In_zone (z2, t2) ->
        let c = Time.Zone.compare z1 z2 in
        if c = 0
        then compare_gadt compare_unzoned compare_a t1 t2
        else c
      | Tag (tag1, t1), Tag (tag2, t2) ->
        let c = compare_a tag1 tag2 in
        if c = 0
        then compare_gadt compare_zoning compare_a t1 t2
        else c
      | And l1, And l2
      | Or l1, Or l2   -> compare_lists compare_zoning compare_a compare_gadt l1 l2
      | Not t1, Not t2 -> compare_gadt compare_zoning compare_a t1 t2
      | If_then_else (s1, s2, s3), If_then_else (s1', s2', s3') ->
        compare_lists compare_zoning compare_a compare_gadt [s1; s2; s3] [s1'; s2'; s3']
      | Shift (span1, t1), Shift (span2, t2) ->
        let c = Time.Span.compare span1 span2 in
        if c = 0
        then compare_gadt compare_zoning compare_a t1 t2
        else c
      | Between ((sie1, s1), (eie1, e1)), Between ((sie2, s2), (eie2, e2)) ->
        List.find ~f:(fun c -> Int.(<>) c 0)
          [ Inclusive_exclusive.compare sie1 sie2
          ; Inclusive_exclusive.compare eie1 eie2
          ; Time.Ofday.compare s1 s2
          ; Time.Ofday.compare e1 e2 ]
        |> Option.value ~default:0
      | At s1, At s2                         -> compare_ofday_set s1 s2
      | Secs s1, Secs s2                     -> compare_int_set s1 s2
      | Mins s1, Mins s2                     -> compare_int_set s1 s2
      | Hours s1, Hours s2                   -> compare_int_set s1 s2
      | Weekdays s1, Weekdays s2             -> compare_day_of_week_set s1 s2
      | Days s1, Days s2                     -> compare_int_set s1 s2
      | Weeks s1, Weeks s2                   -> compare_int_set s1 s2
      | Months s1, Months s2                 -> compare_month_set s1 s2
      | On s1, On s2                         -> compare_date_set s1 s2
      | Before (ie1, bt1), Before (ie2, bt2) ->
        let c = Inclusive_exclusive.compare ie1 ie2 in
        if c = 0
        then compare_boundary_type bt1 bt2
        else c
      | After (ie1, bt1), After (ie2, bt2)   ->
        let c = Inclusive_exclusive.compare ie1 ie2 in
        if c = 0
        then compare_boundary_type bt1 bt2
        else c
      | Always, Always    -> 0
      | Never, Never      -> 0
      | In_zone _, _       -> Pervasives.compare t1 t2
      | Tag _, _           -> Pervasives.compare t1 t2
      | And _, _           -> Pervasives.compare t1 t2
      | Or _, _            -> Pervasives.compare t1 t2
      | Not _, _           -> Pervasives.compare t1 t2
      | If_then_else _, _  -> Pervasives.compare t1 t2
      | Shift _, _         -> Pervasives.compare t1 t2
      | Between _, _       -> Pervasives.compare t1 t2
      | At _, _            -> Pervasives.compare t1 t2
      | Secs _, _          -> Pervasives.compare t1 t2
      | Mins _, _          -> Pervasives.compare t1 t2
      | Hours _, _         -> Pervasives.compare t1 t2
      | Weekdays _, _      -> Pervasives.compare t1 t2
      | Days _, _          -> Pervasives.compare t1 t2
      | Weeks _, _         -> Pervasives.compare t1 t2
      | Months _, _        -> Pervasives.compare t1 t2
      | On _, _            -> Pervasives.compare t1 t2
      | Before _, _        -> Pervasives.compare t1 t2
      | After _, _         -> Pervasives.compare t1 t2
      | Always, _          -> Pervasives.compare t1 t2
      | Never, _           -> Pervasives.compare t1 t2
  ;;
end

module External = Make (struct
    type int_set         = int list [@@deriving compare]
    type ofday_set       = Time.Ofday.t list [@@deriving compare]
    type day_of_week_set = Day_of_week.t list [@@deriving compare]
    type month_set       = Month.t list [@@deriving compare]
    type date_set        = Date.t list [@@deriving compare]
    type boundary_type   = Date.t * Time.Ofday.t [@@deriving compare]
  end)

type 'b zoned_t = (zoned, 'b) External.gadt

module Stable = struct

  (* we want to be included in Core.Core_stable, so we can't rely on it here, forcing us to be
     careful in the type definitions below to use the stable versions. *)
  module Date                = Date.Stable
  module Ofday               = Time.Stable.Ofday
  module Span                = Time.Stable.Span
  module Day_of_week         = Day_of_week.Stable
  module Month               = Month.Stable
  module Zone                = Time.Stable.Zone
  module Binable             = Binable.Stable
  module Inclusive_exclusive = Inclusive_exclusive.Stable

  module V4 = struct
    module Serializable = struct
      module Unzoned = struct
        type 'a t =
          | Tag      of 'a * 'a t
          | And      of 'a t list
          | Or       of 'a t list
          | Not      of 'a t
          | Shift    of Span.V2.t * 'a t
          | Between  of (Inclusive_exclusive.V1.t * Ofday.V1.t) * (Inclusive_exclusive.V1.t * Ofday.V1.t)
          | At       of Ofday.V1.t list
          | Secs     of int list
          | Mins     of int list
          | Hours    of int list
          | Weekdays of Day_of_week.V1.t list
          | Days     of int list
          | Weeks    of int list
          | Months   of Month.V1.t list
          | On       of Date.V1.t list
          | Before   of Inclusive_exclusive.V1.t * (Date.V1.t * Ofday.V1.t)
          | After    of Inclusive_exclusive.V1.t * (Date.V1.t * Ofday.V1.t)
          | Always
          | Never
          | If_then_else of 'a t * 'a t * 'a t
        [@@deriving sexp, bin_io]

        let rec of_gadt (gadt : (unzoned, 'b) External.gadt) =
          let module E = External in
          match gadt with
          | E.Tag      (tag, t)                     -> Tag (tag, of_gadt t)
          | E.And      l                            -> And (List.map l ~f:of_gadt)
          | E.Or       l                            -> Or (List.map l ~f:of_gadt)
          | E.Not      t                            -> Not (of_gadt t)
          | E.If_then_else (t1, t2, t3)             -> If_then_else (of_gadt t1, of_gadt t2, of_gadt t3)
          | E.Shift    (span, t)                    -> Shift (span, of_gadt t)
          | E.Between  ((start_inc_exc, s), (end_inc_exc, e)) -> Between ((start_inc_exc, s), (end_inc_exc, e))
          | E.At       ofdays                       -> At ofdays
          | E.Secs     secs                         -> Secs secs
          | E.Mins     mins                         -> Mins mins
          | E.Hours    hours                        -> Hours hours
          | E.Weekdays weekdays                     -> Weekdays weekdays
          | E.Days     days                         -> Days days
          | E.Weeks    weeks                        -> Weeks weeks
          | E.Months   months                       -> Months months
          | E.On       dates                        -> On dates
          | E.Before   (in_out, (date, ofday))      -> Before (in_out, (date, ofday))
          | E.After    (in_out, (date, ofday))      -> After (in_out, (date, ofday))
          | E.Always                                -> Always
          | E.Never                                 -> Never
        ;;

        let rec to_gadt t : (unzoned, 'b) External.gadt =
          let module E = External in
          match t with
          | Tag      (tag, t)         -> E.Tag (tag, to_gadt t)
          | And      l                -> E.And (List.map l ~f:to_gadt)
          | Or       l                -> E.Or (List.map l ~f:to_gadt)
          | Not      t                -> E.Not (to_gadt t)
          | If_then_else (t1, t2, t3) -> E.If_then_else (to_gadt t1, to_gadt t2, to_gadt t3)
          | Shift    (span, t)        -> E.Shift (span, to_gadt t)
          | Between  (s, e)           -> E.Between (s, e)
          | At       ofdays           -> E.At ofdays
          | Secs     secs             -> E.Secs secs
          | Mins     mins             -> E.Mins mins
          | Hours    hours            -> E.Hours hours
          | Weekdays weekdays         -> E.Weekdays weekdays
          | Days     days             -> E.Days days
          | Weeks    weeks            -> E.Weeks weeks
          | Months   months           -> E.Months months
          | On       dates            -> E.On dates
          | Before   (date, ofday)    -> E.Before (date, ofday)
          | After    (date, ofday)    -> E.After (date, ofday)
          | Always                    -> E.Always
          | Never                     -> E.Never
        ;;
      end

      type 'a t =
        | In_zone      of Zone.V1.t * 'a Unzoned.t
        | Tag          of 'a * 'a t
        | And          of 'a t list
        | Or           of 'a t list
        | Not          of 'a t
        | Shift        of (Span.V2.t * 'a t)
        | Always
        | Never
        | If_then_else of ('a t * 'a t * 'a t)
      [@@deriving sexp, bin_io]

      let rec of_gadt : (zoned, 'b) External.gadt -> 'b t =
        fun gadt ->
          let module E = External in
          match gadt with
          | E.In_zone  (zone, t)        -> In_zone (zone, Unzoned.of_gadt t)
          | E.Tag      (tag, t)         -> Tag (tag, of_gadt t)
          | E.And      l                -> And (List.map l ~f:of_gadt)
          | E.Or       l                -> Or (List.map l ~f:of_gadt)
          | E.Not      t                -> Not (of_gadt t)
          | E.If_then_else (t1, t2, t3) -> If_then_else (of_gadt t1, of_gadt t2, of_gadt t3)
          | E.Shift    (span, t)        -> Shift (span, of_gadt t)
          | E.Always                    -> Always
          | E.Never                     -> Never
      ;;

      let rec to_gadt t : (zoned,_) External.gadt =
        let module E = External in
        match t with
        | In_zone  (zone, t)        -> E.In_zone (zone, Unzoned.to_gadt t)
        | Tag      (tag, t)         -> E.Tag (tag, to_gadt t)
        | And      l                -> E.And (List.map l ~f:to_gadt)
        | Or       l                -> E.Or (List.map l ~f:to_gadt)
        | Not      t                -> E.Not (to_gadt t)
        | If_then_else (t1, t2, t3) -> E.If_then_else (to_gadt t1, to_gadt t2, to_gadt t3)
        | Shift    (span, t)        -> E.Shift (span, to_gadt t)
        | Always                    -> E.Always
        | Never                     -> E.Never
      ;;
    end

    type 'b t = (zoned, 'b) External.gadt
    [@@deriving compare]

    let map t ~f = External.map_tags t ~f

    let t_of_sexp tag_of_sexp sexp =
      Serializable.(to_gadt (t_of_sexp tag_of_sexp sexp))
    ;;

    let sexp_of_t sexp_of_tag t =
      Serializable.(sexp_of_t sexp_of_tag (of_gadt t))
    ;;

    let flag (type b) ?(flag_name = "schedule") ?default ?(doc = "") m () =
      let open Command.Param in
      let module B = (val m : Sexpable.S with type t = b) in
      let arg_type = sexp_conv [%of_sexp: B.t t] in
      match default with
      | None ->
        flag flag_name (required arg_type) ~doc:(sprintf "SCHEDULE %s" doc)
      | Some def ->
        flag flag_name (optional_with_default def arg_type)
          ~doc:(sprintf "SCHEDULE %s (default: %s)"
                  doc (Sexp.to_string_mach (sexp_of_t B.sexp_of_t def)))
    ;;

    include Binable.Of_binable1.V1 (Serializable) (struct
        let to_binable t            = Serializable.of_gadt t
        let of_binable serializable = Serializable.to_gadt serializable

        type nonrec 'a t = 'a t
      end)
  end
end

module Array_set : sig
  type 'a t

  val create  : 'a list -> cmp:('a -> 'a -> int) -> 'a t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val mem     : 'a t -> 'a -> bool

  module Ofday : sig
    type t
    type ofday = Time.Ofday.t

    val create             : ofday list -> t
    val compare            : t -> t -> int
    val mem                : t -> ofday -> bool
    val next_after_rolling : t -> Time.t -> zone:Time.Zone.t -> Time.t option
  end
end = struct
  module T = struct
    type 'a t =
      { set     : 'a Array.t
      ; compare : 'a -> 'a -> int }

    let compare compare_a t1 t2 = Array.compare compare_a t1.set t2.set

    let create l ~cmp:compare =
      let set =
        List.dedup_and_sort l ~compare
        |> List.sort ~compare
        |> Array.of_list
      in
      { set; compare }
    ;;

    let next_after t time ~zone =
      let date = Time.to_date time ~zone in
      let compare ofday t = Time.compare (Time.of_date_ofday date ofday ~zone) t  in
      match Array.binary_search t.set ~compare `First_strictly_greater_than time with
      | None   -> None
      | Some i -> Some t.set.(i)
    ;;

    let%expect_test "test next_after rolling around dst" =
      let t =
        create ~cmp:Time.Ofday.compare
          [ Time.Ofday.create ~hr:1 ()
          ; Time.Ofday.create ~hr:2 ()
          ; Time.Ofday.create ~hr:3 ()
          ]
      in
      let date = Date.of_string "2018-03-11" in
      let zone = Lazy.force Time.Zone.local  in
      let next_after ~hr =
        Time.Ofday.create ~hr ()
        |> Time.of_date_ofday date ~zone
        |> next_after t  ~zone
      in
      let next = next_after ~hr:1 in
      print_s [%sexp (next : Time.Ofday.t option)];
      [%expect {| (02:00:00.000000) |}];
      let next = next_after ~hr:2 in
      print_s [%sexp (next : Time.Ofday.t option)];
      [%expect {| () |}];
      let next = next_after ~hr:3 in
      print_s [%sexp (next : Time.Ofday.t option)];
      [%expect {| () |}];
    ;;

    let next_after_rolling t time ~zone =
      let date = Time.to_date time ~zone in
      match next_after t time ~zone with
      | Some ofday -> Some (Time.of_date_ofday date ofday ~zone)
      | None ->
        if Array.length t.set = 0
        then None
        else Some (Time.of_date_ofday (Date.add_days date 1) t.set.(0) ~zone)
    ;;

    let mem t ?cmp v =
      let compare = Option.value cmp ~default:t.compare in
      (* we do a little loop unrolling because we commonly have very few elements *)
      match t.set with
      | [| v0 |] -> compare v0 v = 0
      | [| v0; v1 |] ->
        compare v0 v = 0
        || compare v1 v = 0
      | [| v0; v1; v2 |] ->
        compare v0 v = 0
        || compare v1 v = 0
        || compare v2 v = 0
      | [| v0; v1; v2; v3 |] ->
        compare v0 v = 0
        || compare v1 v = 0
        || compare v2 v = 0
        || compare v3 v = 0
      | _        ->
        begin match Array.binary_search t.set ~compare `First_equal_to v with
        | None   -> false
        | Some _ -> true
        end
    ;;
  end

  type 'a t = 'a T.t

  let create  = T.create
  let compare = T.compare
  let mem t v = T.mem t v

  module Ofday = struct
    type t = Time.Ofday.t T.t
    type ofday = Time.Ofday.t

    let create  = T.create ~cmp:Time.Ofday.compare
    let compare = T.compare Time.Ofday.compare
    let mem t v = T.mem t ~cmp:Time.Ofday.compare v
    let next_after_rolling t v = T.next_after_rolling t v
  end
end

(* a wall clock time in a known zone *)
module Internal_time : sig
  type t

  val of_time : Time.t -> zone:Time.Zone.t -> t

  val zone    : t -> Time.Zone.t
  val date    : t -> Date.t
  val month   : t -> Month.t
  val week    : t -> int
  val day     : t -> int
  val weekday : t -> Day_of_week.t
  val hour    : t -> int
  val sec     : t -> int
  val min     : t -> int
  val ofday   : t -> Time.Ofday.t

  val span_to_possible_difference_in
    :  t
    -> [ `Date | `Month | `Week | `Weekday | `Day | `Hour | `Min | `Sec ]
    -> Time.Span.t

end = struct
  type t =
    { date          : Date.t
    ; ofday         : Time.Ofday.t
    ; zone          : Time.Zone.t
    }

  let zone t    = t.zone
  let date t    = t.date
  let month t   = Date.month t.date
  let week t    = Date.week_number t.date
  let day t     = Date.day t.date
  let weekday t = Date.day_of_week t.date

  module Fast_parts_of_ofday = struct

    let seconds_since_start_of_day t =
      Time.Ofday.to_span_since_start_of_day t.ofday
      |> Time.Span.to_sec
      |> Float.iround_down_exn
    ;;

    let hour_in_seconds = 60 * 60
    ;;

    let hour t =
      seconds_since_start_of_day t
      |> fun sec ->
      (sec / hour_in_seconds) % 24
    ;;

    let day_in_seconds = 24 * hour_in_seconds
    ;;

    let test_against_parts ~from_parts ~from_t =
      let date = Date.create_exn ~y:2000 ~m:Jan ~d:1 in
      for i = 0 to day_in_seconds - 1 do
        let ofday =
          Time.Ofday.of_span_since_start_of_day
            (Time.Span.of_sec (Float.of_int i))
        in
        let parts = Time.Ofday.to_parts ofday in
        assert (from_parts parts = from_t { date; ofday; zone = Time.Zone.utc })
      done
    ;;

    let%test_unit "hour equals parts for all seconds" =
      test_against_parts ~from_parts:(fun p -> p.hr) ~from_t:hour
    ;;

    let min t =
      seconds_since_start_of_day t
      |> fun sec ->
      (sec / 60) % 60
    ;;

    let%test_unit "min equals parts for all seconds" =
      test_against_parts ~from_parts:(fun p -> p.min) ~from_t:min
    ;;

    let sec t =
      (seconds_since_start_of_day t) % 60
    ;;

    let%test_unit "sec equals parts for all seconds" =
      test_against_parts ~from_parts:(fun p -> p.sec) ~from_t:sec
    ;;
  end

  include Fast_parts_of_ofday

  (* It is possble to see the 24th hour of the day due to permissiveness in our
     Ofday.t parsing and/or floating point math issues.  In these cases we consider
     the hour of the day to be the 0th hour of the next date. *)
  let normalize t =
    if hour t = 24
    then { t with date = Date.add_days t.date 1; ofday = Time.Ofday.start_of_day }
    else t
  ;;

  let next_t_with_difference_in t field =
    let t =
      match field with
      | `Date | `Week | `Weekday | `Day ->
        {t with date = Date.add_days t.date 1; ofday = Time.Ofday.start_of_day }
      | `Month ->
        let t_month = month t in
        let next_month = Month.shift t_month 1 in
        let date =
          if Month.(>) next_month (month t)
          then Date.create_exn ~d:1 ~m:next_month ~y:(Date.year t.date)
          else Date.create_exn ~d:1 ~m:next_month ~y:(Date.year t.date + 1)
        in
        {t with date; ofday = Time.Ofday.start_of_day }
      | `Hour ->
        let date, ofday =
          let hr = hour t in
          if Int.(=) hr 23
          then (Date.add_days t.date 1, Time.Ofday.start_of_day)
          else t.date, Time.Ofday.create ~hr:(hr + 1) ()
        in
        {t with date; ofday }
      | `Min ->
        let date, ofday =
          let hr = hour t in
          let m = min t in
          if Int.(=) hr 23 && Int.(=) m 59
          then (Date.add_days t.date 1, Time.Ofday.start_of_day)
          else if Int.(=) m 59
          then (t.date, Time.Ofday.create ~hr:(hr + 1) ~min:0 ())
          else (t.date, Time.Ofday.create ~hr ~min:(m + 1) ())
        in
        {t with date; ofday }
      | `Sec ->
        let span = Time.Ofday.to_span_since_start_of_day t.ofday in
        let next_sec_span =
          Time.Span.to_sec span
          |> Float.round_down
          |> (+.) 1.
          |> Time.Span.of_sec
        in
        let date, ofday =
          if Time.Span.(>=) span Time.Span.day
          then Date.add_days t.date 1, Time.Ofday.start_of_day
          else t.date, Time.Ofday.of_span_since_start_of_day next_sec_span
        in
        {t with date; ofday }
    in
    normalize t
  ;;

  let span_to_possible_difference_in t field =
    let next_t     = next_t_with_difference_in t field in
    let end_time   = Time.of_date_ofday ~zone:t.zone next_t.date next_t.ofday in
    let start_time = Time.of_date_ofday ~zone:t.zone t.date t.ofday in
    Time.diff end_time start_time
  ;;

  let ofday t = t.ofday

  let of_time time ~zone =
    let date, ofday = Time.to_date_ofday time ~zone in
    normalize
      { date
      ; ofday
      ; zone }
  ;;
end

module Internal = struct
  module T =
    Make (struct
      type int_set         = int Array_set.t [@@deriving compare]
      type ofday_set       = Array_set.Ofday.t [@@deriving compare]
      type day_of_week_set = Day_of_week.t Array_set.t [@@deriving compare]
      type month_set       = Month.t Array_set.t [@@deriving compare]
      type date_set        = Date.t Array_set.t [@@deriving compare]
      type boundary_type   = Time.t [@@deriving compare]
    end)
  include T
  type ('a, 'b) t = ('a, 'b) gadt

  let of_external (ext : (zoned, _) External.gadt) =
    (* this outer type annotation forces all external t's to be zoned, which enables us to
       know that we will always have passed an In_zone barrier before encountering Before
       or After.  *)
    let rec loop : type a. (a, 'b) External.gadt -> Time.Zone.t -> (a, 'b) T.gadt =
      fun ext zone ->
        let int_set   = Array_set.create ~cmp:Int.compare in
        let ofday_set = Array_set.Ofday.create in
        let to_boundary in_out date ofday zone =
          in_out, Time.of_date_ofday ~zone date ofday
        in
        let module E = External in
        match ext with
        | E.In_zone (zone, ext)  -> In_zone (zone, loop ext zone)
        | E.Tag (tag, ext)       -> Tag (tag, loop ext zone)
        | E.And l                -> And (List.map l ~f:(fun t -> loop t zone))
        | E.Or l                 -> Or (List.map l ~f:(fun t -> loop t zone))
        | E.Not t                -> Not (loop t zone)
        | E.If_then_else (t1, t2, t3) ->
          If_then_else (loop t1 zone, loop t2 zone, loop t3 zone)
        | E.Shift (span, t)      -> Shift (span, loop t zone)
        | E.Between ((start_inc_exc, s), (end_inc_exc, e)) ->
          Between ((start_inc_exc, s), (end_inc_exc, e))
        | E.At ofdays            -> At (ofday_set ofdays)
        | E.Secs secs            -> Secs (int_set secs)
        | E.Mins mins            -> Mins (int_set mins)
        | E.Hours hrs            -> Hours (int_set hrs)
        | E.Days days            -> Days (int_set days)
        | E.Weeks weeks          -> Weeks (int_set weeks)
        | E.Weekdays wkds        -> Weekdays (Array_set.create ~cmp:Day_of_week.compare wkds)
        | E.Months months        -> Months (Array_set.create ~cmp:Month.compare months)
        | E.On dates             -> On (Array_set.create ~cmp:Date.compare dates)
        | E.Before (in_out, (date, ofday)) -> Before (to_boundary in_out date ofday zone)
        | E.After (in_out, (date, ofday))  -> After (to_boundary in_out date ofday zone)
        | E.Always               -> Always
        | E.Never                -> Never
    in
    loop ext Time.Zone.utc
  ;;
end

(* Our Schedule has an invariant: if you start with a (zoned, tag) t
   and descend recursively, you have a (unzoned,tag) t if and only if,
   you have traversed a In_zone.
   This In_zone can be used to define a context of some type 'a.
   This invariant is modelled in the following type. *)
module O = struct
  type (_, 'a) t =
    | Z : Time.t                    -> (zoned,   'a) t
    | U : Time.t * Time.Zone.t * 'a -> (unzoned, 'a) t
end

let shift :
  type zoning.
  Time.Span.t
  -> (zoning, _) Internal.t
  -> (zoning, Internal_time.t) O.t
  -> ((zoning, _) Internal.t -> (zoning, Internal_time.t) O.t -> 'a)
  -> 'a
  =
  fun span t o loop ->
    let inverted_span = Time.Span.of_sec (Float.neg (Time.Span.to_sec span)) in
    begin match o with
    | O.U (time, zone, _) ->
      let shifted_time = Time.add time inverted_span in
      loop t (O.U (shifted_time, zone, Internal_time.of_time shifted_time ~zone))
    | O.Z time ->
      let shifted_time = Time.add time inverted_span in
      loop t (O.Z shifted_time)
    end
;;

let boundary_test
      (type s)
      (module Boundary: Comparable.S with type t = s)
      dir
      (in_out : Inclusive_exclusive.t)
  : (s -> s -> bool)  =
  match dir, in_out with
  | `Before, Inclusive -> Boundary.(<=)
  | `Before, Exclusive -> Boundary.(<)
  | `After , Inclusive -> Boundary.(>=)
  | `After , Exclusive -> Boundary.(>)
;;

module Valid_invalid_span = struct
  type t =
    | In_range_for_at_least of Time.Span.t
    | Out_of_range_for_at_least of Time.Span.t
  [@@deriving sexp_of]

  let never   = Out_of_range_for_at_least Time.Span.day
  let forever = In_range_for_at_least Time.Span.day

  let flip t =
    match t with
    | In_range_for_at_least s     -> Out_of_range_for_at_least s
    | Out_of_range_for_at_least s -> In_range_for_at_least s
  ;;

  let limit_to t s2 =
    match t with
    | In_range_for_at_least s1 -> In_range_for_at_least (Time.Span.min s1 s2)
    | Out_of_range_for_at_least s1 -> Out_of_range_for_at_least (Time.Span.min s1 s2)
  ;;

  let with_span t f =
    match t with
    | In_range_for_at_least s     -> In_range_for_at_least (f s)
    | Out_of_range_for_at_least s -> Out_of_range_for_at_least (f s)
  ;;
end

(* In includes_{and,or} we must always return the min, because otherwise we can miss tag
   changes.  Tag changes can happen at any branch of an and/or, so we always have to check
   all changes.  This includes out_of_ranges as well because if you not them, they become
   in_range, and can have tags.
*)
let includes_and ~output_includes_tags l o loop =
  let open Valid_invalid_span in
  match l with
  | []      -> Valid_invalid_span.forever
  | x :: xs ->
    let init = loop x o in
    let min_or_max =
      if output_includes_tags
      then Time.Span.min
      else Time.Span.max
    in
    List.fold xs ~init ~f:(fun acc t ->
      match acc, loop t o with
      | Out_of_range_for_at_least s1, Out_of_range_for_at_least s2 ->
        Out_of_range_for_at_least (min_or_max s1 s2)
      | Out_of_range_for_at_least s1, In_range_for_at_least s2
      | In_range_for_at_least     s1, Out_of_range_for_at_least s2 ->
        Out_of_range_for_at_least (Time.Span.min s1 s2)
      | In_range_for_at_least s1, In_range_for_at_least s2 ->
        In_range_for_at_least (Time.Span.min s1 s2))
;;

let includes_or ~output_includes_tags l o loop =
  let open Valid_invalid_span in
  match l with
  | []      -> Valid_invalid_span.never
  | x :: xs ->
    let init = loop x o in
    let min_or_max =
      if output_includes_tags
      then Time.Span.min
      else Time.Span.max
    in
    List.fold xs ~init ~f:(fun acc t ->
      match acc, loop t o with
      | In_range_for_at_least     s1, In_range_for_at_least s2 ->
        In_range_for_at_least (min_or_max s1 s2)
      | Out_of_range_for_at_least s1, In_range_for_at_least s2
      | In_range_for_at_least     s1, Out_of_range_for_at_least s2 ->
        In_range_for_at_least (Time.Span.min s1 s2)
      | Out_of_range_for_at_least s1, Out_of_range_for_at_least s2 ->
        Out_of_range_for_at_least (Time.Span.min s1 s2))
;;

let calculate_ofday_diff ~s ~e =
  let s = Time.Ofday.to_span_since_start_of_day s in
  let e = Time.Ofday.to_span_since_start_of_day e in
  assert (Time.Span.(<=) s e);
  Time.Span.(-) e s
;;

let s_is_inrange_today_after_ofday ~start_inc_exc ~s ~ofday =
  match (start_inc_exc : Inclusive_exclusive.t)  with
  | Inclusive -> Time.Ofday.(<) ofday s
  | Exclusive -> Time.Ofday.(<=) ofday s
;;

let maybe_exclude_without_crossing_day_boundary s =
  match Time.Ofday.next s with
  | None      -> s
  | Some next -> next
;;

let boundary_end =
  let last_ofday =
    Time.Ofday.prev (Time.Ofday.of_span_since_start_of_day (Time.Span.of_sec 86_400.))
    |> Option.value_exn
  in
  fun ~start_inc_exc ~s ~ofday ->
    if s_is_inrange_today_after_ofday ~start_inc_exc ~s ~ofday
    then
      match start_inc_exc with
      | Inclusive -> s
      | Exclusive -> maybe_exclude_without_crossing_day_boundary s
    else last_ofday
;;

let boundary_span ~start_inc_exc ~s ~end_inc_exc ~e it : Valid_invalid_span.t =
  let ofday = Internal_time.ofday it in
  let in_range =
    (boundary_test (module Time.Ofday) `After start_inc_exc) ofday s
    && (boundary_test (module Time.Ofday) `Before end_inc_exc) ofday e
  in
  if in_range
  then In_range_for_at_least (calculate_ofday_diff ~s:ofday ~e)
  else begin
    let e = boundary_end ~start_inc_exc ~s ~ofday in
    Out_of_range_for_at_least (calculate_ofday_diff ~s:ofday ~e)
  end
;;

let span_from_field it in_set field : Valid_invalid_span.t =
  let span = Internal_time.span_to_possible_difference_in it field in
  if in_set
  then In_range_for_at_least span
  else Out_of_range_for_at_least span
;;

let span_to_next_representable_time time =
  let next_representable_time = Time.next time in
  Time.diff next_representable_time time
;;

let includes (t : (zoned, _) Internal.t) ~output_includes_tags (time : Time.t) : Valid_invalid_span.t =
  let open Internal in
  let module IT = Internal_time in
  let rec loop : type zoning. (zoning, _) t -> (zoning, IT.t) O.t -> Valid_invalid_span.t =
    fun t o ->
      match t, o with
      | In_zone (zone, t), O.Z time ->
        let it = IT.of_time time ~zone in
        let ss = loop t (O.U (time, zone, it)) in
        begin match Time.Zone.next_clock_shift zone ~strictly_after:time with
        | None                 -> ss
        | Some (next_shift, _) ->
          let span_to_next_shift = Time.diff next_shift time in
          Valid_invalid_span.limit_to ss span_to_next_shift
        end
      | Tag (_, t), o                -> loop t o
      | And l, o                     -> includes_and ~output_includes_tags l o loop
      | Or l, o                      -> includes_or ~output_includes_tags l o loop
      | Not t, o                     -> Valid_invalid_span.flip (loop t o)
      | If_then_else (t1, t2, t3), o ->
        (* If_then_else (A, B, C)] is (A && B) || (NOT A && C) *)
        loop (Or [ And [ t1; t2 ]; And [ (Not t1); t3 ]]) o
      | Shift (span, t), o            -> shift span t o loop
      | Between ((start_inc_exc, s), (end_inc_exc, e)), O.U (_, _, it) ->
        boundary_span ~start_inc_exc ~s ~end_inc_exc ~e it
      | At ofdays, O.U (_, _, it)         ->
        let ofday = IT.ofday it in
        let date  = IT.date it  in
        let zone  = IT.zone it  in
        let time = Time.of_date_ofday date ofday ~zone in
        if Array_set.Ofday.mem ofdays ofday
        then In_range_for_at_least (span_to_next_representable_time time)
        else begin
          match Array_set.Ofday.next_after_rolling ofdays time ~zone with
          | None            -> Valid_invalid_span.never
          | Some next_time ->
            let span = Time.diff next_time time in
            Out_of_range_for_at_least span
        end
      | Secs secs, O.U (_, _, it)         ->
        span_from_field it (Array_set.mem secs (IT.sec it)) `Sec
      | Mins mins, O.U (_, _, it)         ->
        span_from_field it (Array_set.mem mins (IT.min it)) `Min
      | Hours hrs, O.U (_, _, it)         ->
        span_from_field it (Array_set.mem hrs (IT.hour it)) `Hour
      | Days days, O.U (_, _, it)         ->
        span_from_field it (Array_set.mem days (IT.day it)) `Day
      | Weeks weeks, O.U (_, _, it)       ->
        span_from_field it (Array_set.mem weeks (IT.week it)) `Week
      | Weekdays weekdays, O.U (_, _, it) ->
        span_from_field it (Array_set.mem weekdays (IT.weekday it)) `Weekday
      | Months months,O.U (_, _, it)      ->
        span_from_field it (Array_set.mem months (IT.month it)) `Month
      | On dates, O.U (_, _, it)          ->
        span_from_field it (Array_set.mem dates (IT.date it)) `Day
      | Before (in_out, boundary), O.U (time, _, _) ->
        let span = Time.abs_diff boundary time in
        if (boundary_test (module Time) `Before in_out) (time :> Time.t) boundary
        then In_range_for_at_least span
        else Out_of_range_for_at_least span
      | After (in_out, boundary), O.U (time, _, _)  ->
        let span = Time.abs_diff boundary time in
        if (boundary_test (module Time) `After in_out) (time :> Time.t) boundary
        then In_range_for_at_least span
        else Out_of_range_for_at_least span
      | Always, O.U _                     -> Valid_invalid_span.forever
      | Always, O.Z _                     -> Valid_invalid_span.forever
      | Never, O.U _                      -> Valid_invalid_span.never
      | Never, O.Z _                      -> Valid_invalid_span.never
  in
  (* some tests, especially those with inclusive ending boundary conditions might
     yield 0/very small sized spans.  This enforces that we always make forward
     progress by ensuring that the returned span will always take us at least to
     the next representable time. This is always correct because if includes is true,
     it is always in the schedule for a least one ulp, and similiarly for excludes. *)
  Valid_invalid_span.with_span (loop t (O.Z time))
    (fun span ->
       if Time.(>) (Time.add time span) time
       then span
       else span_to_next_representable_time time)
;;

let all_tags (t : (zoned, _) Internal.t) ~tag_comparator =
  let empty = Set.Using_comparator.empty ~comparator:tag_comparator in
  let open Internal in
  let module IT = Internal_time in
  let rec loop : type zoning. ('tag, 'cmp) Set.t -> (zoning, _) t -> ('tag, 'cmp) Set.t =
    fun set t ->
      match t with
      | Tag (tag, t)              -> loop (Set.add set tag) t
      | And l                     -> List.fold l ~init:set ~f:loop
      | Or l                      -> List.fold l ~init:set ~f:loop
      | If_then_else (t1, t2, t3) -> List.fold [t1; t2; t3] ~init:set ~f:loop
      | In_zone (_, t)            -> loop set t
      | Not t                     -> loop set t
      | Shift (_, t)              -> loop set t
      | Between _                 -> set
      | At _                      -> set
      | Secs _                    -> set
      | Mins _                    -> set
      | Hours _                   -> set
      | Days _                    -> set
      | Weeks _                   -> set
      | Weekdays _                -> set
      | Months _                  -> set
      | On _                      -> set
      | Before _                  -> set
      | After _                   -> set
      | Always                    -> set
      | Never                     -> set
  in
  loop empty t

let fold_tags (type tag)(type m) (t : (zoned, tag) Internal.t) ~(init:m) ~f time =
  let open Internal in
  let module IT   = Internal_time in
  let maybe_negate under_not m v =
    if under_not then
      match v with
      | None -> Some m
      | Some _ -> None
    else v
  in
  let mem s v under_not m =
    maybe_negate under_not m (if Array_set.mem s v then Some m else None)
  in
  let mem_boundary under_not m boundary_kind in_out time boundary =
    maybe_negate under_not m
      (if (boundary_test (module Time) boundary_kind in_out) (time :> Time.t) boundary
       then Some m
       else None)
  in
  let ofday_mem s it under_not m =
    maybe_negate under_not m (if Array_set.Ofday.mem s (IT.ofday it) then Some m else None)
  in
  let fold_and ~f l m =
    List.fold l ~init:(Some m)
      ~f:(fun m_opt t -> Option.bind m_opt ~f:(fun m -> f t m))
  in
  let fold_or ~f l m =
    List.fold l ~init:None
      ~f:(fun opt_m t ->
        let m = Option.value ~default:m opt_m in
        match f t m with
        | None -> opt_m
        | x -> x)
  in
  let rec loop :
    type zoning.
    (zoning, tag) t
    -> (zoning, IT.t) O.t
    -> under_not:bool
    -> m
    -> m option
    =
    fun t z ~under_not m ->
      match t , z with
      | In_zone (zone, t), O.Z time ->
        let it = IT.of_time time ~zone in
        loop t (O.U (time, zone, it)) ~under_not m
      | Tag (tag, t), z           ->
        Option.map (loop t z ~under_not m) ~f:(fun m -> f m tag)
      | And l, z                  ->
        let f t m = loop t z ~under_not m in
        if under_not then fold_or ~f l m else fold_and ~f l m
      | Or l, z                   ->
        let f t m = loop t z ~under_not m in
        if under_not then fold_and ~f l m else fold_or ~f l m
      | Not t, z                  ->
        loop t z ~under_not:(not under_not) m
      | If_then_else (t1, t2, t3), z ->
        loop (Or [And [t1; t2] ; And [Not t1; t3]]) z ~under_not m
      | Shift (span, t), o        ->
        shift span t o (fun t o -> loop t o ~under_not m)
      | Between ((start_inc_exc, s), (end_inc_exc, e)), O.U (_, _, it)    ->
        let ofday = IT.ofday it in
        maybe_negate under_not m
          (if (boundary_test (module Time.Ofday) `After start_inc_exc) ofday s
           && (boundary_test (module Time.Ofday) `Before end_inc_exc) ofday e
           then Some m
           else None)
      | At ofdays, O.U (_, _, it)          -> ofday_mem ofdays it under_not m
      | Mins mins, O.U (_, _, it)          -> mem mins (IT.min it) under_not m
      | Secs secs, O.U (_, _, it)          -> mem secs (IT.sec it) under_not m
      | Hours hrs, O.U (_, _, it)          -> mem hrs (IT.hour it) under_not m
      | Days days, O.U (_, _, it)          -> mem days (IT.day it) under_not m
      | Weeks weeks, O.U (_, _, it)        -> mem weeks (IT.week it) under_not m
      | Weekdays weekdays, O.U (_, _, it)  -> mem weekdays (IT.weekday it) under_not m
      | Months months, O.U (_, _, it)      -> mem months (IT.month it) under_not m
      | On dates, O.U (_, _, it)           -> mem dates (IT.date it) under_not m
      | Before (in_out, boundary), O.U (time, _, _)  ->
        mem_boundary under_not m `Before in_out time boundary
      | After (in_out, boundary), O.U (time, _, _)   ->
        mem_boundary under_not m `After in_out time boundary
      | Always, O.U _                      -> maybe_negate under_not m (Some m)
      | Always, O.Z _                      -> maybe_negate under_not m (Some m)
      | Never, O.U _                       -> maybe_negate under_not m None
      | Never, O.Z _                       -> maybe_negate under_not m None
  in
  loop t (O.Z time) ~under_not:false init
;;

type ('a, 'b) t = ('a, 'b) External.gadt =
  | In_zone       : Time.Zone.t * (unzoned, 'b) t                   -> (zoned, 'b) t
  | Tag           : 'b * ('a, 'b) t                                 -> ('a, 'b) t
  | And           : ('a, 'b) t list                                 -> ('a, 'b) t
  | Or            : ('a, 'b) t list                                 -> ('a, 'b) t
  | Not           : ('a, 'b) t                                      -> ('a, 'b) t
  | If_then_else  : (('a, 'b) t * ('a, 'b) t * ('a, 'b) t)          -> ('a, 'b) t
  | Shift         : Time.Span.t * ('a, 'b) t                        -> ('a, 'b) t
  | Between       : (Inclusive_exclusive.t * Time.Ofday.t)
                    * (Inclusive_exclusive.t * Time.Ofday.t) -> (unzoned, 'b) t
  | At            : Time.Ofday.t list                               -> (unzoned, 'b) t
  | Secs          : int list                                        -> (unzoned, 'b) t
  | Mins          : int list                                        -> (unzoned, 'b) t
  | Hours         : int list                                        -> (unzoned, 'b) t
  | Weekdays      : Day_of_week.t list                              -> (unzoned, 'b) t
  | Days          : int list                                        -> (unzoned, 'b) t
  | Weeks         : int list                                        -> (unzoned, 'b) t
  | Months        : Month.t list                                    -> (unzoned, 'b) t
  | On            : Date.t list                                     -> (unzoned, 'b) t
  | Before        : (Inclusive_exclusive.t * (Date.t * Time.Ofday.t)) -> (unzoned, 'b) t
  | After         : (Inclusive_exclusive.t * (Date.t * Time.Ofday.t)) -> (unzoned, 'b) t
  | Always        : ('a, 'b) t
  | Never         : ('a, 'b) t

let compare = External.compare_gadt

let tags t time =
  match fold_tags t ~init:[] ~f:(fun l x -> x::l) time with
  | None   -> `Not_included
  | Some l -> `Included l
;;

let to_string_zoned t ~string_of_tag =
  Sexp.to_string (Stable.V4.sexp_of_t (fun tag -> Sexp.Atom (string_of_tag tag)) t)
;;

let sexp_of_zoned_t = Stable.V4.sexp_of_t

module Event = struct
  type no_change =
    [ `No_change_until_at_least of [ `In_range | `Out_of_range ] * Time.t
    ] [@@deriving sexp_of, compare]

  type 'tag transition =
    [ `Enter of Time.t * 'tag list
    | `Leave of Time.t
    ] [@@deriving sexp_of, compare]

  type 'tag tag_change =
    [ `Change_tags of Time.t * 'tag list
    ] [@@deriving sexp_of, compare]

  type 'tag t = [ no_change | 'tag transition | 'tag tag_change] [@@deriving sexp_of]

  let to_time (t : [< 'tag t]) =
    match t with
    | `Enter (time, _)
    | `Change_tags (time, _)
    | `Leave time
    | `No_change_until_at_least (_, time) -> time
  ;;

  let is_no_change_until (t : [< no_change | 'tag transition | 'tag tag_change]) =
    match t with
    | `Enter _
    | `Change_tags _
    | `Leave _ -> false
    | `No_change_until_at_least _ -> true
  ;;
end

let base_compare_tags
      t
      ~prev_tags
      ~continue_looping
      ~time
      ~stop_time
      ~emit_dispatch
      ~loop
  =
  let new_tags = tags t time in
  match prev_tags, new_tags with
  | None      , `Not_included      ->
    begin match continue_looping with
    | None -> (None, `No_change_until_at_least (`Out_of_range, stop_time))
    | Some span -> loop (Time.add time span)
    end
  | None           , `Included tags     -> (Some tags, `Enter (time, tags))
  | Some _         , `Not_included      -> (None, `Leave time)
  | Some prev_tags , `Included new_tags -> emit_dispatch ~prev_tags ~new_tags
;;

let compare_event_with_tags
      ~tag_equal
      t
      ~prev_tags
      ~continue_looping
      ~time
      ~stop_time
      loop
  =
  let emit_dispatch ~prev_tags ~new_tags =
    match List.for_all2 prev_tags new_tags ~f:tag_equal with
    | Ok true ->
      begin match continue_looping with
      | None -> (Some prev_tags, `No_change_until_at_least (`In_range, stop_time))
      | Some span -> loop (Time.add time span)
      end
    | Ok false | Unequal_lengths -> (Some new_tags, `Change_tags (time, new_tags))
  in
  base_compare_tags t ~prev_tags ~continue_looping ~time ~stop_time ~emit_dispatch ~loop
;;

let compare_event_without_tags
      t
      ~prev_tags
      ~continue_looping
      ~time
      ~stop_time
      loop
  =
  let emit_dispatch ~prev_tags:_ ~new_tags =
    match continue_looping with
    | None      -> (Some new_tags, `No_change_until_at_least (`In_range, stop_time))
    | Some span -> loop (Time.add time span)
  in
  base_compare_tags t ~prev_tags ~continue_looping ~time ~stop_time ~emit_dispatch ~loop
;;

type ('tag, 'a) emit =
  | Transitions                 : ('tag, [ Event.no_change | 'tag Event.transition ]) emit
  | Transitions_and_tag_changes : ('tag -> 'tag -> bool)
    -> ('tag, [ Event.no_change | 'tag Event.transition | 'tag Event.tag_change ]) emit

let output_includes_tags (type tag) (type a) emit =
  match (emit : ((tag, a) emit)) with
  | Transitions                   -> false
  | Transitions_and_tag_changes _ -> true
;;

type ('tag, 'a) compare_tags =
  (zoned, 'tag) Internal.t
  -> prev_tags:'tag list option
  -> continue_looping:Time.Span.t option
  -> time:Time.t
  -> stop_time:Time.t
  -> (Time.t -> ('tag list option * 'a))
  -> 'tag list option * 'a

let compare_event (type tag) (type a) (emit : (tag, a) emit) : (tag, a) compare_tags =
  match (emit : ((tag, a) emit)) with
  | Transitions                           -> compare_event_without_tags
  | Transitions_and_tag_changes tag_equal -> compare_event_with_tags ~tag_equal
;;

let next_between t ~start_time ~stop_time ~prev_tags emit =
  let output_includes_tags = output_includes_tags emit in
  let compare_event = compare_event emit in
  let rec loop time =
    if Time.(>=) time stop_time
    then compare_event t ~prev_tags ~continue_looping:None ~time ~stop_time loop
    else begin
      let span =
        match includes ~output_includes_tags t time with
        | In_range_for_at_least span
        | Out_of_range_for_at_least span -> span
      in
      compare_event t ~prev_tags ~continue_looping:(Some span) ~time ~stop_time loop
    end
  in
  loop start_time
;;

let%expect_test "test at around DST" =
  let schedule =
    Internal.In_zone
      (Lazy.force Time.Zone.local,
       At (Array_set.Ofday.create
             (List.init 2 ~f:(fun hr -> Time.Ofday.create ~hr:(hr + 1) ~sec:1 ()))))
  in
  let f start_time =
    let _, time =
      next_between schedule
        ~start_time
        ~stop_time:(Time.of_string "2018-03-13 00:00:00-04:00")
        ~prev_tags:None
        Transitions
    in
    print_s [%sexp (time : [ `Enter of Core_time_float.t * unit list
                           | `Leave of Core_time_float.t
                           | `No_change_until_at_least of
                               [ `In_range | `Out_of_range ] * Core_time_float.t ])]
  in
  let start_time = Time.of_string "2018-03-11 03:00:01-04:00" in
  f start_time;
  [%expect {| (Enter ((2018-03-12 01:00:01.000000-04:00) ())) |}];
  let start_time = Time.prev start_time in
  f start_time;
  [%expect {| (Enter ((2018-03-12 01:00:01.000000-04:00) ())) |}];
;;

let search_one_chunk t ~start_time ~prev_tags emit =
  let max_stop_time = Time.add start_time (Time.Span.of_day 1.) in
  (next_between t ~start_time ~stop_time:max_stop_time ~prev_tags emit)
;;

let to_sequence t ~start_time emit =
  let t = Internal.of_external t in
  let tags t time =
    match tags t time with
    | `Not_included -> []
    | `Included l   -> l
  in
  let output_includes_tags = output_includes_tags emit in
  let in_schedule =
    match includes ~output_includes_tags t start_time with
    | In_range_for_at_least _     -> `In_range
    | Out_of_range_for_at_least _ -> `Out_of_range
  in
  let init =
    let prev_tags =
      match in_schedule with
      | `In_range     -> Some (tags t start_time)
      | `Out_of_range -> None
    in
    (prev_tags, `No_change_until_at_least (in_schedule, start_time))
  in
  let sequence =
    Sequence.unfold ~init ~f:(fun (cur_tags, cur_event) ->
      let start_time = Event.to_time cur_event in
      let (_next_tags, next_event) as next_s =
        search_one_chunk t ~start_time ~prev_tags:cur_tags emit
      in
      Some (next_event, next_s))
  in
  match in_schedule with
  | `In_range     -> `Started_in_range (tags t start_time, sequence)
  | `Out_of_range -> `Started_out_of_range sequence
;;

let to_endless_sequence
      (type tag)
      (type a)
      t
      ~start_time
      ~(emit : (tag, a) emit)
  : [ `Started_in_range of tag list * a Sequence.t
    | `Started_out_of_range of a Sequence.t ]
  =
  match (emit : (tag, a) emit) with
  | Transitions                   -> to_sequence t ~start_time emit
  | Transitions_and_tag_changes _ -> to_sequence t ~start_time emit
;;

let map_tags = External.map_tags

let get_event seq ~stop_time ~f =
  Sequence.take_while seq ~f:(fun event -> Time.(<=) (Event.to_time event) stop_time)
  |> Sequence.filter ~f:(fun event -> not (Event.is_no_change_until event))
  |> Sequence.filter_map ~f
;;

let next_between_gen t start_time stop_time ~f =
  (* go back in time slightly in case there is an event exactly on start_time *)
  let start_time = Time.prev start_time in
  let ( `Started_in_range (_, seq)
      | `Started_out_of_range seq ) =
    to_endless_sequence t ~start_time ~emit:Transitions
  in
  let f event =
    if Time.(<) (Event.to_time event) start_time
    then None
    else f event
  in
  Sequence.hd (get_event seq ~stop_time ~f)
;;

let next_leave_between t start_time stop_time =
  next_between_gen t start_time stop_time ~f:(function
    | `Leave time -> Some time
    | _           -> None)
;;

let next_enter_between t start_time stop_time =
  next_between_gen t start_time stop_time ~f:(function
    | `Enter (time, _) -> Some time
    | _                -> None)
;;

let includes t time =
  match includes ~output_includes_tags:false (Internal.of_external t) time with
  | In_range_for_at_least _     -> true
  | Out_of_range_for_at_least _ -> false
;;

let fold_tags t ~init ~f time  = fold_tags (Internal.of_external t) ~init ~f time
let tags t time                = tags (Internal.of_external t) time
let all_tags t ~tag_comparator = all_tags (Internal.of_external t) ~tag_comparator

let%expect_test "Test At around dst: Times after dst unaffected" =
  let schedule =
    In_zone
      (Lazy.force Time.Zone.local,
       At (List.init 2 ~f:(fun hr -> Time.Ofday.create ~hr:(hr + 1) ~sec:1 ())))
  in
  let time =
    next_enter_between schedule
      (Time.of_string "2018-03-11 03:00:01-04:00")
      (Time.of_string "2018-03-13 00:00:00-04:00")
  in
  print_s [%sexp (time : Time.t option)];
  [%expect {| ((2018-03-12 01:00:01.000000-04:00)) |}];
;;

let%expect_test "Test at around dst: Times before DST unaffected" =
  let schedule =
    In_zone
      (Lazy.force Time.Zone.local,
       At (List.init 2 ~f:(fun hr -> Time.Ofday.create ~hr:(hr + 1) ~sec:1 ())))
  in
  let time =
    next_enter_between schedule
      (Time.of_string "2018-03-11 01:00:01-05:00")
      (Time.of_string "2018-03-13 00:00:00-04:00")
  in
  print_s [%sexp (time : Time.t option)];
  [%expect {| ((2018-03-11 01:00:01.000000-05:00)) |}];
;;

let%expect_test "Test at around dst: Time of the time change unaffected" =
  let schedule =
    In_zone
      (Lazy.force Time.Zone.local,
       At (List.init 3 ~f:(fun hr -> Time.Ofday.create ~hr:(hr + 1) ~sec:1 ())))
  in
  let time =
    next_enter_between schedule
      (Time.of_string "2018-03-11 01:00:02-05:00")
      (Time.of_string "2018-03-13 00:00:00-04:00")
  in
  print_s [%sexp (time : Time.t option)];
  [%expect {| ((2018-03-11 03:00:01.000000-04:00)) |}];
;;
