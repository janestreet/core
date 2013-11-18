
open Core_kernel.Std

open Int.Replace_polymorphic_compare
let _ = (=)    (* turns off the unused open warning *)

module Stable = struct
  module V1 = struct
    (* THIS TYPE AND ITS SERIALIZATIONS SHOULD NEVER BE CHANGED - SEE stable.mli FOR MORE
       DETAILS *)
    module T = struct
      type 'a t =
      | Interval of 'a * 'a
      | Empty
      with bin_io, of_sexp, variants, compare

      type 'a interval = 'a t with bin_io, of_sexp

      let interval_of_sexp a_of_sexp sexp =
        try interval_of_sexp a_of_sexp sexp   (* for backwards compatibility *)
        with _exn ->
          match sexp with
          | Sexp.List [] -> Empty
          | Sexp.List [ lb; ub ] ->
            Interval (a_of_sexp lb, a_of_sexp ub)
          | Sexp.Atom _ | Sexp.List _ ->
            of_sexp_error "Interval.t_of_sexp: expected pair or empty list" sexp

      let sexp_of_interval sexp_of_a t =
        match t with
        | Empty -> Sexp.List []
        | Interval (lb, ub) ->
          Sexp.List [ sexp_of_a lb; sexp_of_a ub ]
    end

    open T

    module Float = struct
      type t = float interval with sexp, bin_io
    end

    module Int = struct
      type t = int interval with sexp, bin_io
    end

    module Time = struct
      type t = Time.Stable.V1.t interval with sexp, bin_io
    end

    module Ofday = struct
      type t = Ofday.Stable.V1.t interval with sexp, bin_io
    end
  end

  let make_tests_v1 ~non_empty =
    let module V = Variantslib.Variant in
    let c f tests variant = (f variant) @ tests in
    V1.T.Variants.fold
      ~init:[]
      ~interval:(c (fun interval ->
        assert (interval.V.rank = 0);
        List.map non_empty ~f:(fun ((lbound, ubound), sexp, bin_io) ->
          interval.V.constructor lbound ubound, sexp, bin_io)))
      ~empty:(c (fun empty ->
        assert (empty.V.rank = 1);
        [ empty.V.constructor, "()", "\001" ]))

  TEST_MODULE "Interval.V1.Float" = Core_kernel.Stable_unit_test.Make(struct
    include V1.Float
    let equal x1 x2 = (V1.T.compare Float.compare x1 x2) = 0

    module V = V1.T.Variants
    let tests = make_tests_v1
      ~non_empty:
      [ (1.5, 120.), "(1.5 120)",
        "\000\000\000\000\000\000\000\248?\000\000\000\000\000\000^@"
      ]
  end)

  TEST_MODULE "Interval.V1.Int" = Core_kernel.Stable_unit_test.Make(struct
    include V1.Int
    let equal x1 x2 = (V1.T.compare Int.compare x1 x2) = 0


    module V = V1.T.Variants
    let tests = make_tests_v1
      ~non_empty: [ (-5, 789), "(-5 789)", "\000\255\251\254\021\003" ]
  end)

  TEST_MODULE "Interval.V1.Time" = struct
    module Arg = struct
      include V1.Time
      let equal x1 x2 = (V1.T.compare Time.compare x1 x2) = 0

      let zone = Zone.of_string "America/New_York"

      module V = V1.T.Variants
      let tests =
        let t1 = Time.of_date_ofday zone
          (Date.create_exn ~y:2013 ~m:Month.Aug ~d:6)
          (Ofday.create  ~hr:7 ~min:30 ~sec:7 ~ms:12 ~us:5 ())
        in
        let t2 = Time.of_date_ofday zone
          (Date.create_exn ~y:2014 ~m:Month.Sep ~d:8)
          (Ofday.create  ~hr:10 ~min:10 ~sec:0 ~ms:22 ~us:0 ())
        in
        make_tests_v1
          ~non_empty: [ (t1, t2),
                        "((2013-08-06 07:30:07.012005-04:00) (2014-09-08 10:10:00.022000-04:00))",
                        "\000\177\196\192\1437\128\212Ash\001.n\003\213A" ]
    end

    (* Bypass sexp serialization tests because [Time.sexp_of_t] gives different
       results depending on the local zone. *)
    include Core_kernel.Stable_unit_test.Make_sexp_deserialization_test(Arg)
    include Core_kernel.Stable_unit_test.Make_bin_io_test(Arg)
  end

  TEST_MODULE "Interval.V1.Ofday" = Core_kernel.Stable_unit_test.Make(struct
    include V1.Ofday
    let equal x1 x2 = (V1.T.compare Ofday.compare x1 x2) = 0

    module V = V1.T.Variants
    let tests =
      let t1 = Ofday.create ~hr:7 ~min:30 ~sec:7 ~ms:12 ~us:5 () in
      let t2 = Ofday.create ~hr:9 ~min:45 ~sec:8 ~ms:0 ~us:1 () in
      make_tests_v1
        ~non_empty:
        [ (t1, t2) , "(07:30:07.012005 09:45:08.000001)",
          "\000\153\158\176\196\192_\218@\223\024\002\000\128$\225@"
        ]
  end)
end

open Stable.V1.T

module type Bound = sig
  type 'a bound
  val compare : 'a bound -> 'a bound -> int
  val ( >= ) : 'a bound -> 'a bound -> bool
  val ( <= ) : 'a bound -> 'a bound -> bool
  val ( =  ) : 'a bound -> 'a bound -> bool
  val ( >  ) : 'a bound -> 'a bound -> bool
  val ( <  ) : 'a bound -> 'a bound -> bool
  val ( <> ) : 'a bound -> 'a bound -> bool
end

module Raw_make (T : Bound) = struct

  module T = struct
    include T
    let _ = ( <> )  (* Prevent unused value warning for "<>" *)
    let max x y = if T.(>=) x y then x else y
    let min x y = if T.(<=) x y then x else y
  end

  module Interval = struct
    let empty = Empty

    let is_malformed = function
      | Empty -> false
      | Interval (x,y) -> T.(>) x y

    let empty_cvt = function
      | Empty -> Empty
      | Interval (x,y) as i -> if T.(>) x y then Empty else i

    let create x y =
      (* if x > y, then this is just the Empty interval. *)
      empty_cvt (Interval (x,y))

    let intersect i1 i2 = match i1,i2 with
      | Empty,_ | _,Empty -> Empty
      | Interval (l1,u1), Interval (l2,u2) -> empty_cvt (Interval (T.max l1 l2, T.min u1 u2))

    let is_empty = function Empty -> true | _ -> false

    let is_empty_or_singleton = function
      | Empty -> true
      | Interval (x,y) -> T.(=) x y

    let bounds = function Empty -> None | Interval (l, u) -> Some (l,u)
    let lbound = function Empty -> None | Interval (l, _) -> Some l
    let ubound = function Empty -> None | Interval (_, u) -> Some u

    let bounds_exn = function
      | Empty -> invalid_arg "Interval.bounds_exn: empty interval"
      | Interval (l,u) -> (l,u)

    let lbound_exn = function
      | Empty -> invalid_arg "Interval.lbound_exn: empty interval"
      | Interval (l,_) -> l

    let ubound_exn = function
      | Empty -> invalid_arg "Interval.ubound_exn: empty interval"
      | Interval (_,u) -> u

    let compare_value i x = match i with
      | Empty -> `Interval_is_empty
      | Interval (l,u) ->
          if T.(<) x l
          then `Below
          else if T.(>) x u
          then `Above
          else `Within

    let contains i x = Pervasives.(=) (compare_value i x) `Within

    let bound i x = match i with
      | Empty -> None
      | Interval (l,u) ->
          let bounded_value =
            if T.(<) x l then l
            else if T.(<) u x then u
            else x in
          Some bounded_value

    let is_superset i1 ~of_:i2 = match i1,i2 with
      | Interval (l1,u1), Interval (l2,u2) ->
          T.(<=) l1 l2 && T.(>=) u1 u2
      | _, Empty -> true
      | Empty, Interval (_, _) -> false

    let is_subset i1 ~of_:i2 =
      is_superset i2 ~of_:i1

    let map t ~f =
      match t with
      | Empty -> Empty
      | Interval (l,u) -> empty_cvt (Interval (f l, f u))
    ;;

    let interval_compare t1 t2 =
      match t1, t2 with
      | Empty, Empty -> 0
      | Empty, Interval _ -> -1
      | Interval _, Empty -> 1
      | Interval (l1,u1), Interval (l2,u2) ->
          let c = T.compare l1 l2 in
          if Int.(<>) c 0 then c else T.compare u1 u2
    ;;

    let are_disjoint_gen ~are_disjoint intervals =
      let intervals = Array.of_list intervals in
      try
        for i = 0 to Array.length intervals - 1 do
          for j = i + 1 to Array.length intervals - 1 do
            if not (are_disjoint intervals.(i) intervals.(j)) then raise Exit
          done
        done;
        true
      with
        Exit -> false

    let are_disjoint intervals =
      are_disjoint_gen intervals
        ~are_disjoint:(fun i1 i2 -> is_empty (intersect i1 i2))

    let are_disjoint_as_open_intervals intervals =
      are_disjoint_gen intervals
        ~are_disjoint:(fun i1 i2 -> is_empty_or_singleton (intersect i1 i2))

    let list_intersect ilist1 ilist2 =
      if not (are_disjoint ilist1) || not (are_disjoint ilist2) then
        invalid_arg "Interval.list_intersect: non-disjoint input list";
      let pairs = List.cartesian_product ilist1 ilist2 in
      List.filter_map pairs ~f:(fun (i1,i2) ->
        let i = intersect i1 i2 in
        if is_empty i then None else Some i)

    let half_open_intervals_are_a_partition intervals =
      let intervals = List.filter ~f:(fun x -> not (is_empty x)) intervals in
      let intervals = List.sort ~cmp:interval_compare intervals in
      (* requires sorted list of intervals *)
      let rec is_partition a = function
        | [] -> true
        | b :: tl -> T.(=) (ubound_exn a) (lbound_exn b) && is_partition b tl
      in
      match intervals with
      | [] -> true
      | x::xs -> is_partition x xs

    let convex_hull intervals =
      List.fold intervals ~init:empty ~f:(fun i1 i2 ->
        (* Compute the convex hull of two intervals *)
        match bounds i1, bounds i2 with
        | None, _    -> i2
        | _   , None -> i1
        | Some (l1,u1), Some (l2,u2) -> create (T.min l1 l2) (T.max u1 u2))
  end

  module Set = struct
    let create_from_intervals intervals =
      let intervals = List.filter intervals
        ~f:(fun i -> not (Interval.is_empty i))
      in
      let intervals =
        let lb i = Interval.lbound_exn i in
        List.sort intervals ~cmp:(fun i i' -> T.compare (lb i) (lb i'))
      in
      if not (Interval.are_disjoint intervals)
      then failwith "Interval_set.create: intervals were not disjoint"
      else intervals
    ;;

    let create pair_list =
      let intervals = List.map pair_list
        ~f:(fun (lbound, ubound) -> Interval.create lbound ubound)
      in
      create_from_intervals intervals
    ;;

    let contains_set ~container ~contained =
      List.for_all contained
        ~f:(fun contained_interval ->
          List.exists container
            ~f:(fun container_interval ->
              Interval.is_superset container_interval ~of_:contained_interval
            )
        )

    let contains t x =
      List.exists t ~f:(fun interval -> Interval.contains interval x)

    let ubound_exn t =
      match t with
      | [] -> invalid_arg "Interval_set.ubound called on empty set"
      | _ -> Interval.ubound_exn (List.last_exn t)

    let lbound_exn t =
      match t with
      | [] -> invalid_arg "Interval_set.lbound called on empty set"
      | _ -> Interval.lbound_exn (List.hd_exn t)

    let ubound t =
      match List.last t with
      | None -> None
      | Some i ->
        match Interval.ubound i with
        | None -> assert false
        | Some x -> Some x

    let lbound t =
      match List.hd t with
      | None -> None
      | Some i ->
        match Interval.lbound i with
        | None -> assert false
        | Some x -> Some x
  end

end

type 'a t = 'a interval with bin_io, sexp
type 'a bound_ = 'a

module C = Raw_make (struct
  type 'a bound = 'a
  include Pervasives
end)

include C.Interval

let t_of_sexp a_of_sexp s =
  let t = t_of_sexp a_of_sexp s in
  if is_malformed t then
    Sexplib.Conv.of_sexp_error "Interval.t_of_sexp error: malformed input" s;
  t
;;

module Set = struct
  type 'a t = 'a interval list with bin_io, sexp
  include C.Set
end

module Make (Bound : sig
  type t with bin_io, sexp
  include Comparable.S with type t := t
end) = struct

  type t = Bound.t interval with bin_io, sexp
  type 'a t_ = t
  type interval = t with bin_io, sexp
  type bound = Bound.t
  type 'a bound_ = bound

  module C = Raw_make (struct
    type 'a bound = Bound.t
    let compare = Bound.compare
    include (Bound : Comparable.Infix with type t := Bound.t)
  end)

  include C.Interval

  let to_poly (t : t) = t

  let t_of_sexp s =
    let t = t_of_sexp s in
    if is_malformed t then
      failwithf "Interval.Make.t_of_sexp error: malformed input %s"
        (Sexp.to_string s) ()
    else
      t
  ;;

  module Set = struct
    type t = interval list with sexp, bin_io
    type 'a t_ = t
    include C.Set
    let to_poly (t : t) = t
  end

end

module type S1 = Interval_intf.S1

module type S = Interval_intf.S
  with type 'a poly_t := 'a t
  with type 'a poly_set := 'a Set.t

module Float = Make (Float)
module Int   = Make (Int  )
module Ofday = Make (Ofday)

(* Tests for list bound functions *)
TEST_MODULE = struct
  let intervals =
    [ Int.empty
    ; Interval (3, 6)
    ; Interval (2, 7)
    ; Int.empty
    ; Interval (4, 5)]

  TEST =
    match Int.convex_hull intervals with
    | Interval (2, 7) -> true
    | _ -> false

  let intervals =
    [ Int.empty
    ; Interval (3, 6)
    ; Interval (2, 3)
    ; Int.empty
    ; Interval (4, 5)]

  TEST =
    match Int.convex_hull intervals with
    | Interval (2, 6) -> true
    | _ -> false

  let intervals =
    [ Int.empty
    ; Int.empty]

  TEST =
    match Int.convex_hull intervals with
    | Empty -> true
    | _ -> false
end
module Time = struct
  include Make(Time)

  let create_ending_after ?(zone = Zone.machine_zone ()) (open_ofday, close_ofday) ~now =
    let close_time =
      Time.occurrence `First_after_or_at now ~zone ~ofday:close_ofday
    in
    let open_time =
      Time.occurrence `Last_before_or_at close_time ~zone ~ofday:open_ofday
    in
    create open_time close_time

  let create_ending_before ?(zone = Zone.machine_zone ())
      (open_ofday, close_ofday) ~ubound =
    let close_time =
      Time.occurrence `Last_before_or_at ubound ~zone ~ofday:close_ofday
    in
    let open_time =
      Time.occurrence `Last_before_or_at close_time ~zone ~ofday:open_ofday
    in
    create open_time close_time

end

