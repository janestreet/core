
open! Import
open! Int.Replace_polymorphic_compare

module Core_time = Import_time.Time

module Stable = struct
  module V1 = struct
    module T = struct
      type 'a t =
        | Interval of 'a * 'a
        | Empty
      [@@deriving bin_io, of_sexp, variants, compare, hash]

      type 'a interval = 'a t [@@deriving bin_io, of_sexp, compare, hash]

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

    type 'a t = 'a interval [@@deriving sexp, bin_io, compare, hash]

    module Float = struct
      module T = struct
        type t = float interval [@@deriving sexp, bin_io, compare, hash]
      end
      include T
      include Comparator.Stable.V1.Make (T)
    end

    module Int = struct
      module T = struct
        type t = int interval [@@deriving sexp, bin_io, compare, hash]
      end
      include T
      include Comparator.Stable.V1.Make (T)
    end

    module Time = struct
      module T = struct
        type t = Core_time.Stable.V1.t interval [@@deriving sexp, bin_io, compare]
      end
      include T
      include Comparator.Stable.V1.Make (T)
    end

    module Time_ns = struct
      module T = struct
        type t =
          Core_time_ns.Stable.V1.t interval [@@deriving sexp, bin_io, compare]
      end
      include T
      include Comparator.Stable.V1.Make (T)
    end

    module Ofday = struct
      module T = struct
        type t = Core_time.Stable.Ofday.V1.t interval [@@deriving sexp, bin_io, compare, hash]
      end
      include T
      include Comparator.Stable.V1.Make (T)
    end

    module Private = struct
      include T
      let to_float t = t
      let to_int t = t
      let to_ofday t = t
      let to_time t = t
    end
  end

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

    let contains i x = Poly.(=) (compare_value i x) `Within

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
      let intervals = List.sort ~compare:interval_compare intervals in
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
        List.sort intervals ~compare:(fun i i' -> T.compare (lb i) (lb i'))
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

type 'a t = 'a interval [@@deriving bin_io, sexp, compare, hash]

module C = Raw_make (struct
    type 'a bound = 'a
    include Poly
  end)

include C.Interval

let t_of_sexp a_of_sexp s =
  let t = t_of_sexp a_of_sexp s in
  if is_malformed t then
    of_sexp_error "Interval.t_of_sexp error: malformed input" s;
  t
;;

module Set = struct
  type 'a t = 'a interval list [@@deriving bin_io, sexp, compare, hash]
  include C.Set
end

module Make (Bound : sig
    type t [@@deriving bin_io, sexp, hash]
    include Comparable.S with type t := t
  end) = struct

  type t = Bound.t interval [@@deriving bin_io, sexp, compare, hash]
  type interval = t [@@deriving bin_io, sexp]
  type bound = Bound.t

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
    type t = interval list [@@deriving sexp, bin_io]
    include C.Set
    let to_poly (t : t) = t
  end

end

module type S1 = Interval_intf.S1

module type S = Interval_intf.S
  with type 'a poly_t := 'a t
  with type 'a poly_set := 'a Set.t

module type S_time = Interval_intf.S_time
  with type 'a poly_t := 'a t
  with type 'a poly_set := 'a Set.t

module Float    = Make (Float)
module Ofday    = Make (Core_time.Ofday)
module Ofday_ns = Make (Core_time_ns.Ofday)

module Int = struct
  include Make(Int)

  let length t =
    match t with
    | Empty -> 0
    | Interval (lo, hi) ->
      let len = 1 + hi - lo in
      (* If [hi] and [lo] are far enough apart (e.g. if [lo <= 0] and
         [hi = Int.max_value]), [len] will overlow. *)
      if len < 0 then
        failwiths ~here:[%here] "interval length not representable" t [%sexp_of: t];
      len

  let get t i =
    let fail () =
      failwiths ~here:[%here] "index out of bounds" (i, t)
        [%sexp_of: int * t]
    in
    match t with
    | Empty -> fail ()
    | Interval (lo, hi) ->
      if i < 0 then fail ();
      let x = lo + i in
      if x < lo || x > hi then fail ();
      x

  let iter t ~f =
    match t with
    | Empty -> ()
    | Interval (lo, hi) ->
      for x = lo to hi do
        f x
      done

  let fold =
    let rec fold_interval ~lo ~hi ~acc ~f =
      if lo = hi
      then f acc hi
      else fold_interval ~lo:(lo+1) ~hi ~acc:(f acc lo) ~f
    in
    fun t ~init ~f ->
      match t with
      | Empty             -> init
      | Interval (lo, hi) -> fold_interval ~lo ~hi ~acc:init ~f

  module For_container = Container.Make0 (struct
      type nonrec t   = t
      module Elt = Int
      let iter = `Custom iter
      let fold = fold
      let length = `Custom length
    end)

  let exists   = For_container.exists
  let for_all  = For_container.for_all
  let sum      = For_container.sum
  let count    = For_container.count
  let find     = For_container.find
  let find_map = For_container.find_map
  let to_list  = For_container.to_list
  let to_array = For_container.to_array
  let fold_result = For_container.fold_result
  let fold_until = For_container.fold_until

  let min_elt t ~compare =
    if not (phys_equal compare Int.compare)
    then For_container.min_elt t ~compare
    else lbound t

  let max_elt t ~compare =
    if not (phys_equal compare Int.compare)
    then For_container.max_elt t ~compare
    else ubound t

  let mem t x =
    if not (phys_equal equal Int.equal)
    then For_container.mem t x
    else contains t x

  (* Note that we use zero-based indexing here, because that's what Binary_searchable
     requires, even though at the end we want to export functions that use the natural
     bounds of the interval.  *)
  module For_binary_search = Binary_searchable.Make (struct
      type nonrec t   = t
      type nonrec elt = bound
      let length = length
      let get    = get
    end)

  let binary_search ?pos ?len t ~compare which elt =
    let zero_based_pos =
      Option.map pos ~f:(fun x -> x - lbound_exn t)
    in
    let zero_based_result =
      For_binary_search.binary_search ?pos:zero_based_pos ?len t ~compare which elt
    in
    Option.map zero_based_result ~f:(fun x -> x + lbound_exn t)

  let binary_search_segmented ?pos ?len t ~segment_of which =
    let zero_based_pos =
      Option.map pos ~f:(fun x -> x - lbound_exn t)
    in
    let zero_based_result =
      For_binary_search.binary_search_segmented
        ?pos:zero_based_pos ?len t ~segment_of which
    in
    Option.map zero_based_result ~f:(fun x -> x + lbound_exn t)

  module Private = struct
    let get = get
  end
end
module type Time_bound = sig
  type t [@@deriving bin_io, sexp, compare, hash]

  include Comparable.S with type t := t

  module Ofday : sig
    type t
  end

  module Zone : sig
    type t

    val local : t Lazy.t
  end

  val occurrence
    :  [`First_after_or_at | `Last_before_or_at ]
    -> t
    -> ofday: Ofday.t
    -> zone: Zone.t
    -> t
end

module Make_time (Time : Time_bound) = struct
  include Make(Time)

  let create_ending_after ?zone (open_ofday, close_ofday) ~now =
    let zone =
      match zone with
      | None   -> Lazy.force Time.Zone.local
      | Some z -> z
    in
    let close_time =
      Time.occurrence `First_after_or_at now ~zone ~ofday:close_ofday
    in
    let open_time =
      Time.occurrence `Last_before_or_at close_time ~zone ~ofday:open_ofday
    in
    create open_time close_time

  let create_ending_before ?zone
        (open_ofday, close_ofday) ~ubound =
    let zone =
      match zone with
      | None   -> Lazy.force Time.Zone.local
      | Some z -> z
    in
    let close_time =
      Time.occurrence `Last_before_or_at ubound ~zone ~ofday:close_ofday
    in
    let open_time =
      Time.occurrence `Last_before_or_at close_time ~zone ~ofday:open_ofday
    in
    create open_time close_time
end

module Time    = Make_time(Core_time)
module Time_ns = Make_time(Core_time_ns)
