
open Std_internal

type 'a interval = Interval of 'a * 'a | Empty with bin_io, of_sexp

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

    let contains i x = compare_value i x = `Within

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

    let map ~f = function
      | Empty -> Empty
      | Interval (l,u) -> empty_cvt (Interval (f l, f u))

    let interval_compare t1 t2 =
      match t1, t2 with
      | Empty, Empty -> 0
      | Empty, Interval _ -> -1
      | Interval _, Empty -> 1
      | Interval (l1,u1), Interval (l2,u2) ->
          let c = T.compare l1 l2 in
          if Core_int.(<>) c 0 then c else T.compare u1 u2
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
        | b :: tl ->
            ubound_exn a = lbound_exn b && is_partition b tl
      in
      match intervals with
      | [] -> true
      | x::xs -> is_partition x xs
    ;;

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


module T = struct
  type 'a bound = 'a
  type 'a t = 'a interval
  include Pervasives
end

type 'a t = 'a interval with bin_io, sexp

module C = Raw_make (T)
include C.Interval

let t_of_sexp a_of_sexp s =
  let t = t_of_sexp a_of_sexp s in
  if is_malformed t then
    Sexplib.Conv.of_sexp_error "Interval.t_of_sexp error: malformed input" s;
  t
;;

module Set = struct
  type 'a t = 'a interval list with bin_io, sexp
  module T = struct
    type 'a i = 'a interval
    type 'a t = 'a interval list
    type 'a interval = 'a i
    type 'a bound = 'a
  end
  include C.Set
end

module Make(M : sig
  type t
  include Comparable.S with type t := t
  include Sexpable.S with type t := t
  include Binable.S with type t := t
end) = struct

  type t = M.t interval with bin_io, sexp
  type interval = t with bin_io, sexp
  type bound = M.t
  type 'a poly_t = t with bin_io, sexp

  module T = struct
    type 'a bound = M.t
    type 'a t = interval
    let compare = M.compare
    include (M:Comparable.Infix with type t := M.t)
  end

  module C = Raw_make(T)
  include C.Interval
  let to_poly (t : t) = t

  let t_of_sexp s =
    let t = t_of_sexp s in
    if is_malformed t then
      Common.failwithf "Interval.Make.t_of_sexp error: malformed input %s"
        (Core_sexp.to_string s) ()
    else
      t
  ;;

  module Set = struct
    type t = interval list with sexp, bin_io
    type 'a poly_t = t with sexp, bin_io
    module T = struct
      type i = interval
      type 'a t = interval list
      type 'a interval = i
      type 'a bound = M.t
    end
    include C.Set
    let to_poly (t : t) = t
  end

end

module type S = Interval_intf.S
module type S1 = Interval_intf.S1

module Float = Make(Float)
module Int = Make(Core_int)
module Time = struct
  include Make(Time)

  let create_ending_after ?(zone = Zone.machine_zone ()) (open_ofday, close_ofday) ~now =
    let close_time =
      Time.ofday_occurrence now zone close_ofday `right_after
    in
    let open_time =
      Time.ofday_occurrence close_time zone open_ofday `right_before
    in
    create open_time close_time

  let create_ending_before ?(zone = Zone.machine_zone ())
      (open_ofday, close_ofday) ~ubound =
    let close_time =
      Time.ofday_occurrence ubound zone close_ofday `right_before
    in
    let open_time =
      Time.ofday_occurrence close_time zone open_ofday `right_before
    in
    create open_time close_time

end
