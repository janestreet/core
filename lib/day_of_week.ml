module Array = Core_array
module Int = Core_int
module String = Core_string
module Hashtbl = Core_hashtbl

let failwithf = Core_printf.failwithf

let num_days = 7

module Stable = struct
  module V1 = struct
    (* IF THIS REPRESENTATION EVER CHANGES, ENSURE THAT EITHER
       (1) all values serialize the same way in both representations, or
       (2) you add a new Day_of_week version to stable.ml *)
    type t = int

    let invariant t =
      assert (0 <= t && t < num_days);
    ;;

    let of_int i =
      if 0 <= i && i < num_days then
        Some i
      else
        None
    ;;

    let of_int_exn i =
      if 0 <= i && i < num_days then
        i
      else
        failwithf "Day_of_week.of_int_exn %d" i ()
    ;;

    let to_int t = t

    let sun = 0
    let mon = 1
    let tue = 2
    let wed = 3
    let thu = 4
    let fri = 5
    let sat = 6

    type variant = [ `Sun | `Mon | `Tue | `Wed | `Thu | `Fri | `Sat ]

    type rep = {
      string : string;
      t : t;
      variant : variant;
    }

    let reps =
      Array.map
        [|("SUN", sun, `Sun);
          ("MON", mon, `Mon);
          ("TUE", tue, `Tue);
          ("WED", wed, `Wed);
          ("THU", thu, `Thu);
          ("FRI", fri, `Fri);
          ("SAT", sat, `Sat)|]
        ~f:(fun (string, t, variant) ->
          { string = string; t = t; variant = variant; })
    ;;

    (* WARNING: if you are going to change this function in a material way, be sure you
       make the appropriate changes to the Stable module below *)
    let to_string t = reps.(t).string

    (* WARNING: if you are going to change this function in a material way, be sure you
       make the appropriate changes to the Stable module below *)
    let of_string =
      let table =
        String.Table.of_alist_exn
          (Array.to_list (Array.map reps ~f:(fun r -> (r.string, r.t))))
      in
      fun str ->
        match Hashtbl.find table (String.uppercase str) with
        | None -> failwithf "Invalid weekday: %s" str ()
        | Some x -> x
    ;;

    include (Sexpable.Of_stringable (struct
      type t = int
      let of_string = of_string
      let to_string = to_string
    end) : Sexpable.S with type t := t)

    let get t = reps.(t).variant

    let create = function
      | `Sun -> 0
      | `Mon -> 1
      | `Tue -> 2
      | `Wed -> 3
      | `Thu -> 4
      | `Fri -> 5
      | `Sat -> 6
    ;;

    let shift t i = Int.Infix.( % ) (t + i) num_days

    let is_sun_or_sat t =
      t = sun || t = sat

    include (Int : sig
      include Binable.S with type t := t
      include Hashable.S with type t := t
      include Comparable.S with type t := t
    end)
  end

  TEST_MODULE "Day_of_week.V1" = Stable_unit_test.Make (struct
    include V1

    let equal = equal

    let tests =
      [ sun, "SUN", "\000"
      ; mon, "MON", "\001"
      ; tue, "TUE", "\002"
      ; wed, "WED", "\003"
      ; thu, "THU", "\004"
      ; fri, "FRI", "\005"
      ; sat, "SAT", "\006"
      ]
  end)
end

include Stable.V1
