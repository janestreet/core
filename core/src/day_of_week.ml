open! Import
module String = Base.String

let failwithf = Printf.failwithf

module Stable = struct
  module V1 = struct
    module T = struct
      type t =
        | Sun
        | Mon
        | Tue
        | Wed
        | Thu
        | Fri
        | Sat
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , enumerate
        , equal ~localize
        , hash
        , quickcheck ~portable
        , stable_witness
        , typerep]

      let to_string t =
        match t with
        | Sun -> "SUN"
        | Mon -> "MON"
        | Tue -> "TUE"
        | Wed -> "WED"
        | Thu -> "THU"
        | Fri -> "FRI"
        | Sat -> "SAT"
      ;;

      let to_string_long t =
        match t with
        | Sun -> "Sunday"
        | Mon -> "Monday"
        | Tue -> "Tuesday"
        | Wed -> "Wednesday"
        | Thu -> "Thursday"
        | Fri -> "Friday"
        | Sat -> "Saturday"
      ;;

      let of_string_internal s =
        match String.uppercase s with
        | "SUN" | "SUNDAY" -> Sun
        | "MON" | "MONDAY" -> Mon
        | "TUE" | "TUESDAY" -> Tue
        | "WED" | "WEDNESDAY" -> Wed
        | "THU" | "THURSDAY" -> Thu
        | "FRI" | "FRIDAY" -> Fri
        | "SAT" | "SATURDAY" -> Sat
        | _ -> failwithf "Day_of_week.of_string: %S" s ()
      ;;

      let of_int_exn i =
        match i with
        | 0 -> Sun
        | 1 -> Mon
        | 2 -> Tue
        | 3 -> Wed
        | 4 -> Thu
        | 5 -> Fri
        | 6 -> Sat
        | _ -> failwithf "Day_of_week.of_int_exn: %d" i ()
      ;;

      let to_int t =
        match t with
        | Sun -> 0
        | Mon -> 1
        | Tue -> 2
        | Wed -> 3
        | Thu -> 4
        | Fri -> 5
        | Sat -> 6
      ;;

      (* Be very generous with of_string.  We accept all possible capitalizations and the
         integer representations as well. *)
      let of_string s =
        try of_string_internal s with
        | _ ->
          (try of_int_exn (Int.of_string s) with
           | _ -> failwithf "Day_of_week.of_string: %S" s ())
      ;;

      (* this is in T rather than outside so that the later functor application to build maps
         uses this sexp representation *)
        include%template Sexpable.Stable.Of_stringable.V1 [@modality portable] (struct
            type nonrec t = t

            let of_string = of_string
            let to_string = to_string
          end)

      let t_sexp_grammar =
        let open Sexplib0.Sexp_grammar in
        let atom name = No_tag { name; clause_kind = Atom_clause } in
        let unsuggested grammar =
          Tag { key = completion_suggested; value = Atom "false"; grammar }
        in
        let int_clause t = unsuggested (atom (Int.to_string (to_int t))) in
        let short_clause t = atom (to_string t) in
        let long_clause t = unsuggested (atom (to_string_long t)) in
        { untyped =
            Lazy
              (Basement.Portable_lazy.from_fun
                 (Portability_hacks.magic_portable__needs_base_and_core
                    (fun () : Sexplib0.Sexp_grammar.grammar ->
                       Variant
                         { case_sensitivity = Case_insensitive
                         ; clauses =
                             List.concat
                               [ List.map all ~f:int_clause
                               ; List.map all ~f:short_clause
                               ; List.map all ~f:long_clause
                               ]
                         })))
        }
      ;;
    end

    include T

    module Unstable = struct
      include T

      include%template (
        Comparable.Make_binable [@mode local] [@modality portable]
          (T) :
          sig
            include
              Comparable.S_binable [@mode local] [@modality portable] with type t := t
          end)

      include%template Hashable.Make_binable [@modality portable] (T)
    end

    include%template
      Comparable.Stable.V1.With_stable_witness.Make [@modality portable] (Unstable)

    include%template
      Hashable.Stable.V1.With_stable_witness.Make [@modality portable] (Unstable)
  end
end

include Stable.V1.Unstable

let weekdays = [ Mon; Tue; Wed; Thu; Fri ]
let weekends = [ Sat; Sun ]

let of_int i =
  try Some (of_int_exn i) with
  | _ -> None
;;

let iso_8601_weekday_number t =
  match t with
  | Mon -> 1
  | Tue -> 2
  | Wed -> 3
  | Thu -> 4
  | Fri -> 5
  | Sat -> 6
  | Sun -> 7
;;

let num_days_in_week = 7
let shift t i = of_int_exn (Int.( % ) (to_int t + i) num_days_in_week)

let num_days ~from ~to_ =
  let d = to_int to_ - to_int from in
  if Int.(d < 0) then d + num_days_in_week else d
;;

let is_sun_or_sat t = t = Sun || t = Sat
let is_weekday t = not (is_sun_or_sat t)
