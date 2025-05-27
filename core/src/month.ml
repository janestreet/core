open! Import

module Stable = struct
  module V1 = struct
    type t =
      | Jan
      | Feb
      | Mar
      | Apr
      | May
      | Jun
      | Jul
      | Aug
      | Sep
      | Oct
      | Nov
      | Dec
    [@@deriving
      compare ~localize
      , equal ~localize
      , globalize
      , hash
      , quickcheck ~portable
      , sexp
      , sexp_grammar
      , typerep
      , variants]

    let failwithf = Printf.failwithf

    let of_int_exn i : t =
      match i with
      | 1 -> Jan
      | 2 -> Feb
      | 3 -> Mar
      | 4 -> Apr
      | 5 -> May
      | 6 -> Jun
      | 7 -> Jul
      | 8 -> Aug
      | 9 -> Sep
      | 10 -> Oct
      | 11 -> Nov
      | 12 -> Dec
      | _ -> failwithf "Month.of_int_exn %d" i ()
    ;;

    let of_int i =
      try Some (of_int_exn i) with
      | _ -> None
    ;;

    let to_int (t : t) =
      match t with
      | Jan -> 1
      | Feb -> 2
      | Mar -> 3
      | Apr -> 4
      | May -> 5
      | Jun -> 6
      | Jul -> 7
      | Aug -> 8
      | Sep -> 9
      | Oct -> 10
      | Nov -> 11
      | Dec -> 12
    ;;

    let to_binable t = to_int t - 1
    let of_binable i = of_int_exn (i + 1)

    include%template
      Binable.Stable.Of_binable.V1 [@mode local] [@modality portable] [@alert "-legacy"]
        (Int.Stable.V1)
        (struct
          type nonrec t = t

          let[@mode m = (global, local)] to_binable = to_binable
          let of_binable = of_binable
        end)

    include%template
      (val (Comparator.Stable.V1.make [@modality portable]) ~compare ~sexp_of_t)

    let stable_witness : t Stable_witness.t =
      Stable_witness.of_serializable Int.Stable.V1.stable_witness of_binable to_binable
    ;;
  end
end

let num_months = 12

module T = struct
  include Stable.V1

  let all = [ Jan; Feb; Mar; Apr; May; Jun; Jul; Aug; Sep; Oct; Nov; Dec ]
  let hash = to_int
end

include T

include%template (
  Hashable.Make_binable [@modality portable] (struct
    include T
  end) :
  sig
    include Hashable.S_binable with type t := t
  end)

include%template
  Comparable.Make_binable_using_comparator [@mode local] [@modality portable] (struct
    include T

    (* In 108.06a and earlier, months in sexps of Maps and Sets were raw ints.  From 108.07
       through 109.13, the output format remained raw as before, but both the raw and
       pretty format were accepted as input.  From 109.14 on, the output format was
       changed from raw to pretty, while continuing to accept both formats.  Once we believe
       most programs are beyond 109.14, we will switch the input format to no longer accept
       raw. *)
    let t_of_sexp sexp =
      match Option.try_with (fun () -> Int.t_of_sexp sexp) with
      | Some i -> of_int_exn (i + 1)
      | None -> T.t_of_sexp sexp
    ;;
  end)

(* Replace the overriden sexp converters from [Comparable.Make_binable] with the ordinary
   symbolic converters. *)
let sexp_of_t = T.sexp_of_t
let t_of_sexp = T.t_of_sexp
let shift t i = of_int_exn (1 + Int.( % ) (to_int t - 1 + i) num_months)

let all_strings =
  Portable_lazy.from_fun (fun () ->
    Iarray.of_list (List.map all ~f:(fun variant -> Sexp.to_string (sexp_of_t variant))))
;;

let to_string (t : t) =
  let all_strings = Portable_lazy.force all_strings in
  all_strings.:(to_int t - 1)
;;

let of_string =
  let table =
    Portable_lazy.from_fun (fun () ->
      Portable_lazy.force all_strings
      |> Iarray.to_list
      |> List.concat_mapi ~f:(fun i s ->
        let t = of_int_exn (i + 1) in
        [ s, t; String.lowercase s, t; String.uppercase s, t ])
      |> String_dict.of_alist_exn)
  in
  fun str ->
    match String_dict.find (Portable_lazy.force table) str with
    | Some x -> x
    | None -> failwithf "Invalid month: %s" str ()
;;

module Export = struct
  type month = t =
    | Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec
end
