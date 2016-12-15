open Import

module Stable = struct
  module V1 = struct
    module T = struct
      type t =
        { ofday : Ofday.Stable.V1.t;
          zone  : Zone.Stable.V1.t;
        }
      [@@deriving bin_io, fields, compare, hash]

      type sexp_repr = Ofday.Stable.V1.t * Zone.Stable.V1.t [@@deriving sexp]

      let sexp_of_t t = [%sexp_of: sexp_repr] (t.ofday, t.zone)

      let t_of_sexp sexp =
        let (ofday, zone) = [%of_sexp: sexp_repr] sexp in
        { ofday; zone; }
      ;;
      let to_time t date = Time0.of_date_ofday ~zone:(zone t) date (ofday t)

    end
    include T
    include Comparable.Make_binable(T)
    include Hashable.Make_binable(T)
  end
end

include Stable.V1

let create ofday zone = { ofday; zone }

let create_local ofday = create ofday Zone.local

let of_string string : t =
  match String.split string ~on:' ' with
  | [ ofday; zone ] ->
    { ofday = Core_kernel.Ofday.of_string ofday;
      zone  = Zone.of_string  zone;
    }
  | _ ->
    failwithf "Ofday.Zoned.of_string %s" string ()
;;

let to_string (t : t) : string =
  String.concat [
    Core_kernel.Ofday.to_string t.ofday;
    " ";
    Zone.to_string t.zone ]
;;

include Pretty_printer.Register (struct
    type nonrec t = t
    let to_string = to_string
    let module_name = "Core.Std.Time.Ofday.Zoned"
  end)

let%test_unit _ =
  List.iter
    [ "12:00 nyc";
      "12:00 America/New_York";
    ] ~f:(fun string ->
      let t = of_string string in
      assert (t = of_string (to_string t));
      assert (t = t_of_sexp (sexp_of_t t)))
;;
