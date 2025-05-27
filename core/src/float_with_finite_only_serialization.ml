open Ppx_compare_lib.Builtin

module Stable = struct
  open Stable_internal
  module Binable = Binable.Stable

  module V1 = struct
    exception Nan_or_inf [@@deriving sexp]

    type t = float [@@deriving compare ~localize, hash, equal ~localize, stable_witness]

    let verify t =
      match Float.classify t with
      | Normal | Subnormal | Zero -> ()
      | Infinite | Nan -> raise Nan_or_inf
    ;;

    include%template
      Binable.Of_binable.V1 [@mode local] [@modality portable] [@alert "-legacy"]
        (Float)
        (struct
          type nonrec t = t

          let of_binable t =
            verify t;
            t
          ;;

          let%template to_binable t =
            verify t;
            t
          [@@mode m = (global, local)]
          ;;
        end)

    let sexp_of_t = Float.sexp_of_t

    let t_of_sexp = function
      | Sexp.Atom _ as sexp ->
        let t = Float.t_of_sexp sexp in
        (try verify t with
         | e -> Import.of_sexp_error (Import.Exn.to_string e) sexp);
        t
      | s -> Import.of_sexp_error "Decimal.t_of_sexp: Expected Atom, found List" s
    ;;

    let t_sexp_grammar = Float.t_sexp_grammar
  end
end

include Stable.V1
