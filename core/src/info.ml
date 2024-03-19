(* This module is trying to minimize dependencies on modules in Core, so as to allow
   [Info], [Error], and [Or_error] to be used in is many places places as possible.
   Please avoid adding new dependencies. *)

open! Import
open! Info_intf

module type S = Base.Info.S

module Source_code_position = Source_code_position0
module Binable = Binable0

module Sexp = struct
  include Sexplib.Sexp

  include (
    struct
      type t = Base.Sexp.t =
        | Atom of string
        | List of t list
      [@@deriving bin_io, compare, hash, stable_witness]
    end :
      sig
        type t [@@deriving bin_io, compare, hash, stable_witness]
      end
      with type t := t)
end

module Binable_exn = struct
  module Stable = struct
    module V1 = struct
      module T = struct
        type t = exn [@@deriving sexp_of, stable_witness]
      end

      include T

      let to_binable t = t |> [%sexp_of: t]
      let of_binable = Exn.create_s

      include
        Binable.Stable.Of_binable.V1 [@alert "-legacy"]
          (Sexp)
          (struct
            include T

            let to_binable = to_binable
            let of_binable = of_binable
          end)

      let stable_witness =
        Stable_witness.of_serializable Sexp.stable_witness of_binable to_binable
      ;;
    end
  end
end

module Extend (Info : Base.Info.S) = struct
  include Info

  module Internal_repr = struct
    module Stable = struct
      module Binable_exn = Binable_exn.Stable

      module Source_code_position = struct
        module V1 = struct
          type t = Source_code_position.Stable.V1.t [@@deriving bin_io, stable_witness]

          (* [sexp_of_t] as defined here is unstable; this is OK because there is no
             [t_of_sexp].  [sexp_of_t] is only used to produce a sexp that is never
             deserialized as a [Source_code_position]. *)
          let sexp_of_t = Source_code_position.sexp_of_t
        end
      end

      module V2 = struct
        type t = Info.Internal_repr.t =
          | Could_not_construct of Sexp.t
          | String of string
          | Exn of Binable_exn.V1.t
          | Sexp of Sexp.t
          | Tag_sexp of string * Sexp.t * Source_code_position.V1.t option
          | Tag_t of string * t
          | Tag_arg of string * Sexp.t * t
          | Of_list of int option * t list
          | With_backtrace of t * string (* backtrace *)
        [@@deriving bin_io, sexp_of, stable_witness]
      end
    end

    include Stable.V2

    let to_info = Info.Internal_repr.to_info
    let of_info = Info.Internal_repr.of_info
  end

  module Stable = struct
    module V2 = struct
      module T = struct
        type t = Info.t [@@deriving sexp, sexp_grammar, compare, equal, hash]
      end

      include T
      include Comparator.Stable.V1.Make (T)

      let to_binable = Info.Internal_repr.of_info
      let of_binable = Info.Internal_repr.to_info

      include
        Binable.Stable.Of_binable.V1 [@alert "-legacy"]
          (Internal_repr.Stable.V2)
          (struct
            type nonrec t = t

            let to_binable = to_binable
            let of_binable = of_binable
          end)

      let stable_witness =
        Stable_witness.of_serializable
          Internal_repr.Stable.V2.stable_witness
          of_binable
          to_binable
      ;;

      include Diffable.Atomic.Make (struct
        type nonrec t = t [@@deriving sexp, bin_io, equal]
      end)
    end

    module V1 = struct
      module T = struct
        type t = Info.t [@@deriving compare]

        include
          Sexpable.Stable.Of_sexpable.V1
            (Sexp)
            (struct
              type nonrec t = t

              let to_sexpable = Info.sexp_of_t
              let of_sexpable = Info.t_of_sexp
            end)

        let compare = compare
      end

      include T
      include Comparator.Stable.V1.Make (T)

      let to_binable = sexp_of_t
      let of_binable = t_of_sexp

      include
        Binable.Stable.Of_binable.V1 [@alert "-legacy"]
          (Sexp)
          (struct
            type nonrec t = t

            let to_binable = to_binable
            let of_binable = of_binable
          end)

      let stable_witness =
        Stable_witness.of_serializable Sexp.stable_witness of_binable to_binable
      ;;
    end
  end

  type t = Stable.V2.t [@@deriving bin_io]

  module Diff = Stable.V2.Diff
end

include Extend (Base.Info)
