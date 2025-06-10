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
    [@@deriving bin_io ~localize, compare ~localize, hash, stable_witness]
  end :
    sig
    @@ portable
      type t [@@deriving bin_io ~localize, compare ~localize, hash, stable_witness]
    end
    with type t := t)
end

module Binable_exn = struct
  module Stable = struct
    module V1 = struct
      module T = struct
        type 'a contended_via_portable = 'a Modes.Contended_via_portable.t
        [@@deriving sexp_of]

        let stable_witness_contended_via_portable x =
          Stable_witness.of_serializable
            x
            Modes.Contended_via_portable.wrap
            Modes.Contended_via_portable.unwrap
        ;;

        type t = exn contended_via_portable Modes.Stable.Global.V1.t
        [@@deriving sexp_of, stable_witness]
      end

      include T

      let%template[@mode m = (global, local)] to_binable t =
        t |> Modes.Global.unwrap |> Modes.Contended_via_portable.unwrap |> [%sexp_of: exn]
      ;;

      let of_binable exn =
        exn |> Exn.create_s |> Modes.Contended_via_portable.wrap |> Modes.Global.wrap
      ;;

      include%template
        Binable.Stable.Of_binable.V1 [@mode local] [@modality portable] [@alert "-legacy"]
          (Sexp)
          (struct
            include T

            let[@mode m = (global, local)] to_binable = (to_binable [@mode m])
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

  (* Note that implementations of Base.Info.S should have t_of_sexp that handles any
     sexp. *)
  let%template quickcheck_generator =
    let module G = Base_quickcheck.Generator in
    (G.union [@mode portable])
      [ (G.map [@mode portable]) G.sexp ~f:t_of_sexp
      ; (G.map [@mode portable]) G.string ~f:of_string
      ]
  ;;

  let%template quickcheck_observer =
    (Base_quickcheck.Observer.of_hash_fold [@mode portable]) hash_fold_t
  ;;

  let%template quickcheck_shrinker =
    (Base_quickcheck.Shrinker.map [@mode portable])
      Base_quickcheck.Shrinker.sexp
      ~f:t_of_sexp
      ~f_inverse:sexp_of_t
  ;;

  module Internal_repr = struct
    module Stable = struct
      module Binable_exn = Binable_exn.Stable

      module Source_code_position = struct
        module V1 = struct
          type t = Source_code_position.Stable.V1.t
          [@@deriving bin_io ~localize, stable_witness]

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
        [@@deriving bin_io ~localize, sexp_of, stable_witness]
      end
    end

    include Stable.V2

    let to_info = Info.Internal_repr.to_info
    let of_info = Info.Internal_repr.of_info
  end

  module Stable = struct
    module V2 = struct
      module T = struct
        type t = Info.t
        [@@deriving
          sexp, sexp_grammar, compare ~localize, equal ~localize, globalize, hash]
      end

      include T

      include%template Comparator.Stable.V1.Make [@modality portable] (T)

      let%template[@mode m = (global, local)] to_binable =
        (Info.Internal_repr.of_info : _ @ m -> _)
      ;;

      let of_binable = Info.Internal_repr.to_info

      include%template
        Binable.Stable.Of_binable.V1 [@mode local] [@modality portable] [@alert "-legacy"]
          (Internal_repr.Stable.V2)
          (struct
            type nonrec t = t

            let[@mode m = (global, local)] to_binable = (to_binable [@mode m])
            let of_binable = of_binable
          end)

      let stable_witness =
        Stable_witness.of_serializable
          Internal_repr.Stable.V2.stable_witness
          of_binable
          to_binable
      ;;

      include%template Diffable.Atomic.Make [@modality portable] (struct
          type nonrec t = t [@@deriving sexp, bin_io, equal ~localize]
        end)
    end

    module V1 = struct
      module T = struct
        type t = Info.t [@@deriving compare ~localize]

        include%template
          Sexpable.Stable.Of_sexpable.V1 [@modality portable]
            (Sexp)
            (struct
              type nonrec t = t

              let to_sexpable = Info.sexp_of_t
              let of_sexpable = Info.t_of_sexp
            end)

        let compare = compare
      end

      include T

      include%template Comparator.Stable.V1.Make [@modality portable] (T)

      let to_binable = sexp_of_t
      let%template[@mode local] to_binable t = sexp_of_t (globalize t)
      let of_binable = t_of_sexp

      include%template
        Binable.Stable.Of_binable.V1 [@mode local] [@modality portable] [@alert "-legacy"]
          (Sexp)
          (struct
            type nonrec t = t

            let%template[@mode l = (local, global)] to_binable = (to_binable [@mode l])
            let of_binable = of_binable
          end)

      let stable_witness =
        Stable_witness.of_serializable Sexp.stable_witness of_binable to_binable
      ;;
    end
  end

  type t = Stable.V2.t [@@deriving bin_io ~localize]

  module Diff = Stable.V2.Diff
end

include Extend (Base.Info)
