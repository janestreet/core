(* This module is trying to minimize dependencies on modules in Core, so as to allow
   [Info], [Error], and [Or_error] to be used in is many places places as possible. Please
   avoid adding new dependencies. *)

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
        type t = exn Modes.Stable.Global.V1.t [@@deriving sexp_of, stable_witness]
      end

      include T

      let to_binable t = Modes.Global.unwrap t |> [%sexp_of: exn]

      let of_binable exn =
        let exn = exn |> Exn.create_s in
        Modes.Global.wrap exn
      ;;

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
  module Utf8 : module type of Base.Info.Utf8 = Base.Info.Utf8

  (* Note that implementations of Base.Info.S should have t_of_sexp that handles any sexp. *)
  let quickcheck_generator =
    let module G = Base_quickcheck.Generator in
    G.union [ G.map G.sexp ~f:t_of_sexp; G.map G.string ~f:of_string ]
  ;;

  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_t

  let quickcheck_shrinker =
    Base_quickcheck.Shrinker.map
      Base_quickcheck.Shrinker.sexp
      ~f:t_of_sexp
      ~f_inverse:sexp_of_t
  ;;

  module Internal_repr = struct
    module Stable = struct
      module Binable_exn = Binable_exn.Stable

      module Source_code_position = struct
        module V1 = struct
          type t = Source_code_position.Stable.V1.t [@@deriving bin_io, stable_witness]

          (* [sexp_of_t] as defined here is unstable; this is OK because there is no
             [t_of_sexp]. [sexp_of_t] is only used to produce a sexp that is never
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
        type t = Info.t [@@deriving sexp, sexp_grammar, compare, equal, globalize, hash]

        let t_of_sexp = Info.t_of_sexp
      end

      include T
      include Comparator.Stable.V1.Make (T)

      let to_binable = (Info.Internal_repr.of_info : _ -> _)
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

        let sexp_of_t = Info.sexp_of_t
        let t_of_sexp = Info.t_of_sexp
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

    module Portable = struct
      type 'a portable = 'a Modes.Portable.t = { portable : 'a }
      [@@unboxed] [@@deriving stable_witness]

      [%%rederive.portable
        type 'a portable = 'a Modes.Portable.t
        [@@deriving compare, equal, hash, sexp_grammar, sexp_of]]

      (* Can't be in rederive due to non-standard type signature. *)
      let portable_of_sexp = Modes.Portable.t_of_sexp

      module V1 = struct
        module T = struct
          type t = V1.t portable [@@deriving compare, sexp, stable_witness]
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
      end

      module V2 = struct
        module T = struct
          type t = V2.t portable
          [@@deriving compare, equal, hash, sexp, sexp_grammar, stable_witness]
        end

        let globalize = Info.Portable.globalize

        include T
        include Comparator.Stable.V1.Make (T)

        (* We define a mode-crossing protocol type that make it possible to convert into a
           portable error in [of_binable]. We can't directly use the bin_io of
           [Internal_repr.t] because it's not mode-crossing to portable: [Exn] contains an
           exn.

           This protocol type is wire-compatible with [Internal_repr.t]. This claim is
           tested in unit tests.
        *)
        module Protocol = struct
          open Stable_witness.Export

          type t =
            | Could_not_construct of Sexp.t
            | String of string
            | Exn of Sexp.t
            | Sexp of Sexp.t
            | Tag_sexp of string * Sexp.t * Source_code_position.Stable.V1.t option
            | Tag_t of string * t
            | Tag_arg of string * Sexp.t * t
            | Of_list of int option * t list
            | With_backtrace of t * string (* backtrace *)
          [@@deriving bin_io, stable_witness]
        end

        let rec repr_of_binable : Protocol.t -> Info.Internal_repr.t = function
          | Could_not_construct x -> Could_not_construct x
          | String x -> String x
          | Exn x ->
            let exn_creator = Exn.create_s x in
            Exn { global = exn_creator }
          | Sexp x -> Sexp x
          | Tag_sexp (tag, x, pos) -> Tag_sexp (tag, x, pos)
          | Tag_t (tag, t) -> Tag_t (tag, repr_of_binable t)
          | Tag_arg (message, tag, t) -> Tag_arg (message, tag, repr_of_binable t)
          | Of_list (n, xs) ->
            Of_list
              ( n
              , List.map xs ~f:(fun x -> repr_of_binable x |> Modes.Portable.wrap)
                |> Modes.Portable.unwrap_list )
          | With_backtrace (t, bt) -> With_backtrace (repr_of_binable t, bt)
        ;;

        let repr_to_binable =
          let rec repr_to_binable : Info.Internal_repr.t -> Protocol.t =
            fun repr ->
            match repr with
            | Could_not_construct x -> Could_not_construct x
            | String x -> String x
            | Exn x ->
              let f = Modes.Global.unwrap x in
              Exn (Exn.sexp_of_t f)
            | Sexp x -> Sexp x
            | Tag_sexp (tag, x, pos) -> Tag_sexp (tag, x, pos)
            | Tag_t (tag, t) -> Tag_t (tag, repr_to_binable t)
            | Tag_arg (message, tag, t) -> Tag_arg (message, tag, repr_to_binable t)
            | Of_list (n, xs) ->
              Of_list (n, List.map xs ~f:(fun repr -> repr_to_binable repr))
            | With_backtrace (t, bt) -> With_backtrace (repr_to_binable t, bt)
          in
          repr_to_binable
        ;;

        let to_binable t =
          let repr = Info.Internal_repr.of_info (Modes.Portable.unwrap t) in
          repr_to_binable repr
        ;;

        let of_binable binable =
          repr_of_binable binable |> Info.Internal_repr.to_info |> Modes.Portable.wrap
        ;;

        (* The mli claims that this is wire-compatible with [Info.Stable.V2.t]. A test
           verifies this. Note that we just use [bin_shape_t] as generated by
           [@@deriving bin_io] -- the generated shape exactly matches up with the shape of
           [Info.Stable.V2.bin_shape_t].
        *)
        include
          Binable.Stable.Of_binable.V1 [@alert "-legacy"]
            (Protocol)
            (struct
              type nonrec t = t

              let to_binable = to_binable
              let of_binable = of_binable
            end)

        include Diffable.Atomic.Make (struct
            type nonrec t = t [@@deriving sexp, bin_io, equal]
          end)
      end
    end
  end

  type t = Stable.V2.t [@@deriving bin_io]
  type info = t [@@deriving quickcheck]

  module Diff = Stable.V2.Diff

  module Portable = struct
    include Info.Portable
    module Diff = Stable.Portable.V2.Diff

    include
      Quickcheckable.Of_quickcheckable
        (struct
          type t = info [@@deriving quickcheck]
        end)
        (struct
          type nonrec t = t

          let of_quickcheckable = Info.to_portable_portabilize
          let to_quickcheckable = Info.of_portable
        end)

    [%%rederive.portable type t = Stable.Portable.V2.t [@@deriving bin_io]]
  end
end

include Extend (Base.Info)
