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
        type t = exn Modes.Stable.Global.V1.t [@@deriving sexp_of, stable_witness]
      end

      include T

      let%template[@mode m = (global, local)] to_binable t =
        Modes.Global.unwrap t |> [%sexp_of: exn]
      ;;

      let of_binable exn =
        let exn = exn |> Exn.create_s in
        Modes.Global.wrap exn
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
  module Utf8 = Base.Info.Utf8

  (* Note that implementations of Base.Info.S should have t_of_sexp that handles any sexp. *)
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
        [@@deriving bin_io ~localize, sexp_of, stable_witness]
      end
    end

    include Stable.V2

    let%template to_info = (Info.Internal_repr.to_info [@mode p])
    [@@mode p = (portable, nonportable)]
    ;;

    let of_info = Info.Internal_repr.of_info
  end

  module Stable = struct
    module V2 = struct
      module T = struct
        type t = Info.t
        [@@deriving
          sexp, sexp_grammar, compare ~localize, equal ~localize, globalize, hash]

        let t_of_sexp = Info.t_of_sexp
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

        let sexp_of_t = Info.sexp_of_t
        let t_of_sexp = Info.t_of_sexp
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

    module Portable = struct
      type 'a portable = 'a Modes.Portable.t = { portable : 'a @@ portable }
      [@@unboxed] [@@deriving stable_witness]

      [%%rederive.portable
        type 'a portable = 'a Modes.Portable.t
        [@@deriving compare ~localize, equal ~localize, hash, sexp_grammar, sexp_of]]

      (* Can't be in rederive due to non-standard type signature. *)
      let portable_of_sexp = Modes.Portable.t_of_sexp

      module V1 = struct
        module T = struct
          type t = V1.t portable [@@deriving compare ~localize, sexp, stable_witness]
        end

        include T

        include%template Comparator.Stable.V1.Make [@modality portable] (T)

        let to_binable = sexp_of_t
        let%template[@mode local] to_binable t = sexp_of_t (Info.Portable.globalize t)
        let of_binable = t_of_sexp

        include%template
          Binable.Stable.Of_binable.V1
            [@modality portable]
            [@mode local]
            [@alert "-legacy"]
            (Sexp)
            (struct
              type nonrec t = t

              let%template[@mode l = (local, global)] to_binable = (to_binable [@mode l])
              let of_binable = of_binable
            end)
      end

      module V2 = struct
        module T = struct
          type t = V2.t portable
          [@@deriving
            compare ~localize, equal ~localize, hash, sexp, sexp_grammar, stable_witness]
        end

        let globalize = Info.Portable.globalize

        include T

        include%template Comparator.Stable.V1.Make [@modality portable] (T)

        (* We define a mode-crossing protocol type that make it possible to convert
           into a portable error in [of_binable]. We can't directly use the bin_io
           of [Internal_repr.t] because it's not mode-crossing to portable:
           [Exn] contains an exn.

           This protocol type is wire-compatible with [Internal_repr.t]. This
           claim is tested in unit tests.
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
          [@@deriving bin_io ~localize, stable_witness]
        end

        let rec repr_of_binable : Protocol.t -> Info.Internal_repr.t @ portable = function
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

        (* Copied from [Local_iterators_to_be_replaced.List.map_local] *)
        let rec map_local t ~f = exclave_
          match t with
          | [] -> []
          | x :: xs ->
            let y = f x in
            let ys = map_local xs ~f in
            y :: ys
        ;;

        let%template[@alloc heap] list_map = List.map
        let%template[@alloc stack] list_map = map_local

        let%template rec repr_to_binable : Info.Internal_repr.t @ m -> Protocol.t @ m =
          fun repr ->
          let repr_to_binable = repr_to_binable [@alloc a] in
          match[@exclave_if_stack a] repr with
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
            Of_list
              ( n
              , (list_map [@alloc a]) xs ~f:(fun repr ->
                  repr_to_binable repr [@exclave_if_local m]) )
          | With_backtrace (t, bt) -> With_backtrace (repr_to_binable t, bt)
        [@@alloc a @ m = (stack_local, heap_global)]
        ;;

        let%template to_binable t =
          let repr = Info.Internal_repr.of_info (Modes.Portable.unwrap t) in
          (repr_to_binable [@alloc a]) repr [@exclave_if_local m]
        [@@alloc a @ m = (stack_local, heap_global)]
        ;;

        let%template of_binable binable =
          repr_of_binable binable
          |> (Info.Internal_repr.to_info [@mode portable])
          |> Modes.Portable.wrap
        ;;

        (* The mli claims that this is wire-compatible with [Info.Stable.V2.t]. A test
           verifies this. Note that we just use [bin_shape_t] as generated by
           [@@deriving bin_io] -- the generated shape exactly matches up with the shape of
           [Info.Stable.V2.bin_shape_t].
        *)
          include%template
            Binable.Stable.Of_binable.V1
              [@alert "-legacy"]
              [@mode local]
              [@modality portable]
              (Protocol)
              (struct
                type nonrec t = t

                let%template[@mode global] to_binable = (to_binable [@alloc heap])
                let%template[@mode local] to_binable = (to_binable [@alloc stack])
                let of_binable = of_binable
              end)

        include%template Diffable.Atomic.Make [@modality portable] (struct
            type nonrec t = t [@@deriving sexp, bin_io, equal]
          end)
      end
    end
  end

  type t = Stable.V2.t [@@deriving bin_io ~localize]
  type info = t [@@deriving quickcheck]

  module Diff = Stable.V2.Diff

  module Portable = struct
    include Info.Portable
    module Diff = Stable.Portable.V2.Diff

    include%template
      Quickcheckable.Of_quickcheckable [@modality portable]
        (struct
          type t = info [@@deriving quickcheck]
        end)
        (struct
          type nonrec t = t

          let of_quickcheckable = Info.to_portable_portabilize
          let to_quickcheckable = Info.of_portable
        end)

    [%%rederive.portable type t = Stable.Portable.V2.t [@@deriving bin_io ~localize]]
  end
end

include Extend (Base.Info)
