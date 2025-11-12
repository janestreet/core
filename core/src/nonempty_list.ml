[@@@ocaml.flambda_o3]

module Stable = struct
  open Stable_internal
  open Ppx_compare_lib.Builtin
  open Base.Exported_for_specific_uses.Globalize
  module Binable = Binable.Stable
  module List = List.Stable
  module Sexpable = Sexpable.Stable

  module V3 = struct
    module T = struct
      type%template ('a : k) t = ('a Base.Nonempty_list.t[@kind k]) =
        | ( :: ) of 'a * ('a List.V1.t[@kind k])
      [@@kind k = base_non_value] [@@deriving compare ~localize, equal ~localize]

      type nonrec ('a : value_or_null) t = 'a Base.Nonempty_list.t =
        | ( :: ) of 'a * 'a list
      [@@deriving compare ~localize, equal ~localize, globalize, hash]
    end

    include T

    module Format = struct
      type 'a t = 'a list [@@deriving bin_io, sexp, stable_witness]
    end

    include%template
      Binable.Of_binable1.V2 [@modality portable]
        (Format)
        (struct
          include T

          let to_binable = Base.Nonempty_list.to_list
          let of_binable = Base.Nonempty_list.of_list_exn

          let caller_identity =
            Bin_prot.Shape.Uuid.of_string "9a63aaee-82e0-11ea-8fb6-aa00005c6184"
          ;;
        end)

    include%template
      Sexpable.Of_sexpable1.V1 [@modality portable]
        (Format)
        (struct
          include T

          let to_sexpable = Base.Nonempty_list.to_list
          let of_sexpable = Base.Nonempty_list.of_list_exn
        end)

    let t_sexp_grammar (type a) ({ untyped = element } : [%sexp_grammar: a])
      : [%sexp_grammar: a t]
      =
      { untyped = List (Cons (element, Many element)) }
    ;;

    module T_stable_witness = Stable_witness.Of_serializable1 (Format) (T)

    let stable_witness : type a. a Stable_witness.t -> a t Stable_witness.t =
      fun witness ->
      T_stable_witness.of_serializable
        Format.stable_witness
        Base.Nonempty_list.of_list_exn
        Base.Nonempty_list.to_list
        witness
    ;;

    let%expect_test _ =
      print_endline [%bin_digest: int t];
      [%expect {| eaa5c1535ea5c1691291b3bdbbd7b014 |}]
    ;;
  end

  module V2 = struct
    module T = struct
      type nonrec 'a t = 'a V3.t = ( :: ) of 'a * 'a list
      [@@deriving compare ~localize, equal ~localize, hash]

      let sexp_of_t = V3.sexp_of_t
      let t_of_sexp = V3.t_of_sexp
      let t_sexp_grammar = V3.t_sexp_grammar
    end

    include T

    module Record_format = struct
      type 'a t =
        { hd : 'a
        ; tl : 'a list
        }
      [@@deriving bin_io, compare ~localize, stable_witness]

      let of_nonempty_list (hd :: tl) = { hd; tl }
      let to_nonempty_list { hd; tl } = hd :: tl
    end

    include%template
      Binable.Of_binable1.V1 [@alert "-legacy"] [@modality portable]
        (Record_format)
        (struct
          include T

          let to_binable = Record_format.of_nonempty_list
          let of_binable = Record_format.to_nonempty_list
        end)

    module T_stable_witness = Stable_witness.Of_serializable1 (Record_format) (T)

    let stable_witness (type a) : a Stable_witness.t -> a t Stable_witness.t =
      fun witness ->
      T_stable_witness.of_serializable
        Record_format.stable_witness
        Record_format.to_nonempty_list
        Record_format.of_nonempty_list
        witness
    ;;

    let%expect_test _ =
      print_endline [%bin_digest: int t];
      [%expect {| 2aede2e9b03754f5dfa5f1a61877b330 |}]
    ;;
  end

  module V1 = struct
    module T = struct
      type 'a t = 'a V2.t = ( :: ) of 'a * 'a list
      [@@deriving compare ~localize, equal ~localize]

      let sexp_of_t = V2.sexp_of_t
      let t_of_sexp = V2.t_of_sexp
      let t_sexp_grammar = V2.t_sexp_grammar
    end

    include T

    module Pair_format = struct
      type 'a t = 'a * 'a list [@@deriving bin_io, compare ~localize, stable_witness]

      let of_nonempty_list (hd :: tl) = hd, tl
      let to_nonempty_list (hd, tl) = hd :: tl
    end

    include%template
      Binable.Of_binable1.V1 [@alert "-legacy"] [@modality portable]
        (Pair_format)
        (struct
          include T

          let to_binable = Pair_format.of_nonempty_list
          let of_binable = Pair_format.to_nonempty_list
        end)

    module T_stable_witness = Stable_witness.Of_serializable1 (Pair_format) (T)

    let stable_witness (type a) : a Stable_witness.t -> a t Stable_witness.t =
      fun witness ->
      T_stable_witness.of_serializable
        Pair_format.stable_witness
        Pair_format.to_nonempty_list
        Pair_format.of_nonempty_list
        witness
    ;;

    let%expect_test _ =
      print_endline [%bin_digest: int t];
      [%expect {| f27871ef428aef2925f18d6be687bf9c |}]
    ;;
  end
end

open Std_internal
module Unstable = Stable.V3
include Base.Nonempty_list

[%%rederive
  type nonrec 'a t = 'a t = ( :: ) of 'a * 'a list
  [@@deriving bin_io ~localize ~portable, quickcheck ~portable, typerep]]

type 'a nonempty_list = 'a t [@@deriving sexp_of]

let validate ~name check t = Validate.list ~name check (to_list t)
let validate_indexed check t = Validate.list_indexed check (to_list t)

let flag arg_type =
  Command.Param.map_flag
    (Command.Param.one_or_more_as_pair arg_type)
    ~f:(fun (one, more) -> one :: more)
;;

let comma_separated_argtype ?key ?strip_whitespace ?unique_values arg_type =
  arg_type
  |> Command.Param.Arg_type.comma_separated
       ~allow_empty:false
       ?strip_whitespace
       ?unique_values
  |> Command.Param.Arg_type.map ?key ~f:of_list_exn
;;

let anons anons =
  let open Command.Anons in
  non_empty_sequence_as_list anons |> map_anons ~f:of_list_exn
;;

(** This relies on the fact that the representation of [List.( :: )] constructor is
    identical to that of [Nonempty_list.( :: )], and that they are each the first
    non-constant constructor in their respective types. *)
module Option = struct
  type 'a t = 'a list
  [@@deriving
    compare ~localize, equal ~localize, sexp, sexp_grammar, hash, quickcheck, typerep]

  let[@inline always] none () = []
  let some (_ :: _ as value : 'a nonempty_list) : 'a t = Obj.magic value
  let unchecked_value (t : 'a t) : 'a nonempty_list = Obj.magic t
  let is_none t = Base.phys_equal t (none ())
  let is_some t = not (is_none t)
  let to_option = of_list

  let of_option = function
    | None -> none ()
    | Some value -> some value
  ;;

  let none = none ()

  let value_exn = function
    | [] -> raise_s [%sexp "Nonempty_list.Option.value_exn: empty list"]
    | _ :: _ as l -> unchecked_value l
  ;;

  let value t ~default = Bool.select (is_none t) default (unchecked_value t)

  module Optional_syntax = struct
    module Optional_syntax = struct
      let is_none = is_none
      let unsafe_value = unchecked_value
    end
  end
end
