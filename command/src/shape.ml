module Stable = struct
  open Sexplib0.Sexp_conv
  open Ppx_compare_lib.Builtin
  open Ppx_stable_witness_runtime.Stable_witness.Export

  module Lazy = struct
    type 'a t = 'a lazy_t [@@deriving sexp, stable_witness]

    let compare = Base.Lazy.compare
  end

  module Anons = struct
    module Grammar = struct
      module V1 = struct
        type t =
          | Zero
          | One of string
          | Many of t
          | Maybe of t
          | Concat of t list
          | Ad_hoc of string
        [@@deriving compare, sexp, stable_witness]

        let rec invariant t =
          Base.Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
            match t with
            | Zero -> ()
            | One _ -> ()
            | Many Zero -> failwith "Many Zero should be just Zero"
            | Many t -> invariant t
            | Maybe Zero -> failwith "Maybe Zero should be just Zero"
            | Maybe t -> invariant t
            | Concat [] | Concat [ _ ] -> failwith "Flatten zero and one-element Concat"
            | Concat ts -> Base.List.iter ts ~f:invariant
            | Ad_hoc _ -> ())
        ;;

        let t_of_sexp sexp =
          let t = [%of_sexp: t] sexp in
          invariant t;
          t
        ;;

        let rec usage =
          let open Import in
          function
          | Zero -> ""
          | One usage -> usage
          | Many Zero -> failwith "bug in command.ml"
          | Many (One _ as t) -> sprintf "[%s ...]" (usage t)
          | Many t -> sprintf "[(%s) ...]" (usage t)
          | Maybe Zero -> failwith "bug in command.ml"
          | Maybe t -> sprintf "[%s]" (usage t)
          | Concat ts -> Base.String.concat ~sep:" " (Base.List.map ts ~f:usage)
          | Ad_hoc usage -> usage
        ;;
      end

      module Model = V1
    end

    module V2 = struct
      type t =
        | Usage of string
        | Grammar of Grammar.V1.t
      [@@deriving compare, sexp, stable_witness]
    end

    module Model = V2
  end

  module Flag_info = struct
    module V1 = struct
      type t =
        { name : string
        ; doc : string
        ; aliases : string list
        }
      [@@deriving compare, sexp, stable_witness]
    end

    module Model = V1
  end

  module Base_info = struct
    module V2 = struct
      type t =
        { summary : string
        ; readme : string option [@sexp.option]
        ; anons : Anons.V2.t
        ; flags : Flag_info.V1.t list
        }
      [@@deriving compare, sexp, stable_witness]
    end

    module V1 = struct
      type t =
        { summary : string
        ; readme : string option [@sexp.option]
        ; usage : string
        ; flags : Flag_info.V1.t list
        }
      [@@deriving sexp, stable_witness]

      let to_latest { summary; readme; usage; flags } =
        { V2.summary; readme; anons = Usage usage; flags }
      ;;

      let of_latest { V2.summary; readme; anons; flags } =
        { summary
        ; readme
        ; usage =
            (match anons with
             | Usage usage -> usage
             | Grammar grammar -> Anons.Grammar.V1.usage grammar)
        ; flags
        }
      ;;
    end

    module Model = V2
  end

  module Group_info = struct
    type a = Dummy_type_because_we_cannot_digest_type_constructors_only_concrete_types
    [@@deriving bin_io]

    module V2 = struct
      type 'a t =
        { summary : string
        ; readme : string option [@sexp.option]
        ; subcommands : (string * 'a) list Lazy.t
        }
      [@@deriving compare, sexp, stable_witness]

      open! Base

      let map t ~f =
        { t with subcommands = Lazy.map t.subcommands ~f:(List.Assoc.map ~f) }
      ;;
    end

    module Model = V2

    module V1 = struct
      type 'a t =
        { summary : string
        ; readme : string option [@sexp.option]
        ; subcommands : (string * 'a) list
        }
      [@@deriving compare, sexp, stable_witness]

      open! Base

      let map t ~f = { t with subcommands = List.Assoc.map t.subcommands ~f }

      let to_latest { summary; readme; subcommands } : 'a Model.t =
        { summary; readme; subcommands = Lazy.from_val subcommands }
      ;;

      let of_latest ({ summary; readme; subcommands } : 'a Model.t) : 'a t =
        { summary; readme; subcommands = Lazy.force subcommands }
      ;;
    end
  end

  module Exec_info = struct
    let abs_path ~dir path =
      if Filename_base.is_absolute path then path else Filename_base.concat dir path
    ;;

    module V3 = struct
      type t =
        { summary : string
        ; readme : string option [@sexp.option]
        ; working_dir : string
        ; path_to_exe : string
        ; child_subcommand : string list
        }
      [@@deriving compare, sexp, stable_witness]

      let to_latest = Base.Fn.id
      let of_latest = Base.Fn.id
    end

    module Model = V3

    module V2 = struct
      type t =
        { summary : string
        ; readme : string option [@sexp.option]
        ; working_dir : string
        ; path_to_exe : string
        }
      [@@deriving sexp, stable_witness]

      let to_v3 t : V3.t =
        { summary = t.summary
        ; readme = t.readme
        ; working_dir = t.working_dir
        ; path_to_exe = t.path_to_exe
        ; child_subcommand = []
        }
      ;;

      let of_v3 (t : V3.t) =
        { summary = t.summary
        ; readme = t.readme
        ; working_dir = t.working_dir
        ; path_to_exe = abs_path ~dir:t.working_dir t.path_to_exe
        }
      ;;

      let to_latest = Base.Fn.compose V3.to_latest to_v3
      let of_latest = Base.Fn.compose of_v3 V3.of_latest
    end

    module V1 = struct
      type t =
        { summary : string
        ; readme : string option [@sexp.option]
        ; (* [path_to_exe] must be absolute. *)
          path_to_exe : string
        }
      [@@deriving sexp, stable_witness]

      let to_v2 t : V2.t =
        { summary = t.summary
        ; readme = t.readme
        ; working_dir = "/"
        ; path_to_exe = t.path_to_exe
        }
      ;;

      let of_v2 (t : V2.t) =
        { summary = t.summary
        ; readme = t.readme
        ; path_to_exe = abs_path ~dir:t.working_dir t.path_to_exe
        }
      ;;

      let to_latest = Base.Fn.compose V2.to_latest to_v2
      let of_latest = Base.Fn.compose of_v2 V2.of_latest
    end
  end

  module Fully_forced = struct
    module V1 = struct
      type t =
        | Basic of Base_info.V2.t
        | Group of t Group_info.V2.t
        | Exec of Exec_info.V3.t * t
      [@@deriving compare, sexp, stable_witness]
    end

    module Model = V1
  end

  module Sexpable = struct
    module V3 = struct
      type t =
        | Base of Base_info.V2.t
        | Group of t Group_info.V2.t
        | Exec of Exec_info.V3.t
        | Lazy of t Lazy.t
      [@@deriving sexp, stable_witness]

      let to_latest = Base.Fn.id
      let of_latest = Base.Fn.id
    end

    module Model = V3

    module V2 = struct
      type t =
        | Base of Base_info.V2.t
        | Group of t Group_info.V1.t
        | Exec of Exec_info.V2.t
      [@@deriving sexp, stable_witness]

      let rec to_latest : t -> Model.t = function
        | Base b -> Base b
        | Exec e -> Exec (Exec_info.V2.to_latest e)
        | Group g -> Group (Group_info.V1.to_latest (Group_info.V1.map g ~f:to_latest))
      ;;

      let rec of_latest : Model.t -> t = function
        | Base b -> Base b
        | Exec e -> Exec (Exec_info.V2.of_latest e)
        | Lazy thunk -> of_latest (Base.Lazy.force thunk)
        | Group g -> Group (Group_info.V1.map (Group_info.V1.of_latest g) ~f:of_latest)
      ;;
    end

    module V1 = struct
      type t =
        | Base of Base_info.V1.t
        | Group of t Group_info.V1.t
        | Exec of Exec_info.V1.t
      [@@deriving sexp, stable_witness]

      let rec to_latest : t -> Model.t = function
        | Base b -> Base (Base_info.V1.to_latest b)
        | Exec e -> Exec (Exec_info.V1.to_latest e)
        | Group g -> Group (Group_info.V1.to_latest (Group_info.V1.map g ~f:to_latest))
      ;;

      let rec of_latest : Model.t -> t = function
        | Base b -> Base (Base_info.V1.of_latest b)
        | Exec e -> Exec (Exec_info.V1.of_latest e)
        | Lazy thunk -> of_latest (Base.Lazy.force thunk)
        | Group g -> Group (Group_info.V1.map (Group_info.V1.of_latest g) ~f:of_latest)
      ;;
    end

    module Versioned = struct
      type t =
        | V1 of V1.t
        | V2 of V2.t
        | V3 of V3.t (* available at least since 2020-04 *)
      [@@deriving sexp, variants, stable_witness]

      let to_latest = function
        | V1 t -> V1.to_latest t
        | V2 t -> V2.to_latest t
        | V3 t -> V3.to_latest t
      ;;

      let of_latest ~version_to_use latest =
        match version_to_use with
        | 1 -> V1 (V1.of_latest latest)
        | 2 -> V2 (V2.of_latest latest)
        | 3 -> V3 (V3.of_latest latest)
        | other ->
          Base.Error.create
            ~here:[%here]
            "unsupported version_to_use"
            other
            [%sexp_of: int]
          |> Base.Error.raise
      ;;
    end
  end
end

open! Base
open! Import

module Anons = struct
  module Grammar = struct
    type t = Stable.Anons.Grammar.Model.t =
      | Zero
      | One of string
      | Many of t
      | Maybe of t
      | Concat of t list
      | Ad_hoc of string
    [@@deriving compare, sexp]

    let invariant = Stable.Anons.Grammar.Model.invariant
    let usage = Stable.Anons.Grammar.Model.usage
  end

  type t = Stable.Anons.Model.t =
    | Usage of string
    | Grammar of Grammar.t
  [@@deriving compare, sexp]
end

module Num_occurrences = struct
  type t =
    { at_least_once : bool
    ; at_most_once : bool
    }
  [@@deriving compare, enumerate, sexp_of]

  let maybe_missing_prefix = "["
  let maybe_missing_suffix = "]"
  let maybe_more_suffix = " ..."

  let to_help_string t ~flag_name =
    let { at_least_once; at_most_once } = t in
    let description =
      if at_least_once
      then flag_name
      else String.concat [ maybe_missing_prefix; flag_name; maybe_missing_suffix ]
    in
    if at_most_once then description else String.concat [ description; maybe_more_suffix ]
  ;;

  let of_help_string name =
    let at_most_once, name =
      match String.chop_suffix name ~suffix:maybe_more_suffix with
      | None -> true, name
      | Some name -> false, name
    in
    let at_least_once, name =
      match
        String.chop_prefix name ~prefix:maybe_missing_prefix
        |> Option.bind ~f:(String.chop_suffix ~suffix:maybe_missing_suffix)
      with
      | None -> true, name
      | Some name -> false, name
    in
    { at_least_once; at_most_once }, name
  ;;

  let%expect_test "to_help_string" =
    let flag_name = "name" in
    List.iter [%all: t] ~f:(fun t ->
      let s = to_help_string t ~flag_name in
      print_s [%message "" ~_:(t : t) s];
      let t', flag_name' = of_help_string s in
      assert ([%compare.equal: t] t t');
      assert ([%compare.equal: string] flag_name flag_name'));
    [%expect
      {|
      (((at_least_once false) (at_most_once false)) "[name] ...")
      (((at_least_once true) (at_most_once false)) "name ...")
      (((at_least_once false) (at_most_once true)) [name])
      (((at_least_once true) (at_most_once true)) name)
      |}]
  ;;
end

module Flag_info = struct
  type t = Stable.Flag_info.Model.t =
    { name : string
    ; doc : string
    ; aliases : string list
    }
  [@@deriving compare, fields ~getters, sexp]

  let parse_name t =
    let num_occurrences, flag_name = Num_occurrences.of_help_string t.name in
    match String.split flag_name ~on:' ' with
    | [ flag_name ] -> Ok (num_occurrences, false, flag_name)
    | [ flag_name; _arg_doc ] -> Ok (num_occurrences, true, flag_name)
    | _ -> Error (Error.create_s [%message "Unable to parse" flag_name])
  ;;

  (* Users are likely to call all three of these functions, in which case we will re-parse
     the [name] several times. We don't expect users of these functions to care about the
     inefficiency. *)
  let flag_name t =
    parse_name t
    |> Or_error.map ~f:(fun ((_ : Num_occurrences.t), (_ : bool), flag_name) -> flag_name)
  ;;

  let num_occurrences t =
    parse_name t
    |> Or_error.map ~f:(fun (num_occurrences, (_ : bool), (_ : string)) ->
         num_occurrences)
  ;;

  let requires_arg t =
    parse_name t
    |> Or_error.map ~f:(fun ((_ : Num_occurrences.t), requires_arg, (_ : string)) ->
         requires_arg)
  ;;

  let help_screen_compare a b =
    match a, b with
    | _, "[-help]" -> -1
    | "[-help]", _ -> 1
    | _, "[-version]" -> -1
    | "[-version]", _ -> 1
    | _, "[-build-info]" -> -1
    | "[-build-info]", _ -> 1
    | _, "help" -> -1
    | "help", _ -> 1
    | _, "version" -> -1
    | "version", _ -> 1
    | _ -> 0
  ;;
end

module Flag_help_display = struct
  type t = Flag_info.t list

  let sort t =
    List.stable_sort t ~compare:(fun a b ->
      Flag_info.help_screen_compare a.Flag_info.name b.Flag_info.name)
  ;;

  let word_wrap_and_strip text width =
    let chunks = String.split text ~on:'\n' in
    List.concat_map chunks ~f:(fun text ->
      let words =
        String.split text ~on:' '
        |> List.filter ~f:(fun word -> not (String.is_empty word))
      in
      match
        List.fold words ~init:None ~f:(fun acc word ->
          Some
            (match acc with
             | None -> [], word
             | Some (lines, line) ->
               (* efficiency is not a concern for the string lengths we expect *)
               let line_and_word = line ^ " " ^ word in
               if String.length line_and_word <= width
               then lines, line_and_word
               else line :: lines, word))
      with
      | None -> []
      | Some (lines, line) -> List.rev (line :: lines))
  ;;

  module Display : sig
    val to_string : t -> string
  end = struct
    let num_cols = 80
    let spaces_string width = String.make width ' '

    let pad_spaces_to_suffix x ~width =
      let slack = width - String.length x in
      x ^ spaces_string slack
    ;;

    let indentation = "  "
    let indent_and_newline x = List.concat [ [ indentation ]; x; [ "\n" ] ]
    let spacing_dot = ". "
    let dot_indentation_offset = 27
    let documentation_start_column = dot_indentation_offset + String.length indentation
    let lhs_width = documentation_start_column
    let lhs_pad_width = dot_indentation_offset + String.length indentation
    let lhs_pad = spaces_string lhs_pad_width

    let lhs_pad_and_newline_terminate =
      List.map ~f:(fun v -> indent_and_newline [ lhs_pad; v ])
    ;;

    let rows flag_name_with_aliases documentation =
      let flag_on_its_own_line =
        let flag_width =
          String.length indentation + String.length flag_name_with_aliases
        in
        if flag_width >= dot_indentation_offset + String.length spacing_dot
        then indent_and_newline [ flag_name_with_aliases ]
        else
          indent_and_newline
            [ pad_spaces_to_suffix ~width:dot_indentation_offset flag_name_with_aliases
            ; spacing_dot
            ]
      in
      let wrapped_documentation =
        word_wrap_and_strip
          documentation
          (num_cols - lhs_width - String.length indentation)
      in
      match wrapped_documentation with
      | [] -> [ flag_on_its_own_line ]
      | doc_wrapped_first_line :: doc_wrapped_rest_lines ->
        let wrapped_doc_lines = lhs_pad_and_newline_terminate doc_wrapped_rest_lines in
        let prefix_doc_wrapped_first_line_with x =
          indent_and_newline
            [ pad_spaces_to_suffix ~width:dot_indentation_offset x
            ; spacing_dot
            ; doc_wrapped_first_line
            ]
        in
        if String.length flag_name_with_aliases >= dot_indentation_offset
        then
          flag_on_its_own_line
          :: prefix_doc_wrapped_first_line_with ""
          :: wrapped_doc_lines
        else
          prefix_doc_wrapped_first_line_with flag_name_with_aliases :: wrapped_doc_lines
    ;;

    let to_string t =
      List.concat_map t ~f:(fun t ->
        let flag_name_with_aliases =
          let flag = t.Flag_info.name in
          String.concat ~sep:", " (flag :: t.aliases)
        in
        rows flag_name_with_aliases t.doc |> List.concat)
      |> String.concat
    ;;
  end

  let to_string t = Display.to_string t
end

module Key_type = struct
  type t =
    | Subcommand
    | Flag

  let to_string = function
    | Subcommand -> "subcommand"
    | Flag -> "flag"
  ;;
end

let lookup_expand alist prefix key_type =
  let is_dash = Char.equal '-' in
  let alist =
    (* no partial matches unless some non-dash char is present *)
    if String.for_all prefix ~f:is_dash
    then List.map alist ~f:(fun (key, (data, _)) -> key, (data, `Full_match_required))
    else alist
  in
  match
    List.filter alist ~f:(function
      | key, (_, `Full_match_required) -> String.( = ) key prefix
      | key, (_, `Prefix) -> String.is_prefix key ~prefix)
  with
  | [ (key, (data, _name_matching)) ] -> Ok (key, data)
  | [] -> Error (sprintf !"unknown %{Key_type} %s" key_type prefix)
  | matches ->
    (match List.find matches ~f:(fun (key, _) -> String.( = ) key prefix) with
     | Some (key, (data, _name_matching)) -> Ok (key, data)
     | None ->
       let matching_keys = List.map ~f:fst matches in
       Error
         (sprintf
            !"%{Key_type} %s is an ambiguous prefix: %s"
            key_type
            prefix
            (String.concat ~sep:", " matching_keys)))
;;

module Base_info = struct
  type t = Stable.Base_info.Model.t =
    { summary : string
    ; readme : string option [@sexp.option]
    ; anons : Anons.t
    ; flags : Flag_info.t list
    }
  [@@deriving compare, fields ~getters, sexp]

  let find_flag t prefix =
    match String.is_prefix prefix ~prefix:"-" with
    | false -> Error (Error.create_s [%message "Flags must begin with '-'" prefix])
    | true ->
      let%bind.Or_error choices =
        List.map t.flags ~f:(fun (flag_info : Flag_info.t) ->
          let%bind.Or_error flag_name = Flag_info.flag_name flag_info in
          Ok
            (List.map (flag_name :: flag_info.aliases) ~f:(fun key ->
               key, (flag_info, `Prefix))))
        |> Or_error.combine_errors
      in
      lookup_expand (List.concat choices) prefix Flag
      |> Result.map_error ~f:Error.of_string
      |> Or_error.map ~f:snd
  ;;

  let get_usage t =
    match t.anons with
    | Usage usage -> usage
    | Grammar grammar -> Anons.Grammar.usage grammar
  ;;
end

module Group_info = struct
  type 'a t = 'a Stable.Group_info.Model.t =
    { summary : string
    ; readme : string option [@sexp.option]
    ; subcommands : (string * 'a) List.t Lazy.t
    }
  [@@deriving compare, fields ~getters, sexp]

  let find_subcommand t prefix =
    match String.is_prefix prefix ~prefix:"-" with
    | true ->
      Error (Error.create_s [%message "Subcommands must not begin with '-'" prefix])
    | false ->
      let choices =
        List.map (force t.subcommands) ~f:(fun (key, a) -> key, (a, `Prefix))
      in
      lookup_expand choices prefix Subcommand
      |> Result.map_error ~f:Error.of_string
      |> Or_error.map ~f:snd
  ;;

  let map = Stable.Group_info.Model.map
end

module Exec_info = struct
  type t = Stable.Exec_info.Model.t =
    { summary : string
    ; readme : string option [@sexp.option]
    ; working_dir : string
    ; path_to_exe : string
    ; child_subcommand : string list
    }
  [@@deriving compare, sexp]
end

module Fully_forced = struct
  type t = Stable.Fully_forced.Model.t =
    | Basic of Base_info.t
    | Group of t Group_info.t
    | Exec of Exec_info.t * t
  [@@deriving compare, sexp]

  let expanded_subcommands t =
    let rec expand = function
      | Exec (_, t) -> expand t
      | Basic _ -> [ [] ]
      | Group { subcommands; _ } ->
        List.concat_map (Lazy.force subcommands) ~f:(fun (name, t) ->
          List.map ~f:(fun path -> name :: path) (expand t))
    in
    (* Reversing so that the commands show up in the same order as help is output *)
    List.rev (expand t)
  ;;
end

module Sexpable = struct
  type t = Stable.Sexpable.Model.t =
    | Base of Base_info.t
    | Group of t Group_info.t
    | Exec of Exec_info.t
    | Lazy of t Lazy.t
  [@@deriving sexp_of]

  let extraction_var = Env_var.to_string COMMAND_OUTPUT_HELP_SEXP

  module Versioned = Stable.Sexpable.Versioned

  let supported_versions =
    let f i supported _ = Set.add supported i in
    Versioned.Variants.fold ~init:(Set.empty (module Int)) ~v1:(f 1) ~v2:(f 2) ~v3:(f 3)
  ;;

  let of_versioned = Versioned.to_latest
  let to_versioned t ~version_to_use = Versioned.of_latest t ~version_to_use
end

type t =
  | Basic of Base_info.t
  | Group of t Group_info.t
  | Exec of Exec_info.t * (unit -> t)
  | Lazy of t Lazy.t

let rec fully_forced : t -> Fully_forced.t = function
  | Basic b -> Basic b
  | Group g -> Group (Group_info.map g ~f:fully_forced)
  | Exec (e, f) -> Exec (e, fully_forced (f ()))
  | Lazy thunk -> fully_forced (Lazy.force thunk)
;;

let rec get_summary = function
  | Basic b -> b.summary
  | Group g -> g.summary
  | Exec (e, _) -> e.summary
  | Lazy thunk -> get_summary (Lazy.force thunk)
;;

let help_text = `Use_Command_unix

module Private = struct
  module Key_type = Key_type

  let abs_path = Stable.Exec_info.abs_path
  let help_screen_compare = Flag_info.help_screen_compare
  let word_wrap = Flag_help_display.word_wrap_and_strip
  let lookup_expand = lookup_expand
end
