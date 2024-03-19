open! Base
open! Import
include Command_intf
module Shape = Shape

(* in order to define expect tests, we want to raise rather than exit if the code is
   running in the test runner process *)
let raise_instead_of_exit =
  match Ppx_inline_test_lib.testing with
  | `Testing `Am_test_runner -> true
  | `Testing `Am_child_of_test_runner | `Not_testing -> false
;;

exception Exit_called of { status : int } [@@deriving sexp_of]

(* [raise_instead_of_exit]-respecting wrappers for [exit] and functions that call it *)
include struct
  let exit status =
    if raise_instead_of_exit then raise (Exit_called { status }) else Stdlib.exit status
  ;;

  module Exn = struct
    let handle_uncaught_and_exit f =
      if raise_instead_of_exit
      then (
        try f () with
        | Exit_called { status = 0 } as exn -> print_s [%sexp (exn : exn)])
      else Exn.handle_uncaught_and_exit f
    ;;
  end
end

let unwords xs = String.concat ~sep:" " xs
let unparagraphs xs = String.concat ~sep:"\n\n" xs

exception Failed_to_parse_command_line of string

let die fmt = ksprintf (fun msg () -> raise (Failed_to_parse_command_line msg)) fmt
let help_screen_compare = Shape.Private.help_screen_compare

(* universal maps are used to pass around values between different bits
   of command line parsing code without having a huge impact on the
   types involved

   1. passing values from parsed args to command-line autocomplete functions
   2. passing special values to a base commands that request them in their spec
 * expanded subcommand path
 * args passed to the base command
 * help text for the base command
 *)
module Env = struct
  include Univ_map

  let key_create name = Univ_map.Key.create ~name Sexplib.Conv.sexp_of_opaque
  let multi_add = Univ_map.Multi.add
  let set_with_default = Univ_map.With_default.set
end

let key_internal_validate_parsing =
  Env.Key.create ~name:"----internal-validate-parsing" [%sexp_of: unit]
;;

module Parsing_outcome : sig
  type 'a t =
    { result : ('a, [ `Missing_required_flags of Error.t ]) Result.t
    ; has_arg : bool
    }

  val return_no_arg : 'a -> 'a t
  val return_with_arg : 'a -> 'a t
  val error : has_arg:bool -> [ `Missing_required_flags of Error.t ] -> 'a t
  val recover_from_missing_required_flags : 'a t -> 'a t t

  val introduce_missing_required_flags
    :  ('a, [ `Missing_required_flags of Error.t ]) Result.t t
    -> 'a t

  include Applicative.S with type 'a t := 'a t
end = struct
  type 'a t =
    { result : ('a, [ `Missing_required_flags of Error.t ]) Result.t
    ; has_arg : bool
    }

  let apply f x =
    { result =
        Result.combine
          f.result
          x.result
          ~ok:(fun f x -> f x)
          ~err:(fun (`Missing_required_flags err_1) (`Missing_required_flags _err_2) ->
            `Missing_required_flags err_1)
    ; has_arg = f.has_arg || x.has_arg
    }
  ;;

  let recover_from_missing_required_flags t = { result = Ok t; has_arg = t.has_arg }

  let introduce_missing_required_flags t =
    { result = Result.join t.result; has_arg = t.has_arg }
  ;;

  let map { result; has_arg } ~f = { result = Result.map result ~f; has_arg }
  let return_no_arg v = { result = Ok v; has_arg = false }
  let return_with_arg v = { result = Ok v; has_arg = true }
  let error ~has_arg err = { result = Error err; has_arg }

  include Applicative.Make (struct
    type nonrec 'a t = 'a t

    let return = return_no_arg
    let map = `Custom map
    let apply = apply
  end)
end

module Auto_complete = struct
  type t = Env.t -> part:string -> string list

  module For_escape = struct
    type t = Env.t -> part:string list -> string list
  end
end

module Completer = struct
  type t = Auto_complete.t option

  module For_escape = struct
    type t = Auto_complete.For_escape.t option
  end

  let run_and_exit t env ~part : Nothing.t =
    Option.iter t ~f:(fun completions ->
      List.iter ~f:print_endline (completions env ~part));
    exit 0
  ;;
end

module Arg_type : sig
  type 'a t

  val extra_doc : 'a t -> string option lazy_t
  val key : 'a t -> 'a Env.Multi.Key.t option
  val complete : 'a t -> Completer.t
  val parse : 'a t -> string -> 'a Or_error.t

  val create
    :  ?complete:Auto_complete.t
    -> ?key:'a Env.Multi.Key.t
    -> (string -> 'a)
    -> 'a t

  val map : ?key:'a Env.Multi.Key.t -> 'b t -> f:('b -> 'a) -> 'a t
  val of_lazy : ?key:'a Env.Multi.Key.t -> 'a t lazy_t -> 'a t

  val of_map
    :  ?accept_unique_prefixes:bool
    -> ?case_sensitive:bool
    -> ?list_values_in_help:bool
    -> ?auto_complete:Auto_complete.t
    -> ?key:'a Env.Multi.Key.t
    -> 'a Map.M(String).t
    -> 'a t

  val of_alist_exn
    :  ?accept_unique_prefixes:bool
    -> ?case_sensitive:bool
    -> ?list_values_in_help:bool
    -> ?auto_complete:Auto_complete.t
    -> ?key:'a Env.Multi.Key.t
    -> (string * 'a) list
    -> 'a t

  val enumerated
    :  ?accept_unique_prefixes:bool
    -> ?case_sensitive:bool
    -> ?list_values_in_help:bool
    -> ?auto_complete:Auto_complete.t
    -> ?key:'a Env.Multi.Key.t
    -> (module Enumerable_stringable with type t = 'a)
    -> 'a t

  val enumerated_sexpable
    :  ?accept_unique_prefixes:bool
    -> ?case_sensitive:bool
    -> ?list_values_in_help:bool
    -> ?auto_complete:Auto_complete.t
    -> ?key:'a Env.Multi.Key.t
    -> (module Enumerable_sexpable with type t = 'a)
    -> 'a t

  val comma_separated
    :  ?allow_empty:bool
    -> ?key:'a list Env.Multi.Key.t
    -> ?strip_whitespace:bool
    -> ?unique_values:bool
    -> 'a t
    -> 'a list t

  module Export : sig
    val string : string t
    val int : int t
    val char : char t
    val float : float t
    val bool : bool t
    val sexp : Sexp.t t
    val sexp_conv : ?complete:Auto_complete.t -> (Sexp.t -> 'a) -> 'a t
  end

  val auto_complete : _ t -> Auto_complete.t
end = struct
  type 'a t =
    { parse : string -> 'a
    ; complete : Completer.t
    ; key : 'a Univ_map.Multi.Key.t option
    ; extra_doc : string option Lazy.t
    }
  [@@deriving fields ~getters]

  let parse t s = Or_error.try_with (fun () -> t.parse s)
  let create' ?complete ?key parse ~extra_doc = { parse; key; complete; extra_doc }

  let create ?complete ?key of_string =
    create' ?complete ?key of_string ~extra_doc:(Lazy.from_val None)
  ;;

  let map ?key t ~f = { t with key; parse = (fun s -> f (t.parse s)) }

  let of_lazy ?key t =
    let parse str = (force t).parse str in
    let complete env ~part =
      match (force t).complete with
      | None ->
        (* See [run_and_exit] - no completions is equivalent to not having a
           [Complete]. *)
        []
      | Some complete -> complete env ~part
    in
    let extra_doc = Lazy.bind t ~f:extra_doc in
    { parse; complete = Some complete; key; extra_doc }
  ;;

  let string = create Fn.id
  let int = create Int.of_string
  let char = create Char.of_string
  let float = create Float.of_string
  let sexp = create Parsexp.Single.parse_string_exn

  let sexp_conv ?complete of_sexp =
    create ?complete (fun s -> of_sexp (Parsexp.Single.parse_string_exn s))
  ;;

  let associative
    ?(accept_unique_prefixes = true)
    ?(list_values_in_help = true)
    ?auto_complete
    ?key
    ~case_sensitive
    alist
    =
    let open struct
      module type S = sig
        include Comparator.S with type t = string

        val is_prefix : string -> prefix:string -> bool
      end

      type 'a t =
        | T :
            { cmp : (module S with type comparator_witness = 'cmp)
            ; map : (string, 'a, 'cmp) Map.t
            }
            -> 'a t
    end in
    let (T { cmp = (module S); map }) =
      let make_map_raise_duplicate_key
        (type cmp)
        (module S : S with type comparator_witness = cmp)
        alist
        =
        match Map.of_alist (module S) alist with
        | `Ok map -> map
        | `Duplicate_key (_ : S.t) ->
          let duplicate_keys =
            List.map alist ~f:(fun (k, (_ : 'a)) -> k, k)
            |> Map.of_alist_multi (module S)
            |> Map.filter ~f:(function
                 | [] | [ _ ] -> false
                 | _ :: _ :: _ -> true)
            |> Map.data
          in
          raise_s
            [%message
              "Command.Spec.Arg_type.of_alist_exn" (duplicate_keys : string list list)]
      in
      let make cmp = T { cmp; map = make_map_raise_duplicate_key cmp alist } in
      if case_sensitive then make (module String) else make (module String.Caseless)
    in
    let complete univ_map ~part:prefix =
      match auto_complete with
      | Some complete -> complete univ_map ~part:prefix
      | None ->
        List.filter_map (Map.to_alist map) ~f:(fun (name, _) ->
          match S.is_prefix name ~prefix with
          | false -> None
          | true ->
            (* Bash completion will not accept [Foo] as a completion for [f]. So we need
               to match the capitalization given. *)
            let suffix = String.subo name ~pos:(String.length prefix) in
            let name = prefix ^ suffix in
            Some name)
    in
    let find arg =
      match Map.find map arg with
      | Some _ as s -> s
      | None ->
        (match accept_unique_prefixes with
         | false -> None
         | true ->
           (match
              Map.to_alist map
              |> List.filter ~f:(fun (name, _) -> S.is_prefix name ~prefix:arg)
            with
            | [ (_singleton_key, v) ] -> Some v
            | [] | _ :: _ :: _ ->
              (* In the two-or-more case we could provide filtered help text, but it's
                 more generally useful to list all the options, which we do below. *)
              None))
    in
    create'
      ~extra_doc:
        (lazy
          (if list_values_in_help
           then (
             let values = String.concat ~sep:", " (Map.keys map) in
             Some [%string "(can be: %{values})"])
           else None))
      ?key
      ~complete
      (fun arg ->
        match find arg with
        | Some v -> v
        | None ->
          let valid_arguments_extra =
            if case_sensitive then "" else " (case insensitive)"
          in
          failwithf
            "valid arguments%s: {%s}"
            valid_arguments_extra
            (String.concat ~sep:"," (Map.keys map))
            ())
  ;;

  let of_alist_exn
    ?accept_unique_prefixes
    ?(case_sensitive = true)
    ?list_values_in_help
    ?auto_complete
    ?key
    alist
    =
    associative
      ?accept_unique_prefixes
      ?list_values_in_help
      ?auto_complete
      ?key
      ~case_sensitive
      alist
  ;;

  let of_map
    ?accept_unique_prefixes
    ?case_sensitive
    ?list_values_in_help
    ?auto_complete
    ?key
    map
    =
    of_alist_exn
      ?accept_unique_prefixes
      ?case_sensitive
      ?list_values_in_help
      ?auto_complete
      ?key
      (Map.to_alist map)
  ;;

  let enumerated
    (type t)
    ?accept_unique_prefixes
    ?case_sensitive
    ?list_values_in_help
    ?auto_complete
    ?key
    (module E : Enumerable_stringable with type t = t)
    =
    of_alist_exn
      ?accept_unique_prefixes
      ?case_sensitive
      ?list_values_in_help
      ?auto_complete
      ?key
      (let%map.List t = E.all in
       E.to_string t, t)
  ;;

  let enumerated_sexpable
    (type t)
    ?accept_unique_prefixes
    ?case_sensitive
    ?list_values_in_help
    ?auto_complete
    ?key
    (module E : Enumerable_sexpable with type t = t)
    =
    enumerated
      ?accept_unique_prefixes
      ?case_sensitive
      ?list_values_in_help
      ?auto_complete
      ?key
      (module struct
        include E

        let to_string t = Sexp.to_string [%sexp (t : E.t)]
      end)
  ;;

  let bool = enumerated ~list_values_in_help:false (module Bool)

  let comma_separated
    ?(allow_empty = false)
    ?key
    ?(strip_whitespace = false)
    ?(unique_values = false)
    t
    =
    let strip = if strip_whitespace then fun str -> String.strip str else Fn.id in
    let complete =
      Option.map t.complete ~f:(fun complete_elt env ~part ->
        let prefixes, suffix =
          match String.split part ~on:',' |> List.rev with
          | [] -> [], part
          | hd :: tl -> List.rev tl, hd
        in
        let is_allowed =
          if not unique_values
          then fun (_ : string) -> true
          else (
            let seen_already =
              prefixes |> List.map ~f:strip |> Set.of_list (module String)
            in
            fun choice -> not (Set.mem seen_already (strip choice)))
        in
        let choices =
          match
            List.filter (complete_elt env ~part:suffix) ~f:(fun choice ->
              (not (String.mem choice ',')) && is_allowed choice)
          with
          (* If there is exactly one choice to auto-complete, add a second choice with
             a trailing comma so that auto-completion will go to the end but bash
             won't add a space.  If there are multiple choices, or a single choice
             that must be final, there is no need to add a dummy option. *)
          | [ choice ] -> [ choice; choice ^ "," ]
          | choices -> choices
        in
        List.map choices ~f:(fun choice -> String.concat ~sep:"," (prefixes @ [ choice ])))
    in
    let of_string string =
      let string = strip string in
      if String.is_empty string
      then
        if allow_empty
        then []
        else failwith "Command.Spec.Arg_type.comma_separated: empty list not allowed"
      else List.map (String.split string ~on:',') ~f:(fun str -> t.parse (strip str))
    in
    create ?key ?complete of_string
  ;;

  module Export = struct
    let string = string
    let int = int
    let char = char
    let float = float
    let bool = bool
    let sexp = sexp
    let sexp_conv = sexp_conv
  end

  let auto_complete t =
    match t.complete with
    | Some f -> f
    | None -> fun _ ~part:_ -> []
  ;;
end

module Flag = struct
  module Num_occurrences = struct
    type t = Shape.Num_occurrences.t =
      { at_least_once : bool
      ; at_most_once : bool
      }
    [@@deriving compare, enumerate, sexp_of]

    let to_help_string = Shape.Num_occurrences.to_help_string

    let to_help_string_deprecated { at_least_once; at_most_once = _ } flag_name =
      to_help_string { at_least_once; at_most_once = true } ~flag_name
    ;;

    let any = { at_least_once = false; at_most_once = false }
    let at_least_once = { at_least_once = true; at_most_once = false }
    let at_most_once = { at_least_once = false; at_most_once = true }
    let exactly_once = { at_least_once = true; at_most_once = true }
  end

  type action =
    | No_arg of (Env.t -> Env.t)
    | Print_info_and_quit of (Env.t -> string)
    | Arg of (Env.t -> string -> Env.t) * Completer.t
    | Rest of (Env.t -> string list -> Env.t) * Completer.For_escape.t

  module Internal = struct
    type t =
      { name : string
      ; aliases : string list
      ; aliases_excluded_from_help : string list
          (* [aliases_excluded_from_help] are aliases that don't show up in -help output.
         Currently they're only used for double-dash built-in flags like --help and
         --version. *)
      ; action : action
      ; doc : string
      ; num_occurrences : Num_occurrences.t
      ; check_available : Env.t -> unit
      ; name_matching : [ `Prefix | `Full_match_required ]
      }

    let wrap_if_optional t flag_name =
      Num_occurrences.to_help_string t.num_occurrences ~flag_name
    ;;

    module Doc = struct
      type t =
        { arg_doc : string option
        ; doc : string
        }

      let parse ~action ~doc =
        let arg_doc, doc =
          match (action : action), String.lsplit2 doc ~on:' ' with
          | (No_arg _ | Print_info_and_quit _), _ -> None, doc
          | Arg _, (None | Some ("", _)) -> Some "_", doc
          | Rest _, (None | Some ("", _)) -> None, doc
          | (Arg _ | Rest _), Some (arg, doc) -> Some arg, doc
        in
        { doc = String.strip doc; arg_doc }
      ;;

      let concat ~name ~arg_doc =
        match arg_doc with
        | None -> name
        | Some arg_doc -> name ^ " " ^ arg_doc
      ;;
    end

    module Deprecated = struct
      let wrap_if_optional t x =
        Num_occurrences.to_help_string_deprecated t.num_occurrences x
      ;;

      (* flag help in the format of the old command. used for injection *)
      let help
        ({ name
         ; doc
         ; aliases
         ; action
         ; num_occurrences = _
         ; check_available = _
         ; name_matching = _
         ; aliases_excluded_from_help = _
         } as t)
        =
        if String.is_prefix doc ~prefix:" "
        then
          (name, String.lstrip doc)
          :: List.map aliases ~f:(fun x -> x, sprintf "same as \"%s\"" name)
        else (
          let { Doc.arg_doc; doc } = Doc.parse ~action ~doc in
          (wrap_if_optional t (Doc.concat ~name ~arg_doc), doc)
          :: List.map aliases ~f:(fun x ->
               ( wrap_if_optional t (Doc.concat ~name:x ~arg_doc)
               , sprintf "same as \"%s\"" name )))
      ;;
    end

    let align
      ({ name
       ; doc
       ; aliases
       ; action
       ; num_occurrences = _
       ; check_available = _
       ; name_matching = _
       ; aliases_excluded_from_help = _
       } as t)
      : Shape.Flag_info.t
      =
      let { Doc.arg_doc; doc } = Doc.parse ~action ~doc in
      let name = wrap_if_optional t (Doc.concat ~name ~arg_doc) in
      { name; doc; aliases }
    ;;

    let create flags =
      match
        Map.of_alist (module String) (List.map flags ~f:(fun flag -> flag.name, flag))
      with
      | `Duplicate_key flag -> failwithf "multiple flags named %s" flag ()
      | `Ok map ->
        List.concat_map flags ~f:(fun flag -> flag.name :: flag.aliases)
        |> List.find_a_dup ~compare:[%compare: string]
        |> Option.iter ~f:(fun x -> failwithf "multiple flags or aliases named %s" x ());
        map
    ;;
  end

  type 'a state =
    { action : action
    ; read : Env.t -> 'a Parsing_outcome.t
    ; num_occurrences : Num_occurrences.t
    ; extra_doc : string option Lazy.t
    }

  type 'a t = string -> 'a state

  let arg_flag name arg_type read write num_occurrences =
    { read
    ; num_occurrences
    ; action =
        (let update env arg =
           match Arg_type.parse arg_type arg with
           | Error error ->
             die
               "failed to parse %s value %S.\n%s"
               name
               arg
               (Error.to_string_hum error)
               ()
           | Ok arg ->
             let env = write env arg in
             (match Arg_type.key arg_type with
              | None -> env
              | Some key -> Env.multi_add env ~key ~data:arg)
         in
         Arg (update, Arg_type.complete arg_type))
    ; extra_doc = Arg_type.extra_doc arg_type
    }
  ;;

  let map_flag (t : _ t) ~f input =
    let { action; read; num_occurrences; extra_doc } = t input in
    { action
    ; read = (fun env -> Parsing_outcome.map (read env) ~f)
    ; num_occurrences
    ; extra_doc
    }
  ;;

  let write_option name key env arg =
    Env.update env key ~f:(function
      | None -> arg
      | Some _ -> die "flag %s passed more than once" name ())
  ;;

  let required_value ?default arg_type name num_occurrences =
    let key = Env.Key.create ~name [%sexp_of: _] in
    let read env =
      match Env.find env key with
      | Some v -> Parsing_outcome.return_with_arg v
      | None ->
        (match default with
         | Some v -> Parsing_outcome.return_no_arg v
         | None ->
           Parsing_outcome.error
             ~has_arg:false
             (`Missing_required_flags
               (Error.of_string (sprintf "missing required flag: %s" name))))
    in
    let write env arg = write_option name key env arg in
    arg_flag name arg_type read write num_occurrences
  ;;

  let required arg_type name = required_value arg_type name Num_occurrences.exactly_once

  let optional_with_default default arg_type name =
    required_value ~default arg_type name Num_occurrences.at_most_once
  ;;

  let optional arg_type name =
    let key = Env.Key.create ~name [%sexp_of: _] in
    let read env =
      match Env.find env key with
      | None -> Parsing_outcome.return_no_arg None
      | Some _ as value -> Parsing_outcome.return_with_arg value
    in
    let write env arg = write_option name key env arg in
    arg_flag name arg_type read write Num_occurrences.at_most_once
  ;;

  let no_arg_general ~is_required ~key_value ~deprecated_hook name =
    let key = Env.Key.create ~name [%sexp_of: unit] in
    let read env =
      match Env.mem env key with
      | true -> Parsing_outcome.return_with_arg true
      | false ->
        if is_required
        then
          Parsing_outcome.error
            ~has_arg:false
            (`Missing_required_flags
              (Error.of_string (sprintf "missing required flag: %s" name)))
        else Parsing_outcome.return_no_arg false
    in
    let write env =
      if Env.mem env key
      then die "flag %s passed more than once" name ()
      else Env.set env ~key ~data:()
    in
    let action env =
      let env =
        Option.fold key_value ~init:env ~f:(fun env (key, value) ->
          Env.set_with_default env ~key ~data:value)
      in
      write env
    in
    let action =
      match deprecated_hook with
      | None -> action
      | Some f ->
        fun env ->
          let env = action env in
          f ();
          env
    in
    { read
    ; action = No_arg action
    ; num_occurrences =
        (if is_required
         then Num_occurrences.exactly_once
         else Num_occurrences.at_most_once)
    ; extra_doc = Lazy.from_val None
    }
  ;;

  let no_arg name =
    no_arg_general name ~is_required:false ~key_value:None ~deprecated_hook:None
  ;;

  let no_arg_required v name =
    map_flag
      (no_arg_general ~is_required:true ~key_value:None ~deprecated_hook:None)
      ~f:(function
        | true -> v
        | false -> assert false)
      name
  ;;

  let no_arg_register ~key ~value name =
    no_arg_general
      name
      ~is_required:false
      ~key_value:(Some (key, value))
      ~deprecated_hook:None
  ;;

  let no_arg_some value =
    map_flag no_arg ~f:(function
      | true -> Some value
      | false -> None)
  ;;

  let listed arg_type name =
    let key = Env.With_default.Key.create ~default:[] ~name [%sexp_of: _ list] in
    let read env =
      match List.rev (Env.With_default.find env key) with
      | [] -> Parsing_outcome.return_no_arg []
      | _ :: _ as value_list -> Parsing_outcome.return_with_arg value_list
    in
    let write env arg = Env.With_default.change env key ~f:(fun list -> arg :: list) in
    arg_flag name arg_type read write Num_occurrences.any
  ;;

  let one_or_more_as_pair arg_type name =
    let key = Env.With_default.Key.create ~default:[] ~name [%sexp_of: _ list] in
    let read env =
      match List.rev (Env.With_default.find env key) with
      | first :: rest -> Parsing_outcome.return_with_arg (first, rest)
      | [] ->
        Parsing_outcome.error
          ~has_arg:false
          (`Missing_required_flags
            (Error.of_string (sprintf "missing required flag: %s" name)))
    in
    let write env arg = Env.With_default.change env key ~f:(fun q -> arg :: q) in
    arg_flag name arg_type read write Num_occurrences.at_least_once
  ;;

  let one_or_more_as_list arg_type =
    one_or_more_as_pair arg_type |> map_flag ~f:(fun (x, xs) -> x :: xs)
  ;;

  let escape_general ~complete ~deprecated_hook name =
    let key = Env.Key.create ~name [%sexp_of: string list] in
    let action env cmd_line = Env.set env ~key ~data:cmd_line in
    let read env =
      match Env.find env key with
      | None -> Parsing_outcome.return_no_arg None
      | Some _ as value -> Parsing_outcome.return_with_arg value
    in
    let action =
      match deprecated_hook with
      | None -> action
      | Some f ->
        fun env x ->
          f x;
          action env x
    in
    { action = Rest (action, complete)
    ; read
    ; num_occurrences = Num_occurrences.at_most_once
    ; extra_doc = Lazy.from_val None
    }
  ;;

  let no_arg_abort ~exit _name =
    { action = No_arg (fun _ -> Nothing.unreachable_code (exit ()))
    ; num_occurrences = Num_occurrences.at_most_once
    ; read =
        (fun _ ->
          (* We know that the flag wasn't passed here because if it was passed
              then the [action] would have called [exit]. *)
          Parsing_outcome.return_no_arg ())
    ; extra_doc = Lazy.from_val None
    }
  ;;

  let escape name = escape_general ~complete:None ~deprecated_hook:None name

  let escape_with_autocomplete ~complete name =
    escape_general ~complete:(Some complete) ~deprecated_hook:None name
  ;;

  module Deprecated = struct
    let no_arg ~hook name =
      no_arg_general ~is_required:false ~deprecated_hook:(Some hook) ~key_value:None name
    ;;

    let escape ~hook = escape_general ~complete:None ~deprecated_hook:(Some hook)
  end
end

module Path : sig
  type t

  val empty : t
  val create : path_to_exe:string -> t
  val of_parts : string list -> t
  val append : t -> subcommand:string -> t
  val replace_first : t -> from:string -> to_:string -> t
  val parts : t -> string list
  val parts_exe_basename : t -> string list
  val to_string : t -> string
  val to_string_dots : t -> string
  val pop_help : t -> t
  val length : t -> int
  val is_empty : t -> bool
end = struct
  type t = string list

  let empty = []
  let create ~path_to_exe = [ path_to_exe ]
  let of_parts parts = List.rev parts
  let append t ~subcommand = subcommand :: t
  let parts = List.rev

  let parts_exe_basename t =
    match List.rev t with
    | [] -> []
    | hd :: tl -> Filename_base.basename hd :: tl
  ;;

  let to_string t = unwords (parts_exe_basename t)
  let length = List.length

  let replace_first t ~from ~to_ =
    let rec aux parts ~acc ~from ~to_ =
      match parts with
      | [] -> acc
      | hd :: tl ->
        if String.( = ) hd from
        then List.rev_append tl (to_ :: acc)
        else aux tl ~acc:(hd :: acc) ~from ~to_
    in
    aux (parts t) ~acc:[] ~from ~to_
  ;;

  let pop_help = function
    | "help" :: t -> t
    | _ -> assert false
  ;;

  let to_string_dots t =
    (match t with
     | [] -> []
     | last :: init -> last :: List.map init ~f:(Fn.const "."))
    |> to_string
  ;;

  let is_empty = List.is_empty
end

module Anons = struct
  module Grammar : sig
    type t = Shape.Anons.Grammar.t

    val zero : t
    val one : string -> t
    val many : t -> t
    val maybe : t -> t
    val maybe_idempotent : t -> t
    val concat : t list -> t
    val ad_hoc : usage:string -> t

    include Invariant.S with type t := t

    val names : t -> string list
  end = struct
    type t = Shape.Anons.Grammar.t =
      | Zero
      | One of string
      | Many of t
      | Maybe of t
      | Concat of t list
      | Ad_hoc of string

    let invariant = Shape.Anons.Grammar.invariant
    let usage = Shape.Anons.Grammar.usage

    let rec is_fixed_arity = function
      | Zero -> true
      | One _ -> true
      | Many _ -> false
      | Maybe _ -> false
      | Ad_hoc _ -> false
      | Concat ts ->
        (match List.rev ts with
         | [] -> failwith "bug in command.ml"
         | last :: others ->
           assert (List.for_all others ~f:is_fixed_arity);
           is_fixed_arity last)
    ;;

    let rec names = function
      | Zero -> []
      | One s -> [ s ]
      | Many t -> names t
      | Maybe t -> names t
      | Ad_hoc s -> [ s ]
      | Concat ts -> List.concat_map ts ~f:names
    ;;

    let zero = Zero
    let one name = One name

    let many = function
      | Zero -> Zero (* strange, but not non-sense *)
      | t ->
        if not (is_fixed_arity t)
        then
          failwithf
            "iteration of variable-length grammars such as %s is disallowed"
            (usage t)
            ();
        Many t
    ;;

    let maybe = function
      | Zero -> Zero (* strange, but not non-sense *)
      | t -> Maybe t
    ;;

    let maybe_idempotent = function
      | Zero -> Zero (* strange, but not non-sense *)
      | Maybe t -> Maybe t
      | Many t -> Many t
      | t -> Maybe t
    ;;

    let concat = function
      | [] -> Zero
      | car :: cdr ->
        let car, cdr =
          List.fold cdr ~init:(car, []) ~f:(fun (t1, acc) t2 ->
            match t1, t2 with
            | Zero, t | t, Zero -> t, acc
            | _, _ ->
              if is_fixed_arity t1
              then t2, t1 :: acc
              else
                failwithf
                  "the grammar %s for anonymous arguments is not supported because there \
                   is the possibility for arguments (%s) following a variable number of \
                   arguments (%s).  Supporting such grammars would complicate the \
                   implementation significantly."
                  (usage (Concat (List.rev (t2 :: t1 :: acc))))
                  (usage t2)
                  (usage t1)
                  ())
        in
        (match cdr with
         | [] -> car
         | _ :: _ -> Concat (List.rev (car :: cdr)))
    ;;

    let ad_hoc ~usage = Ad_hoc usage
  end

  module Parser : sig
    module Basic : sig
      type +'a t

      module For_opening : sig
        val return : 'a -> 'a t
        val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
        val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
      end

      val from_env : (Env.t -> 'a) -> 'a t
    end

    type +'a t = 'a Parsing_outcome.t Basic.t

    val one : name:string -> 'a Arg_type.t -> 'a t
    val maybe : 'a t -> 'a option t
    val sequence : 'a t -> 'a list t
    val stop_parsing : 'a t -> 'a t
    val final_value : 'a Basic.t -> Env.t -> 'a

    module Consume_result : sig
      type nonrec 'a t =
        { (* If emacs highlights [parser] as if it were a keyword, that's only because
             [parser] was a keyword in camlp4. [parser] is a regular name in OCaml. *)
          parser : 'a Basic.t
        ; parse_flags : bool
        ; update_env : Env.t -> Env.t
        }
    end

    val consume : 'a Basic.t -> string -> for_completion:bool -> 'a Consume_result.t
    val complete : 'a Basic.t -> Env.t -> part:string -> Nothing.t

    module For_opening : sig
      val return : 'a -> 'a t
      val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
      val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    end
  end = struct
    module Basic = struct
      type 'a t =
        | Done of (Env.t -> 'a)
        | More of 'a more
        (* A [Test] will (generally) return a [Done _] value if there is no more input and
           a [More] parser to use if there is any more input. *)
        | Test of (more:bool -> 'a t)
        (* If we're only completing, we can't pull values out, but we can still step through
           [t]s (which may have completion set up). *)
        | Only_for_completion of packed list
        | Stop_parsing of 'a t

      and 'a more =
        { name : string
        ; parse : string -> for_completion:bool -> 'a parse_result
        ; complete : Completer.t
        }

      and packed = Packed : 'a t -> packed

      and 'a parse_result =
        { parser : 'a t
        ; update_env : Env.t -> Env.t
        }

      let parse_more { name; parse; complete } ~f =
        let parse arg ~for_completion =
          let { parser; update_env } = parse arg ~for_completion in
          { parser = f parser; update_env }
        in
        More { name; parse; complete }
      ;;

      let pack_for_completion = function
        | Done _ -> [] (* won't complete or consume anything *)
        | (More _ | Test _ | Stop_parsing _) as x -> [ Packed x ]
        | Only_for_completion ps -> ps
      ;;

      let rec ( <*> ) t_left t_right =
        match t_left, t_right with
        (* [Done] *)
        | Done f, Done x ->
          Done
            (fun env ->
              let f_outcome = f env in
              let x_outcome = x env in
              f_outcome x_outcome)
        (* next step [More] *)
        | More more, _ -> parse_more more ~f:(fun tl -> tl <*> t_right)
        | Done _, More more -> parse_more more ~f:(fun tr -> t_left <*> tr)
        (* next step [Only_for_completion] *)
        | Only_for_completion _, _ | Done _, Only_for_completion _ ->
          Only_for_completion (pack_for_completion t_left @ pack_for_completion t_right)
        (* next step [Stop_parsing] *)
        | Stop_parsing tl, tr | (Done _ as tl), Stop_parsing tr -> Stop_parsing (tl <*> tr)
        (* next step [Test] *)
        | Test test, _ -> Test (fun ~more -> test ~more <*> t_right)
        | Done _, Test test -> Test (fun ~more -> t_left <*> test ~more)
      ;;

      let return a = Done (fun _ -> a)
      let ( >>| ) t f = return f <*> t
      let from_env f = Done (fun env -> f env)

      module For_opening = struct
        let return = return
        let ( <*> ) = ( <*> )
        let ( >>| ) = ( >>| )
      end
    end

    open Basic

    type 'a t = 'a Parsing_outcome.t Basic.t

    let ( >>| ) t f = t >>| Parsing_outcome.map ~f
    let ( <*> ) t_left t_right = return Parsing_outcome.( <*> ) <*> t_left <*> t_right
    let return a = return (Parsing_outcome.return a)
    let return_with_arg a = Done (fun _ -> Parsing_outcome.return_with_arg a)
    let stop_parsing t = Stop_parsing t

    let one_more ~name arg_type =
      let parse anon ~for_completion =
        match Arg_type.parse arg_type anon with
        | Error error ->
          if for_completion
          then
            (* we don't *really* care about this value, so just put in a dummy value so
               completion can continue *)
            { parser = Only_for_completion []; update_env = Fn.id }
          else
            die "failed to parse %s value %S\n%s" name anon (Error.to_string_hum error) ()
        | Ok v ->
          { parser = return_with_arg v
          ; update_env =
              (fun env ->
                Option.fold (Arg_type.key arg_type) ~init:env ~f:(fun env key ->
                  Env.multi_add env ~key ~data:v))
          }
      in
      More { name; parse; complete = Arg_type.complete arg_type }
    ;;

    let one ~name arg_type =
      Test
        (fun ~more ->
          if more
          then one_more ~name arg_type
          else
            Done
              (fun _ ->
                Parsing_outcome.error
                  ~has_arg:false
                  (`Missing_required_flags
                    (Error.of_string (sprintf "missing anonymous argument: %s" name)))))
    ;;

    let maybe t =
      Test
        (fun ~more ->
          if more then return_with_arg (fun a -> Some a) <*> t else return None)
    ;;

    let sequence t =
      let rec loop =
        Test
          (fun ~more ->
            if more then return (fun v acc -> v :: acc) <*> t <*> loop else return [])
      in
      loop
    ;;

    let rec final_value t env =
      match t with
      | Done a -> a env
      | Stop_parsing t -> final_value t env
      | Test f -> final_value (f ~more:false) env
      | More _ ->
        (* this doesn't happen because all occurrences of [More] are protected
           by [Test], which means there will always be an extra argument to give
           before requesting the final value *)
        assert false
      | Only_for_completion _ ->
        failwith "BUG: asked for final value when doing completion"
    ;;

    module Consume_result = struct
      type nonrec 'a t =
        { parser : 'a Basic.t
        ; parse_flags : bool
        ; update_env : Env.t -> Env.t
        }
    end

    let rec consume
      : type a. a Basic.t -> string -> for_completion:bool -> a Consume_result.t
      =
      fun t arg ~for_completion ->
      match t with
      | Done _ -> die "too many anonymous arguments" ()
      | Test f -> consume (f ~more:true) arg ~for_completion
      | More { parse; _ } ->
        let { parser; update_env } = parse arg ~for_completion in
        { parser; parse_flags = true; update_env }
      | Stop_parsing t -> { (consume t arg ~for_completion) with parse_flags = false }
      | Only_for_completion packed ->
        (match packed with
         | [] ->
           { parser = Only_for_completion []; parse_flags = true; update_env = Fn.id }
         | Packed t :: rest ->
           let ({ update_env; parse_flags; parser } : _ Consume_result.t) =
             consume t arg ~for_completion
           in
           { update_env
           ; parse_flags
           ; parser = Only_for_completion (pack_for_completion parser @ rest)
           })
    ;;

    let rec complete : type a. a Basic.t -> Env.t -> part:string -> Nothing.t =
      fun t env ~part ->
      match t with
      | Done _ -> exit 0
      | Test f -> complete (f ~more:true) env ~part
      | More { complete; _ } -> Completer.run_and_exit complete env ~part
      | Stop_parsing t -> complete t env ~part
      | Only_for_completion t ->
        (match t with
         | [] -> exit 0
         | Packed t :: _ -> complete t env ~part)
    ;;

    module For_opening = struct
      let return = return
      let ( <*> ) = ( <*> )
      let ( >>| ) = ( >>| )
    end
  end

  open Parser.For_opening

  type 'a t =
    { p : 'a Parser.t
    ; grammar : Grammar.t
    }

  let t2 t1 t2 =
    { p = return (fun a1 a2 -> a1, a2) <*> t1.p <*> t2.p
    ; grammar = Grammar.concat [ t1.grammar; t2.grammar ]
    }
  ;;

  let t3 t1 t2 t3 =
    { p = return (fun a1 a2 a3 -> a1, a2, a3) <*> t1.p <*> t2.p <*> t3.p
    ; grammar = Grammar.concat [ t1.grammar; t2.grammar; t3.grammar ]
    }
  ;;

  let t4 t1 t2 t3 t4 =
    { p = return (fun a1 a2 a3 a4 -> a1, a2, a3, a4) <*> t1.p <*> t2.p <*> t3.p <*> t4.p
    ; grammar = Grammar.concat [ t1.grammar; t2.grammar; t3.grammar; t4.grammar ]
    }
  ;;

  let normalize str =
    (* Verify the string is not empty or surrounded by whitespace *)
    let strlen = String.length str in
    if strlen = 0 then failwith "Empty anonymous argument name provided";
    if String.( <> ) (String.strip str) str
    then failwithf "argument name %S has surrounding whitespace" str ();
    (* If the string contains special surrounding characters, don't do anything *)
    let has_special_chars =
      let special_chars =
        Set.of_list (module Char) [ '<'; '>'; '['; ']'; '('; ')'; '{'; '}' ]
      in
      String.exists str ~f:(Set.mem special_chars)
    in
    if has_special_chars then str else String.uppercase str
  ;;

  let ( %: ) name arg_type =
    let name = normalize name in
    { p = Parser.one ~name arg_type; grammar = Grammar.one name }
  ;;

  let map_anons t ~f = { p = t.p >>| f; grammar = t.grammar }
  let maybe t = { p = Parser.maybe t.p; grammar = Grammar.maybe t.grammar }

  let maybe_with_default default t =
    let t = maybe t in
    { t with p = (t.p >>| fun v -> Option.value ~default v) }
  ;;

  let sequence t = { p = Parser.sequence t.p; grammar = Grammar.many t.grammar }
  let non_empty_sequence_as_pair t = t2 t (sequence t)

  let non_empty_sequence_as_list t =
    let t = non_empty_sequence_as_pair t in
    { t with p = (t.p >>| fun (x, xs) -> x :: xs) }
  ;;

  let escape t = { p = Parser.stop_parsing t.p; grammar = t.grammar }

  module Deprecated = struct
    let ad_hoc ~usage_arg =
      { p =
          Parser.sequence
            (Parser.one ~name:"WILL NEVER BE PRINTED" Arg_type.Export.string)
      ; grammar = Grammar.ad_hoc ~usage:usage_arg
      }
    ;;
  end
end

module Cmdline = struct
  type t =
    | Nil
    | Cons of string * t
    | Complete of string
  [@@deriving compare]

  let of_list args = List.fold_right args ~init:Nil ~f:(fun arg args -> Cons (arg, args))

  let rec to_list = function
    | Nil -> []
    | Cons (x, xs) -> x :: to_list xs
    | Complete x -> [ x ]
  ;;

  let rec ends_in_complete = function
    | Complete _ -> true
    | Nil -> false
    | Cons (_, args) -> ends_in_complete args
  ;;

  let extend t ~extend ~path =
    if ends_in_complete t
    then t
    else (
      let path_list = Option.value ~default:[] (List.tl (Path.parts path)) in
      of_list (to_list t @ extend path_list))
  ;;
end

module Key_type = Shape.Private.Key_type

let assert_no_underscores key_type flag_or_subcommand =
  if String.exists flag_or_subcommand ~f:(fun c -> Char.( = ) c '_')
  then
    failwithf
      "%s %s contains an underscore. Use a dash instead."
      (Key_type.to_string key_type)
      flag_or_subcommand
      ()
;;

let normalize key_type key =
  assert_no_underscores key_type key;
  match key_type with
  | Key_type.Flag ->
    if String.equal key "-" then failwithf !"invalid %{Key_type} name: %S" key_type key ();
    if String.exists key ~f:Char.is_whitespace
    then failwithf !"invalid %{Key_type} name (contains whitespace): %S" key_type key ();
    if String.is_prefix ~prefix:"-" key then key else "-" ^ key
  | Key_type.Subcommand -> String.lowercase key
;;

let lookup_expand = Shape.Private.lookup_expand

let lookup_expand_with_aliases map prefix key_type =
  let alist =
    List.concat_map (Map.data map) ~f:(fun flag ->
      let { Flag.Internal.name
          ; aliases
          ; aliases_excluded_from_help
          ; action = _
          ; doc = _
          ; num_occurrences = _
          ; check_available = _
          ; name_matching
          }
        =
        flag
      in
      let data = flag, name_matching in
      let aliases = aliases_excluded_from_help @ aliases in
      (name, data) :: List.map aliases ~f:(fun alias -> alias, data))
  in
  match List.find_a_dup alist ~compare:(fun (s1, _) (s2, _) -> String.compare s1 s2) with
  | None -> lookup_expand alist prefix key_type
  | Some (flag, _) -> failwithf "multiple flags named %s" flag ()
;;

module Command_base = struct
  type t =
    { summary : string
    ; readme : (unit -> string) option
    ; flags : Flag.Internal.t Map.M(String).t
    ; anons : unit -> ([ `Parse_args ] -> [ `Run_main ] -> unit) Anons.Parser.Basic.t
    ; usage : Anons.Grammar.t
    }

  module Deprecated = struct
    let subcommand_cmp_fst (a, _) (c, _) = help_screen_compare a c

    let flags_help ?(display_help_flags = true) t =
      let flags = Map.data t.flags in
      let flags =
        if display_help_flags
        then flags
        else List.filter flags ~f:(fun f -> String.( <> ) f.name "-help")
      in
      List.concat_map ~f:Flag.Internal.Deprecated.help flags
    ;;
  end

  let formatted_flags t =
    Map.data t.flags
    |> List.map ~f:Flag.Internal.align
    (* this sort puts optional flags after required ones *)
    |> List.sort ~compare:(fun a b -> String.compare a.name b.name)
    |> Shape.Flag_help_display.sort
  ;;

  let shape t : Shape.Base_info.t =
    { summary = t.summary
    ; readme = Option.map t.readme ~f:(fun readme -> readme ())
    ; anons = Grammar t.usage
    ; flags = formatted_flags t
    }
  ;;

  let path_key = Env.key_create "path"
  let args_key = Env.key_create "args"
  let help_key = Env.key_create "help"
  let normalized_path = ref None
  let normalized_args = ref None

  let indent_by_2 str =
    String.split ~on:'\n' str
    |> List.map ~f:(fun line -> "  " ^ line)
    |> String.concat ~sep:"\n"
  ;;

  let get_flag_and_action t arg =
    match lookup_expand_with_aliases t.flags arg Flag with
    | Error msg -> die "%s" msg ()
    | Ok (flag_name, flag) -> flag_name, flag.action
  ;;

  let get_complete_flag_name t arg (args : Cmdline.t) =
    let flag, action = get_flag_and_action t arg in
    match action with
    | Print_info_and_quit _info -> [ flag ]
    | No_arg _f -> [ flag ]
    | Arg (_f, _comp) ->
      (match args with
       | Cons (arg, _rest) -> [ flag; arg ]
       | Nil | Complete _ -> [])
    | Rest (_f, _comp) -> flag :: Cmdline.to_list args
  ;;

  let run_flag t env arg (args : Cmdline.t) =
    let flag, action = get_flag_and_action t arg in
    match action with
    | Print_info_and_quit info ->
      let completing = Cmdline.ends_in_complete args in
      (* If we're doing completion, version/help info aren't useful completion
         responses. *)
      if completing
      then env, args
      else (
        print_endline (info env);
        exit 0)
    | No_arg f -> f env, args
    | Arg (f, comp) ->
      (match args with
       | Nil -> die "missing argument for flag %s" flag ()
       | Cons (arg, rest) ->
         let env =
           try f env arg with
           | Failed_to_parse_command_line _ as e ->
             if Cmdline.ends_in_complete rest then env else raise e
         in
         env, rest
       | Complete part -> Nothing.unreachable_code (Completer.run_and_exit comp env ~part))
    | Rest (f, comp) ->
      let arg_list = Cmdline.to_list args in
      if Cmdline.ends_in_complete args
      then Nothing.unreachable_code (Completer.run_and_exit comp env ~part:arg_list);
      f env arg_list, Nil
  ;;

  let rec run_cmdline
    t
    env
    parser
    (cmdline : Cmdline.t)
    ~for_completion
    ~parse_flags
    ~normalized_args
    =
    match cmdline with
    | Nil ->
      List.iter (Map.data t.flags) ~f:(fun flag -> flag.check_available env);
      ( `Only_validate_parsing (Env.mem env key_internal_validate_parsing)
      , Anons.Parser.final_value parser env
      , List.concat (List.rev normalized_args) )
    | Complete part ->
      if parse_flags && String.is_prefix part ~prefix:"-"
      then (
        List.iter (Map.keys t.flags) ~f:(fun name ->
          if String.is_prefix name ~prefix:part then print_endline name);
        exit 0)
      else Nothing.unreachable_code (Anons.Parser.complete parser env ~part)
    | Cons (arg, args) ->
      let arg, args, arg_is_flag =
        match parse_flags with
        | false -> arg, args, false
        | true ->
          (match arg, args with
           (* the '-anon' flag is here as an escape hatch in case you have an
              anonymous argument that starts with a hyphen. *)
           | "-anon", Cons (arg, args) -> arg, args, false
           (* support the common Unix convention where "-" means stdin *)
           | "-", _ -> arg, args, false
           | _, _ -> arg, args, String.is_prefix arg ~prefix:"-")
      in
      (match arg_is_flag with
       | true ->
         let normalized_args = get_complete_flag_name t arg args :: normalized_args in
         let env, args = run_flag t env arg args in
         run_cmdline ~normalized_args t env parser args ~parse_flags ~for_completion
       | false ->
         let parse_flags1 = parse_flags in
         let ({ parser; parse_flags = parse_flags2; update_env }
               : _ Anons.Parser.Consume_result.t)
           =
           Anons.Parser.consume parser arg ~for_completion
         in
         let env = update_env env in
         let parse_flags = parse_flags1 && parse_flags2 in
         run_cmdline
           ~normalized_args:([ arg ] :: normalized_args)
           t
           env
           parser
           ~parse_flags
           args
           ~for_completion)
  ;;

  let run_exn exn ~for_completion ~path ~verbose_on_parse_error =
    match exn with
    | Failed_to_parse_command_line _ when for_completion -> exit 0
    | Exit_called { status } -> exit status
    | _ ->
      let exn_str =
        match exn with
        | Failed_to_parse_command_line msg -> msg
        | _ -> Sexp.to_string_hum [%sexp (exn : exn)]
      in
      let verbose = Option.value verbose_on_parse_error ~default:true in
      let error_msg =
        if verbose
        then
          String.concat
            ~sep:"\n\n"
            [ "Error parsing command line:"
            ; indent_by_2 exn_str
            ; "For usage information, run"
            ; "  " ^ Path.to_string path ^ " -help\n"
            ]
        else exn_str
      in
      prerr_endline error_msg;
      exit 1
  ;;

  let run
    t
    env
    ~when_parsing_succeeds
    ~path
    ~args
    ~verbose_on_parse_error
    ~help_text
    ~on_failure
    =
    let for_completion = Cmdline.ends_in_complete args in
    let env =
      env
      |> Env.set ~key:path_key ~data:path
      |> Env.set ~key:args_key ~data:(Cmdline.to_list args)
      |> Env.set ~key:help_key ~data:help_text
    in
    match
      Result.try_with (fun () ->
        let is_using_validate_parsing, main, parsed_normalized_args =
          run_cmdline
            t
            env
            (t.anons ())
            ~for_completion
            ~parse_flags:true
            ~normalized_args:[]
            args
        in
        normalized_path := Some path;
        normalized_args := Some parsed_normalized_args;
        is_using_validate_parsing, main `Parse_args)
    with
    | Ok (`Only_validate_parsing true, (_thunk : _)) ->
      when_parsing_succeeds ();
      exit 0
    | Ok (`Only_validate_parsing false, thunk) ->
      when_parsing_succeeds ();
      thunk `Run_main
    | Error exn -> on_failure exn ~for_completion ~path ~verbose_on_parse_error
  ;;

  module Param = struct
    type +'a t =
      { f : unit -> (unit -> 'a Parsing_outcome.t) Anons.Parser.Basic.t
      ; usage : unit -> Anons.Grammar.t
      ; flags : unit -> Flag.Internal.t list
      }

    open Anons.Parser.Basic.For_opening

    let wrap_value v () = Parsing_outcome.return_no_arg v

    let apply f x =
      { f =
          (fun () ->
            return (fun f x () ->
              (* order of evaluation here affects in what order the users' callbacks
                  are evaluated, so it's important to call [f] before [x] *)
              let f_outcome = f () in
              let x_outcome = x () in
              Parsing_outcome.apply f_outcome x_outcome)
            <*> f.f ()
            <*> x.f ())
      ; flags = (fun () -> x.flags () @ f.flags ())
      ; usage = (fun () -> Anons.Grammar.concat [ f.usage (); x.usage () ])
      }
    ;;

    let empty_spec : 'm. ('m -> 'm) t =
      { f = (fun () -> return (fun () -> Parsing_outcome.return_no_arg Fn.id))
      ; flags = (fun () -> [])
      ; usage = (fun () -> Anons.Grammar.zero)
      }
    ;;

    let map_outcome x ~f =
      { f =
          (fun () ->
            x.f ()
            >>| fun x () ->
            let x_outcome = x () in
            f x_outcome)
      ; flags = x.flags
      ; usage = x.usage
      }
    ;;

    let map x ~f = map_outcome x ~f:(Parsing_outcome.map ~f)

    let lookup key =
      { f =
          (fun () ->
            Anons.Parser.Basic.from_env (fun env -> Env.find_exn env key) >>| wrap_value)
      ; flags = (fun () -> [])
      ; usage = (fun () -> Anons.Grammar.zero)
      }
    ;;

    let path : Path.t t = lookup path_key
    let args : string list t = lookup args_key
    let help : string Lazy.t t = lookup help_key

    (* This is only used internally, for the help command. *)
    let env =
      { f = (fun () -> Anons.Parser.Basic.from_env (fun env -> env) >>| wrap_value)
      ; flags = (fun () -> [])
      ; usage = (fun () -> Anons.Grammar.zero)
      }
    ;;

    include struct
      module Arg_type = Arg_type
      include Arg_type.Export
    end

    include struct
      open Anons

      let ( %: ) = ( %: )
      let map_anons = map_anons
      let maybe = maybe
      let maybe_with_default = maybe_with_default
      let sequence = sequence
      let non_empty_sequence_as_pair = non_empty_sequence_as_pair
      let non_empty_sequence_as_list = non_empty_sequence_as_list
      let t2 = t2
      let t3 = t3
      let t4 = t4

      let anon spec =
        Anons.Grammar.invariant spec.grammar;
        { f = (fun () -> spec.p >>| fun outcome () -> outcome)
        ; flags = (fun () -> [])
        ; usage = (fun () -> spec.grammar)
        }
      ;;
    end

    let escape_anon ~final_anon =
      Anons.escape (t2 final_anon (sequence ("ARG" %: string))) |> anon
    ;;

    include struct
      open Flag

      let map_flag = map_flag
      let escape = escape
      let escape_with_autocomplete = escape_with_autocomplete
      let listed = listed
      let one_or_more_as_pair = one_or_more_as_pair
      let one_or_more_as_list = one_or_more_as_list
      let no_arg = no_arg
      let no_arg_required = no_arg_required
      let no_arg_register = no_arg_register
      let no_arg_abort = no_arg_abort
      let no_arg_some = no_arg_some
      let optional = optional
      let optional_with_default = optional_with_default
      let required = required

      let flag_internal
        ?(aliases = [])
        ?full_flag_required
        name
        mode
        ~doc
        ~aliases_excluded_from_help
        =
        let normalize flag = normalize Key_type.Flag flag in
        let name = normalize name in
        let aliases = List.map ~f:normalize aliases in
        let { read; action; num_occurrences; extra_doc } = mode name in
        let check_available =
          match num_occurrences.at_least_once with
          | false -> (ignore : Univ_map.t -> unit)
          | true -> fun env -> ignore (read env : _)
        in
        let name_matching =
          if Option.is_some full_flag_required then `Full_match_required else `Prefix
        in
        { f =
            (fun () ->
              Anons.Parser.Basic.from_env (fun env -> read env) >>| fun v () -> v)
        ; flags =
            (fun () ->
              [ { name
                ; aliases
                ; aliases_excluded_from_help
                ; doc =
                    (match force extra_doc with
                     | Some extra_doc -> [%string "%{doc} %{extra_doc}"]
                     | None -> doc)
                ; action
                ; num_occurrences
                ; check_available
                ; name_matching
                }
              ])
        ; usage = (fun () -> Anons.Grammar.zero)
        }
      ;;

      let flag = flag_internal ~aliases_excluded_from_help:[]

      let flag_optional_with_default_doc
        ?aliases
        ?full_flag_required
        name
        arg_type
        sexp_of_default
        ~default
        ~doc
        =
        let doc =
          match sexp_of_default default with
          | Sexp.Atom "_" -> doc
          | default_sexp -> sprintf !"%s (default: %{Sexp})" doc default_sexp
        in
        flag
          ?aliases
          ?full_flag_required
          name
          (optional_with_default default arg_type)
          ~doc
      ;;
    end

    let return v =
      { f = (fun () -> return (fun () -> Parsing_outcome.return_no_arg v))
      ; flags = (fun () -> [])
      ; usage = (fun () -> Anons.Grammar.zero)
      }
    ;;

    let recover_from_missing_required_flags t =
      { t with
        f =
          (fun () ->
            t.f ()
            >>| fun f () ->
            let outcome = f () in
            Parsing_outcome.recover_from_missing_required_flags outcome)
      }
    ;;

    let introduce_missing_required_flags t =
      { t with
        f =
          (fun () ->
            t.f ()
            >>| fun f () ->
            let outcome = f () in
            Parsing_outcome.introduce_missing_required_flags outcome)
      }
    ;;

    let optional_to_required t =
      { t with
        f =
          (fun () ->
            t.f ()
            >>| fun f () ->
            let outcome = f () in
            Parsing_outcome.introduce_missing_required_flags
              (Parsing_outcome.map outcome ~f:(function
                | None ->
                  Error
                    (`Missing_required_flags
                      (Error.of_string "[optional_to_required] got a [None] result"))
                | Some v -> Ok v)))
      }
    ;;

    include Applicative.Make (struct
      type nonrec 'a t = 'a t

      let return = return
      let apply = apply
      let map = `Custom map
    end)

    let arg_names t =
      let flags = Flag.Internal.create (t.flags ()) in
      let flag_names = Map.keys flags in
      let anon_names = Anons.Grammar.names (t.usage ()) in
      List.concat [ flag_names; anon_names ]
    ;;

    let required_arg_names t =
      let flags = Flag.Internal.create (t.flags ()) in
      List.filter_map (Map.to_alist flags) ~f:(fun (name, flag) ->
        if flag.num_occurrences.at_least_once then Some name else None)
    ;;

    module Choose_one = struct
      type 'a param = 'a t

      module Choice_name : sig
        type t [@@deriving compare, sexp_of]

        include Comparator.S with type t := t

        val to_string : t -> string
        val list_to_string : t list -> string
        val create_exn : 'a param -> t
        val enumerate_required_flags : t -> except:string -> string option
      end = struct
        module T = struct
          type t =
            { all_args : string list
            ; required_args : string list
            }
          [@@deriving compare]

          let sexp_of_t t = [%sexp (t.all_args : string list)]
        end

        include T
        include Comparator.Make (T)

        let create_exn param =
          let required_args = required_arg_names param in
          let names = arg_names param in
          let names_with_commas = List.filter names ~f:(fun s -> String.contains s ',') in
          if not (List.is_empty names_with_commas)
          then
            Error.create
              ~here:[%here]
              "For simplicity, [Command.Spec.choose_one] does not support names with \
               commas."
              names_with_commas
              [%sexp_of: string list]
            |> Error.raise;
          match names with
          | [] ->
            raise_s
              [%message "[choose_one] expects choices to read command-line arguments."]
          | _ :: _ -> { all_args = names; required_args }
        ;;

        let to_string t =
          match t.required_args with
          | [] -> String.concat ~sep:"," t.all_args
          | _ :: _ -> String.concat ~sep:"," t.required_args
        ;;

        let enumerate_required_flags t ~except =
          match List.filter t.required_args ~f:(fun x -> not (String.equal except x)) with
          | [] -> None
          | _ :: _ as l -> Some (String.concat ~sep:"," l)
        ;;

        let list_to_string ts = List.map ts ~f:to_string |> String.concat ~sep:"\n  "
      end

      module If_nothing_chosen = struct
        type (_, _) t =
          | Default_to : 'a -> ('a, 'a) t
          | Raise : ('a, 'a) t
          | Return_none : ('a, 'a option) t
      end

      let choose_one_non_optional
        (type a b)
        ?(new_behavior = true)
        (ts : a param list)
        ~(if_nothing_chosen : (a, b) If_nothing_chosen.t)
        =
        let fix_flag t =
          if new_behavior
          then (
            let name_of_the_group = Choice_name.create_exn t in
            let fix_num_occurrences flag =
              { flag with
                Flag.Internal.num_occurrences =
                  { flag.Flag.Internal.num_occurrences with at_least_once = false }
              }
            and fix_doc flag =
              { flag with
                Flag.Internal.doc =
                  sprintf
                    "%s%s"
                    flag.Flag.Internal.doc
                    (match
                       Choice_name.enumerate_required_flags
                         ~except:flag.name
                         name_of_the_group
                     with
                     | None -> ""
                     | Some group -> sprintf " [requires: \"%s\"]" group)
              }
            and make_anons_optional (anon : Anons.Grammar.t) =
              Anons.Grammar.maybe_idempotent anon
            in
            { t with
              usage = (fun () -> make_anons_optional (t.usage ()))
            ; flags =
                (fun () ->
                  List.map (t.flags ()) ~f:(fun flag_internal ->
                    flag_internal |> fix_num_occurrences |> fix_doc))
            })
          else t
        in
        match
          List.map ts ~f:(fun t -> Choice_name.create_exn t, fix_flag t)
          |> Map.of_alist (module Choice_name)
        with
        | `Duplicate_key name ->
          Error.create
            ~here:[%here]
            "[Command.Spec.choose_one] called with duplicate name"
            name
            [%sexp_of: Choice_name.t]
          |> Error.raise
        | `Ok ts ->
          Map.fold ts ~init:(return []) ~f:(fun ~key:name ~data:t acc ->
            map2
              acc
              (recover_from_missing_required_flags t)
              ~f:(fun acc { result = value; has_arg } ->
              match has_arg with
              | false -> acc
              | true -> (name, value) :: acc))
          |> map ~f:(fun value_list ->
               let arg_counter = List.length value_list in
               let missing_flag_error fmt =
                 ksprintf
                   (fun msg () -> Error (`Missing_required_flags (Error.of_string msg)))
                   fmt
               in
               let more_than_one_error passed =
                 die
                   !"Cannot pass more than one of these: \n\
                    \  %{Choice_name.list_to_string}"
                   (List.map passed ~f:fst)
                   ()
               and success_list, error_list =
                 List.partition_map value_list ~f:(function
                   | name, Ok value -> First (name, value)
                   | name, Error err -> Second (name, err))
               in
               match success_list with
               | _ :: _ :: _ as passed -> more_than_one_error passed
               | [ (_, (value : a)) ] ->
                 if arg_counter > 1
                 then more_than_one_error value_list
                 else
                   Ok
                     (match if_nothing_chosen with
                      | Default_to (_ : a) -> (value : b)
                      | Raise -> (value : b)
                      | Return_none -> (Some value : b))
               | [] ->
                 (match error_list with
                  | [ (name, `Missing_required_flags err) ] ->
                    Error
                      (`Missing_required_flags
                        (Error.of_string
                           (sprintf
                              "Not all flags in group \"%s\" are given: %s"
                              (Choice_name.to_string name)
                              (Error.to_string_hum err))))
                  | _ ->
                    (match if_nothing_chosen with
                     | Default_to value -> Ok value
                     | Return_none -> Ok None
                     | Raise ->
                       missing_flag_error
                         !"Must pass one of these:\n  %{Choice_name.list_to_string}"
                         (Map.keys ts)
                         ())))
          |> introduce_missing_required_flags
      ;;

      let choose_one
        (type a b)
        (ts : a option param list)
        ~(if_nothing_chosen : (a, b) If_nothing_chosen.t)
        =
        choose_one_non_optional
          ~new_behavior:false
          ~if_nothing_chosen
          (List.map ts ~f:(fun t ->
             map_outcome t ~f:(fun { Parsing_outcome.result; has_arg } ->
               match result with
               | Ok (Some value) -> { Parsing_outcome.result = Ok value; has_arg = true }
               | Ok None ->
                 { has_arg = false
                 ; result =
                     Error
                       (`Missing_required_flags (Error.of_string "missing required flag"))
                 }
               | Error _ as result -> { has_arg; result })))
      ;;
    end

    module If_nothing_chosen = Choose_one.If_nothing_chosen

    let choose_one = Choose_one.choose_one

    let choose_one_non_optional lst ~if_nothing_chosen =
      Choose_one.choose_one_non_optional lst ~if_nothing_chosen
    ;;

    let and_arg_names t = map t ~f:(fun value -> value, arg_names t)

    let and_arg_name t =
      match arg_names t with
      | [ name ] -> map t ~f:(fun value -> value, name)
      | names ->
        raise_s
          [%message
            "[and_arg_name] expects exactly one name, got" ~_:(names : string list)]
    ;;

    let parse { flags; usage = _; f } args =
      let cmdline = Cmdline.of_list args in
      let result = ref None in
      run
        { summary = ""
        ; readme = None
        ; flags = flags () |> Flag.Internal.create
        ; anons =
            (fun () ->
              let open Anons.Parser.Basic.For_opening in
              f ()
              >>| fun params `Parse_args `Run_main ->
              let outcome = params () in
              match outcome.result with
              | Error (`Missing_required_flags err) -> result := Some (Error err)
              | Ok x -> result := Some (Ok x))
        ; usage = Anons.Grammar.zero
        }
        Univ_map.empty
        ~when_parsing_succeeds:Fn.id
        ~args:cmdline
        ~path:Path.empty
        ~verbose_on_parse_error:(Some true)
        ~help_text:(lazy "No help for parsing")
        ~on_failure:
          (fun
            exn
            ~for_completion:(_ : bool)
            ~path:(_ : Path.t)
            ~verbose_on_parse_error:(_ : bool option)
            -> result := Some (Error (Error.of_exn exn)));
      Option.value_exn ~here:[%here] !result
    ;;
  end

  module Spec = struct
    type ('a, 'b) t = ('a -> 'b) Param.t
    type 'a param = 'a Param.t

    let apply = Param.apply
    let ( ++ ) t1 t2 = Param.map2 t1 t2 ~f:(fun f1 f2 x -> f2 (f1 x))
    let ( +> ) t1 p2 = Param.map2 t1 p2 ~f:(fun f1 p2 x -> (f1 x) p2)
    let ( +< ) t1 p2 = Param.map2 p2 t1 ~f:(fun p2 f1 x -> f1 (x p2))
    let step f = Param.return f

    (* Ideally this would be [let empty = Param.return Fn.id], but unfortunately that
       doesn't compile because of the value restriction *)
    let empty = Param.empty_spec
    let const x = Param.return x
    let map = Param.map
    let wrap f t = Param.map t ~f:(fun run main -> f ~run ~main)
    let of_param p = map p ~f:(fun f k -> k f)
    let to_param t m = map t ~f:(fun f -> f m)
    let path : Path.t param = Param.path
    let args : string list param = Param.args
    let help : string Lazy.t param = Param.help

    include struct
      module Arg_type = Arg_type
      include Arg_type.Export
    end

    include struct
      open Anons

      type 'a anons = 'a t

      let ( %: ) = ( %: )
      let map_anons = map_anons
      let maybe = maybe
      let maybe_with_default = maybe_with_default
      let non_empty_sequence_as_list = non_empty_sequence_as_list
      let non_empty_sequence_as_pair = non_empty_sequence_as_pair
      let sequence = sequence
      let t2 = t2
      let t3 = t3
      let t4 = t4
      let anon = Param.anon
    end

    let escape_anon = Param.escape_anon

    include struct
      open Flag

      type 'a flag = 'a t

      let map_flag = map_flag
      let escape = escape
      let escape_with_autocomplete = escape_with_autocomplete
      let listed = listed
      let one_or_more_as_pair = one_or_more_as_pair
      let one_or_more_as_list = one_or_more_as_list
      let no_arg = no_arg
      let no_arg_required = no_arg_required
      let no_arg_register = no_arg_register
      let no_arg_abort = no_arg_abort
      let no_arg_some = no_arg_some
      let optional = optional
      let optional_with_default = optional_with_default
      let required = required
      let flag = Param.flag
      let flag_optional_with_default_doc = Param.flag_optional_with_default_doc

      include Applicative.Make (struct
        type nonrec 'a t = 'a Param.t

        let return = Param.return
        let apply = apply
        let map = `Custom map
      end)

      let pair = Param.both
    end

    let flags_of_args_exn args =
      List.fold args ~init:empty ~f:(fun acc (name, spec, doc) ->
        let gen f flag_type =
          step (fun m x ->
            f x;
            m)
          +> Param.flag name flag_type ~doc
        in
        let call f arg_type = gen (fun x -> Option.iter x ~f) (Param.optional arg_type) in
        let set r arg_type = call (fun x -> r := x) arg_type in
        let set_bool r b = gen (fun passed -> if passed then r := b) Param.no_arg in
        acc
        ++
        match (spec : Stdlib.Arg.spec) with
        | Unit f -> gen (fun passed -> if passed then f ()) Param.no_arg
        | Set r -> set_bool r true
        | Clear r -> set_bool r false
        | String f -> call f string
        | Set_string r -> set r string
        | Int f -> call f int
        | Set_int r -> set r int
        | Float f -> call f float
        | Set_float r -> set r float
        | Bool f -> call f bool
        | Symbol (syms, f) ->
          let arg_type =
            Arg_type.of_alist_exn
              ~list_values_in_help:false
              (List.map syms ~f:(fun sym -> sym, sym))
          in
          call f arg_type
        | Rest f -> gen (fun x -> Option.iter x ~f:(List.iter ~f)) Param.escape
        | Tuple _ ->
          failwith "Arg.Tuple is not supported by Command.Spec.flags_of_args_exn"
        | ((Expand _) [@if ocaml_version >= (4, 05, 0)]) ->
          failwith "Arg.Expand is not supported by Command.Spec.flags_of_args_exn"
        | ((Rest_all _) [@if ocaml_version >= (4, 12, 0)]) ->
          failwith "Arg.Rest_all is not supported by Command.Spec.flags_of_args_exn")
    ;;

    module Deprecated = struct
      include Flag.Deprecated
      include Anons.Deprecated
    end

    let arg_names = Param.arg_names

    module If_nothing_chosen = Param.Choose_one.If_nothing_chosen

    let choose_one = Param.choose_one
    let choose_one_non_optional = Param.choose_one_non_optional
    let and_arg_names = Param.and_arg_names
    let and_arg_name = Param.and_arg_name
  end
end

module Group = struct
  type 'a t =
    { summary : string
    ; readme : (unit -> string) option
    ; subcommands : (string * 'a) list Lazy.t
    ; body : (path:string list -> unit) option
    }

  let shape ~subcommand_to_shape t : _ Shape.Group_info.t =
    { summary = t.summary
    ; readme = Option.map ~f:(fun readme -> readme ()) t.readme
    ; subcommands = Lazy.map t.subcommands ~f:(List.Assoc.map ~f:subcommand_to_shape)
    }
  ;;
end

let abs_path = Shape.Private.abs_path
let comp_cword = Env_var.COMP_CWORD

module Exec = struct
  type t =
    { summary : string
    ; readme : (unit -> string) option
    ; (* If [path_to_exe] is relative, interpret w.r.t. [working_dir] *)
      working_dir : string
    ; path_to_exe : string
    ; child_subcommand : string list
    ; env : env option
    }

  let shape t : Shape.Exec_info.t =
    { summary = t.summary
    ; readme = Option.map ~f:(fun readme -> readme ()) t.readme
    ; working_dir = t.working_dir
    ; path_to_exe = t.path_to_exe
    ; child_subcommand = t.child_subcommand
    }
  ;;
end

(* A proxy command is the structure of an Exec command obtained by running it in a
   special way *)
module Proxy = struct
  module Kind = struct
    type 'a t =
      | Base of Shape.Base_info.t
      | Group of 'a Shape.Group_info.t
      | Exec of Shape.Exec_info.t
      | Lazy of 'a t Lazy.t
  end

  type t =
    { working_dir : string
    ; path_to_exe : string
    ; path_to_subcommand : string list
    ; child_subcommand : string list
    ; kind : t Kind.t
    }
end

type t =
  | Base of Command_base.t
  | Group of t Group.t
  | Exec of Exec.t
  | Lazy of t Lazy.t

let rec sexpable_shape : t -> Shape.Sexpable.t = function
  | Base base -> Base (Command_base.shape base)
  | Exec exec -> Exec (Exec.shape exec)
  | Group group -> Group (Group.shape ~subcommand_to_shape:sexpable_shape group)
  | Lazy thunk -> Lazy (Lazy.map ~f:sexpable_shape thunk)
;;

type ('main, 'result) basic_spec_command =
  summary:string
  -> ?readme:(unit -> string)
  -> ('main, unit -> 'result) Command_base.Spec.t
  -> 'main
  -> t

let extend_exn ~mem ~add map key_type ~key data =
  if mem map key
  then failwithf "there is already a %s named %s" (Key_type.to_string key_type) key ();
  add map ~key ~data
;;

let extend_map_exn map key_type ~key data =
  extend_exn map key_type ~key data ~mem:Map.mem ~add:Map.set
;;

let extend_alist_exn alist key_type ~key data =
  extend_exn
    alist
    key_type
    ~key
    data
    ~mem:(fun alist key -> List.Assoc.mem alist key ~equal:String.equal)
    ~add:(fun alist ~key ~data -> List.Assoc.add alist key data ~equal:String.equal)
;;

module Bailout_dump_flag = struct
  let add base ~name ~aliases ~aliases_excluded_from_help ~text ~text_summary =
    let flags = base.Command_base.flags in
    let flags =
      extend_map_exn
        flags
        Key_type.Flag
        ~key:name
        { name
        ; aliases_excluded_from_help
        ; aliases
        ; num_occurrences = Flag.Num_occurrences.at_most_once
        ; check_available = ignore
        ; action = Print_info_and_quit (fun env -> text env)
        ; doc = sprintf " print %s and exit" text_summary
        ; name_matching = `Prefix
        }
    in
    { base with flags }
  ;;
end

let basic ~summary ?readme { Command_base.Param.usage; flags; f } =
  let flags = flags () in
  let usage = usage () in
  let anons () =
    let open Anons.Parser.Basic.For_opening in
    f ()
    >>| fun params `Parse_args ->
    let outcome = params () in
    match outcome.result with
    | Error (`Missing_required_flags err) -> die "%s" (Error.to_string_hum err) ()
    | Ok thunk -> fun `Run_main -> thunk ()
  in
  let flags = Flag.Internal.create flags in
  let base = { Command_base.summary; readme; usage; flags; anons } in
  let base =
    Bailout_dump_flag.add
      base
      ~name:"-help"
      ~aliases:[ "-?" ]
      ~aliases_excluded_from_help:[ "--help" ]
      ~text_summary:"this help text"
      ~text:(fun env -> Lazy.force (Env.find_exn env Command_base.help_key))
  in
  Base base
;;

let basic_spec ~summary ?readme spec main =
  basic ~summary ?readme (Command_base.Spec.to_param spec main)
;;

let subs_key : (string * t) list Env.Key.t = Env.key_create "subcommands"

let lazy_group ~summary ?readme ?preserve_subcommand_order ?body alist =
  let subcommands =
    Lazy.map alist ~f:(fun alist ->
      let alist =
        List.map alist ~f:(fun (name, t) -> normalize Key_type.Subcommand name, t)
      in
      match Map.of_alist (module String) alist with
      | `Duplicate_key name -> failwithf "multiple subcommands named %s" name ()
      | `Ok map ->
        (match preserve_subcommand_order with
         | Some () -> alist
         | None -> Map.to_alist map))
  in
  Group { summary; readme; subcommands; body }
;;

let group ~summary ?readme ?preserve_subcommand_order ?body alist =
  let readme = Option.map readme ~f:(fun f () -> String.strip (f ())) in
  lazy_group ~summary ?readme ?preserve_subcommand_order ?body (Lazy.from_val alist)
;;

let exec ~summary ?readme ?(child_subcommand = []) ?env ~path_to_exe () =
  let working_dir =
    Filename_base.dirname
    @@
    match path_to_exe with
    | `Absolute _ | `Relative_to_me _ -> Stdlib.Sys.executable_name
    | `Relative_to_argv0 _ -> Stdlib.Sys.argv.(0)
  in
  let path_to_exe =
    match path_to_exe with
    | `Absolute p ->
      if not (Filename_base.is_absolute p)
      then failwith "Path passed to `Absolute must be absolute"
      else p
    | `Relative_to_me p | `Relative_to_argv0 p ->
      if not (Filename_base.is_relative p)
      then failwith "Path passed to `Relative_to_me must be relative"
      else p
  in
  Exec { summary; readme; working_dir; path_to_exe; child_subcommand; env }
;;

let of_lazy thunk = Lazy thunk

let rec proxy_of_sexpable
  sexpable
  ~working_dir
  ~path_to_exe
  ~child_subcommand
  ~path_to_subcommand
  : Proxy.t
  =
  let kind =
    kind_of_sexpable
      sexpable
      ~working_dir
      ~path_to_exe
      ~child_subcommand
      ~path_to_subcommand
  in
  { working_dir; path_to_exe; path_to_subcommand; child_subcommand; kind }

and kind_of_sexpable
  sexpable
  ~working_dir
  ~path_to_exe
  ~child_subcommand
  ~path_to_subcommand
  =
  match (sexpable : Shape.Sexpable.t) with
  | Base b -> Proxy.Kind.Base b
  | Exec e -> Proxy.Kind.Exec e
  | Lazy l ->
    Proxy.Kind.Lazy
      (Lazy.map l ~f:(fun sexpable ->
         kind_of_sexpable
           sexpable
           ~working_dir
           ~path_to_exe
           ~child_subcommand
           ~path_to_subcommand))
  | Group g ->
    Proxy.Kind.Group
      { g with
        subcommands =
          Lazy.map
            g.subcommands
            ~f:
              (List.map ~f:(fun (str, sexpable) ->
                 let path_to_subcommand = path_to_subcommand @ [ str ] in
                 let proxy =
                   proxy_of_sexpable
                     sexpable
                     ~working_dir
                     ~path_to_exe
                     ~child_subcommand
                     ~path_to_subcommand
                 in
                 str, proxy))
      }
;;

module Version_info (Version_util : Version_util) = struct
  let print_version ~version = print_endline (force version)
  let print_build_info ~build_info = print_endline (force build_info)

  let command ~version ~build_info =
    basic
      ~summary:"print version information"
      Command_base.Param.(
        return (fun version_flag build_info_flag ->
          if build_info_flag
          then print_build_info ~build_info
          else if version_flag
          then print_version ~version
          else (
            print_build_info ~build_info;
            print_version ~version);
          exit 0)
        <*> flag "-version" no_arg ~doc:" print the version of this build"
        <*> flag "-build-info" no_arg ~doc:" print build info for this build")
  ;;

  let rec add ~version ~build_info unversioned =
    match unversioned with
    | Base base ->
      let base =
        Bailout_dump_flag.add
          base
          ~name:"-version"
          ~aliases:[]
          ~aliases_excluded_from_help:[ "--version" ]
          ~text_summary:"the version of this build"
          ~text:(fun _ -> force version)
      in
      let base =
        Bailout_dump_flag.add
          base
          ~name:"-build-info"
          ~aliases:[]
          ~aliases_excluded_from_help:[ "--build-info" ]
          ~text_summary:"info about this build"
          ~text:(fun _ -> force build_info)
      in
      Base base
    | Group group ->
      let subcommands =
        Lazy.map group.Group.subcommands ~f:(fun subcommands ->
          extend_alist_exn
            subcommands
            Key_type.Subcommand
            ~key:"version"
            (command ~version ~build_info))
      in
      Group { group with Group.subcommands }
    | Exec exec -> Exec exec
    | Lazy thunk -> Lazy (lazy (add ~version ~build_info (Lazy.force thunk)))
  ;;

  let normalize_version_lines lines =
    String.concat ~sep:"\n" (List.sort lines ~compare:String.compare)
  ;;

  let default_version = lazy (normalize_version_lines Version_util.version_list)

  let default_build_info =
    lazy
      (* lazy to avoid loading all the time zone stuff at toplevel *)
      (Version_util.reprint_build_info Version_util.Time.sexp_of_t)
  ;;
end

let%test_module "Version_info" =
  (module struct
    module Version_info = Version_info (struct
      let version_list = [ "hg://some/path_0xdeadbeef"; "ssh://a/path_8badf00d" ]
      let reprint_build_info to_sexp = Sexp.to_string (to_sexp ())

      module Time = struct
        type t = unit [@@deriving sexp_of]
      end
    end)

    let%expect_test "print version where multiple repos are used" =
      Version_info.print_version ~version:Version_info.default_version;
      [%expect
        {|
        hg://some/path_0xdeadbeef
        ssh://a/path_8badf00d
        |}]
    ;;

    let%expect_test "print build info" =
      Version_info.print_build_info ~build_info:(lazy "some build info");
      [%expect {| some build info |}]
    ;;
  end)
;;

let rec summary = function
  | Base x -> x.summary
  | Group x -> x.summary
  | Exec x -> x.summary
  | Lazy thunk -> summary (Lazy.force thunk)
;;

module Spec = struct
  include Command_base.Spec

  let path = map ~f:Path.parts_exe_basename path
end

module Deprecated = struct
  module Spec = Spec.Deprecated

  let summary = summary

  let rec get_flag_names = function
    | Base base -> base.Command_base.flags |> Map.keys
    | Lazy thunk -> get_flag_names (Lazy.force thunk)
    | Group _ | Exec _ -> assert false
  ;;

  let help_recursive ~cmd ~with_flags ~expand_dots t s =
    let rec help_recursive_rec ~cmd t s =
      let new_s = s ^ (if expand_dots then cmd else ".") ^ " " in
      match t with
      | Lazy thunk ->
        let t = Lazy.force thunk in
        help_recursive_rec ~cmd t s
      | Base base ->
        let base_help = s ^ cmd, summary (Base base) in
        if with_flags
        then
          base_help
          :: List.map
               ~f:(fun (flag, h) -> new_s ^ flag, h)
               (List.sort
                  ~compare:Command_base.Deprecated.subcommand_cmp_fst
                  (Command_base.Deprecated.flags_help ~display_help_flags:false base))
        else [ base_help ]
      | Group { summary; subcommands; readme = _; body = _ } ->
        (s ^ cmd, summary)
        :: (Lazy.force subcommands
            |> List.sort ~compare:Command_base.Deprecated.subcommand_cmp_fst
            |> List.concat_map ~f:(fun (cmd', t) -> help_recursive_rec ~cmd:cmd' t new_s)
           )
      | Exec _ ->
        (* Command.exec does not support deprecated commands *)
        []
    in
    help_recursive_rec ~cmd t s
  ;;
end

(* This script works in both bash (via readarray) and zsh (via read -A).  If you change
   it, please test in both bash and zsh.  It does not work tcsh (different function
   syntax). *)
let autocomplete_function ~argv_0 ~pid =
  let fname =
    (* Note: we pad the pid to a deterministic length, as in 2023 it was determined that
       if multiple invocations occurred at the same time of requesting these functions to
       be written to the same file (e.g. 2 shells opening at /exactly/ the right time)
       there would be bad extra bytes left over in the written file, so making it,
       deterministic in length irrespective of the pid is important. Given that pids don't
       exceed 65536, 10 digits should give us lots of breathing room. *)
    sprintf "_jsautocom_%010d" pid
  in
  sprintf
    "function %s {\n\
    \  export COMP_CWORD\n\
    \  COMP_WORDS[0]=%s\n\
    \  if type readarray > /dev/null\n\
    \  then readarray -t COMPREPLY < <(\"${COMP_WORDS[@]}\")\n\
    \  else IFS=\"\n\
     \" read -d \"\" -A COMPREPLY < <(\"${COMP_WORDS[@]}\")\n\
    \  fi\n\
     }\n\
     complete -F %s %s\n\
     %!"
    fname
    argv_0
    fname
    argv_0
;;

let%expect_test "Demonstrate [autocomplete_function]" =
  autocomplete_function ~argv_0:"<argv_0>" ~pid:12345 |> print_endline;
  [%expect
    {|
    function _jsautocom_0000012345 {
      export COMP_CWORD
      COMP_WORDS[0]=<argv_0>
      if type readarray > /dev/null
      then readarray -t COMPREPLY < <("${COMP_WORDS[@]}")
      else IFS="
    " read -d "" -A COMPREPLY < <("${COMP_WORDS[@]}")
      fi
    }
    complete -F _jsautocom_0000012345 <argv_0>
    |}]
;;

module For_unix (For_unix_with_string_env_var : For_unix with type env_var := string) =
struct
  module Version_info = Version_info (For_unix_with_string_env_var.Version_util)

  module For_unix_with_command_env_var : For_unix with type env_var := Env_var.t = struct
    (* We force access to env vars to go through [Command_env_var] so that we can keep an
       accurate enumeration of the variables we use. *)

    include For_unix_with_string_env_var

    module Unix = struct
      include Unix

      let putenv ~key ~data = putenv ~key:(Env_var.to_string key) ~data
      let unsetenv key = unsetenv (Env_var.to_string key)
      let unsafe_getenv key = unsafe_getenv (Env_var.to_string key)

      let convert_env env =
        let convert_command_env_var_to_string list =
          List.map list ~f:(fun (env_var, str) -> Env_var.to_string env_var, str)
        in
        match env with
        | `Replace list -> `Replace (convert_command_env_var_to_string list)
        | `Extend list -> `Extend (convert_command_env_var_to_string list)
        | `Override list -> `Override (convert_command_env_var_to_string list)
        | `Replace_raw _ as replace -> replace
      ;;

      let exec ~prog ~argv ?use_path ?env () =
        exec ~prog ~argv ?use_path ?env:(Option.map env ~f:convert_env) ()
      ;;

      let create_process_env ?working_dir ?prog_search_path ?argv0 ~prog ~args ~env () =
        create_process_env
          ?working_dir
          ?prog_search_path
          ?argv0
          ~prog
          ~args
          ~env:(convert_env env)
          ()
      ;;
    end
  end

  open For_unix_with_command_env_var

  (* Clear the setting of environment variable associated with command-line
     completion and recursive help so that subprocesses don't see them.

     Use [unsafe_getenv] so setuid-root programs can still read environment variables.
     There is no security risk here because the values are only used as triggers to dump
     out command information. *)
  let getenv_and_clear var =
    let value = Unix.unsafe_getenv var in
    if Option.is_some value then Unix.unsetenv var;
    value
  ;;

  let maybe_comp_cword () = getenv_and_clear comp_cword |> Option.map ~f:Int.of_string

  let set_comp_cword new_value =
    let new_value = Int.to_string new_value in
    Unix.putenv ~key:comp_cword ~data:new_value
  ;;

  module Exec = struct
    include Exec

    let exec_with_args t ~args ~maybe_new_comp_cword =
      let prog = abs_path ~dir:t.working_dir t.path_to_exe in
      let args = t.child_subcommand @ args in
      let env = t.env in
      Option.iter maybe_new_comp_cword ~f:(fun n ->
        (* The logic for tracking [maybe_new_comp_cword] doesn't take into account whether
           this exec specifies a child subcommand. If it does, COMP_CWORD needs to be set
           higher to account for the arguments used to specify the child subcommand. *)
        set_comp_cword (n + List.length t.child_subcommand));
      Nothing.unreachable_code
        (For_unix_with_string_env_var.Unix.exec ?env ~prog ~argv:(prog :: args) ())
    ;;
  end

  module Sexpable = struct
    include Shape.Sexpable

    let read_stdout_and_stderr (process_info : Unix.Process_info.t) =
      (* We need to read each of stdout and stderr in a separate thread to avoid deadlocks
         if the child process decides to wait for a read on one before closing the other.
         Buffering may hide this problem until output is "sufficiently large". *)
      let start_reading descr info =
        let output = ref None in
        let thread =
          Thread.create
            ~on_uncaught_exn:`Print_to_stderr
            (fun () ->
              let result =
                Result.try_with (fun () ->
                  descr |> Unix.in_channel_of_descr |> In_channel.input_all)
              in
              output := Some result)
            ()
        in
        Staged.stage (fun () ->
          Thread.join thread;
          Unix.close descr;
          match !output with
          | None -> raise_s [%message "BUG failed to read" (info : Info.t)]
          | Some (Ok output) -> output
          | Some (Error exn) -> raise exn)
      in
      (* We might hang forever trying to join the reading threads if the child process keeps
         the file descriptor open. Not handling this because I think we've never seen it
         in the wild despite running vulnerable code for years. *)
      (* We have to start both threads before joining any of them. *)
      let finish_stdout = start_reading process_info.stdout (Info.of_string "stdout") in
      let finish_stderr = start_reading process_info.stderr (Info.of_string "stderr") in
      Staged.unstage finish_stdout (), Staged.unstage finish_stderr ()
    ;;

    let of_external ~working_dir ~path_to_exe ~child_subcommand =
      let process_info =
        Unix.create_process_env
          ()
          ~prog:(abs_path ~dir:working_dir path_to_exe)
          ~args:child_subcommand
          ~env:
            (let help_sexp =
               supported_versions |> Set.sexp_of_m__t (module Int) |> Sexp.to_string
             in
             `Extend [ COMMAND_OUTPUT_HELP_SEXP, help_sexp ])
      in
      Unix.close process_info.stdin;
      let stdout, stderr = read_stdout_and_stderr process_info in
      Unix.wait process_info.pid;
      (* Now we've killed all the processes and threads we made. *)
      match stdout |> Sexplib.Sexp.of_string |> Versioned.t_of_sexp |> of_versioned with
      | exception exn ->
        raise_s
          [%message
            "cannot parse command shape"
              ~_:(exn : exn)
              (stdout : string)
              (stderr : string)]
      | t -> t
    ;;

    let rec find (t : t) ~path_to_subcommand =
      match path_to_subcommand with
      | [] -> t
      | sub :: subs ->
        if String.is_prefix sub ~prefix:"-"
        then t
        else (
          match t with
          | Base _ -> failwithf "unexpected subcommand %S" sub ()
          | Lazy thunk -> find (Lazy.force thunk) ~path_to_subcommand
          | Exec { path_to_exe; working_dir; child_subcommand; _ } ->
            find
              (of_external ~working_dir ~path_to_exe ~child_subcommand)
              ~path_to_subcommand:(sub :: (subs @ child_subcommand))
          | Group g ->
            (match List.Assoc.find (Lazy.force g.subcommands) ~equal:String.equal sub with
             | None -> failwithf "unknown subcommand %S" sub ()
             | Some t -> find t ~path_to_subcommand:subs))
    ;;
  end

  let proxy_of_exe ~working_dir path_to_exe child_subcommand =
    Sexpable.of_external ~working_dir ~path_to_exe ~child_subcommand
    |> proxy_of_sexpable
         ~working_dir
         ~path_to_exe
         ~child_subcommand
         ~path_to_subcommand:[]
  ;;

  let rec shape_of_proxy proxy : Shape.t = shape_of_proxy_kind proxy.Proxy.kind

  and shape_of_exe () ~child_subcommand ~path_to_exe ~working_dir =
    shape_of_proxy (proxy_of_exe ~working_dir path_to_exe child_subcommand)

  and shape_of_proxy_kind kind =
    match kind with
    | Base b -> Basic b
    | Lazy l -> Lazy (Lazy.map ~f:shape_of_proxy_kind l)
    | Group g ->
      Group
        { g with
          subcommands = Lazy.map g.subcommands ~f:(List.Assoc.map ~f:shape_of_proxy)
        }
    | Exec ({ child_subcommand; path_to_exe; working_dir; _ } as e) ->
      Exec (e, shape_of_exe ~child_subcommand ~path_to_exe ~working_dir)
  ;;

  let rec shape t : Shape.t =
    match t with
    | Base b -> Basic (Command_base.shape b)
    | Group g -> Group (Group.shape ~subcommand_to_shape:shape g)
    | Exec ({ Exec.child_subcommand; path_to_exe; working_dir; _ } as e) ->
      Exec (Exec.shape e, shape_of_exe ~child_subcommand ~path_to_exe ~working_dir)
    | Lazy thunk -> shape (Lazy.force thunk)
  ;;

  let gather_help ~recursive ~flags ~expand_dots shape =
    let rec loop path acc shape =
      let string_of_path = if expand_dots then Path.to_string else Path.to_string_dots in
      let gather_group path acc subcommands =
        let filtered_subcommands =
          (* Only show the [help] subcommand at top-level. *)
          if Path.is_empty path
          then subcommands
          else List.Assoc.remove ~equal:String.( = ) subcommands "help"
        in
        filtered_subcommands
        |> List.stable_sort ~compare:(fun a b -> help_screen_compare (fst a) (fst b))
        |> List.fold ~init:acc ~f:(fun acc (subcommand, shape) ->
             let path = Path.append path ~subcommand in
             let name = string_of_path path in
             let doc = Shape.get_summary shape in
             let acc = { Shape.Flag_info.name; doc; aliases = [] } :: acc in
             if recursive then loop path acc shape else acc)
      in
      match shape with
      | Exec (_, shape) ->
        (* If the executable being called doesn't use [Core.Command], then sexp extraction
           will fail. *)
        (try loop path acc (shape ()) with
         | _ -> acc)
      | Group g -> gather_group path acc (Lazy.force g.subcommands)
      | Basic b ->
        if flags
        then
          b.flags
          |> List.filter ~f:(fun fmt -> String.( <> ) fmt.name "[-help]")
          |> List.fold ~init:acc ~f:(fun acc fmt ->
               let path = Path.append path ~subcommand:fmt.name in
               let fmt = { fmt with name = string_of_path path } in
               fmt :: acc)
        else acc
      | Lazy thunk -> loop path acc (Lazy.force thunk)
    in
    loop Path.empty [] shape |> List.rev
  ;;

  let group_or_exec_help_text ~flags ~path ~summary ~readme ~format_list =
    unparagraphs
      (List.filter_opt
         [ Some summary
         ; Some (String.concat [ "  "; Path.to_string path; " SUBCOMMAND" ])
         ; readme
         ; Some (if flags then "=== subcommands and flags ===" else "=== subcommands ===")
         ; Some (Shape.Flag_help_display.to_string format_list)
         ])
  ;;

  let rec help_for_shape shape path ~expand_dots ~flags ~recursive =
    let format_list = gather_help ~expand_dots ~flags ~recursive shape in
    match shape with
    | Basic b ->
      let usage = Shape.Base_info.get_usage b in
      unparagraphs
        (List.filter_opt
           [ Some b.summary
           ; Some ("  " ^ Path.to_string path ^ " " ^ usage)
           ; b.readme
           ; Some "=== flags ==="
           ; Some (Shape.Flag_help_display.to_string b.flags)
           ])
    | Group g ->
      group_or_exec_help_text
        ~flags
        ~path
        ~readme:g.readme
        ~summary:g.summary
        ~format_list
    | Exec (e, _) ->
      group_or_exec_help_text
        ~flags
        ~path
        ~readme:e.readme
        ~summary:e.summary
        ~format_list
    | Lazy thunk -> help_for_shape (Lazy.force thunk) path ~expand_dots ~flags ~recursive
  ;;

  let help_subcommand ~summary ~readme =
    basic
      ~summary:"explain a given subcommand (perhaps recursively)"
      Command_base.Param.(
        return (fun recursive flags expand_dots path (env : Env.t) cmd_opt () ->
          let subs =
            match Env.find env subs_key with
            | Some subs -> subs
            | None -> assert false
            (* maintained by [dispatch] *)
          in
          let path =
            let path = Path.pop_help path in
            Option.fold cmd_opt ~init:path ~f:(fun path subcommand ->
              Path.append path ~subcommand)
          in
          let path, shape =
            match cmd_opt with
            | None ->
              let subcommands = List.Assoc.map subs ~f:shape |> Lazy.from_val in
              let readme = Option.map readme ~f:(fun readme -> readme ()) in
              path, Shape.Group { readme; summary; subcommands }
            | Some cmd ->
              (match
                 lookup_expand
                   (List.Assoc.map subs ~f:(fun x -> x, `Prefix))
                   cmd
                   Subcommand
               with
               | Error e ->
                 die
                   "unknown subcommand %s for command %s: %s"
                   cmd
                   (Path.to_string path)
                   e
                   ()
               | Ok (possibly_expanded_name, t) ->
                 (* Fix the unexpanded value *)
                 let path =
                   Path.replace_first ~from:cmd ~to_:possibly_expanded_name path
                 in
                 path, shape t)
          in
          print_endline (help_for_shape shape path ~recursive ~flags ~expand_dots))
        <*> flag "-recursive" no_arg ~doc:" show subcommands of subcommands, etc."
        <*> flag "-flags" no_arg ~doc:" show flags as well in recursive help"
        <*> flag "-expand-dots" no_arg ~doc:" expand subcommands in recursive help"
        <*> path
        <*> env
        <*> anon (maybe ("SUBCOMMAND" %: string)))
  ;;

  let dump_autocomplete_function () =
    autocomplete_function ~argv_0:Stdlib.Sys.argv.(0) ~pid:(Unix.getpid () |> Pid.to_int)
    |> printf "%s"
  ;;

  let dump_help_sexp ~supported_versions t ~path_to_subcommand =
    Set.inter Sexpable.supported_versions supported_versions
    |> Set.max_elt
    |> function
    | None ->
      Error.create
        ~here:[%here]
        "Couldn't choose a supported help output version for Command.exec from the given \
         supported versions."
        Sexpable.supported_versions
        (Set.sexp_of_m__t (module Int))
      |> Error.raise
    | Some version_to_use ->
      sexpable_shape t
      |> Sexpable.find ~path_to_subcommand
      |> Sexpable.to_versioned ~version_to_use
      |> Sexpable.Versioned.sexp_of_t
      |> Sexp.to_string
      |> print_string
  ;;

  let handle_environment t ~argv =
    match argv with
    | [] -> failwith "missing executable name"
    | cmd :: args ->
      Option.iter (getenv_and_clear COMMAND_OUTPUT_HELP_SEXP) ~f:(fun version ->
        let supported_versions =
          Sexplib.Sexp.of_string version |> Set.m__t_of_sexp (module Int)
        in
        dump_help_sexp ~supported_versions t ~path_to_subcommand:args;
        exit 0);
      Option.iter (getenv_and_clear COMMAND_OUTPUT_INSTALLATION_BASH) ~f:(fun _ ->
        dump_autocomplete_function ();
        exit 0);
      cmd, args
  ;;

  let process_args ~cmd ~args =
    let maybe_comp_cword = maybe_comp_cword () in
    let args =
      match maybe_comp_cword with
      | None -> Cmdline.of_list args
      | Some comp_cword ->
        let args = List.take (args @ [ "" ]) comp_cword in
        List.fold_right args ~init:Cmdline.Nil ~f:(fun arg args ->
          match args with
          | Cmdline.Nil -> Cmdline.Complete arg
          | _ -> Cmdline.Cons (arg, args))
    in
    Path.create ~path_to_exe:cmd, args, maybe_comp_cword
  ;;

  module Only_validate_parsing = struct
    let flag base =
      let name = "-validate-parsing" in
      let flags = base.Command_base.flags in
      let flags =
        extend_map_exn
          flags
          Key_type.Flag
          ~key:name
          { name
          ; aliases_excluded_from_help = [ "--validate-parsing" ]
          ; aliases = []
          ; num_occurrences = Flag.Num_occurrences.at_most_once
          ; check_available = ignore
          ; action =
              No_arg (fun env -> Env.set ~key:key_internal_validate_parsing ~data:() env)
          ; doc = " validate arguments are parsed correctly and exit immediately"
          ; name_matching = `Prefix
          }
      in
      { base with flags }
    ;;

    let rec add = function
      | Base base -> Base (flag base)
      | Exec _ as t -> t
      | Group { summary; readme; subcommands; body } ->
        let subcommands =
          Lazy.map subcommands ~f:(fun subcommands ->
            List.map subcommands ~f:(fun (name, command) -> name, add command))
        in
        Group { summary; readme; subcommands; body }
      | Lazy thunk -> Lazy (lazy (add (Lazy.force thunk)))
    ;;
  end

  let rec add_help_subcommands = function
    | Base _ as t -> t
    | Exec _ as t -> t
    | Group { summary; readme; subcommands; body } ->
      let subcommands =
        Lazy.map subcommands ~f:(fun subcommands ->
          extend_alist_exn
            (List.Assoc.map subcommands ~f:add_help_subcommands)
            Key_type.Subcommand
            ~key:"help"
            (help_subcommand ~summary ~readme))
      in
      Group { summary; readme; subcommands; body }
    | Lazy thunk -> Lazy (lazy (add_help_subcommands (Lazy.force thunk)))
  ;;

  let maybe_apply_extend args ~extend ~path =
    Option.value_map extend ~default:args ~f:(fun f ->
      Cmdline.extend args ~extend:f ~path)
  ;;

  let rec dispatch
    t
    env
    ~extend
    ~path
    ~args
    ~maybe_new_comp_cword
    ~version
    ~build_info
    ~verbose_on_parse_error
    ~when_parsing_succeeds
    ~complete_subcommands
    =
    match t with
    | Lazy thunk ->
      let t = Lazy.force thunk in
      dispatch
        t
        env
        ~extend
        ~path
        ~args
        ~maybe_new_comp_cword
        ~version
        ~build_info
        ~verbose_on_parse_error
        ~when_parsing_succeeds
        ~complete_subcommands
    | Base base ->
      let args = maybe_apply_extend args ~extend ~path in
      let help_text =
        lazy
          (help_for_shape (shape t) path ~recursive:false ~flags:true ~expand_dots:false)
      in
      Command_base.run
        base
        env
        ~path
        ~args
        ~verbose_on_parse_error
        ~help_text
        ~when_parsing_succeeds
        ~on_failure:Command_base.run_exn
    | Exec exec ->
      let args = Cmdline.to_list (maybe_apply_extend args ~extend ~path) in
      Exec.exec_with_args ~args exec ~maybe_new_comp_cword
    | Group ({ summary; readme; subcommands = subs; body } as group) ->
      let completing = Cmdline.ends_in_complete args in
      let env = Env.set env ~key:subs_key ~data:(Lazy.force subs) in
      let die_showing_help msg =
        if completing
        then exit 0
        else (
          eprintf
            "%s\n%!"
            (help_for_shape
               ~recursive:false
               ~flags:false
               ~expand_dots:false
               (shape (Group { summary; readme; subcommands = subs; body }))
               path);
          die "%s" msg ())
      in
      let rec parse_group args ~maybe_new_comp_cword =
        let maybe_new_comp_cword = Option.map ~f:Int.pred maybe_new_comp_cword in
        let skip rest = parse_group rest ~maybe_new_comp_cword in
        let resolve sub rest =
          let subs = List.Assoc.map (Lazy.force subs) ~f:(fun x -> x, `Prefix) in
          match lookup_expand subs sub Subcommand with
          | Error msg -> die_showing_help msg
          | Ok (sub, t) ->
            dispatch
              t
              env
              ~when_parsing_succeeds
              ~extend
              ~path:(Path.append path ~subcommand:sub)
              ~args:rest
              ~maybe_new_comp_cword
              ~version
              ~build_info
              ~verbose_on_parse_error
              ~complete_subcommands
        in
        match (args : Cmdline.t) with
        | Nil ->
          (match body with
           | None ->
             die_showing_help
               (sprintf "missing subcommand for command %s" (Path.to_string path))
           | Some body -> body ~path:(Path.parts_exe_basename path))
        | Cons (sub, rest) ->
          (* Match for flags recognized when subcommands are expected next *)
          (match sub with
           (* Recognized at the top level command only *)
           | ("-version" | "--version") when Path.length path = 1 ->
             if completing
             then skip rest
             else (
               Version_info.print_version ~version;
               exit 0)
           | ("-build-info" | "--build-info") when Path.length path = 1 ->
             if completing
             then skip rest
             else (
               Version_info.print_build_info ~build_info;
               exit 0)
           (* Recognized everywhere *)
           | "-help" | "--help" ->
             if completing
             then skip rest
             else (
               match rest with
               | Nil | Complete (_ : string) ->
                 print_endline
                   (help_for_shape
                      ~recursive:false
                      ~flags:false
                      ~expand_dots:false
                      (shape (Group { group with subcommands = subs }))
                      path);
                 exit 0
               | Cmdline.Cons (first_of_rest, rest_of_rest) ->
                 resolve first_of_rest (Cons (sub, rest_of_rest)))
           | (_ : string) -> resolve sub rest)
        | Complete part ->
          let subs =
            Lazy.force subs
            |> List.map ~f:fst
            |> List.filter ~f:(fun name -> String.is_prefix name ~prefix:part)
            |> List.sort ~compare:String.compare
          in
          (match complete_subcommands with
           | Some f ->
             let subcommands =
               shape t |> Shape.fully_forced |> Shape.Fully_forced.expanded_subcommands
             in
             (match f ~path:(Path.parts path) ~part subcommands with
              | None -> exit 1
              | Some to_output ->
                print_endline (String.concat ~sep:" " to_output);
                exit 0)
           | None ->
             List.iter subs ~f:print_endline;
             exit 0)
      in
      parse_group args ~maybe_new_comp_cword
  ;;

  let run
    ?(add_validate_parsing_flag = false)
    ?verbose_on_parse_error
    ?version
    ?build_info
    ?(argv = Array.to_list Stdlib.Sys.argv)
    ?extend
    ?(when_parsing_succeeds = Fn.id)
    ?complete_subcommands
    t
    =
    let build_info =
      match build_info with
      | Some v -> lazy v
      | None -> Version_info.default_build_info
    in
    let version =
      match version with
      | None -> Version_info.default_version
      | Some v ->
        (* [version] was space delimited at some point and newline delimited
           at another.  We always print one (repo, revision) pair per line
           and ensure sorted order *)
        lazy
          (Version_info.normalize_version_lines
             (String.split v ~on:' ' |> List.concat_map ~f:(String.split ~on:'\n')))
    in
    Exn.handle_uncaught_and_exit (fun () ->
      let t = Version_info.add t ~version ~build_info in
      let t = add_help_subcommands t in
      let t = if add_validate_parsing_flag then Only_validate_parsing.add t else t in
      let cmd, args = handle_environment t ~argv in
      let path, args, maybe_new_comp_cword = process_args ~cmd ~args in
      try
        dispatch
          t
          Env.empty
          ~extend
          ~path
          ~args
          ~maybe_new_comp_cword
          ~version
          ~build_info
          ~verbose_on_parse_error
          ~when_parsing_succeeds
          ~complete_subcommands
      with
      | Failed_to_parse_command_line msg ->
        if Cmdline.ends_in_complete args
        then exit 0
        else (
          prerr_endline msg;
          exit 1))
  ;;

  let deprecated_run t ~cmd ~args ~is_help ~is_help_rec ~is_help_rec_flags ~is_expand_dots
    =
    let path_strings = String.split cmd ~on:' ' in
    let path = Path.of_parts path_strings in
    let args = if is_expand_dots then "-expand-dots" :: args else args in
    let args = if is_help_rec_flags then "-flags" :: args else args in
    let args = if is_help_rec then "-r" :: args else args in
    let args = if is_help then "-help" :: args else args in
    let args = Cmdline.of_list args in
    let t = add_help_subcommands t in
    dispatch
      t
      Env.empty
      ~path
      ~args
      ~extend:None
      ~maybe_new_comp_cword:None
      ~version:Version_info.default_version
      ~build_info:Version_info.default_build_info
      ~verbose_on_parse_error:None
      ~when_parsing_succeeds:Fn.id
      ~complete_subcommands:None
  ;;
end

module Param = struct
  module type S = sig
    type +'a t

    include Applicative.S with type 'a t := 'a t

    val help : string Lazy.t t
    val path : string list t
    val args : string list t

    val flag
      :  ?aliases:string list
      -> ?full_flag_required:unit
      -> string
      -> 'a Flag.t
      -> doc:string
      -> 'a t

    val flag_optional_with_default_doc
      :  ?aliases:string list
      -> ?full_flag_required:unit
      -> string
      -> 'a Arg_type.t
      -> ('a -> Sexp.t)
      -> default:'a
      -> doc:string
      -> 'a t

    val anon : 'a Anons.t -> 'a t
    val escape_anon : final_anon:'a Anons.t -> ('a * string list) t

    module If_nothing_chosen : sig
      type (_, _) t =
        | Default_to : 'a -> ('a, 'a) t
        | Raise : ('a, 'a) t
        | Return_none : ('a, 'a option) t
    end

    val choose_one
      :  'a option t list
      -> if_nothing_chosen:('a, 'b) If_nothing_chosen.t
      -> 'b t

    val choose_one_non_optional
      :  'a t list
      -> if_nothing_chosen:('a, 'b) If_nothing_chosen.t
      -> 'b t

    val and_arg_names : 'a t -> ('a * string list) t
    val and_arg_name : 'a t -> ('a * string) t
    val arg_names : 'a t -> string list
  end

  include Command_base.Param

  let path = map ~f:Path.parts_exe_basename path
end

module Let_syntax = struct
  include Param

  module Let_syntax = struct
    include Param
    module Open_on_rhs = Param
  end
end

type 'result basic_command =
  summary:string -> ?readme:(unit -> string) -> (unit -> 'result) Param.t -> t

let basic ~summary ?readme param =
  let readme = Option.map readme ~f:(fun f () -> String.strip (f ())) in
  basic ~summary ?readme param
;;

let basic_or_error ~summary ?readme param =
  basic
    ~summary
    ?readme
    (let%map run = param in
     fun () ->
       match run () with
       | Ok () -> ()
       | Error e ->
         Stdio.prerr_endline (Error.to_string_hum e);
         exit 1)
;;

module For_telemetry = struct
  let normalized_path () = Option.map !Command_base.normalized_path ~f:Path.parts
  let normalized_args () = !Command_base.normalized_args
end

module Private = struct
  let abs_path = abs_path
  let word_wrap = Shape.Private.word_wrap

  module Anons = Anons
  module Cmdline = Cmdline
  module For_unix = For_unix
  module Path = Path

  module Spec = struct
    include Spec

    let to_string_for_choose_one param =
      Command_base.Param.Choose_one.Choice_name.(create_exn param |> to_string)
    ;;
  end
end

let run = `Use_Command_unix
let shape = `Use_Command_unix
