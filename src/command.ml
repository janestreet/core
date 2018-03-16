open! Import
open Import_time

module Unix     = Core_unix
module Filename = Core_filename

let unwords      xs = String.concat ~sep:" "    xs
let unparagraphs xs = String.concat ~sep:"\n\n" xs

exception Failed_to_parse_command_line of string

let die fmt = Printf.ksprintf (fun msg () -> raise (Failed_to_parse_command_line msg)) fmt

let help_screen_compare a b =
  match (a, b) with
  | (_, "[-help]")       -> -1 | ("[-help]",       _) -> 1
  | (_, "[-version]")    -> -1 | ("[-version]",    _) -> 1
  | (_, "[-build-info]") -> -1 | ("[-build-info]", _) -> 1
  | (_, "help")          -> -1 | ("help",        _)   -> 1
  | (_, "version")       -> -1 | ("version",     _)   -> 1
  | _ -> 0

module Format : sig
  module V1 : sig
    type t = {
      name    : string;
      doc     : string;
      aliases : string list;
    } [@@deriving sexp]

    val sort      : t list -> t list
    val to_string : t list -> string
  end
end = struct
  module V1 = struct
    type t = {
      name    : string;
      doc     : string;
      aliases : string list;
    } [@@deriving sexp]

    let sort ts =
      List.stable_sort ts ~compare:(fun a b -> help_screen_compare a.name b.name)

    let word_wrap text width =
      let chunks = String.split text ~on:'\n' in
      List.concat_map chunks ~f:(fun text ->
        let words =
          String.split text ~on:' '
          |> List.filter ~f:(fun word -> not (String.is_empty word))
        in
        match
          List.fold words ~init:None ~f:(fun acc word ->
            Some begin
              match acc with
              | None -> ([], word)
              | Some (lines, line) ->
                (* efficiency is not a concern for the string lengths we expect *)
                let line_and_word = line ^ " " ^ word in
                if String.length line_and_word <= width then
                  (lines, line_and_word)
                else
                  (line :: lines, word)
            end)
        with
        | None -> []
        | Some (lines, line) -> List.rev (line :: lines))

    let%test_module "word wrap" =
      (module struct

        let%test _ = word_wrap "" 10 = []

        let short_word = "abcd"

        let%test _ = word_wrap short_word (String.length short_word) = [short_word]

        let%test _ = word_wrap "abc\ndef\nghi" 100 = ["abc"; "def"; "ghi"]

        let long_text =
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus \
           fermentum condimentum eros, sit amet pulvinar dui ultrices in."

        let%test _ = word_wrap long_text 1000 =
                     ["Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus \
                       fermentum condimentum eros, sit amet pulvinar dui ultrices in."]

        let%test _ = word_wrap long_text 39 =
      (*
                        .........1.........2.........3.........4
                        1234567890123456789012345678901234567890
                     *)
                     ["Lorem ipsum dolor sit amet, consectetur";
                      "adipiscing elit. Vivamus fermentum";
                      "condimentum eros, sit amet pulvinar dui";
                      "ultrices in."]

        (* no guarantees: too-long words just overhang the soft bound *)
        let%test _ = word_wrap long_text 2 =
                     ["Lorem"; "ipsum"; "dolor"; "sit"; "amet,"; "consectetur";
                      "adipiscing"; "elit."; "Vivamus"; "fermentum"; "condimentum";
                      "eros,"; "sit"; "amet"; "pulvinar"; "dui"; "ultrices"; "in."]

      end)

    let to_string ts =
      let n =
        List.fold ts ~init:0
          ~f:(fun acc t -> Int.max acc (String.length t.name))
      in
      let num_cols = 80 in (* anything more dynamic is likely too brittle *)
      let extend x =
        let slack = n - String.length x in
        x ^ String.make slack ' '
      in
      let lhs_width = n + 4 in
      let lhs_pad = String.make lhs_width ' ' in
      String.concat
        (List.map ts ~f:(fun t ->
           let rows k v =
             let vs = word_wrap v (num_cols - lhs_width) in
             match vs with
             | [] -> ["  "; k; "\n"]
             | v :: vs ->
               let first_line = ["  "; extend k; "  "; v; "\n"] in
               let rest_lines = List.map vs ~f:(fun v -> [lhs_pad; v; "\n"]) in
               List.concat (first_line :: rest_lines)
           in
           String.concat
             (List.concat
                (rows t.name t.doc
                 :: begin
                   match t.aliases with
                   | [] -> []
                   | [x] -> [rows "" (sprintf "(alias: %s)" x)]
                   | xs  ->
                     [rows "" (sprintf "(aliases: %s)" (String.concat ~sep:", " xs))]
                 end))))

  end
end

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

  let key_create name = Univ_map.Key.create ~name sexp_of_opaque
  let multi_add = Univ_map.Multi.add
  let set_with_default = Univ_map.With_default.set
end

module Completer = struct
  type t = (Env.t -> part:string -> string list) option

  let run_and_exit t env ~part : never_returns =
    Option.iter t ~f:(fun completions ->
      List.iter ~f:print_endline (completions env ~part));
    exit 0
end

module Arg_type = struct
  type 'a t = {
    parse : string -> ('a, exn) Result.t;
    complete : Completer.t;
    key : 'a Univ_map.Multi.Key.t option;
  }

  let create ?complete ?key of_string =
    let parse x = Result.try_with (fun () -> of_string x) in
    { parse; key; complete }

  let map ?key t ~f =
    let parse str = Result.map (t.parse str) ~f in
    let complete = t.complete in
    { parse ; complete ; key }

  let string             = create Fn.id
  let int                = create Int.of_string
  let char               = create Char.of_string
  let float              = create Float.of_string
  let date               = create Date.of_string
  let percent            = create Percent.of_string
  let time               = create Time.of_string_abs
  let time_ofday         = create Time.Ofday.Zoned.of_string
  let time_ofday_unzoned = create Time.Ofday.of_string
  let time_zone          = create Time.Zone.of_string
  let time_span          = create Time.Span.of_string
  let host_and_port      = create Host_and_port.of_string
  let sexp               = create Sexp.of_string
  let sexp_conv of_sexp  = create (fun s -> of_sexp (Sexp.of_string s))
  let ip_address         = create Unix.Inet_addr.of_string

  let file ?key of_string =
    create ?key of_string ~complete:(fun _ ~part ->
      let completions =
        (* `compgen -f` handles some fiddly things nicely, e.g. completing "foo" and
           "foo/" appropriately. *)
        let command = sprintf "bash -c 'compgen -f %s'" part in
        let chan_in = Unix.open_process_in command in
        let completions = In_channel.input_lines chan_in in
        ignore (Unix.close_process_in chan_in);
        List.map (List.sort ~compare:String.compare completions) ~f:(fun comp ->
          if Sys.is_directory comp
          then comp ^ "/"
          else comp)
      in
      match completions with
      | [dir] when String.is_suffix dir ~suffix:"/" ->
        (* If the only match is a directory, we fake out bash here by creating a bogus
           entry, which the user will never see - it forces bash to push the completion
           out to the slash. Then when the user hits tab again, they will be at the end
           of the line, at the directory with a slash and completion will continue into
           the subdirectory.
        *)
        [dir; dir ^ "x"]
      | _ -> completions
    )

  let of_map ?key map =
    create ?key
      ~complete:(fun _ ~part:prefix ->
        List.filter_map (Map.to_alist map) ~f:(fun (name, _) ->
          if String.is_prefix name ~prefix then Some name else None))
      (fun arg ->
         match Map.find map arg with
         | Some v -> v
         | None ->
           failwithf "valid arguments: {%s}" (String.concat ~sep:"," (Map.keys map)) ())

  let of_alist_exn ?key alist =
    match String.Map.of_alist alist with
    | `Ok map -> of_map ?key map
    | `Duplicate_key key ->
      failwithf "Command.Spec.Arg_type.of_alist_exn: duplicate key %s" key ()

  let bool = of_alist_exn [("true", true); ("false", false)]

  let comma_separated ?key ?(strip_whitespace = false) ?(unique_values = false) t =
    let strip =
      if strip_whitespace
      then (fun str -> String.strip str)
      else Fn.id
    in
    let complete =
      Option.map t.complete ~f:(fun complete_elt ->
        (fun env ~part ->
           let prefixes, suffix =
             match String.split part ~on:',' |> List.rev with
             | []       -> [], part
             | hd :: tl -> List.rev tl, hd
           in
           let is_allowed =
             if not (unique_values) then
               (fun (_ : string) -> true)
             else begin
               let seen_already =
                 prefixes
                 |> List.map ~f:strip
                 |> String.Set.of_list
               in
               (fun choice -> not (Set.mem seen_already (strip choice)))
             end
           in
           let choices =
             match
               List.filter (complete_elt env ~part:suffix) ~f:(fun choice ->
                 not (String.mem choice ',')
                 && is_allowed choice)
             with
             (* If there is exactly one choice to auto-complete, add a second choice with
                a trailing comma so that auto-completion will go to the end but bash
                won't add a space.  If there are multiple choices, or a single choice
                that must be final, there is no need to add a dummy option. *)
             | [ choice ] -> [ choice; choice ^ "," ]
             | choices    -> choices
           in
           List.map choices ~f:(fun choice ->
             String.concat ~sep:"," (prefixes @ [choice]))))
    in
    let of_string string =
      let string = strip string in
      if String.is_empty string
      then []
      else
        List.map (String.split string ~on:',') ~f:(fun str ->
          Result.ok_exn (t.parse (strip str)))
    in
    create ?key ?complete of_string

  module Export = struct
    let string                   = string
    let int                      = int
    let char                     = char
    let float                    = float
    let bool                     = bool
    let date                     = date
    let percent                  = percent
    let time                     = time
    let time_ofday               = time_ofday
    let time_ofday_unzoned       = time_ofday_unzoned
    let time_zone                = time_zone
    let time_span                = time_span
    let file                     = file Fn.id
    let host_and_port            = host_and_port
    let sexp                     = sexp
    let sexp_conv                = sexp_conv
    let ip_address               = ip_address
  end
end

module Flag = struct
  type action =
    | No_arg of (Env.t                -> Env.t)
    | Arg    of (Env.t -> string      -> Env.t) * Completer.t
    | Rest   of (Env.t -> string list -> Env.t)

  module Internal = struct
    type t = {
      name : string;
      aliases : string list;
      action : action;
      doc : string;
      check_available : [ `Optional | `Required of (Env.t -> unit) ];
      name_matching : [`Prefix | `Full_match_required];
    }

    let wrap_if_optional t x =
      match t.check_available with
      | `Optional -> sprintf "[%s]" x
      | `Required _ -> x

    module Doc = struct
      type t =
        { arg_doc : string option
        ; doc     : string
        }

      let parse ~action ~doc =
        let arg_doc =
          match (action : action) with
          | No_arg _ -> None
          | Rest _ | Arg _ ->
            match String.lsplit2 doc ~on:' ' with
            | None | Some ("", _) -> None
            | Some (arg, doc) -> Some (arg, doc)
        in
        match arg_doc with
        | None                -> { doc = String.strip doc; arg_doc = None }
        | Some (arg_doc, doc) -> { doc = String.strip doc; arg_doc = Some arg_doc }
      ;;

      let concat ~name ~arg_doc =
        match arg_doc with
        | None -> name
        | Some arg_doc -> name ^ " " ^ arg_doc
      ;;
    end

    module Deprecated = struct
      (* flag help in the format of the old command. used for injection *)
      let help
            ({name; doc; aliases; action; check_available=_; name_matching=_ } as t)
        =
        if String.is_prefix doc ~prefix:" " then
          (name, String.lstrip doc)
          :: List.map aliases ~f:(fun x -> (x, sprintf "same as \"%s\"" name))
        else (
          let { Doc. arg_doc; doc } = Doc.parse ~action ~doc in
          (wrap_if_optional t (Doc.concat ~name ~arg_doc), doc)
          :: List.map aliases ~f:(fun x ->
            (wrap_if_optional t (Doc.concat ~name:x ~arg_doc)
            , sprintf "same as \"%s\"" name)))
      ;;
    end

    let align ({name; doc; aliases; action; check_available=_; name_matching=_ } as t) =
      let { Doc. arg_doc; doc } = Doc.parse ~action ~doc in
      let name = wrap_if_optional t (Doc.concat ~name ~arg_doc) in
      { Format.V1.name; doc; aliases}
    ;;

    let create flags =
      match String.Map.of_alist (List.map flags ~f:(fun flag -> (flag.name, flag))) with
      | `Duplicate_key flag -> failwithf "multiple flags named %s" flag ()
      | `Ok map ->
        List.concat_map flags ~f:(fun flag -> flag.name :: flag.aliases)
        |> List.find_a_dup ~compare:[%compare: string]
        |> Option.iter ~f:(fun x -> failwithf "multiple flags or aliases named %s" x ());
        map
    ;;
  end

  type 'a state = {
    action : action;
    read : Env.t -> 'a;
    optional : bool;
  }

  type 'a t = string -> 'a state

  let arg_flag name arg_type read write ~optional =
    { read; optional;
      action =
        let update env arg =
          match arg_type.Arg_type.parse arg with
          | Error exn ->
            die "failed to parse %s value %S.\n%s" name arg (Exn.to_string exn) ()
          | Ok arg ->
            let env = write env arg in
            match arg_type.Arg_type.key with
            | None -> env
            | Some key -> Env.multi_add env key arg
        in
        Arg (update, arg_type.Arg_type.complete);
    }

  let map_flag t ~f =
    fun input ->
      let {action; read; optional} = t input in
      { action;
        read = (fun env -> f (read env));
        optional;
      }

  let write_option name key env arg =
    Env.update env key ~f:(function
      | None -> arg
      | Some _ -> die "flag %s passed more than once" name ()
    )

  let required_value ?default arg_type name ~optional =
    let key = Env.Key.create ~name [%sexp_of: _] in
    let read env =
      match Env.find env key with
      | Some v -> v
      | None ->
        match default with
        | Some v -> v
        | None -> die "missing required flag: %s" name ()
    in
    let write env arg = write_option name key env arg in
    arg_flag name arg_type read write ~optional

  let required arg_type name =
    required_value arg_type name ~optional:false

  let optional_with_default default arg_type name =
    required_value ~default arg_type name ~optional:true

  let optional arg_type name =
    let key = Env.Key.create ~name [%sexp_of: _] in
    let read env = Env.find env key in
    let write env arg = write_option name key env arg in
    arg_flag name arg_type read write ~optional:true

  let no_arg_general ~key_value ~deprecated_hook name =
    let key = Env.Key.create ~name [%sexp_of: unit] in
    let read env = Env.mem env key in
    let write env =
      if Env.mem env key then
        die "flag %s passed more than once" name ()
      else
        Env.set env key ()
    in
    let action env =
      let env =
        Option.fold key_value ~init:env
          ~f:(fun env (key, value) ->
            Env.set_with_default env key value)
      in
      write env
    in
    let action =
      match deprecated_hook with
      | None -> action
      | Some f ->
        (fun env ->
           let env = action env in
           f ();
           env
        )
    in
    { read; action = No_arg action; optional = true }

  let no_arg name = no_arg_general name ~key_value:None ~deprecated_hook:None

  let no_arg_register ~key ~value name =
    no_arg_general name ~key_value:(Some (key, value)) ~deprecated_hook:None

  let listed arg_type name =
    let key =
      Env.With_default.Key.create ~default:[] ~name [%sexp_of: _ list]
    in
    let read env = List.rev (Env.With_default.find env key) in
    let write env arg =
      Env.With_default.change env key ~f:(fun list -> arg :: list)
    in
    arg_flag name arg_type read write ~optional:true

  let one_or_more arg_type name =
    let key =
      Env.With_default.Key.create ~default:Fqueue.empty ~name [%sexp_of: _ Fqueue.t]
    in
    let read env =
      match Fqueue.to_list (Env.With_default.find env key) with
      | first :: rest -> (first, rest)
      | [] -> die "missing required flag: %s" name ()
    in
    let write env arg =
      Env.With_default.change env key ~f:(fun q -> Fqueue.enqueue q arg)
    in
    arg_flag name arg_type read write ~optional:false

  let escape_general ~deprecated_hook name =
    let key = Env.Key.create ~name [%sexp_of: string list] in
    let action = (fun env cmd_line -> Env.set env key cmd_line) in
    let read env = Env.find env key in
    let action =
      match deprecated_hook with
      | None -> action
      | Some f ->
        (fun env x ->
           f x;
           action env x
        )
    in
    { action = Rest action; read; optional = true }

  let no_arg_abort ~exit _name = {
    action = No_arg (fun _ -> never_returns (exit ()));
    optional = true;
    read = (fun _ -> ());
  }

  let escape name = escape_general ~deprecated_hook:None name

  module Deprecated = struct
    let no_arg ~hook name = no_arg_general ~deprecated_hook:(Some hook) ~key_value:None name
    let escape ~hook      = escape_general ~deprecated_hook:(Some hook)
  end

end

module Path : sig
  type t
  val empty : t
  val root : string -> t
  val add : t -> subcommand:string -> t
  val replace_first : t -> from:string -> to_:string -> t
  val commands : t -> string list
  val to_string : t -> string
  val to_string_dots : t -> string
  val pop_help : t -> t
  val length : t -> int
end = struct
  type t = string list
  let empty = []
  let root cmd = [Filename.basename cmd]
  let add t ~subcommand = subcommand :: t
  let commands t = List.rev t
  let to_string t = unwords (commands t)
  let length = List.length
  let replace_first t ~from ~to_ =
    let replaced : unit Set_once.t = Set_once.create () in
    List.rev_map (List.rev t) ~f:(fun x ->
      match Set_once.get replaced with
      | Some () -> x
      | None ->
        if String.(<>) x from
        then x
        else begin
          Set_once.set_exn replaced [%here] ();
          to_
        end)
  let pop_help = function
    | "help" :: t -> t
    | _ -> assert false
  let to_string_dots t =
    let t =
      match t with
      | [] -> []
      | last :: init -> last :: List.map init ~f:(Fn.const ".")
    in
    to_string t
end

let%test_unit _ =
  let path =
    Path.empty
    |> Path.add ~subcommand:"foo"
    |> Path.add ~subcommand:"bar"
    |> Path.add ~subcommand:"bar"
    |> Path.add ~subcommand:"baz"
  in
  [%test_result: string list] (Path.commands path) ~expect:["foo"; "bar"; "bar"; "baz"];
  let path = Path.replace_first path ~from:"bar" ~to_:"qux" in
  [%test_result: string list] (Path.commands path) ~expect:["foo"; "qux"; "bar"; "baz"];
  ()

module Anons = struct

  module Grammar : sig
    type t

    val zero : t
    val one : string -> t
    val many : t -> t
    val maybe : t -> t
    val concat : t list -> t
    val usage : t -> string
    val ad_hoc : usage:string -> t

    include Invariant.S with type t := t

    module Sexpable : sig
      module V1 : sig
        type t =
          | Zero
          | One of string
          | Many of t
          | Maybe of t
          | Concat of t list
          | Ad_hoc of string
        [@@deriving bin_io, compare, sexp]

        val usage : t -> string
      end

      type t = V1.t [@@deriving bin_io, compare, sexp]
    end
    val to_sexpable : t -> Sexpable.t

    val names : t -> string list

  end = struct

    module Sexpable = struct
      module V1 = struct
        type t =
          | Zero
          | One of string
          | Many of t
          | Maybe of t
          | Concat of t list
          | Ad_hoc of string
        [@@deriving bin_io, compare, sexp]

        let rec invariant t = Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
          match t with
          | Zero -> ()
          | One _ -> ()
          | Many Zero -> failwith "Many Zero should be just Zero"
          | Many t -> invariant t
          | Maybe Zero -> failwith "Maybe Zero should be just Zero"
          | Maybe t -> invariant t
          | Concat [] | Concat [ _ ] -> failwith "Flatten zero and one-element Concat"
          | Concat ts -> List.iter ts ~f:invariant
          | Ad_hoc _ -> ())
        ;;

        let t_of_sexp sexp =
          let t = [%of_sexp: t] sexp in
          invariant t;
          t
        ;;

        let rec usage = function
          | Zero -> ""
          | One usage -> usage
          | Many Zero -> failwith "bug in command.ml"
          | Many (One _ as t) -> sprintf "[%s ...]" (usage t)
          | Many t -> sprintf "[(%s) ...]" (usage t)
          | Maybe Zero -> failwith "bug in command.ml"
          | Maybe t -> sprintf "[%s]" (usage t)
          | Concat ts -> String.concat ~sep:" " (List.map ts ~f:usage)
          | Ad_hoc usage -> usage
        ;;
      end
      include V1
    end

    type t = Sexpable.V1.t =
      | Zero
      | One of string
      | Many of t
      | Maybe of t
      | Concat of t list
      | Ad_hoc of string

    let to_sexpable = Fn.id
    let invariant = Sexpable.V1.invariant
    let usage = Sexpable.V1.usage

    let rec is_fixed_arity = function
      | Zero     -> true
      | One _    -> true
      | Many _   -> false
      | Maybe _  -> false
      | Ad_hoc _ -> false
      | Concat ts ->
        match List.rev ts with
        | [] -> failwith "bug in command.ml"
        | last :: others ->
          assert (List.for_all others ~f:is_fixed_arity);
          is_fixed_arity last
    ;;

    let rec names = function
      | Zero      -> []
      | One s     -> [ s ]
      | Many t    -> names t
      | Maybe t   -> names t
      | Ad_hoc s  -> [ s ]
      | Concat ts -> List.concat_map ts ~f:names
    ;;

    let zero = Zero
    let one name = One name

    let many = function
      | Zero -> Zero (* strange, but not non-sense *)
      | t ->
        if not (is_fixed_arity t)
        then failwithf "iteration of variable-length grammars such as %s is disallowed"
               (usage t) ();
        Many t
    ;;

    let maybe = function
      | Zero -> Zero (* strange, but not non-sense *)
      | t -> Maybe t
    ;;

    let concat = function
      | [] -> Zero
      | car :: cdr ->
        let car, cdr =
          List.fold cdr ~init:(car, []) ~f:(fun (t1, acc) t2 ->
            match t1, t2 with
            | Zero, t | t, Zero -> (t, acc)
            | _, _ ->
              if is_fixed_arity t1
              then (t2, t1 :: acc)
              else
                failwithf "the grammar %s for anonymous arguments \
                           is not supported because there is the possibility for \
                           arguments (%s) following a variable number of \
                           arguments (%s).  Supporting such grammars would complicate \
                           the implementation significantly."
                  (usage (Concat (List.rev (t2 :: t1 :: acc))))
                  (usage t2)
                  (usage t1)
                  ())
        in
        match cdr with
        | [] -> car
        | _ :: _ -> Concat (List.rev (car :: cdr))
    ;;

    let ad_hoc ~usage = Ad_hoc usage

  end

  module Parser : sig
    type +'a t
    val from_env : (Env.t -> 'a) -> 'a t
    val one : name:string -> 'a Arg_type.t -> 'a t
    val maybe : 'a t -> 'a option t
    val sequence : 'a t -> 'a list t
    val final_value : 'a t -> Env.t -> 'a
    val consume : 'a t -> string -> for_completion:bool -> (Env.t -> Env.t) * 'a t
    val complete : 'a t -> Env.t -> part:string -> never_returns
    module For_opening : sig
      val return : 'a -> 'a t
      val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
      val (>>|) : 'a t -> ('a -> 'b) -> 'b t
    end
  end = struct

    type 'a t =
      | Done of (Env.t -> 'a)
      | More of 'a more
      (* A [Test] will (generally) return a [Done _] value if there is no more input and
         a [More] parser to use if there is any more input. *)
      | Test of (more:bool -> 'a t)
      (* If we're only completing, we can't pull values out, but we can still step through
         [t]s (which may have completion set up). *)
      | Only_for_completion of packed list

    and 'a more = {
      name : string;
      parse : string -> for_completion:bool -> (Env.t -> Env.t) * 'a t;
      complete : Completer.t;
    }

    and packed = Packed : 'a t -> packed

    let return a = Done (fun _ -> a)

    let from_env f = Done f

    let pack_for_completion = function
      | Done _ -> [] (* won't complete or consume anything *)
      | More _ | Test _ as x -> [Packed x]
      | Only_for_completion ps -> ps

    let rec (<*>) tf tx =
      match tf with
      | Done f ->
        begin match tx with
        | Done x -> Done (fun env -> f env (x env))
        | Test test -> Test (fun ~more -> tf <*> test ~more)
        | More {name; parse; complete} ->
          let parse arg ~for_completion =
            let (upd, tx') = parse arg ~for_completion in
            (upd, tf <*> tx')
          in
          More {name; parse; complete}
        | Only_for_completion packed ->
          Only_for_completion packed
        end
      | Test test -> Test (fun ~more -> test ~more <*> tx)
      | More {name; parse; complete} ->
        let parse arg ~for_completion =
          let (upd, tf') = parse arg ~for_completion in
          (upd, tf' <*> tx)
        in
        More {name; parse; complete}
      | Only_for_completion packed ->
        Only_for_completion (packed @ pack_for_completion tx)

    let (>>|) t f = return f <*> t

    let one_more ~name {Arg_type.complete; parse = of_string; key} =
      let parse anon ~for_completion =
        match of_string anon with
        | Error exn ->
          if for_completion then
            (* we don't *really* care about this value, so just put in a dummy value so
               completion can continue *)
            (Fn.id, Only_for_completion [])
          else
            die "failed to parse %s value %S\n%s" name anon (Exn.to_string exn) ()
        | Ok v ->
          let update env =
            Option.fold key ~init:env ~f:(fun env key -> Env.multi_add env key v)
          in
          (update, return v)
      in
      More {name; parse; complete}

    let one ~name arg_type =
      Test (fun ~more ->
        if more then
          one_more ~name arg_type
        else
          die "missing anonymous argument: %s" name ())

    let maybe t =
      Test (fun ~more ->
        if more
        then t >>| fun a -> Some a
        else return None)

    let sequence t =
      let rec loop =
        Test (fun ~more ->
          if more then
            return (fun v acc -> v :: acc) <*> t <*> loop
          else
            return [])
      in
      loop

    let rec final_value t env =
      match t with
      | Done a -> a env
      | Test f -> final_value (f ~more:false) env
      | More {name; _} -> die "missing anonymous argument: %s" name ()
      | Only_for_completion _ ->
        failwith "BUG: asked for final value when doing completion"

    let rec consume
      : type a . a t -> string -> for_completion:bool -> ((Env.t -> Env.t) * a t)
      = fun t arg ~for_completion ->
        match t with
        | Done _ -> die "too many anonymous arguments" ()
        | Test f -> consume (f ~more:true) arg ~for_completion
        | More {parse; _} -> parse arg ~for_completion
        | Only_for_completion packed ->
          match packed with
          | [] -> (Fn.id, Only_for_completion [])
          | (Packed t) :: rest ->
            let (upd, t) = consume t arg ~for_completion in
            (upd, Only_for_completion (pack_for_completion t @ rest))

    let rec complete
      : type a . a t -> Env.t -> part:string -> never_returns
      = fun t env ~part ->
        match t with
        | Done _ -> exit 0
        | Test f -> complete (f ~more:true) env ~part
        | More {complete; _} -> Completer.run_and_exit complete env ~part
        | Only_for_completion t ->
          match t with
          | [] -> exit 0
          | (Packed t) :: _ -> complete t env ~part

    module For_opening = struct
      let return = return
      let (<*>) = (<*>)
      let (>>|) = (>>|)
    end
  end

  open Parser.For_opening

  type 'a t = {
    p : 'a Parser.t;
    grammar : Grammar.t;
  }

  let t2 t1 t2 = {
    p =
      return (fun a1 a2 -> (a1, a2))
      <*> t1.p
      <*> t2.p
  ;
    grammar = Grammar.concat [t1.grammar; t2.grammar];
  }

  let t3 t1 t2 t3 = {
    p =
      return (fun a1 a2 a3 -> (a1, a2, a3))
      <*> t1.p
      <*> t2.p
      <*> t3.p
  ;
    grammar = Grammar.concat [t1.grammar; t2.grammar; t3.grammar];
  }

  let t4 t1 t2 t3 t4 = {
    p =
      return (fun a1 a2 a3 a4 -> (a1, a2, a3, a4))
      <*> t1.p
      <*> t2.p
      <*> t3.p
      <*> t4.p
  ;
    grammar = Grammar.concat [t1.grammar; t2.grammar; t3.grammar; t4.grammar];
  }

  let normalize str =
    (* Verify the string is not empty or surrounded by whitespace *)
    let strlen = String.length str in
    if strlen = 0 then failwith "Empty anonymous argument name provided";
    if String.(<>) (String.strip str) str then
      failwithf "argument name %S has surrounding whitespace" str ();
    (* If the string contains special surrounding characters, don't do anything *)
    let has_special_chars =
      let special_chars = Char.Set.of_list ['<'; '>'; '['; ']'; '('; ')'; '{'; '}'] in
      String.exists str ~f:(Set.mem special_chars)
    in
    if has_special_chars then str else String.uppercase str

  let%test _ = String.equal (normalize "file")   "FILE"
  let%test _ = String.equal (normalize "FiLe")   "FILE"
  let%test _ = String.equal (normalize "<FiLe>") "<FiLe>"
  let%test _ = String.equal (normalize "(FiLe)") "(FiLe)"
  let%test _ = String.equal (normalize "[FiLe]") "[FiLe]"
  let%test _ = String.equal (normalize "{FiLe}") "{FiLe}"
  let%test _ = String.equal (normalize "<file" ) "<file"
  let%test _ = String.equal (normalize "<fil>a") "<fil>a"
  let%test _ = try ignore (normalize ""        ); false with _ -> true
  let%test _ = try ignore (normalize " file "  ); false with _ -> true
  let%test _ = try ignore (normalize "file "   ); false with _ -> true
  let%test _ = try ignore (normalize " file"   ); false with _ -> true

  let (%:) name arg_type =
    let name = normalize name in
    { p = Parser.one ~name arg_type; grammar = Grammar.one name; }

  let map_anons t ~f = {
    p = t.p >>| f;
    grammar = t.grammar;
  }

  let maybe t = {
    p = Parser.maybe t.p;
    grammar = Grammar.maybe t.grammar;
  }

  let maybe_with_default default t =
    let t = maybe t in
    { t with p = t.p >>| fun v -> Option.value ~default v }

  let sequence t = {
    p = Parser.sequence t.p;
    grammar = Grammar.many t.grammar;
  }

  let non_empty_sequence_as_pair t = t2 t (sequence t)

  let non_empty_sequence_as_list t =
    let t = non_empty_sequence_as_pair t in
    { t with p = t.p >>| fun (x, xs) -> x :: xs }

  module Deprecated = struct
    let ad_hoc ~usage_arg = {
      p = Parser.sequence (Parser.one ~name:"WILL NEVER BE PRINTED" Arg_type.string);
      grammar = Grammar.ad_hoc ~usage:usage_arg
    }
  end

end

module Cmdline = struct
  type t = Nil | Cons of string * t | Complete of string

  let of_list args =
    List.fold_right args ~init:Nil ~f:(fun arg args -> Cons (arg, args))

  let rec to_list = function
    | Nil -> []
    | Cons (x, xs) -> x :: to_list xs
    | Complete x -> [x]

  let rec ends_in_complete = function
    | Complete _ -> true
    | Nil -> false
    | Cons (_, args) -> ends_in_complete args

  let extend t ~extend ~path =
    if ends_in_complete t then t else begin
      let path_list = Option.value ~default:[] (List.tl (Path.commands path)) in
      of_list (to_list t @ extend path_list)
    end

end

let%test_module "Cmdline.extend" =
  (module struct
    let path_of_list subcommands =
      List.fold subcommands ~init:(Path.root "exe") ~f:(fun path subcommand ->
        Path.add path ~subcommand)

    let extend path =
      match path with
      | ["foo"; "bar"] -> ["-foo"; "-bar"]
      | ["foo"; "baz"] -> ["-foobaz"]
      | _ -> ["default"]

    let test path args expected =
      let expected = Cmdline.of_list expected in
      let observed =
        let path = path_of_list path in
        let args = Cmdline.of_list args in
        Cmdline.extend args ~extend ~path
      in
      Pervasives.(=) expected observed

    let%test _ = test ["foo"; "bar"] ["anon"; "-flag"] ["anon"; "-flag"; "-foo"; "-bar"]
    let%test _ = test ["foo"; "baz"] []                ["-foobaz"]
    let%test _ = test ["zzz"]        ["x"; "y"; "z"]   ["x"; "y"; "z"; "default"]
  end)

module Key_type = struct
  type t = Subcommand | Flag
  let to_string = function
    | Subcommand -> "subcommand"
    | Flag       -> "flag"
end

let assert_no_underscores key_type flag_or_subcommand =
  if String.exists flag_or_subcommand ~f:(fun c -> c = '_') then
    failwithf "%s %s contains an underscore. Use a dash instead."
      (Key_type.to_string key_type) flag_or_subcommand ()

let normalize key_type key =
  assert_no_underscores key_type key;
  match key_type with
  | Key_type.Flag ->
    if String.equal key "-" then failwithf "invalid key name: %S" key ();
    if String.is_prefix ~prefix:"-" key then key else "-" ^ key
  | Key_type.Subcommand -> String.lowercase key

let lookup_expand alist prefix key_type =
  match
    List.filter alist ~f:(function
      | (key, (_, `Full_match_required)) -> String.(=) key prefix
      | (key, (_, `Prefix)) -> String.is_prefix key ~prefix)
  with
  | [(key, (data, _name_matching))] -> Ok (key, data)
  | [] ->
    Error (sprintf !"unknown %{Key_type} %s" key_type prefix)
  | matches ->
    match List.find matches ~f:(fun (key, _) -> String.(=) key prefix) with
    | Some (key, (data, _name_matching)) -> Ok (key, data)
    | None ->
      let matching_keys = List.map ~f:fst matches in
      Error (sprintf !"%{Key_type} %s is an ambiguous prefix: %s"
               key_type prefix (String.concat ~sep:", " matching_keys))

let lookup_expand_with_aliases map prefix key_type =
  let alist =
    List.concat_map (String.Map.data map) ~f:(fun flag ->
      let
        { Flag.Internal. name; aliases; action=_; doc=_; check_available=_; name_matching }
        = flag
      in
      let data = (flag, name_matching) in
      (name, data) :: List.map aliases ~f:(fun alias -> (alias, data)))
  in
  match List.find_a_dup alist ~compare:(fun (s1, _) (s2, _) -> String.compare s1 s2) with
  | None -> lookup_expand alist prefix key_type
  | Some (flag, _) -> failwithf "multiple flags named %s" flag ()

module Base = struct

  type t = {
    summary : string;
    readme : (unit -> string) option;
    flags : Flag.Internal.t String.Map.t;
    anons : unit -> ([`Parse_args] -> [`Run_main] -> unit) Anons.Parser.t;
    usage : Anons.Grammar.t;
  }

  module Deprecated = struct
    let subcommand_cmp_fst (a, _) (c, _) =
      help_screen_compare a c

    let flags_help ?(display_help_flags = true) t =
      let flags = String.Map.data t.flags in
      let flags =
        if display_help_flags
        then flags
        else List.filter flags ~f:(fun f -> f.name <> "-help")
      in
      List.concat_map ~f:Flag.Internal.Deprecated.help flags
  end

  let formatted_flags t =
    String.Map.data t.flags
    |> List.map ~f:Flag.Internal.align
    (* this sort puts optional flags after required ones *)
    |> List.sort ~compare:(fun a b -> String.compare a.Format.V1.name b.name)
    |> Format.V1.sort

  let help_text ~path t =
    unparagraphs
      (List.filter_opt [
         Some t.summary;
         Some ("  " ^ Path.to_string path ^ " " ^ Anons.Grammar.usage t.usage);
         Option.map t.readme ~f:(fun readme -> readme ());
         Some "=== flags ===";
         Some (Format.V1.to_string (formatted_flags t));
       ])

  module Sexpable = struct

    module V2 = struct
      type anons =
        | Usage of string
        | Grammar of Anons.Grammar.Sexpable.V1.t
      [@@deriving sexp]

      type t = {
        summary : string;
        readme  : string sexp_option;
        anons   : anons;
        flags   : Format.V1.t list;
      } [@@deriving sexp]

    end

    module V1 = struct
      type t = {
        summary : string;
        readme  : string sexp_option;
        usage   : string;
        flags   : Format.V1.t list;
      } [@@deriving sexp]

      let to_latest { summary; readme; usage; flags; } = {
        V2.
        summary;
        readme;
        anons = Usage usage;
        flags;
      }

      let of_latest { V2.summary; readme; anons; flags; } = {
        summary;
        readme;
        usage =
          begin match anons with
          | Usage usage -> usage
          | Grammar grammar -> Anons.Grammar.Sexpable.V1.usage grammar
          end;
        flags;
      }

    end

    include V2
  end

  let to_sexpable t = {
    Sexpable.
    summary = t.summary;
    readme  = Option.map t.readme ~f:(fun readme -> readme ());
    anons   = Grammar (Anons.Grammar.to_sexpable t.usage);
    flags   = formatted_flags t;
  }

  let path_key = Env.key_create "path"
  let args_key = Env.key_create "args"
  let help_key = Env.key_create "help"

  let run t env ~path ~args =
    let help_text = lazy (help_text ~path t) in
    let env = Env.set env path_key path in
    let env = Env.set env args_key (Cmdline.to_list args) in
    let env = Env.set env help_key help_text in
    let rec loop env anons = function
      | Cmdline.Nil ->
        List.iter (String.Map.data t.flags) ~f:(fun flag ->
          match flag.check_available with
          | `Optional -> ()
          | `Required check -> check env);
        Anons.Parser.final_value anons env
      | Cons ("-anon", Cons (arg, args)) ->
        (* the very special -anon flag is here as an escape hatch in case you have an
           anonymous argument that starts with a hyphen. *)
        anon env anons arg args
      | Cons (arg, args) ->
        if String.is_prefix arg ~prefix:"-"
        && not (String.equal arg "-") (* support the convention where "-" means stdin *)
        then begin
          let flag = arg in
          let (flag, { Flag.Internal. action; name=_; aliases=_; doc=_; check_available=_;
                       name_matching=_ }) =
            match lookup_expand_with_aliases t.flags flag Key_type.Flag with
            | Error msg -> die "%s" msg ()
            | Ok x -> x
          in
          match action with
          | No_arg f ->
            let env = f env in
            loop env anons args
          | Arg (f, comp) ->
            begin match args with
            | Nil -> die "missing argument for flag %s" flag ()
            | Cons (arg, rest) ->
              let env =
                try f env arg with
                | Failed_to_parse_command_line _ as e ->
                  if Cmdline.ends_in_complete rest then env else raise e
              in
              loop env anons rest
            | Complete part ->
              never_returns (Completer.run_and_exit comp env ~part)
            end
          | Rest f ->
            if Cmdline.ends_in_complete args then exit 0;
            let env = f env (Cmdline.to_list args) in
            loop env anons Nil
        end else
          anon env anons arg args
      | Complete part ->
        if String.is_prefix part ~prefix:"-" then begin
          List.iter (String.Map.keys t.flags) ~f:(fun name ->
            if String.is_prefix name ~prefix:part then print_endline name);
          exit 0
        end else
          never_returns (Anons.Parser.complete anons env ~part);
    and anon env anons arg args =
      let (env_upd, anons) =
        Anons.Parser.consume anons arg ~for_completion:(Cmdline.ends_in_complete args)
      in
      let env = env_upd env in
      loop env anons args
    in
    match Result.try_with (fun () -> loop env (t.anons ()) args `Parse_args) with
    | Ok thunk -> thunk `Run_main
    | Error exn ->
      match exn with
      | Failed_to_parse_command_line _ when Cmdline.ends_in_complete args ->
        exit 0
      | _ ->
        print_endline "Error parsing command line.  Run with -help for usage information.";
        (match exn with
         | Failed_to_parse_command_line msg ->
           prerr_endline msg;
         | _ ->
           prerr_endline (Sexp.to_string_hum ([%sexp (exn : exn)])));
        exit 1

  module Spec = struct

    type ('a, 'b) t = {
      f     : unit -> ('a -> 'b) Anons.Parser.t;
      usage : unit -> Anons.Grammar.t;
      flags : unit -> Flag.Internal.t list;
    }

    (* the (historical) reason that [param] is defined in terms of [t] rather than the
       other way round is that the delayed evaluation mattered for sequencing of
       read/write operations on ref cells in the old representation of flags *)
    type 'a param = { param : 'm. ('a -> 'm, 'm) t }

    open Anons.Parser.For_opening

    let app t1 t2 ~f = {
      f = (fun () ->
        return f
        <*> t1.f ()
        <*> t2.f ()
      );
      flags = (fun () -> t2.flags () @ t1.flags ());
      usage = (fun () -> Anons.Grammar.concat [t1.usage (); t2.usage ()]);
    }

    (* So sad.  We can't define [apply] in terms of [app] because of the value
       restriction. *)
    let apply pf px = {
      param = {
        f = (fun () ->
          return (fun mf mx k -> mf (fun f -> (mx (fun x -> k (f x)))))
          <*> pf.param.f ()
          <*> px.param.f ()
        );
        flags = (fun () -> px.param.flags () @ pf.param.flags ());
        usage = (fun () -> Anons.Grammar.concat [pf.param.usage (); px.param.usage ()]);
      }
    }

    let (++) t1 t2 = app t1 t2 ~f:(fun f1 f2 x -> f2 (f1 x))
    let (+>) t1 p2 = app t1 p2.param ~f:(fun f1 f2 x -> f2 (f1 x))
    let (+<) t1 p2 = app p2.param t1 ~f:(fun f2 f1 x -> f1 (f2 x))

    let step f = {
      f = (fun () -> return f);
      flags = (fun () -> []);
      usage = (fun () -> Anons.Grammar.zero);
    }

    let empty : 'm. ('m, 'm) t = {
      f = (fun () -> return Fn.id);
      flags = (fun () -> []);
      usage = (fun () -> Anons.Grammar.zero);
    }

    let const v =
      { param =
          { f = (fun () -> return (fun k -> k v));
            flags = (fun () -> []);
            usage = (fun () -> Anons.Grammar.zero); } }

    let map p ~f =
      { param =
          { f = (fun () -> p.param.f () >>| fun c k -> c (fun v -> k (f v)));
            flags = p.param.flags;
            usage = p.param.usage; } }

    let wrap f t =
      { f = (fun () -> t.f () >>| fun run main -> f ~run ~main);
        flags = t.flags;
        usage = t.usage; }

    let of_params params =
      let t = params.param in
      { f = (fun () -> t.f () >>| fun run main -> run Fn.id main);
        flags = t.flags;
        usage = t.usage; }

    let to_params (t : ('a, 'b) t) : ('a -> 'b) param =
      { param = {
          f = (fun () -> t.f () >>| fun f k -> k f);
          flags = t.flags;
          usage = t.usage;
        }
      }

    let of_param p = p.param

    let to_param t main = map (to_params t) ~f:(fun k -> k main)

    let lookup key =
      { param =
          { f = (fun () -> Anons.Parser.from_env (fun env m -> m (Env.find_exn env key)));
            flags = (fun () -> []);
            usage = (fun () -> Anons.Grammar.zero);
          }
      }

    let path : Path.t        param = lookup path_key
    let args : string list   param = lookup args_key
    let help : string Lazy.t param = lookup help_key

    (* This is only used internally, for the help command. *)
    let env =
      { param =
          { f = (fun () -> Anons.Parser.from_env (fun env m -> m env));
            flags = (fun () -> []);
            usage = (fun () -> Anons.Grammar.zero);
          }
      }

    include struct
      module Arg_type = Arg_type
      include Arg_type.Export
    end

    include struct
      open Anons
      type 'a anons = 'a t
      let (%:)                       = (%:)
      let map_anons                  = map_anons
      let maybe                      = maybe
      let maybe_with_default         = maybe_with_default
      let sequence                   = sequence
      let non_empty_sequence_as_pair = non_empty_sequence_as_pair
      let non_empty_sequence_as_list = non_empty_sequence_as_list
      let t2                         = t2
      let t3                         = t3
      let t4                         = t4

      let anon spec =
        Anons.Grammar.invariant spec.grammar;
        {
          param = {
            f = (fun () -> spec.p >>| fun v k -> k v);
            flags = (fun () -> []);
            usage = (fun () -> spec.grammar);
          }
        }
    end

    include struct
      open Flag
      type 'a flag = 'a t
      let map_flag              = map_flag
      let escape                = escape
      let listed                = listed
      let one_or_more           = one_or_more
      let no_arg                = no_arg
      let no_arg_register       = no_arg_register
      let no_arg_abort          = no_arg_abort
      let optional              = optional
      let optional_with_default = optional_with_default
      let required              = required

      let flag ?(aliases = []) ?full_flag_required name mode ~doc =
        let normalize flag = normalize Key_type.Flag flag in
        let name = normalize name in
        let aliases = List.map ~f:normalize aliases in
        let {read; action; optional} = mode name in
        let check_available =
          if optional then `Optional else `Required (fun env -> ignore (read env))
        in
        let name_matching =
          if Option.is_some full_flag_required then `Full_match_required else `Prefix
        in
        { param =
            { f = (fun () -> Anons.Parser.from_env (fun env m -> m (read env)));
              flags = (fun () -> [{ name; aliases; doc; action;
                                    check_available; name_matching }]);
              usage = (fun () -> Anons.Grammar.zero);
            }
        }

      let flag_optional_with_default_doc
            ?aliases ?full_flag_required name arg_type sexp_of_default ~default ~doc =
        flag ?aliases ?full_flag_required name (optional_with_default default arg_type)
          ~doc:(sprintf !"%s (default: %{Sexp})" doc (sexp_of_default default))
      ;;

      include Applicative.Make (struct
          type nonrec 'a t = 'a param
          let return = const
          let apply = apply
          let map = `Custom map
        end)

      let pair = both
    end

    let flags_of_args_exn args =
      List.fold args ~init:empty ~f:(fun acc (name, spec, doc) ->
        let gen f flag_type = step (fun m x -> f x; m) +> flag name flag_type ~doc in
        let call f arg_type = gen (fun x -> Option.iter x ~f) (optional arg_type) in
        let set r arg_type = call (fun x -> r := x) arg_type in
        let set_bool r b = gen (fun passed -> if passed then r := b) no_arg in
        acc ++ begin
          match spec with
          | Arg.Unit f -> gen (fun passed -> if passed then f ()) no_arg
          | Arg.Set   r -> set_bool r true
          | Arg.Clear r -> set_bool r false
          | Arg.String     f -> call f string
          | Arg.Set_string r -> set  r string
          | Arg.Int        f -> call f int
          | Arg.Set_int    r -> set  r int
          | Arg.Float      f -> call f float
          | Arg.Set_float  r -> set  r float
          | Arg.Bool       f -> call f bool
          | Arg.Symbol (syms, f) ->
            let arg_type =
              Arg_type.of_alist_exn (List.map syms ~f:(fun sym -> (sym, sym)))
            in
            call f arg_type
          | Arg.Rest f -> gen (fun x -> Option.iter x ~f:(List.iter ~f)) escape
          | Arg.Tuple _ ->
            failwith "Arg.Tuple is not supported by Command.Spec.flags_of_args_exn"
          | Arg.Expand _ [@if ocaml_version >= (4, 05, 0)] ->
            failwith "Arg.Expand is not supported by Command.Spec.flags_of_args_exn"
        end)

    module Deprecated = struct
      include Flag.Deprecated
      include Anons.Deprecated
    end

    let to_string_for_choose_one param =
      let t = param.param in
      let flag_names = Map.keys (Flag.Internal.create (t.flags ())) in
      let anon_names = Anons.Grammar.names (t.usage ()) in
      let names = List.concat [ flag_names; anon_names; ] in
      let names_with_commas = List.filter names ~f:(fun s -> String.contains s ',') in
      if not (List.is_empty names_with_commas) then
        failwiths
          "For simplicity, [Command.Spec.choose_one] does not support names with commas."
          names_with_commas [%sexp_of: string list];
      String.concat ~sep:"," names
    ;;

    let choose_one ts ~if_nothing_chosen =
      let ts = List.map ts ~f:(fun t -> to_string_for_choose_one t, t) in
      Option.iter (List.find_a_dup (List.map ~f:fst ts) ~compare:String.compare)
        ~f:(fun name ->
          failwiths "Command.Spec.choose_one called with duplicate name" name
            [%sexp_of: string]);
      List.fold ts ~init:(return None) ~f:(fun init (name, t) ->
        map2 init t ~f:(fun init value ->
          match value with
          | None -> init
          | Some value ->
            match init with
            | None -> Some (name, value)
            | Some (name', _) ->
              die "Cannot have values for both %s and %s" name name' ()))
      |> map ~f:(function
        | Some (_, value) -> value
        | None ->
          match if_nothing_chosen with
          | `Default_to value -> value
          | `Raise ->
            die "One of these must have a value: %s"
              (String.concat ~sep:", " (List.map ~f:fst ts)) ())
    ;;

    let%test_unit "choose_one" =
      let should_raise reason flags =
        match choose_one flags ~if_nothing_chosen:`Raise with
        | exception _ -> ()
        | _ -> failwiths "failed to raise despite" reason [%sexp_of: string]
      in
      should_raise "duplicate names" [
        flag "-foo" (optional int) ~doc:"";
        flag "-foo" (optional int) ~doc:"";
      ];
    ;;

  end
end

let group_or_exec_help_text ~show_flags ~path ~summary ~readme ~format_list =
  unparagraphs (List.filter_opt [
    Some summary;
    Some (String.concat ["  "; Path.to_string path; " SUBCOMMAND"]);
    Option.map readme ~f:(fun readme -> readme ());
    Some
      (if show_flags
       then "=== subcommands and flags ==="
       else "=== subcommands ===");
    Some (Format.V1.to_string format_list);
  ])
;;

module Group = struct
  type 'a t = {
    summary     : string;
    readme      : (unit -> string) option;
    subcommands : (string * 'a) list Lazy.t;
    body        : (path:string list -> unit) option;
  }

  let help_text ~show_flags ~to_format_list ~path t =
    group_or_exec_help_text
      ~show_flags
      ~path
      ~readme:t.readme
      ~summary:t.summary
      ~format_list:(to_format_list t)
  ;;

  module Sexpable = struct
    module V2 = struct
      type 'a t = {
        summary     : string;
        readme      : string sexp_option;
        subcommands : (string, 'a) List.Assoc.t Lazy.t;
      } [@@deriving sexp]

      let map t ~f =
        { t with subcommands = Lazy.map t.subcommands ~f:(List.Assoc.map ~f) }
      ;;
    end

    module Latest = V2

    module V1 = struct
      type 'a t = {
        summary     : string;
        readme      : string sexp_option;
        subcommands : (string, 'a) List.Assoc.t;
      } [@@deriving sexp]

      let map t ~f =
        { t with subcommands = List.Assoc.map t.subcommands ~f }
      ;;

      let to_latest { summary; readme; subcommands } : 'a Latest.t =
        { summary; readme; subcommands = Lazy.from_val subcommands }
      ;;

      let of_latest ({ summary; readme; subcommands } : 'a Latest.t) : 'a t =
        { summary; readme; subcommands = Lazy.force subcommands }
      ;;
    end

    include Latest
  end

  let to_sexpable ~subcommand_to_sexpable t =
    { Sexpable.
      summary = t.summary;
      readme  = Option.map ~f:(fun readme -> readme ()) t.readme;
      subcommands = Lazy.map t.subcommands ~f:(List.Assoc.map ~f:subcommand_to_sexpable);
    }
end

let abs_path ~dir path =
  if Filename.is_absolute path
  then path
  else Filename.concat dir path
;;

let%test_unit _ = [
  "/",    "./foo",         "/foo";
  "/tmp", "/usr/bin/grep", "/usr/bin/grep";
  "/foo", "bar",           "/foo/bar";
  "foo",  "bar",           "foo/bar";
  "foo",  "../bar",        "foo/../bar";
] |> List.iter ~f:(fun (dir, path, expected) ->
  [%test_eq: string] (abs_path ~dir path) expected)

let comp_cword = "COMP_CWORD"

(* clear the setting of environment variable associated with command-line
   completion and recursive help so that subprocesses don't see them. *)
let getenv_and_clear var =
  let value = Core_sys.getenv var in
  if Option.is_some value then Unix.unsetenv var;
  value
;;

let maybe_comp_cword () =
  getenv_and_clear comp_cword
  |> Option.map ~f:Int.of_string
;;

let set_comp_cword new_value =
  let new_value = Int.to_string new_value in
  Unix.putenv ~key:comp_cword ~data:new_value
;;

module Exec = struct
  type t = {
    summary          : string;
    readme           : (unit -> string) option;
    (* If [path_to_exe] is relative, interpret w.r.t. [working_dir] *)
    working_dir      : string;
    path_to_exe      : string;
    child_subcommand : string list;
  }

  module Sexpable = struct
    module V3 = struct
      type t = {
        summary          : string;
        readme           : string sexp_option;
        working_dir      : string;
        path_to_exe      : string;
        child_subcommand : string list;
      } [@@deriving sexp]

      let to_latest = Fn.id
      let of_latest = Fn.id
    end

    module V2 = struct
      type t = {
        summary     : string;
        readme      : string sexp_option;
        working_dir : string;
        path_to_exe : string;
      } [@@deriving sexp]

      let to_v3 t : V3.t = {
        summary = t.summary;
        readme = t.readme;
        working_dir = t.working_dir;
        path_to_exe = t.path_to_exe;
        child_subcommand = [];
      }

      let of_v3 (t : V3.t) = {
        summary = t.summary;
        readme = t.readme;
        working_dir = t.working_dir;
        path_to_exe = abs_path ~dir:t.working_dir t.path_to_exe;
      }

      let to_latest = Fn.compose V3.to_latest to_v3
      let of_latest = Fn.compose of_v3 V3.of_latest
    end

    module V1 = struct
      type t = {
        summary     : string;
        readme      : string sexp_option;
        (* [path_to_exe] must be absolute. *)
        path_to_exe : string;
      } [@@deriving sexp]

      let to_v2 t : V2.t = {
        summary = t.summary;
        readme = t.readme;
        working_dir = "/";
        path_to_exe = t.path_to_exe;
      }

      let of_v2 (t : V2.t) = {
        summary = t.summary;
        readme = t.readme;
        path_to_exe = abs_path ~dir:t.working_dir t.path_to_exe;
      }

      let to_latest = Fn.compose V2.to_latest to_v2
      let of_latest = Fn.compose of_v2 V2.of_latest

    end

    include V3
  end

  let to_sexpable t =
    { Sexpable.
      summary  = t.summary;
      readme   = Option.map ~f:(fun readme -> readme ()) t.readme;
      working_dir = t.working_dir;
      path_to_exe = t.path_to_exe;
      child_subcommand = t.child_subcommand;
    }

  let exec_with_args t ~args ~maybe_new_comp_cword =
    let prog = abs_path ~dir:t.working_dir t.path_to_exe in
    let args = t.child_subcommand @ args in
    Option.iter maybe_new_comp_cword ~f:(fun n ->
      (* The logic for tracking [maybe_new_comp_cword] doesn't take into account whether
         this exec specifies a child subcommand. If it does, COMP_CWORD needs to be set
         higher to account for the arguments used to specify the child subcommand. *)
      set_comp_cword (n + List.length t.child_subcommand)
    );
    never_returns (Unix.exec ~prog ~argv:(prog :: args) ())
  ;;

  let help_text ~show_flags ~to_format_list ~path t =
    group_or_exec_help_text
      ~show_flags
      ~path
      ~readme:(t.readme)
      ~summary:(t.summary)
      ~format_list:(to_format_list t)
  ;;
end

(* A proxy command is the structure of an Exec command obtained by running it in a
   special way *)
module Proxy = struct

  module Kind = struct
    type 'a t =
      | Base  of Base.Sexpable.t
      | Group of 'a Group.Sexpable.t
      | Exec  of Exec.Sexpable.t
      | Lazy  of 'a t Lazy.t
  end

  type t = {
    working_dir        : string;
    path_to_exe        : string;
    path_to_subcommand : string list;
    child_subcommand   : string list;
    kind               : t Kind.t;
  }

  let rec get_summary_from_kind (kind : t Kind.t) =
    match kind with
    | Base  b -> b.summary
    | Group g -> g.summary
    | Exec  e -> e.summary
    | Lazy  l -> get_summary_from_kind (Lazy.force l)

  let get_summary t = get_summary_from_kind t.kind

  let rec get_readme_from_kind (kind : t Kind.t) =
    match kind with
    | Base  b -> b.readme
    | Group g -> g.readme
    | Exec  e -> e.readme
    | Lazy  l -> get_readme_from_kind (Lazy.force l)

  let get_readme t = get_readme_from_kind t.kind

  let help_text ~show_flags ~to_format_list ~path t =
    group_or_exec_help_text
      ~show_flags
      ~path
      ~readme:(get_readme t |> Option.map ~f:const)
      ~summary:(get_summary t)
      ~format_list:(to_format_list t)
end

type t =
  | Base  of Base.t
  | Group of t Group.t
  | Exec  of Exec.t
  | Proxy of Proxy.t
  | Lazy  of t Lazy.t

module Sexpable = struct

  let supported_versions : int Queue.t = Queue.create ()
  let add_version n = Queue.enqueue supported_versions n

  module V3 = struct
    let () = add_version 3

    type t =
      | Base  of Base.Sexpable.V2.t
      | Group of t Group.Sexpable.V2.t
      | Exec  of Exec.Sexpable.V3.t
      | Lazy  of t Lazy.t
    [@@deriving sexp]

    let to_latest = Fn.id
    let of_latest = Fn.id
  end

  module Latest = V3

  module V2 = struct
    let () = add_version 2

    type t =
      | Base of Base.Sexpable.V2.t
      | Group of t Group.Sexpable.V1.t
      | Exec  of Exec.Sexpable.V2.t
    [@@deriving sexp]

    let rec to_latest : t -> Latest.t = function
      | Base b -> Base b
      | Exec e -> Exec (Exec.Sexpable.V2.to_latest e)
      | Group g ->
        Group (Group.Sexpable.V1.to_latest (Group.Sexpable.V1.map g ~f:to_latest))
    ;;

    let rec of_latest : Latest.t -> t = function
      | Base b -> Base b
      | Exec e -> Exec (Exec.Sexpable.V2.of_latest e)
      | Lazy thunk -> of_latest (Lazy.force thunk)
      | Group g ->
        Group (Group.Sexpable.V1.map (Group.Sexpable.V1.of_latest g) ~f:of_latest)
    ;;

  end

  module V1 = struct
    let () = add_version 1

    type t =
      | Base  of Base.Sexpable.V1.t
      | Group of t Group.Sexpable.V1.t
      | Exec  of Exec.Sexpable.V1.t
    [@@deriving sexp]

    let rec to_latest : t -> Latest.t = function
      | Base b -> Base (Base.Sexpable.V1.to_latest b)
      | Exec e -> Exec (Exec.Sexpable.V1.to_latest e)
      | Group g ->
        Group (Group.Sexpable.V1.to_latest (Group.Sexpable.V1.map g ~f:to_latest))
    ;;

    let rec of_latest : Latest.t -> t = function
      | Base b -> Base (Base.Sexpable.V1.of_latest b)
      | Exec e -> Exec (Exec.Sexpable.V1.of_latest e)
      | Lazy thunk -> of_latest (Lazy.force thunk)
      | Group g ->
        Group (Group.Sexpable.V1.map (Group.Sexpable.V1.of_latest g) ~f:of_latest)
    ;;

  end

  module Internal : sig
    type t [@@deriving sexp]
    val of_latest : version_to_use:int -> Latest.t -> t
    val to_latest : t -> Latest.t
  end = struct
    type t =
      | V1 of V1.t
      | V2 of V2.t
      | V3 of V3.t
    [@@deriving sexp]

    let to_latest = function
      | V1 t -> V1.to_latest t
      | V2 t -> V2.to_latest t
      | V3 t -> V3.to_latest t

    let of_latest ~version_to_use latest =
      match version_to_use with
      | 1 -> V1 (V1.of_latest latest)
      | 2 -> V2 (V2.of_latest latest)
      | 3 -> V3 (V3.of_latest latest)
      | other -> failwiths "unsupported version_to_use" other [%sexp_of: int]
    ;;

  end

  include Latest

  let supported_versions = Int.Set.of_list (Queue.to_list supported_versions)

  let rec get_summary = function
    | Base  x -> x.summary
    | Group x -> x.summary
    | Exec  x -> x.summary
    | Lazy  x -> get_summary (Lazy.force x)

  let extraction_var = "COMMAND_OUTPUT_HELP_SEXP"

  let read_stdout_and_stderr (process_info : Unix.Process_info.t) =
    (* We need to read each of stdout and stderr in a separate thread to avoid deadlocks
       if the child process decides to wait for a read on one before closing the other.
       Buffering may hide this problem until output is "sufficiently large". *)
    let start_reading descr info =
      let output = Set_once.create () in
      let thread = Core_thread.create (fun () ->
        Result.try_with (fun () ->
          descr
          |> Unix.in_channel_of_descr
          |> In_channel.input_all)
        |> Set_once.set_exn output [%here]) ()
      in
      stage (fun () ->
        Core_thread.join thread;
        Unix.close descr;
        match Set_once.get output with
        | None ->
          raise_s [%message "BUG failed to read" (info : Info.t)]
        | Some (Ok output) -> output
        | Some (Error exn) -> raise exn)
    in
    (* We might hang forever trying to join the reading threads if the child process keeps
       the file descriptor open. Not handling this because I think we've never seen it
       in the wild despite running vulnerable code for years. *)
    (* We have to start both threads before joining any of them. *)
    let finish_stdout = start_reading process_info.stdout (Info.of_string "stdout") in
    let finish_stderr = start_reading process_info.stderr (Info.of_string "stderr") in
    unstage finish_stdout (), unstage finish_stderr ()
  ;;

  let of_external ~working_dir ~path_to_exe ~child_subcommand =
    let process_info =
      Unix.create_process_env ()
        ~prog:(abs_path ~dir:working_dir path_to_exe)
        ~args:child_subcommand
        ~env:(`Extend [
          ( extraction_var
          , supported_versions |> Int.Set.sexp_of_t |> Sexp.to_string
          )
        ])
    in
    Unix.close process_info.stdin;
    let stdout, stderr = read_stdout_and_stderr process_info in
    ignore (Unix.wait (`Pid process_info.pid));
    (* Now we've killed all the processes and threads we made. *)
    match
      stdout
      |> Sexp.of_string
      |> Internal.t_of_sexp
      |> Internal.to_latest
    with
    | exception exn ->
      raise_s [%message "cannot parse command shape"
                          ~_:(exn : exn) (stdout : string) (stderr : string)]
    | t -> t
  ;;

  let rec find (t : t) ~path_to_subcommand =
    match path_to_subcommand with
    | [] -> t
    | sub :: subs ->
      match t with
      | Base _ -> failwithf "unexpected subcommand %S" sub ()
      | Lazy thunk -> find (Lazy.force thunk) ~path_to_subcommand
      | Exec {path_to_exe; working_dir; child_subcommand; _} ->
        find
          (of_external ~working_dir ~path_to_exe ~child_subcommand)
          ~path_to_subcommand:(sub :: (subs @ child_subcommand))
      | Group g ->
        match List.Assoc.find (Lazy.force g.subcommands) ~equal:String.equal sub with
        | None -> failwithf "unknown subcommand %S" sub ()
        | Some t -> find t ~path_to_subcommand:subs

end

let rec sexpable_of_proxy_kind (kind : Proxy.t Proxy.Kind.t) =
  match kind with
  | Base  base  -> Sexpable.Base base
  | Exec  exec  -> Sexpable.Exec exec
  | Lazy  thunk -> Sexpable.Lazy (Lazy.map ~f:sexpable_of_proxy_kind thunk)
  | Group group ->
    Sexpable.Group
      { group with
        subcommands =
          Lazy.map group.subcommands
            ~f:(List.map ~f:(fun (str, proxy) ->
              (str, sexpable_of_proxy_kind proxy.Proxy.kind)))
      }

let sexpable_of_proxy proxy = sexpable_of_proxy_kind proxy.Proxy.kind

let rec to_sexpable = function
  | Base  base  -> Sexpable.Base  (Base.to_sexpable base)
  | Exec  exec  -> Sexpable.Exec  (Exec.to_sexpable exec)
  | Proxy proxy -> sexpable_of_proxy proxy
  | Group group ->
    Sexpable.Group (Group.to_sexpable ~subcommand_to_sexpable:to_sexpable group)
  | Lazy  thunk -> Sexpable.Lazy (Lazy.map ~f:to_sexpable thunk)

type ('main, 'result) basic_spec_command
  =  summary:string
  -> ?readme:(unit -> string)
  -> ('main, unit -> 'result) Base.Spec.t
  -> 'main
  -> t

let rec get_summary = function
  | Base  base  -> base.summary
  | Group group -> group.summary
  | Exec  exec  -> exec.summary
  | Proxy proxy -> Proxy.get_summary proxy
  | Lazy  thunk -> get_summary (Lazy.force thunk)

let extend_exn ~mem ~add map key_type ~key data =
  if mem map key then
    failwithf "there is already a %s named %s" (Key_type.to_string key_type) key ();
  add map ~key ~data

let extend_map_exn map key_type ~key data =
  extend_exn map key_type ~key data ~mem:Map.mem ~add:Map.set

let extend_alist_exn alist key_type ~key data =
  extend_exn alist key_type ~key data
    ~mem:(fun alist key -> List.Assoc.mem alist key ~equal:String.equal)
    ~add:(fun alist ~key ~data -> List.Assoc.add alist key data ~equal:String.equal)

module Bailout_dump_flag = struct
  let add base ~name ~aliases ~text ~text_summary =
    let flags = base.Base.flags in
    let flags =
      extend_map_exn flags Key_type.Flag ~key:name
        { name;
          aliases;
          check_available = `Optional;
          action = No_arg (fun env -> print_endline (text env); exit 0);
          doc = sprintf " print %s and exit" text_summary;
          name_matching = `Prefix;
        }
    in
    { base with Base.flags }
end

let basic_spec ~summary ?readme {Base.Spec.usage; flags; f} main =
  let flags = flags () in
  let usage = usage () in
  let anons () =
    let open Anons.Parser.For_opening in
    f ()
    >>| fun k `Parse_args ->
    let thunk = k main in
    fun `Run_main -> thunk ()
  in
  let flags = Flag.Internal.create flags in
  let base = { Base.summary; readme; usage; flags; anons } in
  let base =
    Bailout_dump_flag.add base ~name:"-help" ~aliases:["-?"]
      ~text_summary:"this help text"
      ~text:(fun env -> Lazy.force (Env.find_exn env Base.help_key))
  in
  Base base

let basic = basic_spec

let subs_key : (string * t) list Env.Key.t = Env.key_create "subcommands"

let gather_help ~recursive ~show_flags ~expand_dots sexpable =
  let rec loop rpath acc sexpable =
    let string_of_path =
      if expand_dots
      then Path.to_string
      else Path.to_string_dots
    in
    let gather_exec rpath acc {Exec.Sexpable.working_dir; path_to_exe; child_subcommand; _} =
      loop rpath acc (Sexpable.of_external ~working_dir ~path_to_exe ~child_subcommand)
    in
    let gather_group rpath acc subs =
      let subs =
        if recursive && rpath <> Path.empty
        then List.Assoc.remove ~equal:String.(=) subs "help"
        else subs
      in
      let alist =
        List.stable_sort subs ~compare:(fun a b -> help_screen_compare (fst a) (fst b))
      in
      List.fold alist ~init:acc ~f:(fun acc (subcommand, t) ->
        let rpath = Path.add rpath ~subcommand in
        let key = string_of_path rpath in
        let doc = Sexpable.get_summary t in
        let acc = Fqueue.enqueue acc { Format.V1. name = key; doc; aliases = [] } in
        if recursive
        then loop rpath acc t
        else acc)
    in
    match sexpable with
    | Sexpable.Exec exec -> gather_exec rpath acc exec
    | Sexpable.Lazy thunk -> loop rpath acc (Lazy.force thunk)
    | Sexpable.Group group ->
      gather_group rpath acc (Lazy.force group.Group.Sexpable.subcommands)
    | Sexpable.Base base   ->
      if show_flags then begin
        base.Base.Sexpable.flags
        |> List.filter ~f:(fun fmt -> fmt.Format.V1.name <> "[-help]")
        |> List.fold ~init:acc ~f:(fun acc fmt ->
          let rpath = Path.add rpath ~subcommand:fmt.Format.V1.name in
          let fmt = { fmt with Format.V1.name = string_of_path rpath } in
          Fqueue.enqueue acc fmt)
      end else
        acc
  in
  loop Path.empty Fqueue.empty sexpable
;;

let help_subcommand ~summary ~readme =
  basic ~summary:"explain a given subcommand (perhaps recursively)"
    Base.Spec.(
      empty
      +> flag "-recursive"   no_arg ~doc:" show subcommands of subcommands, etc."
      +> flag "-flags"       no_arg ~doc:" show flags as well in recursive help"
      +> flag "-expand-dots" no_arg ~doc:" expand subcommands in recursive help"
      +> path
      +> env
      +> anon (maybe ("SUBCOMMAND" %: string))
    )
    (fun recursive show_flags expand_dots path (env : Env.t) cmd_opt () ->
       let subs =
         match Env.find env subs_key with
         | Some subs -> subs
         | None -> assert false (* maintained by [dispatch] *)
       in
       let path =
         let path = Path.pop_help path in
         Option.fold cmd_opt ~init:path
           ~f:(fun path subcommand -> Path.add path ~subcommand)
       in
       let format_list t =
         gather_help ~recursive ~show_flags ~expand_dots (to_sexpable t)
         |> Fqueue.to_list
       in
       let group_help_text group ~path =
         let to_format_list g = format_list (Group g) in
         Group.help_text ~show_flags ~to_format_list ~path group
       in
       let exec_help_text exec ~path =
         let to_format_list e = format_list (Exec e) in
         Exec.help_text ~show_flags ~to_format_list ~path exec
       in
       let proxy_help_text proxy ~path =
         let to_format_list p = format_list (Proxy p) in
         Proxy.help_text ~show_flags ~to_format_list ~path proxy
       in
       let text =
         match cmd_opt with
         | None ->
           group_help_text ~path {
             readme;
             summary;
             subcommands = Lazy.from_val subs;
             body = None;
           }
         | Some cmd ->
           match
             lookup_expand (List.Assoc.map subs ~f:(fun x -> (x, `Prefix))) cmd Subcommand
           with
           | Error e ->
             die "unknown subcommand %s for command %s: %s" cmd (Path.to_string path) e ()
           | Ok (possibly_expanded_name, t) ->
             (* Fix the unexpanded value *)
             let path = Path.replace_first ~from:cmd ~to_:possibly_expanded_name path in
             let rec help_text = function
               | Exec  exec  -> exec_help_text exec ~path
               | Group group -> group_help_text group ~path
               | Base  base  -> Base.help_text ~path base
               | Proxy proxy -> proxy_help_text proxy ~path
               | Lazy  thunk -> help_text (Lazy.force thunk)
             in
             help_text t
       in
       print_endline text)

let lazy_group ~summary ?readme ?preserve_subcommand_order ?body alist =
  let subcommands =
    Lazy.map alist ~f:(fun alist ->
      let alist =
        List.map alist ~f:(fun (name, t) -> (normalize Key_type.Subcommand name, t))
      in
      match String.Map.of_alist alist with
      | `Duplicate_key name -> failwithf "multiple subcommands named %s" name ()
      | `Ok map ->
        match preserve_subcommand_order with
        | Some () -> alist
        | None -> Map.to_alist map)
  in
  Group {summary; readme; subcommands; body}

let group ~summary ?readme ?preserve_subcommand_order ?body alist =
  lazy_group ~summary ?readme ?preserve_subcommand_order ?body (Lazy.from_val alist)

let exec ~summary ?readme ?(child_subcommand=[]) ~path_to_exe () =
  let working_dir =
    Filename.dirname @@
    match path_to_exe with
    | `Absolute _ | `Relative_to_me _ -> Sys.executable_name
    | `Relative_to_argv0 _ -> Sys.argv.(0)
  in
  let path_to_exe =
    match path_to_exe with
    | `Absolute p        ->
      if not (Filename.is_absolute p)
      then failwith "Path passed to `Absolute must be absolute"
      else p
    | `Relative_to_me p | `Relative_to_argv0 p ->
      if not (Filename.is_relative p)
      then failwith "Path passed to `Relative_to_me must be relative"
      else p
  in
  Exec {summary; readme; working_dir; path_to_exe; child_subcommand}

let of_lazy thunk = Lazy thunk

module Shape = struct
  module Flag_info = struct

    type t = Format.V1.t = {
      name : string;
      doc : string;
      aliases : string list;
    } [@@deriving bin_io, compare, fields, sexp]

  end

  module Base_info = struct

    type grammar = Anons.Grammar.Sexpable.V1.t =
      | Zero
      | One of string
      | Many of grammar
      | Maybe of grammar
      | Concat of grammar list
      | Ad_hoc of string
    [@@deriving bin_io, compare, sexp]

    type anons = Base.Sexpable.V2.anons =
      | Usage of string
      | Grammar of grammar
    [@@deriving bin_io, compare, sexp]

    type t = Base.Sexpable.V2.t = {
      summary : string;
      readme  : string sexp_option;
      anons   : anons;
      flags   : Flag_info.t list;
    } [@@deriving bin_io, compare, fields, sexp]

  end

  module Group_info = struct

    type 'a t = 'a Group.Sexpable.V2.t = {
      summary     : string;
      readme      : string sexp_option;
      subcommands : (string * 'a) List.t Lazy.t;
    } [@@deriving bin_io, compare, fields, sexp]

    let map = Group.Sexpable.V2.map

  end

  module Exec_info = struct

    type t = Exec.Sexpable.V3.t = {
      summary          : string;
      readme           : string sexp_option;
      working_dir      : string;
      path_to_exe      : string;
      child_subcommand : string list;
    } [@@deriving bin_io, compare, fields, sexp]

  end

  module T = struct

    type t =
      | Basic of Base_info.t
      | Group of t Group_info.t
      | Exec of Exec_info.t * (unit -> t)
      | Lazy of t Lazy.t

  end

  module Fully_forced = struct

    type t =
      | Basic of Base_info.t
      | Group of t Group_info.t
      | Exec of Exec_info.t * t
    [@@deriving bin_io, compare, sexp]

    let rec create : T.t -> t = function
      | Basic b -> Basic b
      | Group g -> Group (Group_info.map g ~f:create)
      | Exec (e, f) -> Exec (e, create (f ()))
      | Lazy thunk -> create (Lazy.force thunk)

  end

  include T

end

let rec proxy_of_sexpable
          sexpable
          ~working_dir
          ~path_to_exe
          ~child_subcommand
          ~path_to_subcommand
  : Proxy.t =
  let kind =
    kind_of_sexpable
      sexpable
      ~working_dir
      ~path_to_exe
      ~child_subcommand
      ~path_to_subcommand
  in
  {working_dir; path_to_exe; path_to_subcommand; child_subcommand; kind}

and kind_of_sexpable
      sexpable
      ~working_dir
      ~path_to_exe
      ~child_subcommand
      ~path_to_subcommand
  =
  match (sexpable : Sexpable.t) with
  | Base  b -> Proxy.Kind.Base b
  | Exec  e -> Proxy.Kind.Exec e
  | Lazy  l ->
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
          Lazy.map g.subcommands
            ~f:(List.map ~f:(fun (str, sexpable) ->
              let path_to_subcommand = path_to_subcommand @ [str] in
              let proxy =
                proxy_of_sexpable
                  sexpable
                  ~working_dir
                  ~path_to_exe
                  ~child_subcommand
                  ~path_to_subcommand
              in
              (str, proxy)))
      }

let proxy_of_exe ~working_dir path_to_exe child_subcommand =
  let sexpable = Sexpable.of_external ~working_dir ~path_to_exe ~child_subcommand in
  proxy_of_sexpable sexpable ~working_dir ~path_to_exe ~child_subcommand ~path_to_subcommand:[]

let rec shape_of_proxy proxy : Shape.t =
  shape_of_proxy_kind proxy.Proxy.kind

and shape_of_proxy_kind kind =
  match kind with
  | Base  b -> Basic b
  | Lazy  l -> Lazy (Lazy.map ~f:shape_of_proxy_kind l)
  | Group g ->
    Group { g with subcommands = Lazy.map g.subcommands ~f:(List.Assoc.map ~f:shape_of_proxy) }
  | Exec  e ->
    let f () =
      shape_of_proxy (proxy_of_exe ~working_dir:e.working_dir e.path_to_exe e.child_subcommand)
    in
    Exec (e, f)
;;

let rec shape t : Shape.t =
  match t with
  | Base  b -> Basic (Base.to_sexpable b)
  | Group g -> Group (Group.to_sexpable ~subcommand_to_sexpable:shape g)
  | Proxy p -> shape_of_proxy p
  | Exec  e ->
    let f () =
      shape_of_proxy (proxy_of_exe ~working_dir:e.working_dir e.path_to_exe e.child_subcommand)
    in
    Exec (Exec.to_sexpable e, f)
  | Lazy thunk -> shape (Lazy.force thunk)
;;

module Version_info = struct
  let sanitize_version ~version =
    (* [version] was space delimited at some point and newline delimited
       at another.  We always print one (repo, revision) pair per line
       and ensure sorted order *)
    String.split version ~on:' '
    |> List.concat_map ~f:(String.split ~on:'\n')
    |> List.sort ~compare:String.compare

  let print_version ~version = List.iter (sanitize_version ~version) ~f:print_endline
  let print_build_info ~build_info = print_endline (force build_info)

  let command ~version ~build_info =
    basic ~summary:"print version information"
      Base.Spec.(
        empty
        +> flag "-version" no_arg ~doc:" print the version of this build"
        +> flag "-build-info" no_arg ~doc:" print build info for this build"
      )
      (fun version_flag build_info_flag ->
         begin
           if build_info_flag then print_build_info ~build_info
           else if version_flag then print_version ~version
           else (print_build_info ~build_info; print_version ~version)
         end;
         exit 0)

  let rec add
            ~version
            ~build_info
            unversioned =
    match unversioned with
    | Base base ->
      let base =
        Bailout_dump_flag.add base ~name:"-version" ~aliases:[]
          ~text_summary:"the version of this build"
          ~text:(fun _ -> String.concat ~sep:"\n" (sanitize_version ~version))
      in
      let base =
        Bailout_dump_flag.add base ~name:"-build-info" ~aliases:[]
          ~text_summary:"info about this build" ~text:(fun _ -> force build_info)
      in
      Base base
    | Group group ->
      let subcommands =
        Lazy.map group.Group.subcommands ~f:(fun subcommands ->
          extend_alist_exn subcommands Key_type.Subcommand ~key:"version"
            (command ~version ~build_info))
      in
      Group { group with Group.subcommands }
    | Proxy proxy -> Proxy proxy
    | Exec  exec  -> Exec  exec
    | Lazy  thunk -> Lazy (lazy (add ~version ~build_info (Lazy.force thunk)))

end

(* This script works in both bash (via readarray) and zsh (via read -A).  If you change
   it, please test in both bash and zsh.  It does not work in ksh (unexpected null byte)
   and tcsh (different function syntax). *)
let dump_autocomplete_function () =
  let fname = sprintf "_jsautocom_%s" (Pid.to_string (Unix.getpid ())) in
  printf
    "function %s {
  export COMP_CWORD
  COMP_WORDS[0]=%s
  if type readarray > /dev/null
  then readarray -t COMPREPLY < <(\"${COMP_WORDS[@]}\")
  else IFS=\"\n\" read -d \"\x00\" -A COMPREPLY < <(\"${COMP_WORDS[@]}\")
  fi
}
complete -F %s %s
%!" fname Sys.argv.(0) fname Sys.argv.(0)
;;

let dump_help_sexp ~supported_versions t ~path_to_subcommand =
  Int.Set.inter Sexpable.supported_versions supported_versions
  |> Int.Set.max_elt
  |> function
  | None ->
    failwiths "Couldn't choose a supported help output version for Command.exec \
               from the given supported versions."
      Sexpable.supported_versions Int.Set.sexp_of_t;
  | Some version_to_use ->
    to_sexpable t
    |> Sexpable.find ~path_to_subcommand
    |> Sexpable.Internal.of_latest ~version_to_use
    |> Sexpable.Internal.sexp_of_t
    |> Sexp.to_string
    |> print_string
;;

let handle_environment t ~argv =
  match argv with
  | [] -> failwith "missing executable name"
  | cmd :: args ->
    Option.iter (getenv_and_clear Sexpable.extraction_var)
      ~f:(fun version ->
        let supported_versions = Sexp.of_string version |> Int.Set.t_of_sexp in
        dump_help_sexp ~supported_versions t ~path_to_subcommand:args;
        exit 0);
    Option.iter (getenv_and_clear "COMMAND_OUTPUT_INSTALLATION_BASH")
      ~f:(fun _ ->
        dump_autocomplete_function ();
        exit 0);
    (cmd, args)
;;

let process_args ~cmd ~args =
  let maybe_comp_cword = maybe_comp_cword () in
  let args =
    match maybe_comp_cword with
    | None -> Cmdline.of_list args
    | Some comp_cword ->
      let args = List.take (args @ [""]) comp_cword in
      List.fold_right args ~init:Cmdline.Nil ~f:(fun arg args ->
        match args with
        | Cmdline.Nil -> Cmdline.Complete arg
        | _ -> Cmdline.Cons (arg, args))
  in
  (Path.root cmd, args, maybe_comp_cword)
;;

let rec add_help_subcommands = function
  | Base  _ as t -> t
  | Exec  _ as t -> t
  | Proxy _ as t -> t
  | Group {summary; readme; subcommands; body} ->
    let subcommands =
      Lazy.map subcommands ~f:(fun subcommands ->
        extend_alist_exn
          (List.Assoc.map subcommands ~f:add_help_subcommands)
          Key_type.Subcommand
          ~key:"help"
          (help_subcommand ~summary ~readme))
    in
    Group {summary; readme; subcommands; body}
  | Lazy thunk ->
    Lazy (lazy (add_help_subcommands (Lazy.force thunk)))
;;

let maybe_apply_extend args ~extend ~path =
  Option.value_map extend ~default:args
    ~f:(fun f -> Cmdline.extend args ~extend:f ~path)
;;

let rec dispatch t env ~extend ~path ~args ~maybe_new_comp_cword ~version ~build_info =
  let to_format_list (group : _ Group.t) : Format.V1.t list =
    let group = Group.to_sexpable ~subcommand_to_sexpable:to_sexpable group in
    List.map (Lazy.force group.subcommands) ~f:(fun (name, sexpable) ->
      { Format.V1. name; aliases = []; doc = Sexpable.get_summary sexpable })
    |> Format.V1.sort
  in
  match t with
  | Lazy thunk ->
    let t = Lazy.force thunk in
    dispatch t env ~extend ~path ~args ~maybe_new_comp_cword ~version ~build_info
  | Base base ->
    let args = maybe_apply_extend args ~extend ~path in
    Base.run base env ~path ~args
  | Exec exec ->
    let args = Cmdline.to_list (maybe_apply_extend args ~extend ~path) in
    Exec.exec_with_args ~args exec ~maybe_new_comp_cword
  | Proxy proxy ->
    let args =
      proxy.path_to_subcommand
      @ Cmdline.to_list (maybe_apply_extend args ~extend ~path)
    in
    let exec =
      { Exec.
        working_dir = proxy.working_dir;
        path_to_exe = proxy.path_to_exe;
        child_subcommand = proxy.child_subcommand;
        summary = Proxy.get_summary proxy;
        readme = Proxy.get_readme proxy |> Option.map ~f:const;
      }
    in
    Exec.exec_with_args ~args exec ~maybe_new_comp_cword
  | Group ({summary; readme; subcommands = subs; body} as group) ->
    let env = Env.set env subs_key (Lazy.force subs) in
    let die_showing_help msg =
      if not (Cmdline.ends_in_complete args) then begin
        eprintf "%s\n%!"
          (Group.help_text ~to_format_list ~path ~show_flags:false
             {summary; readme; subcommands = subs; body});
        die "%s" msg ()
      end
    in
    match args with
    | Nil ->
      begin
        match body with
        | None -> die_showing_help (sprintf "missing subcommand for command %s" (Path.to_string path))
        | Some body -> body ~path:(Path.commands path)
      end
    | Cons (sub, rest) ->
      let (sub, rest) =
        (* Match for flags recognized when subcommands are expected next *)
        match (sub, rest) with
        (* Recognized at the top level command only *)
        | ("-version", _) when Path.length path = 1 ->
          Version_info.print_version ~version;
          exit 0
        | ("-build-info", _) when Path.length path = 1 ->
          Version_info.print_build_info ~build_info;
          exit 0
        (* Recognized everywhere *)
        | ("-help", Nil) ->
          print_endline
            (Group.help_text ~to_format_list ~path ~show_flags:false
               {group with subcommands = subs});
          exit 0
        | ("-help", Cmdline.Cons (sub, rest)) -> (sub, Cmdline.Cons ("-help", rest))
        | _ -> (sub, rest)
      in
      begin
        match
          lookup_expand
            (List.Assoc.map (Lazy.force subs) ~f:(fun x -> (x, `Prefix)))
            sub
            Subcommand
        with
        | Error msg -> die_showing_help msg
        | Ok (sub, t) ->
          dispatch t env
            ~extend
            ~path:(Path.add path ~subcommand:sub)
            ~args:rest
            ~maybe_new_comp_cword:(Option.map ~f:Int.pred maybe_new_comp_cword)
            ~version
            ~build_info
      end
    | Complete part ->
      let subs =
        Lazy.force subs
        |> List.map ~f:fst
        |> List.filter ~f:(fun name -> String.is_prefix name ~prefix:part)
        |> List.sort ~compare:String.compare
      in
      List.iter subs ~f:print_endline;
      exit 0
;;

let default_version, default_build_info =
  Version_util.version,
  (* lazy to avoid loading all the time zone stuff at toplevel *)
  lazy (Version_util.reprint_build_info Time.sexp_of_t)

let run
      ?(version = default_version)
      ?build_info
      ?(argv=Array.to_list Sys.argv)
      ?extend
      t =
  let build_info =
    match build_info with
    | Some v -> lazy v
    | None -> default_build_info
  in
  Exn.handle_uncaught ~exit:true (fun () ->
    let t = Version_info.add t ~version ~build_info in
    let t = add_help_subcommands t in
    let (cmd, args) = handle_environment t ~argv in
    let (path, args, maybe_new_comp_cword) = process_args ~cmd ~args in
    try
      dispatch t Env.empty ~extend ~path ~args ~maybe_new_comp_cword
        ~version ~build_info
    with
    | Failed_to_parse_command_line msg ->
      if Cmdline.ends_in_complete args then
        exit 0
      else begin
        prerr_endline msg;
        exit 1
      end)
;;

let rec summary = function
  | Base  x -> x.summary
  | Group x -> x.summary
  | Exec  x -> x.summary
  | Proxy x -> Proxy.get_summary x
  | Lazy thunk -> summary (Lazy.force thunk)

module Spec = struct
  include Base.Spec
  let path = map ~f:Path.commands path
end

module Deprecated = struct

  module Spec = Spec.Deprecated

  let summary = get_summary

  let rec get_flag_names = function
    | Base base -> base.Base.flags |> String.Map.keys
    | Lazy thunk -> get_flag_names (Lazy.force thunk)
    | Group _
    | Proxy _
    | Exec  _ -> assert false

  let help_recursive ~cmd ~with_flags ~expand_dots t s =
    let rec help_recursive_rec ~cmd t s =
      let new_s = s ^ (if expand_dots then cmd else ".") ^ " " in
      match t with
      | Lazy thunk ->
        let t = Lazy.force thunk in
        help_recursive_rec ~cmd t s
      | Base base ->
        let base_help = s ^ cmd, summary (Base base) in
        if with_flags then
          base_help ::
          List.map ~f:(fun (flag, h) -> (new_s ^ flag, h))
            (List.sort ~compare:Base.Deprecated.subcommand_cmp_fst
               (Base.Deprecated.flags_help ~display_help_flags:false base))
        else
          [base_help]
      | Group {summary; subcommands; readme = _; body = _} ->
        (s ^ cmd, summary)
        :: begin
          Lazy.force subcommands
          |> List.sort ~compare:Base.Deprecated.subcommand_cmp_fst
          |> List.concat_map ~f:(fun (cmd', t) ->
            help_recursive_rec ~cmd:cmd' t new_s)
        end
      | (Proxy _ | Exec _) ->
        (* Command.exec does not support deprecated commands *)
        []
    in
    help_recursive_rec ~cmd t s

  let version = default_version
  let build_info = default_build_info

  let run t ~cmd ~args ~is_help ~is_help_rec ~is_help_rec_flags ~is_expand_dots =
    let path_strings = String.split cmd ~on: ' ' in
    let path =
      List.fold path_strings ~init:Path.empty ~f:(fun p subcommand ->
        Path.add p ~subcommand)
    in
    let args = if is_expand_dots    then "-expand-dots" :: args else args in
    let args = if is_help_rec_flags then "-flags"       :: args else args in
    let args = if is_help_rec       then "-r"           :: args else args in
    let args = if is_help           then "-help"        :: args else args in
    let args = Cmdline.of_list args in
    let t = add_help_subcommands t in
    dispatch t Env.empty ~path ~args ~extend:None ~maybe_new_comp_cword:None
      ~version ~build_info

end

(* testing claims made in the mli about order of evaluation and [flags_of_args_exn] *)
let%test_module "Command.Spec.flags_of_args_exn" =
  (module struct

    let args q = [
      ( "flag1", Arg.Unit (fun () -> Queue.enqueue q 1), "enqueue 1");
      ( "flag2", Arg.Unit (fun () -> Queue.enqueue q 2), "enqueue 2");
      ( "flag3", Arg.Unit (fun () -> Queue.enqueue q 3), "enqueue 3");
    ]

    let parse argv =
      let q = Queue.create () in
      let command = basic ~summary:"" (Spec.flags_of_args_exn (args q)) Fn.id in
      run ~argv command;
      Queue.to_list q

    let%test _ = parse ["foo.exe";"-flag1";"-flag2";"-flag3"] = [1;2;3]
    let%test _ = parse ["foo.exe";"-flag2";"-flag3";"-flag1"] = [1;2;3]
    let%test _ = parse ["foo.exe";"-flag3";"-flag2";"-flag1"] = [1;2;3]

  end)

(* NOTE: all that follows is simply namespace management boilerplate.  This will go away
   once we re-work the internals of Command to use Applicative from the ground up. *)

module Param = struct
  module type S = sig
    type +'a t
    include Applicative.S with type 'a t := 'a t

    val help : string Lazy.t t
    val path : string list   t
    val args : string list   t

    val flag
      :  ?aliases:string list
      -> ?full_flag_required:unit
      -> string
      -> 'a Flag.t
      -> doc:string
      -> 'a t

    val flag_optional_with_default_doc
      :  ?aliases            : string list
      -> ?full_flag_required : unit
      -> string
      -> 'a Arg_type.t
      -> ('a -> Sexp.t)
      -> default:'a
      -> doc : string
      -> 'a t

    val anon : 'a Anons.t -> 'a t

    val choose_one
      :  'a option t list
      -> if_nothing_chosen:[ `Default_to of 'a | `Raise ]
      -> 'a t
  end

  module A = struct
    type 'a t = 'a Spec.param
    include Applicative.Make (struct
        type nonrec 'a t = 'a t
        let return = Spec.const
        let apply = Spec.apply
        let map = `Custom Spec.map
      end)
  end

  include A

  let help       = Spec.help
  let path       = Spec.path
  let args       = Spec.args
  let flag       = Spec.flag
  let anon       = Spec.anon
  let choose_one = Spec.choose_one
  let flag_optional_with_default_doc = Spec.flag_optional_with_default_doc

  module Arg_type = Arg_type
  include Arg_type.Export
  include struct
    open Flag
    let listed                = listed
    let no_arg                = no_arg
    let no_arg_abort          = no_arg_abort
    let no_arg_register       = no_arg_register
    let one_or_more           = one_or_more
    let optional              = optional
    let optional_with_default = optional_with_default
    let required              = required
    let escape                = escape
  end
  include struct
    open Anons
    let (%:)                       = (%:)
    let maybe                      = maybe
    let maybe_with_default         = maybe_with_default
    let non_empty_sequence_as_pair = non_empty_sequence_as_pair
    let non_empty_sequence_as_list = non_empty_sequence_as_list
    let sequence                   = sequence
    let t2                         = t2
    let t3                         = t3
    let t4                         = t4
  end
end

module Let_syntax = struct
  include Param
  module Let_syntax = struct
    include Param
    module Open_on_rhs = Param
  end
end

type 'result basic_command
  =  summary : string
  -> ?readme : (unit -> string)
  -> (unit -> 'result) Param.t
  -> t

let basic ~summary ?readme param =
  let spec =
    Spec.of_params @@ Param.map param ~f:(fun run () () -> run ())
  in
  basic ~summary ?readme spec ()

let%expect_test "choose_one strings" =
  let open Param in
  let to_string = Spec.to_string_for_choose_one in
  print_string (to_string begin
    flag "-a" no_arg ~doc:""
  end);
  [%expect {| -a |} ];
  print_string (to_string begin
    map2 ~f:Tuple2.create
      (flag "-a" no_arg ~doc:"")
      (flag "-b" no_arg ~doc:"")
  end);
  [%expect {| -a,-b |} ];
  print_string (to_string begin
    map2 ~f:Tuple2.create
      (flag "-a" no_arg ~doc:"")
      (flag "-b" (optional int) ~doc:"")
  end);
  [%expect {| -a,-b |} ];
  printf !"%{sexp: string Or_error.t}"
    (Or_error.try_with (fun () ->
       to_string begin
         map2 ~f:Tuple2.create
           (flag "-a" no_arg ~doc:"")
           (flag "-b,c" (optional int) ~doc:"")
       end));
  [%expect {|
    (Error
     ("For simplicity, [Command.Spec.choose_one] does not support names with commas."
      (-b,c) *:*:*)) (glob) |}];
  print_string (to_string begin
    map2 ~f:Tuple2.create
      (anon ("FOO" %: string))
      (flag "-a" no_arg ~doc:"")
  end);
  [%expect {| -a,FOO |} ];
  print_string (to_string begin
    map2 ~f:Tuple2.create
      (anon ("FOO" %: string))
      (map2 ~f:Tuple2.create
         (flag "-a" no_arg ~doc:"")
         (flag "-b" no_arg ~doc:""))
  end);
  [%expect {| -a,-b,FOO |} ];
  print_string (to_string begin
    map2 ~f:Tuple2.create
      (anon (maybe ("FOO" %: string)))
      (flag "-a" no_arg ~doc:"")
  end);
  [%expect {| -a,FOO |} ];
  print_string (to_string begin
    map2 ~f:Tuple2.create
      (anon ("fo{}O" %: string))
      (flag "-a" no_arg ~doc:"")
  end);
  [%expect {| -a,fo{}O |} ];
;;

let%test_unit "multiple runs" =
  let r = ref (None, "not set") in
  let command =
    let open Let_syntax in
    basic ~summary:"test"
      [%map_open
        let a = flag "int" (optional int) ~doc:"INT some number"
        and b = anon ("string" %: string)
        in
        fun () -> r := (a, b)
      ]
  in
  let test args expect =
    run command ~argv:(Sys.argv.(0) :: args);
    [%test_result: int option * string] !r ~expect
  in
  test ["foo"; "-int"; "23"] (Some 23, "foo");
  test ["-int"; "17"; "bar"] (Some 17, "bar");
  test ["baz"]               (None,    "baz");
;;
