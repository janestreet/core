open Core_kernel.Std

module Unix = Core_unix

let unwords      xs = String.concat ~sep:" "    xs
let unparagraphs xs = String.concat ~sep:"\n\n" xs

exception Failed_to_parse_command_line of string

let die fmt = Printf.ksprintf (fun msg () -> raise (Failed_to_parse_command_line msg)) fmt

let help_screen_compare a b =
  match (a, b) with
  | (_, "[-help]")       -> -1 | ("[-help]",       _) -> 1
  | (_, "[-version]")    -> -1 | ("[-version]",    _) -> 1
  | (_, "[-build-info]") -> -1 | ("[-build-info]", _) -> 1
  | (_, "help")        -> -1 | ("help",        _) -> 1
  | (_, "version")     -> -1 | ("version",     _) -> 1
  | _ -> String.compare a b

module Format : sig
  type t = {
    name : string;
    doc : string;
    aliases : string list;
  }
  val sort : t list -> t list
  val to_string : t list -> string
end = struct
  type t = {
    name : string;
    doc : string;
    aliases : string list;
  }

  let sort ts =
    List.sort ts ~cmp:(fun a b -> help_screen_compare a.name b.name)

  let word_wrap text width =
    let chunks = String.split text ~on:'\n' in
    List.concat_map chunks ~f:(fun text ->
      let words =
        String.split text ~on:' '
        |! List.filter ~f:(fun word -> not (String.is_empty word))
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

  TEST_MODULE "word wrap" = struct

    TEST = word_wrap "" 10 = []

    let short_word = "abcd"

    TEST = word_wrap short_word (String.length short_word) = [short_word]

    TEST = word_wrap "abc\ndef\nghi" 100 = ["abc"; "def"; "ghi"]

    let long_text =
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus \
       fermentum condimentum eros, sit amet pulvinar dui ultrices in."

    TEST = word_wrap long_text 1000 =
      ["Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus \
        fermentum condimentum eros, sit amet pulvinar dui ultrices in."]

    TEST = word_wrap long_text 39 =
      (*
        .........1.........2.........3.........4
        1234567890123456789012345678901234567890
      *)
      ["Lorem ipsum dolor sit amet, consectetur";
       "adipiscing elit. Vivamus fermentum";
       "condimentum eros, sit amet pulvinar dui";
       "ultrices in."]

    (* no guarantees: too-long words just overhang the soft bound *)
    TEST = word_wrap long_text 2 =
      ["Lorem"; "ipsum"; "dolor"; "sit"; "amet,"; "consectetur";
       "adipiscing"; "elit."; "Vivamus"; "fermentum"; "condimentum";
       "eros,"; "sit"; "amet"; "pulvinar"; "dui"; "ultrices"; "in."]

  end

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
              | xs  -> [rows "" (sprintf "(aliases: %s)" (String.concat ~sep:", " xs))]
            end))))

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

  let string = create Fn.id
  let int    = create Int.of_string
  let float  = create Float.of_string
  let bool   = create Bool.of_string
  let date   = create Date.of_string
  let time   = create Time.of_string_abs
  let time_ofday = create Time.Ofday.Zoned.of_string
  let time_ofday_unzoned = create Time.Ofday.of_string
  let time_span = create Time.Span.of_string

  let file ?key of_string =
    create ?key of_string ~complete:(fun _ ~part ->
      let prog = "bash" in
      let args = ["bash"; "-c"; "compgen -f $0"; part] in
      never_returns (Unix.exec ~prog ~args ()))

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
end

module Flag = struct

  type action =
    | No_arg of (Env.t -> Env.t)
    | Arg    of (string -> Env.t -> Env.t) * Completer.t
    | Rest   of (string list -> unit)

  type t = {
    name : string;
    aliases : string list;
    action : action;
    doc : string;
    check_available : [`Optional | `Required of (unit -> unit)];
  }

  let wrap_if_optional t x =
    match t.check_available with
    | `Optional -> sprintf "[%s]" x
    | `Required _ -> x

  module Deprecated = struct
    (* flag help in the format of the old command. used for injection *)
    let help ({name; doc; aliases; action=_; check_available=_ } as t)  =
      if String.is_prefix doc ~prefix:" " then
        (name, String.lstrip doc)
          :: List.map aliases ~f:(fun x -> (x, sprintf "same as \"%s\"" name))
      else
        let (arg, doc) =
          match String.lsplit2 doc ~on:' ' with
          | None -> (doc, "")
          | Some pair -> pair
        in
        (wrap_if_optional t (name ^ " " ^ arg), String.lstrip doc)
          :: List.map aliases ~f:(fun x ->
              (wrap_if_optional t (x ^ " " ^ arg), sprintf "same as \"%s\"" name))
  end

  let align ({name; doc; aliases; action=_; check_available=_ } as t) =
    let (name, doc) =
      match String.lsplit2 doc ~on:' ' with
      | None | Some ("", _) -> (name, String.strip doc)
      | Some (arg, doc) -> (name ^ " " ^ arg, doc)
    in
    let name = wrap_if_optional t name in
    { Format.name; doc; aliases}

  module Spec = struct

    type 'a state = {
      action : action;
      read : unit -> 'a;
      optional : bool;
    }

    type 'a t = string -> 'a state

    let arg_flag name arg_type read write ~optional =
      { read; optional;
        action =
          let update arg env =
            match arg_type.Arg_type.parse arg with
            | Error exn ->
              die "failed to parse %s value %S.\n%s" name arg (Exn.to_string exn) ()
            | Ok arg ->
              write arg;
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
          read = (fun () -> f (read ()));
          optional;
        }

    let write_option name v arg =
      match !v with
      | None -> v := Some arg
      | Some _ -> die "flag %s passed more than once" name ()

    let required_value ?default arg_type name ~optional =
      let v = ref None in
      let read () =
        match !v with
        | Some v -> v
        | None ->
          match default with
          | Some v -> v
          | None -> die "missing required flag: %s" name ()
      in
      let write arg = write_option name v arg in
      arg_flag name arg_type read write ~optional

    let required arg_type name =
      required_value arg_type name ~optional:false

    let optional_with_default default arg_type name =
      required_value ~default arg_type name ~optional:true

    let optional arg_type name =
      let v = ref None in
      let read () = !v in
      let write arg = write_option name v arg in
      arg_flag name arg_type read write ~optional:true

    let no_arg_general ~key_value ~deprecated_hook name =
      let v = ref false in
      let read () = !v in
      let write () =
        if !v then
          die "flag %s passed more than once" name ()
        else
          v := true
      in
      let action env =
        let env =
          Option.fold key_value ~init:env
            ~f:(fun env (key, value) ->
              Env.set_with_default env key value)
        in
        write ();
        env
      in
      let action =
        match deprecated_hook with
        | None -> action
        | Some f ->
          (fun x ->
            let env = action x in
            f ();
            env
          )
      in
      { read; action = No_arg action; optional = true }

    let no_arg name = no_arg_general name ~key_value:None ~deprecated_hook:None

    let no_arg_register ~key ~value name =
      no_arg_general name ~key_value:(Some (key, value)) ~deprecated_hook:None

    let listed arg_type name =
      let v = ref [] in
      let read () = List.rev !v in
      let write arg = v := arg :: !v in
      arg_flag name arg_type read write ~optional:true

    let escape_general ~deprecated_hook _name =
      let cell = ref None in
      let action = (fun cmd_line -> cell := Some cmd_line) in
      let read () = !cell in
      let action =
        match deprecated_hook with
        | None -> action
        | Some f ->
          (fun x ->
            f x;
            action x
          )
      in
      { action = Rest action; read; optional = true }

    let no_arg_abort ~exit name = {
      action = No_arg (fun _ -> never_returns (exit ()));
      optional = true;
      read = (fun () ->
        failwithf "BUG! somehow no_arg_abort flag %s is being read" name ());
    }

    let escape name = escape_general ~deprecated_hook:None name

    module Deprecated = struct
      let no_arg ~hook name = no_arg_general ~deprecated_hook:(Some hook) ~key_value:None name
      let escape ~hook      = escape_general ~deprecated_hook:(Some hook)
    end

  end

end

module Path : sig
  type t
  val empty : t
  val root : string -> t
  val add : t -> subcommand:string -> t
  val commands : t -> string list
  val to_string : t -> string
  val to_string_dots : t -> string
  val pop_help : t -> t
end = struct
  type t = string list
  let empty = []
  let root cmd = [Filename.basename cmd]
  let add t ~subcommand = subcommand :: t
  let commands t = List.rev t
  let to_string t = unwords (commands t)
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

module Anon = struct

  module Grammar : sig
    type t
    val zero : t
    val one : string -> t
    val many : t -> t
    val maybe : t -> t
    val concat : t list -> t
    val usage : t -> string
    val ad_hoc : usage:string -> t
  end = struct

    type s = {
      usage : string;
      number : [`One | `Fixed | `Variable]
    }

    type t = s option

    let usage = function
      | None -> ""
      | Some s -> s.usage

    let zero = None

    let one name = Some { usage = name; number = `One }

    let many = function
      | None -> None (* strange, but not non-sense *)
      | Some s ->
        match s.number with
        | `Variable ->
          failwithf "iteration of variable-length grammars such as %s is disallowed"
            s.usage ()
        | (`One | `Fixed) as g ->
          let s_usage =
            match g with
            | `One -> s.usage
            | `Fixed -> sprintf "(%s)" s.usage
          in
          Some { usage = sprintf "[%s ...]" s_usage; number = `Variable }

    let maybe = function
      | None -> None (* strange, but not non-sense *)
      | Some s ->
        Some { usage = sprintf "[%s]" s.usage; number = `Variable }

    let concat2 t1 t2 =
      match (t1, t2) with
      | (None, t) | (t, None) -> t
      | (Some s1, Some s2) ->
        let combined_usage = s1.usage ^ " " ^ s2.usage in
        match s1.number with
        | `Variable ->
            failwithf "the grammar %s for anonymous arguments \
              is not supported because there is the possibility for \
              arguments (%s) following a variable number of \
              arguments (%s).  Supporting such grammars would complicate \
              the implementation significantly." combined_usage s2.usage s1.usage ()
        | `One | `Fixed ->
            Some {
              usage = combined_usage;
              number =
                match s2.number with
                | `Variable -> `Variable;
                | `One | `Fixed -> `Fixed
            }

    let rec concat = function
      | [] -> zero
      | t :: ts -> concat2 t (concat ts)

    let ad_hoc ~usage = Some { usage ; number = `Variable }
  end

  module Parser : sig
    type 'a t
    val one : name:string -> 'a Arg_type.t -> 'a t
    val maybe : 'a t -> 'a option t
    val sequence : 'a t -> 'a list t
    val final_value : 'a t -> 'a
    val consume : 'a t -> string -> (Env.t -> Env.t) * 'a t
    val complete : 'a t -> Env.t -> part:string -> never_returns
    module For_opening : sig
      val return : 'a -> 'a t
      val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
      val (>>|) : 'a t -> ('a -> 'b) -> 'b t
    end
  end = struct

    type 'a t =
      | Done of 'a
      | Test of (more:bool -> 'a t)
      | More of string (* name of the expected argument *)
          * (string -> (Env.t -> Env.t) * 'a t)
          * Completer.t

    let return a = Done a

    let rec (>>=) t f =
      match t with
      | Done a -> f a
      | Test g -> Test (fun ~more -> g ~more >>= f)
      | More (name, g, complete) ->
        let g x =
          let (upd, t) = g x in
          (upd, t >>= f)
        in
        More (name, g, complete)

    let (>>|) t f = t >>= fun x -> return (f x)

    let one_more ~name {Arg_type.complete; parse = of_string; key} =
      More (name, (fun anon ->
        match of_string anon with
        | Error exn ->
          die "failed to parse %s value %S\n%s" name anon (Exn.to_string exn) ()
        | Ok v ->
          let update env =
            Option.fold key ~init:env ~f:(fun env key -> Env.multi_add env key v)
          in
          (update, Done v)), complete)

    let one ~name arg_type =
      Test (fun ~more ->
        if more then
          one_more ~name arg_type
        else
          die "missing anonymous argument: %s" name ())

    let maybe t =
      Test (fun ~more ->
        if more
        then t >>= fun a -> Done (Some a)
        else return None)

    let sequence t =
      let rec loop acc =
        Test (fun ~more ->
          if more then
            t >>= fun v -> loop (v :: acc)
          else
            return (List.rev acc))
      in
      loop []

    let rec final_value = function
      | Done a -> a
      | Test f -> final_value (f ~more:false)
      | More (name, _, _) -> die "missing anonymous argument: %s" name ()

    let rec consume t arg =
      match t with
      | Done _ -> die "too many anonymous arguments" ()
      | Test f -> consume (f ~more:true) arg
      | More (_, f, _) -> f arg

    let rec complete t env ~part =
      match t with
      | Done _ -> exit 0
      | Test f -> complete (f ~more:true) env ~part
      | More (_, _, comp) -> Completer.run_and_exit comp env ~part

    module For_opening = struct
      let return = return
      let (>>=) = (>>=)
      let (>>|) = (>>|)
    end
  end

  open Parser.For_opening

  module Spec = struct

    type 'a t = {
      p : 'a Parser.t;
      grammar : Grammar.t;
    }

    let t2 t1 t2 = {
      p = begin
        t1.p >>= fun a1 ->
        t2.p >>= fun a2 ->
        return (a1, a2)
      end;
      grammar = Grammar.concat [t1.grammar; t2.grammar];
    }

    let t3 t1 t2 t3 = {
      p = begin
        t1.p >>= fun a1 ->
        t2.p >>= fun a2 ->
        t3.p >>= fun a3 ->
        return (a1, a2, a3)
      end;
      grammar = Grammar.concat [t1.grammar; t2.grammar; t3.grammar];
    }

    let t4 t1 t2 t3 t4 = {
      p = begin
        t1.p >>= fun a1 ->
        t2.p >>= fun a2 ->
        t3.p >>= fun a3 ->
        t4.p >>= fun a4 ->
        return (a1, a2, a3, a4)
      end;
      grammar = Grammar.concat [t1.grammar; t2.grammar; t3.grammar; t4.grammar];
    }

    let normalize str =
      (* Verify the string is not empty or surrounded by whitespace *)
      let strlen = String.length str in
      if strlen = 0 then failwith "Empty anonymous argument name provided";
      if String.(<>) (String.strip str) str then
        failwithf "argument name %S has surrounding whitespace" str ();
      (* Now check for special surrounding characters and malformed pairs *)
      let surrounded =
        List.exists [('<','>'); ('[',']'); ('(',')'); ('{','}')]
          ~f:(fun (prefix, suffix) ->
            let prefix_match = Char.(=) prefix (String.get str 0) in
            let suffix_match = Char.(=) suffix (String.get str (strlen - 1)) in
            if Bool.(<>) prefix_match suffix_match then
              failwithf "argument name %S has malformed surrounding characters" str ();
            prefix_match)
      in
      if surrounded then str else String.uppercase str

    TEST = String.equal (normalize "file")   "FILE"
    TEST = String.equal (normalize "FiLe")   "FILE"
    TEST = String.equal (normalize "<FiLe>") "<FiLe>"
    TEST = String.equal (normalize "(FiLe)") "(FiLe)"
    TEST = String.equal (normalize "[FiLe]") "[FiLe]"
    TEST = String.equal (normalize "{FiLe}") "{FiLe}"
    TEST = try ignore (normalize "<file"   ); false with _ -> true
    TEST = try ignore (normalize "<file>a" ); false with _ -> true
    TEST = try ignore (normalize "file}"   ); false with _ -> true
    TEST = try ignore (normalize ""        ); false with _ -> true
    TEST = try ignore (normalize " file "  ); false with _ -> true
    TEST = try ignore (normalize "file "   ); false with _ -> true
    TEST = try ignore (normalize " file"   ); false with _ -> true

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
      { t with p = t.p >>= fun v -> return (Option.value ~default v) }

    let sequence t = {
      p = Parser.sequence t.p;
      grammar = Grammar.many t.grammar;
    }

    module Deprecated = struct
      let ad_hoc ~usage_arg = {
        p = Parser.sequence (Parser.one ~name:"WILL NEVER BE PRINTED" Arg_type.string);
        grammar = Grammar.ad_hoc ~usage:usage_arg
      }
    end

  end

end

module Args = struct
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

TEST_MODULE "Args.extend" = struct
  let path_of_list subcommands =
    List.fold subcommands ~init:(Path.root "exe") ~f:(fun path subcommand ->
      Path.add path ~subcommand)

  let extend path =
    match path with
    | ["foo"; "bar"] -> ["-foo"; "-bar"]
    | ["foo"; "baz"] -> ["-foobaz"]
    | _ -> ["default"]

  let test path args expected =
    let expected = Args.of_list expected in
    let observed =
      let path = path_of_list path in
      let args = Args.of_list args in
      Args.extend args ~extend ~path
    in
    Pervasives.(=) expected observed

  TEST = test ["foo"; "bar"] ["anon"; "-flag"] ["anon"; "-flag"; "-foo"; "-bar"]
  TEST = test ["foo"; "baz"] []                ["-foobaz"]
  TEST = test ["zzz"]        ["x"; "y"; "z"]   ["x"; "y"; "z"; "default"]
end

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

let lookup_expand map prefix key_type =
  match String.Map.find map prefix with
  | Some data -> Ok (prefix, data)
  | None ->
    let alist = String.Map.to_alist map in
    match List.filter alist ~f:(fun (key, _) -> String.is_prefix key ~prefix) with
    | [(key, data)] -> Ok (key, data)
    | [] ->
      Error (sprintf "unknown %s %s" (Key_type.to_string key_type) prefix)
    | matches ->
      let matching_keys = List.map ~f:fst matches in
      Error (sprintf "%s %s is an ambiguous prefix: %s"
        (Key_type.to_string key_type) prefix (String.concat ~sep:", " matching_keys))

let lookup_expand_with_aliases map prefix key_type =
  let map =
    String.Map.of_alist
      (List.concat_map (String.Map.data map) ~f:(fun flag ->
        let { Flag.name; aliases; action=_; doc=_; check_available=_ } = flag in
        (name, flag) :: List.map aliases ~f:(fun alias -> (alias, flag))))
  in
  match map with
  | `Ok map -> lookup_expand map prefix key_type
  | `Duplicate_key flag -> failwithf "multiple flags named %s" flag ()

module Base = struct

  type t = {
    summary : string;
    readme : (unit -> string) option;
    flags : Flag.t String.Map.t;
    anons : Env.t -> ([`Parse_args] -> [`Run_main] -> unit) Anon.Parser.t;
    usage : Anon.Grammar.t;
  }

  module Deprecated = struct
    let subcommand_cmp_fst (a, _) (c, _) =
      help_screen_compare a c

    let flags_help ?(display_help_flags = true) t =
      let flags = String.Map.data t.flags in
      let flags =
        if display_help_flags
        then flags
        else List.filter flags ~f:(fun f -> f.Flag.name <> "-help")
      in
      List.concat_map ~f:Flag.Deprecated.help flags
  end

  let summary t = t.summary

  let formatted_flags t = Format.sort (List.map (String.Map.data t.flags) ~f:Flag.align)

  let help_text ~path t =
    unparagraphs
      (List.filter_opt [
        Some t.summary;
        Some ("  " ^ Path.to_string path ^ " " ^ Anon.Grammar.usage t.usage);
        Option.map t.readme ~f:(fun thunk -> thunk ());
        Some "=== flags ===";
        Some (Format.to_string (formatted_flags t));
      ])

  let path_key = Env.key_create "path"
  let args_key = Env.key_create "args"
  let help_key = Env.key_create "help"

  let run t env ~path ~args =
    let help_text = lazy (help_text ~path t) in
    let env = Env.set env path_key path in
    let env = Env.set env args_key (Args.to_list args) in
    let env = Env.set env help_key help_text in
    let rec loop env anons = function
      | Args.Nil ->
        List.iter (String.Map.data t.flags) ~f:(fun flag ->
          match flag.Flag.check_available with
          | `Optional -> ()
          | `Required check -> check ());
        Anon.Parser.final_value anons
      | Args.Cons (arg, args) ->
        if String.is_prefix arg ~prefix:"-"
        && not (String.equal arg "-") (* support the convention where "-" means stdin *)
        then begin
          let flag = arg in
          let (flag, { Flag.action; name=_; aliases=_; doc=_; check_available=_ }) =
            match lookup_expand_with_aliases t.flags flag Key_type.Flag with
            | Error msg -> die "%s" msg ()
            | Ok x -> x
          in
          match action with
          | Flag.No_arg f ->
            let env = f env in
            loop env anons args
          | Flag.Arg (f, comp) ->
            begin match args with
            | Args.Nil -> die "missing argument for flag %s" flag ()
            | Args.Cons (arg, rest) ->
              let env = f arg env in
              loop env anons rest
            | Args.Complete part ->
              never_returns (Completer.run_and_exit comp env ~part)
            end
          | Flag.Rest f ->
            if Args.ends_in_complete args then exit 0;
            f (Args.to_list args);
            loop env anons Args.Nil
        end else begin
          let (env_upd, anons) = Anon.Parser.consume anons arg in
          let env = env_upd env in
          loop env anons args
        end
      | Args.Complete part ->
        if String.is_prefix part ~prefix:"-" then begin
          List.iter (String.Map.keys t.flags) ~f:(fun name ->
            if String.is_prefix name ~prefix:part then print_endline name);
          exit 0
        end else
          never_returns (Anon.Parser.complete anons env ~part);
    in
    match Result.try_with (fun () -> loop env (t.anons env) args `Parse_args) with
    | Ok thunk -> thunk `Run_main
    | Error exn ->
      match exn with
      | Failed_to_parse_command_line _ when Args.ends_in_complete args ->
        exit 0
      | Failed_to_parse_command_line msg ->
        print_endline (Lazy.force help_text);
        prerr_endline msg;
        exit 1
      | _ ->
        print_endline (Lazy.force help_text);
        raise exn

  module Spec = struct

    type ('a, 'b) t = {
      f : Env.t -> ('a -> 'b) Anon.Parser.t;
      usage : unit -> Anon.Grammar.t;
      flags : unit -> Flag.t list;
    }

    (* the reason that [param] is defined in terms of [t] rather than the other
       way round is that the delayed evaluation matters for sequencing of read/write
       operations on ref cells in the representation of flags *)
    type 'a param = { param : 'm. ('a -> 'm, 'm) t }

    open Anon.Parser.For_opening

    let app t1 t2 ~f = {
      f = (fun env ->
        t1.f env >>= fun v1 ->
        t2.f env >>= fun v2 ->
        return (f v1 v2)
      );
      flags = (fun () -> t2.flags () @ t1.flags ());
      usage = (fun () -> Anon.Grammar.concat [t1.usage (); t2.usage ()]);
    }

    (* So sad.  We can't define [apply] in terms of [app] because of the value
       restriction. *)
    let apply pf px = {
      param = {
        f = (fun env ->
          pf.param.f env >>= fun mf ->
          px.param.f env >>= fun mx ->
          return (fun k -> mf (fun f -> mx (fun x -> k (f x))))
        );
        flags = (fun () -> px.param.flags () @ pf.param.flags ());
        usage = (fun () -> Anon.Grammar.concat [pf.param.usage (); px.param.usage ()]);
      }
    }

    let (++) t1 t2 = app t1 t2 ~f:(fun f1 f2 x -> f2 (f1 x))
    let (+>) t1 p2 = app t1 p2.param ~f:(fun f1 f2 x -> f2 (f1 x))
    let (+<) t1 p2 = app p2.param t1 ~f:(fun f2 f1 x -> f1 (f2 x))

    let step f = {
      f = (fun _env -> return f);
      flags = (fun () -> []);
      usage = (fun () -> Anon.Grammar.zero);
    }

    let empty : 'm. ('m, 'm) t = {
      f = (fun _env -> return Fn.id);
      flags = (fun () -> []);
      usage = (fun () -> Anon.Grammar.zero);
    }

    let const v =
      { param =
        { f = (fun _env -> return (fun k -> k v));
          flags = (fun () -> []);
          usage = (fun () -> Anon.Grammar.zero); } }

    let map p ~f =
      { param =
        { f =
          (fun env -> p.param.f env >>= fun c -> return (fun k -> c (fun v -> k (f v))));
          flags = p.param.flags;
          usage = p.param.usage; } }

    let wrap f t =
      { f =
        (fun env -> t.f env >>= fun run -> return (fun main -> f ~run ~main));
        flags = t.flags;
        usage = t.usage; }

    let lookup key =
      { param =
        { f = (fun env -> return (fun m -> m (Env.find_exn env key)));
          flags = (fun () -> []);
          usage = (fun () -> Anon.Grammar.zero); } }

    let path : Path.t        param = lookup path_key
    let args : string list   param = lookup args_key
    let help : string Lazy.t param = lookup help_key

    let env =
      { param =
        { f = (fun env -> return (fun m -> m env));
          flags = (fun () -> []);
          usage = (fun () -> Anon.Grammar.zero); } }

    let pair p1 p2 = apply (apply (const (fun x y -> (x, y))) p1) p2

    include struct
      module Arg_type = Arg_type
      open Arg_type
      let string    = string
      let int       = int
      let float     = float
      let bool      = bool
      let date      = date
      let time      = time
      let time_ofday = time_ofday
      let time_ofday_unzoned = time_ofday_unzoned
      let time_span = time_span
      let file      = file Fn.id
    end

    include struct
      open Anon.Spec
      type 'a anons = 'a t
      let (%:) = (%:)
      let map_anons = map_anons
      let maybe = maybe
      let maybe_with_default = maybe_with_default
      let sequence = sequence
      let t2 = t2
      let t3 = t3
      let t4 = t4

      let anon spec = {
        param = {
          f = (fun _env -> spec.p >>= fun v -> return (fun k -> k v));
          flags = (fun () -> []);
          usage = (fun () -> spec.grammar);
        }
      }
    end

    include struct
      open Flag.Spec
      type 'a flag = 'a t
      let map_flag = map_flag
      let escape = escape
      let listed = listed
      let no_arg = no_arg
      let no_arg_register = no_arg_register
      let no_arg_abort = no_arg_abort
      let optional = optional
      let optional_with_default = optional_with_default
      let required = required

      let flag ?(aliases = []) name mode ~doc =
        let normalize flag = normalize Key_type.Flag flag in
        let name = normalize name in
        let aliases = List.map ~f:normalize aliases in
        let {read; action; optional} = mode name in
        let check_available =
          if optional then `Optional else `Required (fun () -> ignore (read ()))
        in
        { param =
          { f = (fun _env -> return (fun k -> k (read ())));
            flags = (fun () -> [{ Flag.name; aliases; doc; action; check_available }]);
            usage = (fun () -> Anon.Grammar.zero);
          }
        }
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
        end)

    module Deprecated = struct
      include Flag.Spec.Deprecated
      include Anon.Spec.Deprecated
    end

  end

end

type t =
  | Base of Base.t
  | Group of group

and group = {
  summary : string;
  readme : (unit -> string) option;
  subcommands : t String.Map.t
}

type ('main, 'result) basic_command
  =  summary:string
  -> ?readme:(unit -> string)
  -> ('main, unit -> 'result) Base.Spec.t
  -> 'main
  -> t

let get_summary = function
  | Base base -> Base.summary base
  | Group { summary; readme=_; subcommands=_ } -> summary

let group_help ~path ~summary ~readme subs =
  unparagraphs (List.filter_opt [
    Some summary;
    Some (String.concat ["  "; Path.to_string path; " SUBCOMMAND"]);
    Option.map readme ~f:(fun f -> f ());
    Some begin
      let subs = String.Map.to_alist subs in
      unparagraphs [
        "=== subcommands ===";
        Format.to_string
          (Format.sort
            (List.map subs ~f:(fun (name, summary) ->
              { Format. name; aliases = []; doc = summary })))
      ]
    end;
  ])

let extend_map_exn map key_type ~key data =
  if String.Map.mem map key then
    failwithf "there is already a %s named %s" (Key_type.to_string key_type) key ();
  String.Map.add map ~key ~data

module Bailout_dump_flag = struct
  let add base ~name ~aliases ~text ~text_summary =
    let flags = base.Base.flags in
    let flags =
      extend_map_exn flags Key_type.Flag ~key:name
        { Flag.
          name;
          aliases;
          check_available = `Optional;
          action = Flag.No_arg
            (fun env ->
              print_endline (text env);
              exit 0
            );
          doc = sprintf " print %s and exit" text_summary;
        }
    in
    { base with Base.flags }
end

let basic ~summary ?readme {Base.Spec.usage; flags; f} main =
  let flags = flags () in
  let usage = usage () in
  let anons env =
    let open Anon.Parser.For_opening in
    f env
    >>= fun k ->
    return (fun `Parse_args ->
      let thunk = k main in
      fun `Run_main -> thunk ())
  in
  let flags =
    match
      String.Map.of_alist (List.map flags ~f:(fun flag -> (flag.Flag.name, flag)))
    with
    | `Duplicate_key flag -> failwithf "multiple flags named %s" flag ()
    | `Ok map ->
      begin (* check for alias collision, too *)
        match
          String.Map.of_alist
            (List.concat_map flags ~f:(fun { Flag.name; Flag.aliases; action=_; doc=_; check_available=_ } ->
              (name, ()) :: List.map aliases ~f:(fun alias -> (alias, ()))))
        with
        | `Duplicate_key x -> failwithf "multiple flags or aliases named %s" x ()
        | `Ok _ -> ()
      end;
      map
  in
  let base = { Base.summary; readme; usage; flags; anons } in
  let base =
    Bailout_dump_flag.add base ~name:"-help" ~aliases:["-?"]
      ~text_summary:"this help text"
      ~text:(fun env -> Lazy.force (Env.find_exn env Base.help_key))
  in
  Base base

let subs_key : t String.Map.t Env.Key.t = Env.key_create "subcommands"

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
      let subs : t String.Map.t =
        match Env.find env subs_key with
        | Some subs -> subs
        | None -> assert false (* maintained by [dispatch] *)
      in
      let path =
        let path = Path.pop_help path in
        Option.fold cmd_opt ~init:path
          ~f:(fun path subcommand -> Path.add path ~subcommand)
      in
      let string_of_path =
        if expand_dots
        then Path.to_string
        else Path.to_string_dots
      in
      let rec gather_group rpath acc subs =
        let subs =
          if recursive && rpath <> Path.empty
          then String.Map.remove subs "help"
          else subs
        in
        let alist = String.Map.to_alist subs in
        let alist =
          List.sort alist ~cmp:(fun a b -> help_screen_compare (fst a) (fst b))
        in
        List.fold alist ~init:acc ~f:(fun acc (subcommand, t) ->
          let rpath = Path.add rpath ~subcommand in
          let key = string_of_path rpath in
          let doc = get_summary t in
          let acc = Fqueue.enqueue acc { Format.name = key; doc; aliases = [] } in
          if recursive
          then gather rpath acc t
          else acc)
      and
        gather rpath acc = function
          | Group { subcommands; summary=_; readme=_ } -> gather_group rpath acc subcommands
          | Base base ->
            if show_flags then begin
              Base.formatted_flags base
              |! List.filter ~f:(fun fmt -> fmt.Format.name <> "[-help]")
              |! List.fold ~init:acc ~f:(fun acc fmt ->
                  let rpath = Path.add rpath ~subcommand:fmt.Format.name in
                  let fmt = { fmt with Format.name = string_of_path rpath } in
                  Fqueue.enqueue acc fmt)
            end else
              acc
      in
      let group_help_text { readme; summary; subcommands } =
        let menu = Fqueue.to_list (gather_group Path.empty Fqueue.empty subcommands) in
        unparagraphs (List.filter_opt [
          Some summary;
          Some (String.concat ["  "; Path.to_string path; " SUBCOMMAND"]);
          Option.map readme ~f:(fun f -> f ());
          Some
            (if show_flags
             then "=== subcommands and flags ==="
             else "=== subcommands ===");
          Some (Format.to_string menu);
        ])
      in
      let text =
        match cmd_opt with
        | None -> group_help_text { readme; summary; subcommands = subs }
        | Some cmd ->
          match String.Map.find subs cmd with
          | None ->
            die "unknown subcommand %s for command %s" cmd (Path.to_string path) ()
          | Some t ->
            match t with
            | Group group -> group_help_text group
            | Base base -> Base.help_text ~path base
      in
      print_endline text)

let group ~summary ?readme alist =
  let alist =
    List.map alist ~f:(fun (name, t) -> (normalize Key_type.Subcommand name, t))
  in
  let subcommands =
    match String.Map.of_alist alist with
    | `Ok subs -> subs
    | `Duplicate_key name -> failwithf "multiple subcommands named %s" name ()
  in
  Group { summary; readme; subcommands }

INCLUDE "version_defaults.mlh"
module Version_info = struct
  let command ~version ~build_info =
    basic ~summary:"print version information"
      Base.Spec.(
        empty
        +> flag "-version" no_arg ~doc:" print the version of this build"
        +> flag "-build-info" no_arg ~doc:" print build info for this build"
      )
      (fun version_flag build_info_flag ->
        begin
          let print_version () =
            (* [version] was space delimited at some point and newline delimited
               at another.  We always print one (repo, revision) pair per line
               and ensure sorted order *)
            String.split version ~on:' '
            |! List.concat_map ~f:(String.split ~on:'\n')
            |! List.sort ~cmp:String.compare
            |! List.iter ~f:print_endline
          in
          let print_build_info () = print_endline build_info in
          if build_info_flag then print_build_info ()
          else if version_flag then print_version ()
          else (print_build_info (); print_version ())
        end;
        exit 0)

  let add
      ?(version = DEFAULT_VERSION)
      ?(build_info = DEFAULT_BUILDINFO)
      unversioned =
    match unversioned with
    | Base base ->
      let base =
        Bailout_dump_flag.add base ~name:"-version" ~aliases:[]
          ~text_summary:"the version of this build" ~text:(Fn.const version)
      in
      let base =
        Bailout_dump_flag.add base ~name:"-build-info" ~aliases:[]
          ~text_summary:"info about this build" ~text:(Fn.const build_info)
      in
      Base base
    | Group group ->
      let subcommands =
        extend_map_exn group.subcommands Key_type.Subcommand ~key:"version"
          (command ~version ~build_info)
      in
      Group { group with subcommands }

end

(* clear the setting of environment variable associated with command-line
   completion so that subprocesses don't see them. *)
let getenv_and_clear var =
  let value = Core_sys.getenv var in
  if Option.is_some value then Unix.unsetenv var;
  value

let dump_autocomplete_function () =
  let fname = sprintf "_jsautocom_%s" (Pid.to_string (Unix.getpid ())) in
  printf
"function %s {
  export COMP_CWORD
  COMP_WORDS[0]=%s
  COMPREPLY=($(\"${COMP_WORDS[@]}\"))
}
complete -F %s %s
%!" fname Sys.argv.(0) fname Sys.argv.(0)

let args_of_argv = function
  | [] -> failwith "missing executable name"
  | cmd :: args ->
    match getenv_and_clear "COMMAND_OUTPUT_INSTALLATION_BASH" with
    | Some _ ->
      dump_autocomplete_function ();
      exit 0
    | None ->
      ( Path.root cmd
      , match
          Option.bind (getenv_and_clear "COMP_CWORD") (fun i ->
            Option.try_with (fun () -> Int.of_string i))
        with
        | None -> Args.of_list args
        | Some i ->
          let args = List.take (args @ [""]) i in
          List.fold_right args ~init:Args.Nil ~f:(fun arg args ->
            match args with
            | Args.Nil -> Args.Complete arg
            | _ -> Args.Cons (arg, args))
      )

let rec add_help_subcommands = function
  | Base _ as t -> t
  | Group { summary; readme; subcommands } ->
    let subcommands = String.Map.map subcommands ~f:add_help_subcommands in
    let subcommands =
      extend_map_exn subcommands Key_type.Subcommand ~key:"help"
        (help_subcommand ~summary ~readme)
    in
    Group { summary; readme; subcommands }

let rec dispatch t env ~extend ~path ~args =
  match t with
  | Base base ->
    let args =
      match extend with
      | None -> args
      | Some extend -> Args.extend args ~extend ~path
    in
    Base.run base env ~path ~args
  | Group { summary; readme; subcommands = subs } ->
    let env = Env.set env subs_key subs in
    let die_showing_help msg =
      if not (Args.ends_in_complete args) then begin
        let subs = String.Map.map subs ~f:get_summary in
        eprintf "%s\n%!" (group_help ~path ~summary ~readme subs);
        die "%s" msg ()
      end
    in
    match args with
    | Args.Nil ->
      die_showing_help (sprintf "missing subcommand for command %s" (Path.to_string path))
    | Args.Cons (sub, rest) ->
      let (sub, rest) =
        match (sub, rest) with
        | ("-help", Args.Nil) ->
          let subs = String.Map.map subs ~f:get_summary in
          print_endline (group_help ~path ~summary ~readme subs);
          exit 0
        | ("-help", Args.Cons (sub, rest)) -> (sub, Args.Cons ("-help", rest))
        | _ -> (sub, rest)
      in
      begin
        match lookup_expand subs sub Key_type.Subcommand with
        | Error msg -> die_showing_help msg
        | Ok (sub, t) ->
          dispatch t env ~extend ~path:(Path.add path ~subcommand:sub) ~args:rest
      end
    | Args.Complete part ->
      List.iter (String.Map.keys subs) ~f:(fun name ->
        if String.is_prefix name ~prefix:part then print_endline name);
      exit 0

let run ?version ?build_info ?(argv=Array.to_list Sys.argv) ?extend t =
  Exn.handle_uncaught ~exit:true (fun () ->
    let t = Version_info.add t ?version ?build_info in
    let t = add_help_subcommands t in
    let (path, args) = args_of_argv argv in
    try
      dispatch t Env.empty ~extend ~path ~args
    with
    | Failed_to_parse_command_line msg ->
      if Args.ends_in_complete args then
        exit 0
      else begin
        prerr_endline msg;
        exit 1
      end)

module Spec = struct
  include Base.Spec
  let path = map ~f:Path.commands path
end

module Deprecated = struct

  module Spec = Spec.Deprecated

  let summary = get_summary

  let get_flag_names = function
    | Base base -> base.Base.flags |! String.Map.keys
    | Group _ -> assert false

  let help_recursive ~cmd ~with_flags ~expand_dots t s =
    let rec help_recursive_rec ~cmd t s =
      let new_s = s ^ (if expand_dots then cmd else ".") ^ " " in
      match t with
      | Base base ->
        let base_help = s ^ cmd, summary (Base base) in
        if with_flags then
          base_help ::
            List.map ~f:(fun (flag, h) -> (new_s ^ flag, h))
              (List.sort ~cmp:Base.Deprecated.subcommand_cmp_fst
                (Base.Deprecated.flags_help ~display_help_flags:false base))
        else
          [base_help]
      | Group { summary; subcommands; readme=_ } ->
        (s ^ cmd, summary)
        :: begin
          String.Map.to_alist subcommands
          |! List.sort ~cmp:Base.Deprecated.subcommand_cmp_fst
          |! List.concat_map ~f:(fun (cmd', t) ->
              help_recursive_rec ~cmd:cmd' t new_s)
        end
    in
    help_recursive_rec ~cmd t s

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
    let args = Args.of_list args in
    let t = add_help_subcommands t in
    dispatch t Env.empty ~path ~args ~extend:None

  let version = DEFAULT_VERSION
  let build_info = DEFAULT_BUILDINFO
end

(* testing claims made in the mli about order of evaluation and [flags_of_args_exn] *)
TEST_MODULE "Command.Spec.flags_of_args_exn" = struct

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

  TEST = parse ["foo.exe";"-flag1";"-flag2";"-flag3"] = [1;2;3]
  TEST = parse ["foo.exe";"-flag2";"-flag3";"-flag1"] = [1;2;3]
  TEST = parse ["foo.exe";"-flag3";"-flag2";"-flag1"] = [1;2;3]

end
